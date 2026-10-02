//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using System.Xml;

using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.Properties;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Threading;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.ProjectSystem.LanguageService
{
    /// <summary>
    /// Feeds the X# code model (<see cref="XProject"/>) of a project that is loaded by the CPS project system.
    /// It replaces the data feeding that XSharpProjectNode (MPFproj) performs: hierarchy walking and the parsing
    /// of the response file after a real build (spec WP-5, Part B).
    /// </summary>
    /// <remarks>
    /// One instance per UnconfiguredProject. The data comes from the active configuration
    /// (IActiveConfiguredProjectSubscriptionService), so there is one XProject per project, as with MPFproj.
    /// Three subscriptions:
    /// <list type="bullet">
    /// <item>source items (evaluation) -> XProject.AddFile / RemoveFile</item>
    /// <item>XSharpProjectProperties + EvaluatedProjectReference (evaluation) -> RootNamespace, output paths,
    ///       provisional parse options (dialect) and X# project references</item>
    /// <item>XSharpCompilerCommandLineArgs (design-time build) -> XParseOptions and assembly references</item>
    /// </list>
    /// Because the design-time build does not run the compiler, parse options and references are available
    /// before the first real build.
    /// </remarks>
    [DebuggerDisplay("{" + nameof(DisplayName) + ",nq}")]
    [Export(ExportContractNames.Scopes.UnconfiguredProject, typeof(IProjectDynamicLoadComponent))]
    [AppliesTo(XSharpCapabilities.XSharpCps)]
    internal sealed class XSharpProjectAdapter : IProjectDynamicLoadComponent, IXSharpProject
    {
        private const string ProjectPropertiesRule = "XSharpProjectProperties";
        private const string ProjectReferenceRule = "EvaluatedProjectReference";
        private const string CommandLineRule = "XSharpCompilerCommandLineArgs";

        /// <summary>
        /// Item types whose items are registered with the code model. XProject.AddFile determines the file type
        /// from the extension, non source files end up in the "other files" of the project, as with MPFproj.
        /// </summary>
        private static readonly HashSet<string> FileItemTypes = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "Compile", "None", "Content", "EmbeddedResource", "Page", "ApplicationDefinition", "Resource", "VOBinary", "NativeResource",
        };

        private readonly UnconfiguredProject project;
        private readonly IActiveConfiguredProjectSubscriptionService subscriptions;
        private readonly IActiveConfigurationGroupService configurationGroups;
        private readonly IProjectThreadingService threading;
        private readonly IntellisenseErrorStore errors = new IntellisenseErrorStore();
        private readonly HashSet<string> files = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        private readonly HashSet<string> projectReferences = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        // One design-time build subscription per TargetFramework of a cross targeting project (TargetFrameworks),
        // keyed by target framework. Recreated when the active configuration group changes.
        private readonly Dictionary<string, IDisposable> targetLinks = new Dictionary<string, IDisposable>(StringComparer.OrdinalIgnoreCase);
        // The command line of the last successful design-time build per TargetFramework.
        private readonly Dictionary<string, XSharpCommandLine> commandLines = new Dictionary<string, XSharpCommandLine>(StringComparer.OrdinalIgnoreCase);
        // Two locks, always taken in this order (modelGate, then gate):
        // - modelGate serializes the changes of the code model (the three subscriptions, AddFileNode/DeleteFileNode,
        //   load/unload); XProject.AddFile/RemoveFile do database I/O.
        // - gate only protects the file set and the model field, briefly, so HasFileNode (called by the VO designers on
        //   the UI thread) never waits for that I/O.
        private readonly object modelGate = new object();
        private readonly object gate = new object();
        private bool parseOptionsFromCommandLine;

        // One code model per TargetFramework: every target has its own compiler options and assembly references,
        // so the types of e.g. net48 and net8.0-windows do not end up in the same model.
        private readonly Dictionary<string, XProject> models = new Dictionary<string, XProject>(StringComparer.OrdinalIgnoreCase);
        // The model of the active configuration: the one the editor, the designers and IXSharpProject use.
        // Null for the target frameworks of a cross targeting project until the configuration group has arrived.
        private XProject model;
        private string primaryTargetFramework;

        private List<IDisposable> links;
        private IntellisenseErrorList errorList;
        private CommentTaskList taskList;
        private SourceFileWatcher sourceWatcher;
        private XParseOptions parseOptions = XParseOptions.Default;
        private string rootNamespace = "";
        private string intermediateOutputPath = "";
        private string outputFile = "";
        private bool prefixClassesWithDefaultNamespace;

        [ImportingConstructor]
        public XSharpProjectAdapter(
            UnconfiguredProject project,
            IActiveConfiguredProjectSubscriptionService subscriptions,
            IActiveConfigurationGroupService configurationGroups,
            IProjectThreadingService threading)
        {
            this.project = project;
            this.subscriptions = subscriptions;
            this.configurationGroups = configurationGroups;
            this.threading = threading;
        }

        internal XProject Model => model;
        /// <summary>
        /// The code model of a TargetFramework, or the primary model when <paramref name="targetFramework"/> is empty
        /// or unknown.
        /// </summary>
        internal XProject GetModel(string targetFramework)
        {
            lock (modelGate)
            {
                if (IsCrossTargeting(targetFramework) && models.TryGetValue(targetFramework, out var target))
                    return target;
                return model;
            }
        }
        /// <summary>
        /// The code models per TargetFramework of a cross targeting project.
        /// </summary>
        internal IReadOnlyDictionary<string, XProject> ModelsPerTarget
        {
            get
            {
                lock (modelGate)
                {
                    return models.Where(p => IsCrossTargeting(p.Key))
                        .ToDictionary(p => p.Key, p => p.Value, StringComparer.OrdinalIgnoreCase);
                }
            }
        }
        private string ProjectFolder => Path.GetDirectoryName(project.FullPath);

        #region IProjectDynamicLoadComponent

        public async Task LoadAsync()
        {
            if (!await EnsureSolutionIsOpenAsync())
            {
                XSettings.Information("XSharpProjectAdapter: X# solution model is not open, no code model for " + project.FullPath);
                return;
            }
            // Create the code model on the UI thread, like MPFproj: the projects of a solution load in parallel, and
            // creating two XProjects at the same time on background threads raced in the code model (the lazily
            // created OrphanedFiles project was added twice: SQLite "FOREIGN KEY constraint failed" with a new
            // X# database, verified in VS).
            await threading.JoinableTaskFactory.SwitchToMainThreadAsync();
            // Before the first walk, so the parser finds the comment tasks (TODO etc.)
            CommentTokens.EnsureSet();
            lock (modelGate)
            {
                if (model != null)
                    return;
                // The primary model (no target framework yet): for a project with a single TargetFramework this is
                // the only model. For a cross targeting project the configuration group renames/replaces it below.
                XmlDocument xDocument = new XmlDocument();
                xDocument.Load(project.FullPath);
                var xElement = xDocument.DocumentElement?.GetElementsByTagName("TargetFrameworks").Cast<XmlElement>().LastOrDefault();
                string frameworks = "";
                if (xElement != null)
                    frameworks = xElement.InnerText;
                xDocument = null;
                xElement = null;

                XProject newModel;
                if (!string.IsNullOrEmpty(frameworks))
                {
                    var frameworksArray = frameworks.Split(';');
                    var first = frameworksArray.FirstOrDefault();
                    foreach (var framework in frameworksArray)
                    {
                        if (!string.IsNullOrEmpty(framework))
                            CreateModel(framework);
                    }
                    newModel = GetModel(first);
                }
                else
                {
                    newModel = CreateModel(SingleTarget);
                }
                lock (gate)
                {
                    model = newModel;
                }
                newModel.ProjectWalkComplete += OnProjectWalkComplete;
                newModel.FileWalkComplete += OnFileWalkComplete;
                lock (gate)
                {
                    model = newModel;
                }
            }
            XSettings.Information("XSharpProjectAdapter: created code model for " + project.FullPath);
            // Forms added with "Add New Item" open in the shadow designer instead of the code editor
            ShadowDesigner.NewFormDesignerRedirect.EnsureAdvised(threading.JoinableTaskFactory);
            // Source files that change on disk (editor saves, VO designers, external tools) are walked again, like
            // XSharpProjectNode.OnFileChanged
            sourceWatcher = new SourceFileWatcher(ProjectFolder, IsSourceFileOfProject, WalkChangedFile, WalkProject);
            errorList = new IntellisenseErrorList(threading.JoinableTaskFactory, DisplayName, errors.GetAll);
            errors.Changed = errorList.Update;
            taskList = new CommentTaskList(threading.JoinableTaskFactory, DisplayName);

            var linkOptions = new DataflowLinkOptions { PropagateCompletion = true };
            links = new List<IDisposable>
            {
                // The source item rules are dynamic (one per item type): take the rule names from
                // SourceItemRuleNamesSource. LinkTo without rule names subscribes to no rules at all.
                subscriptions.SourceItemsRuleSource.SourceBlock.LinkTo(
                    DataflowBlockSlim.CreateActionBlock<IProjectVersionedValue<IProjectSubscriptionUpdate>>(OnSourceItemsChanged),
                    subscriptions.SourceItemRuleNamesSource.SourceBlock,
                    linkOptions),
                subscriptions.ProjectRuleSource.SourceBlock.LinkTo(
                    DataflowBlockSlim.CreateActionBlock<IProjectVersionedValue<IProjectSubscriptionUpdate>>(OnEvaluationChanged),
                    linkOptions,
                    ruleNames: new[] { ProjectPropertiesRule, ProjectReferenceRule }),
                // Feeds the code model: the active configuration (the first TargetFramework of a cross targeting project)
                subscriptions.ProjectBuildRuleSource.SourceBlock.LinkTo(
                    DataflowBlockSlim.CreateActionBlock<IProjectVersionedValue<IProjectSubscriptionUpdate>>(OnDesignTimeBuildChanged),
                    linkOptions,
                    ruleNames: new[] { CommandLineRule }),
                // And one subscription per TargetFramework, so a design-time build runs for each of them
                configurationGroups.ActiveConfigurationGroupSource.SourceBlock.LinkTo(
                    DataflowBlockSlim.CreateActionBlock<IProjectVersionedValue<IConfigurationGroup<ProjectConfiguration>>>(OnConfigurationGroupChanged),
                    linkOptions),
            };
        }

        /// <summary>
        /// Creates a code model for one TargetFramework (null for a project that does not cross target) and
        /// registers the files that are already known.
        /// </summary>
        /// <remarks>Call under <see cref="modelGate"/>.</remarks>
        private XProject CreateModel(string targetFramework)
        {
            var newModel = IsCrossTargeting(targetFramework)
                ? new XProject(this, targetFramework, this.Url)
                : new XProject(this);
            newModel.ProjectWalkComplete += OnProjectWalkComplete;
            newModel.FileWalkComplete += OnFileWalkComplete;
            newModel.ResetParseOptions(parseOptions);
            List<string> known;
            lock (gate)
            {
                known = files.ToList();
            }
            foreach (var file in known)
                newModel.AddFile(file);
            foreach (var reference in projectReferences)
                newModel.AddProjectReference(reference);
            models[targetFramework] = newModel;
            XSettings.Information($"XSharpProjectAdapter: created code model for {project.FullPath}" +
                (IsCrossTargeting(targetFramework) ? " (" + targetFramework + ")" : ""));
            return newModel;
        }


        /// <summary>
        /// Closes a model: unhook the events, then <see cref="XProject.Close"/> outside the locks.
        /// </summary>
        /// <remarks>Call under <see cref="modelGate"/>; close the returned model outside the locks.</remarks>
        private XProject DetachModel(XProject toClose)
        {
            if (toClose == null)
                return null;
            toClose.ProjectWalkComplete -= OnProjectWalkComplete;
            toClose.FileWalkComplete -= OnFileWalkComplete;
            return toClose;
        }
        /// <summary>
        /// All code models: the primary one and, for a cross targeting project, the one of every TargetFramework.
        /// </summary>
        /// <remarks>Call under <see cref="modelGate"/>.</remarks>
        private List<XProject> AllModels()
        {
            var all = new List<XProject>(models.Values);
            if (model != null && !all.Contains(model))
                all.Add(model);
            return all;
        }
        public Task UnloadAsync()
        {
            List<XProject> oldModels;
            lock (modelGate)
            {
                if(links != null)
                {
                    foreach (var link in links)
                        link.Dispose();
                    links = null;
                }
                foreach (var link in targetLinks.Values)
                    link.Dispose();
                targetLinks.Clear();
                if (model != null)
                {
                    model.ProjectWalkComplete -= OnProjectWalkComplete;
                    model.FileWalkComplete -= OnFileWalkComplete;
                }
                oldModels = AllModels().Select(DetachModel).ToList();
                models.Clear();
                primaryTargetFramework = null;
                lock (gate)
                {
                    model = null;
                    files.Clear();
                }
                errors.Changed = null;
                sourceWatcher?.Dispose();
                sourceWatcher = null;
                errorList?.Dispose();
                errorList = null;
                taskList?.Dispose();
                taskList = null;
                projectReferences.Clear();
            }
            foreach (var old in oldModels)
                old?.Close();
            return Task.CompletedTask;
        }

        /// <summary>
        /// Makes sure that the X# solution model (database) is open before the code model is created.
        /// </summary>
        /// <remarks>
        /// The X# project package opens it in OnBeforeOpenSolution. For solutions with the MPFproj project type
        /// VS loads that package before the solution opens (it provides the project factory). For the CPS project
        /// type it does not: the X# package is only loaded later (autoload), misses OnBeforeOpenSolution and does
        /// not see the solution yet while it is still opening, so the database was never opened.
        /// Load the X# project package (shell link, logging, settings) and open the database for the solution
        /// file that is being loaded, like XSharpShellLink does in OnBeforeOpenSolution.
        /// The solution file does not have to exist yet: a project that is opened without a solution
        /// (devenv project.xsproj, File > Open > Project) or a new project lives in a solution that VS only writes
        /// when it is saved. XSolution.Open only needs the folder and the name (database in .vs\{name}), so the
        /// database is opened for the future solution file; without a solution file name the project file is used.
        /// </remarks>
        private async Task<bool> EnsureSolutionIsOpenAsync()
        {
            if (XSolution.IsOpen)
                return true;
            await threading.JoinableTaskFactory.SwitchToMainThreadAsync();
            if (ServiceProvider.GlobalProvider.GetService(typeof(SVsShell)) is IVsShell shell)
            {
                var packageGuid = new Guid(XSharpConstants.guidXSharpProjectPkgString);
                if (shell is IVsShell7 shell7)
                    await shell7.LoadPackageAsync(ref packageGuid);
                else
                    shell.LoadPackage(ref packageGuid, out _);
                await threading.JoinableTaskFactory.SwitchToMainThreadAsync();
            }
            if (!XSolution.IsOpen)
            {
                string solutionFile = null;
                if (ServiceProvider.GlobalProvider.GetService(typeof(SVsSolution)) is IVsSolution solution &&
                    solution.GetSolutionInfo(out _, out var file, out _) == 0)
                {
                    solutionFile = file;
                }
                if (string.IsNullOrEmpty(solutionFile))
                    solutionFile = Path.ChangeExtension(project.FullPath, ".sln");
                XSettings.Information("XSharpProjectAdapter: opening the X# solution model for " + solutionFile +
                    (File.Exists(solutionFile) ? "" : " (solution file not saved yet)"));
                XSolution.Open(solutionFile);
            }
            return XSolution.IsOpen;
        }

        #endregion

        #region Subscriptions
        private void OnDesignTimeBuildChanged(IProjectVersionedValue<IProjectSubscriptionUpdate> update)
        {
            try
            {
                lock (modelGate)
                {
                    // Served by the per-configuration subscription (OnTargetDesignTimeBuildChanged) as soon as it exists
                    if (primaryTargetFramework != null)
                        return;
                }
                var commandLine = ParseCommandLine(update);
                if (commandLine == null)
                {
                    // No changes, or a failed design-time build (no arguments): keep the previous options and references.
                    return;
                }
                var options = XParseOptions.FromVsValues(commandLine.ParseOptions);
                XProject current;
                lock (modelGate)
                {
                    current = model;
                    if (current == null)
                        return;
                    parseOptions = options;
                    parseOptionsFromCommandLine = true;
                    current.ResetParseOptions(options);
                    current.RefreshReferences(commandLine.References);
                    foreach (var reference in projectReferences)
                        current.AddProjectReference(reference);
                }
                // Loads the referenced assemblies: outside the locks. XProject guards it itself (_resolvingReferences),
                // and the ModelWalker and the type lookups call it without these locks as well.
                current.ResolveReferences();
                XSettings.Information($"XSharpProjectAdapter: {project.FullPath}: design-time build, {commandLine.ParseOptions.Count} options, {commandLine.References.Count} references, dialect {options.Dialect}");
                WalkProject();
            }
            catch (Exception e)
            {
                XSettings.Exception(e);
            }
        }

        private void OnSourceItemsChanged(IProjectVersionedValue<IProjectSubscriptionUpdate> update)
        {
            try
            {
                var added = new List<string>();
                var removed = new List<string>();
                lock (modelGate)
                {
                    var current = model;
                    if (current == null)
                        return;
                    foreach (var change in update.Value.ProjectChanges)
                    {
                        if (!FileItemTypes.Contains(change.Key) || !change.Value.Difference.AnyChanges)
                            continue;
                        var diff = change.Value.Difference;
                        foreach (var item in diff.RemovedItems)
                            removed.Add(XSharpCommandLine.MakeFullPath(item, ProjectFolder));
                        foreach (var item in diff.AddedItems)
                            added.Add(XSharpCommandLine.MakeFullPath(item, ProjectFolder));
                        foreach (var rename in diff.RenamedItems)
                        {
                            removed.Add(XSharpCommandLine.MakeFullPath(rename.Key, ProjectFolder));
                            added.Add(XSharpCommandLine.MakeFullPath(rename.Value, ProjectFolder));
                        }
                    }
                    List<string> toRemove, toAdd;
                    lock (gate)
                    {
                        toRemove = removed.Where(files.Remove).ToList();
                        toAdd = added.Where(files.Add).ToList();
                    }
                    // Database I/O, outside gate
                    foreach (var target in AllModels())
                    {
                        foreach (var file in toRemove)
                            target.RemoveFile(file);
                        foreach (var file in toAdd)
                            target.AddFile(file);
                    }
                }
                if (added.Count > 0 || removed.Count > 0)
                {
                    XSettings.Information($"XSharpProjectAdapter: {project.FullPath}: {added.Count} files added, {removed.Count} files removed");
                    WalkProject();
                }
            }
            catch (Exception e)
            {
                XSettings.Exception(e);
            }
        }

        private void OnEvaluationChanged(IProjectVersionedValue<IProjectSubscriptionUpdate> update)
        {
            try
            {
                var changes = update.Value.ProjectChanges;
                lock (modelGate)
                {
                    if (model == null)
                        return;
                    if (changes.TryGetValue(ProjectPropertiesRule, out var properties) && properties.Difference.AnyChanges)
                    {
                        ReadProjectProperties(properties.After);
                        XSettings.Information($"XSharpProjectAdapter: {project.FullPath}: properties RootNamespace={rootNamespace}, TargetPath={outputFile}, NS={prefixClassesWithDefaultNamespace}");
                    }
                    if (changes.TryGetValue(ProjectReferenceRule, out var references) && references.Difference.AnyChanges)
                    {
                        UpdateProjectReferences(references.After);
                    }
                }
            }
            catch (Exception e)
            {
                XSettings.Exception(e);
            }
        }

        /// <summary>
        /// Design-time build of one TargetFramework. Only collects the command line: the code model has a single set
        /// of parse options and references, which <see cref="OnDesignTimeBuildChanged"/> feeds from the active
        /// configuration.
        /// </summary>
        private void OnTargetDesignTimeBuildChanged(string targetFramework, IProjectVersionedValue<IProjectSubscriptionUpdate> update)
        {
            try
            {
                var commandLine = ParseCommandLine(update);
                if (commandLine == null)
                    return;
                var options = XParseOptions.FromVsValues(commandLine.ParseOptions);
                XProject target;
                lock (modelGate)
                {
                    if (model == null || !models.TryGetValue(targetFramework, out target))
                        return;
                    commandLines[targetFramework] = commandLine;
                    if (string.Equals(targetFramework, primaryTargetFramework, StringComparison.OrdinalIgnoreCase))
                    {
                        // The active configuration also drives IXSharpProject.ParseOptions
                        parseOptions = options;
                        parseOptionsFromCommandLine = true;
                    }
                    target.ResetParseOptions(options);
                    target.RefreshReferences(commandLine.References);
                    foreach (var reference in projectReferences)
                        target.AddProjectReference(reference);
                }
                // Loads the referenced assemblies: outside the locks. XProject guards it itself (_resolvingReferences).
                target.ResolveReferences();
                XSettings.Information($"XSharpProjectAdapter: {project.FullPath}" +
                     (IsCrossTargeting(targetFramework) ? " (" + targetFramework + ")" : "") +
                     $": design-time build, {commandLine.ParseOptions.Count} options, " +
                     $"{commandLine.References.Count} references, dialect {options.Dialect}");
                ModelWalker.AddProject(target);
            }
            catch (Exception e)
            {
                XSettings.Exception(e);
            }
        }

        /// <summary>
        /// Key for the single model of a project that does not cross target (no TargetFramework dimension).
        /// </summary>
        private const string SingleTarget = "";
        private static string GetTargetFramework(ProjectConfiguration configuration)
        {
            return configuration.Dimensions.TryGetValue("TargetFramework", out var target) && !string.IsNullOrEmpty(target)
                ? target : SingleTarget;
        }
        private static bool IsCrossTargeting(string targetFramework) => !string.IsNullOrEmpty(targetFramework);

        /// <summary>
        /// The command line of a design-time build, or null when there are no changes or the build failed
        /// (a failed design-time build returns no arguments).
        /// </summary>
        private XSharpCommandLine ParseCommandLine(IProjectVersionedValue<IProjectSubscriptionUpdate> update)
        {
            if (!update.Value.ProjectChanges.TryGetValue(CommandLineRule, out var change) || !change.Difference.AnyChanges)
                return null;
            var arguments = change.After.Items.Keys.ToList();
            if (arguments.Count == 0)
                return null;
            return XSharpCommandLine.Parse(arguments, ProjectFolder);
        }

        /// <summary>
        /// The compiler options and references per TargetFramework, from the design-time builds that have completed.
        /// </summary>
        internal IReadOnlyDictionary<string, XSharpCommandLine> CommandLinesPerTarget
        {
            get
            {
                lock (modelGate)
                {
                    return new Dictionary<string, XSharpCommandLine>(commandLines, StringComparer.OrdinalIgnoreCase);
                }
            }
        }      /// <summary>
               /// The active configuration group contains one <see cref="ProjectConfiguration"/> per TargetFramework of a
               /// cross targeting project (TargetFrameworks) and a single one for a project with one TargetFramework.
               /// It fires again when the user switches Configuration/Platform or when TargetFrameworks is edited.
               /// </summary>
               /// <remarks>
               /// CPS only runs a design-time build for a ConfiguredProject that has a subscriber on its build rule source.
               /// <see cref="IActiveConfiguredProjectSubscriptionService"/> only covers the active configuration (the first
               /// TargetFramework), so the other targets need their own subscription to produce compiler options and references.
               /// </remarks>
        private void OnConfigurationGroupChanged(IProjectVersionedValue<IConfigurationGroup<ProjectConfiguration>> update)
        {
            // Fire and forget: LoadConfiguredProjectAsync may not block this dataflow action (and must not block
            // the UI thread). Exceptions are logged in the task itself.
            threading.JoinableTaskFactory.RunAsync(() => UpdateTargetSubscriptionsAsync(update.Value)).Task.Forget();
        }
        private async Task UpdateTargetSubscriptionsAsync(IConfigurationGroup<ProjectConfiguration> configurations)
        {
            try
            {
                var wanted = new Dictionary<string, ProjectConfiguration>(StringComparer.OrdinalIgnoreCase);
                foreach (var configuration in configurations)
                    wanted[GetTargetFramework(configuration)] = configuration;

                // The group fires more than once: the first time often for the configuration without a
                // TargetFramework dimension (key SingleTarget), only afterwards for the real target frameworks of a
                // cross targeting project. Hand the primary model over to the first target framework then, instead of
                // keeping a SingleTarget model next to the models of the targets.
                RekeyPrimaryModel(wanted.Keys.ToList());

                // Drop the targets that no longer exist
                var obsolete = new List<IDisposable>();
                var closing = new List<XProject>();
                lock (modelGate)
                {
                    foreach (var target in targetLinks.Keys.Where(t => !wanted.ContainsKey(t)).ToList())
                    {
                        obsolete.Add(targetLinks[target]);
                        targetLinks.Remove(target);
                    }
                    foreach (var target in models.Keys.Where(t => !wanted.ContainsKey(t)).ToList())
                    {
                        var old = models[target];
                        models.Remove(target);
                        commandLines.Remove(target);
                        // The primary model is closed in UnloadAsync, never here
                        if (!ReferenceEquals(old, model))
                            closing.Add(DetachModel(old)); closing.Add(DetachModel(old));

                    }
                }
                foreach (var link in obsolete)
                    link.Dispose();
                foreach (var old in closing)
                    old?.Close();

                foreach (var pair in wanted)
                {
                    lock (modelGate)
                    {
                        if (model == null)
                            return;
                        if (targetLinks.ContainsKey(pair.Key))
                            continue;
                    }
                    // Loading the ConfiguredProject and subscribing to its build rule source is what makes CPS
                    // schedule a design-time build for this configuration.
                    var configured = await project.LoadConfiguredProjectAsync(pair.Value).ConfigureAwait(false);
                    var buildSource = configured?.Services.ProjectSubscription?.ProjectBuildRuleSource;
                    if (buildSource == null)
                        continue;
                    var targetFramework = pair.Key;
                    var link = buildSource.SourceBlock.LinkTo(
                        DataflowBlockSlim.CreateActionBlock<IProjectVersionedValue<IProjectSubscriptionUpdate>>(
                            u => OnTargetDesignTimeBuildChanged(targetFramework, u)),
                        new DataflowLinkOptions { PropagateCompletion = true },
                        ruleNames: new[] { CommandLineRule });
                    bool keep;
                    lock (modelGate)
                    {
                        keep = model != null && !targetLinks.ContainsKey(targetFramework);
                        lock (modelGate)
                        {
                            keep = model != null && !targetLinks.ContainsKey(targetFramework);
                            if (keep)
                            {
                                targetLinks[targetFramework] = link;
                                if (!models.ContainsKey(targetFramework))
                                    CreateModel(targetFramework);
                            }
                        }
                    }
                    if (!keep)
                        link.Dispose();
                }
            }
            catch (Exception e)
            {
                XSettings.Exception(e);
            }
        }
        /// <summary>
        /// Makes sure that the primary model (the one that <see cref="LoadAsync"/> created) is registered under the
        /// key of the first configuration of <paramref name="targets"/>, and under that key only.
        /// </summary>
        /// <remarks>
        /// A project switches between a single TargetFramework and TargetFrameworks (and the group fires before the
        /// target frameworks are known), so the key of the primary model changes: from SingleTarget to the first
        /// target framework, or back. Without this the SingleTarget model stays behind in the dictionary next to the
        /// models of the target frameworks.
        /// Call outside <see cref="modelGate"/>: the model of an obsolete key is closed here.
        /// </remarks>
        private void RekeyPrimaryModel(IList<string> targets)
        {
            if (targets.Count == 0)
                return;
            // Keep the current key when it is still one of the targets: the editor and the designers keep working
            // with the same XProject.
            var newKey = primaryTargetFramework != null && targets.Contains(primaryTargetFramework, StringComparer.OrdinalIgnoreCase)
                ? primaryTargetFramework : targets[0];
            var closing = new List<XProject>();
            lock (modelGate)
            {
                if (model == null)
                    return;
                if (string.Equals(primaryTargetFramework, newKey, StringComparison.OrdinalIgnoreCase) &&
                    models.TryGetValue(newKey, out var registered) && ReferenceEquals(registered, model))
                {
                    return;
                }
                // Remove the primary model from the key it had (SingleTarget, or a target framework that is gone)
                foreach (var key in models.Where(p => ReferenceEquals(p.Value, model)).Select(p => p.Key).ToList())
                {
                    models.Remove(key);
                    commandLines.Remove(key);
                    if (targetLinks.TryGetValue(key, out var link))
                    {
                        targetLinks.Remove(key);
                        link.Dispose();
                    }
                }
                // XProject takes its TargetFramework in the constructor: replace the primary model instead of
                // re-keying it when the project turns out to cross target.
                if (IsCrossTargeting(newKey))
                {
                    closing.Add(DetachModel(model));
                    var replacement = CreateModel(newKey);   // registers itself in models
                    lock (gate)
                    {
                        model = replacement;
                    }
                }
                else
                {
                    models[newKey] = model;
                }
                primaryTargetFramework = newKey;
                XSettings.Information($"XSharpProjectAdapter: {project.FullPath}: primary code model is " +
                    (IsCrossTargeting(newKey) ? newKey : "the single target"));
            }
            foreach (var old in closing)
                old?.Close();
        }
        private void ReadProjectProperties(IProjectRuleSnapshot snapshot)
        {
            rootNamespace = GetProperty(snapshot, "RootNamespace");
            intermediateOutputPath = GetProperty(snapshot, "IntermediateOutputPath");
            if (intermediateOutputPath.Length > 0)
                intermediateOutputPath = XSharpCommandLine.MakeFullPath(intermediateOutputPath, ProjectFolder);
            outputFile = GetProperty(snapshot, "TargetPath");
            prefixClassesWithDefaultNamespace = string.Equals(GetProperty(snapshot, "NS"), "true", StringComparison.OrdinalIgnoreCase);
            if (!parseOptionsFromCommandLine)
            {
                // Provisional options until the first design-time build has returned the command line
                var dialect = GetProperty(snapshot, "Dialect");
                var options = new List<string> { "dialect:" + (dialect.Length > 0 ? dialect : "Core"), "i:" + XParseOptions.DefaultIncludeDir };
                parseOptions = XParseOptions.FromVsValues(options);
                foreach (var target in AllModels())
                    target.ResetParseOptions(parseOptions);
            }
        }

        private void UpdateProjectReferences(IProjectRuleSnapshot snapshot)
        {
            // X# -> X# project references: the code model reads the types from the referenced XProject (source).
            // References to other projects arrive as /reference: to their output assembly in the command line.
            var current = new HashSet<string>(
                snapshot.Items.Keys
                    .Select(item => XSharpCommandLine.MakeFullPath(item, ProjectFolder))
                    .Where(XProject.IsXSharpProject),
                StringComparer.OrdinalIgnoreCase);
            foreach (var removed in projectReferences.Where(r => !current.Contains(r)).ToList())
            {
                projectReferences.Remove(removed);
                foreach (var target in AllModels())
                    target.RemoveProjectReference(removed);
            }
            foreach (var added in current.Where(r => !projectReferences.Contains(r)).ToList())
            {
                projectReferences.Add(added);
                foreach (var target in AllModels())
                    target.AddProjectReference(added);
            }
        }

        private static string GetProperty(IProjectRuleSnapshot snapshot, string name)
        {
            return snapshot.Properties.TryGetValue(name, out var value) && value != null ? value : "";
        }

        // Like XSharpProjectNode.OnProjectWalkComplete/OnFileWalkComplete: refresh the comment tasks
        private void OnProjectWalkComplete(XProject xProject) => RefreshCommentTasks();

        private void OnFileWalkComplete(XFile xFile) => RefreshCommentTasks();

        private void RefreshCommentTasks()
        {
            try
            {
                var current = model;
                var tasks = taskList;
                if (current != null && tasks != null)
                    tasks.Update(current.GetCommentTasks());
            }
            catch (Exception e)
            {
                XSettings.Exception(e);
            }
        }

        /// <summary>
        /// For <see cref="SourceFileWatcher"/> (its thread): <paramref name="path"/> is a source file of this project.
        /// </summary>
        private bool IsSourceFileOfProject(string path)
        {
            XProject current;
            lock (gate)
            {
                current = model;
                if (current == null || !files.Contains(path))
                    return false;
            }
            return current.FindXFile(path)?.IsSource == true;
        }

        /// <summary>
        /// For <see cref="SourceFileWatcher"/> (background thread): walk a changed source file again. notify: true
        /// raises FileWalkComplete, which refreshes the Task List.
        /// </summary>
        private void WalkChangedFile(string path)
        {
            var current = model;
            var file = current?.FindXFile(path);
            if (file == null || !file.IsSource)
                return;
            XSettings.Information("XSharpProjectAdapter: " + path + " changed on disk, walking it again");
            current.WalkFile(file, true);
        }

        private void WalkProject()
        {
            List<XProject> all;
            lock (modelGate)
            {
                all = AllModels();
            }
            foreach (var target in all)
                ModelWalker.AddProject(target);
        }

        #endregion

        #region IXSharpProject

        public string DisplayName => Path.GetFileNameWithoutExtension(project.FullPath);
        public string IntermediateOutputPath => intermediateOutputPath;
        public string OutputFile => outputFile;
        public XParseOptions ParseOptions => parseOptions;
        public bool PrefixClassesWithDefaultNamespace => prefixClassesWithDefaultNamespace;
        public string RootNameSpace => rootNamespace;
        public string Url => project.FullPath;

        public bool HasFileNode(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                return false;
            var fullPath = XSharpCommandLine.MakeFullPath(fileName, ProjectFolder);
            lock (gate)
            {
                return files.Contains(fullPath);
            }
        }

        /// <summary>
        /// Called by the VO designers (XSharpVoEditors, Functions.EnsureFileNodeExists) after they have written a
        /// generated file (.prg, .rc, ...) to disk.
        /// </summary>
        /// <remarks>
        /// In SDK-style X# projects these files are included by the globs of XSharp.SDK.Props (including
        /// DependentUpon), so the project file does not change: CPS picks the file up with the next evaluation.
        /// The file is registered with the code model immediately, so HasFileNode is true right away; the later
        /// source item update finds it already registered.
        /// Projects without default items (EnableDefaultItems=false, legacy format) would need an explicit item
        /// through CPS; that belongs to the legacy-format step.
        /// </remarks>
        public void AddFileNode(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                return;
            var fullPath = XSharpCommandLine.MakeFullPath(fileName, ProjectFolder);
            lock (modelGate)
            {
                XProject current;
                lock (gate)
                {
                    current = model;
                    if (current == null || !files.Add(fullPath))
                        return;
                }
                foreach (var target in AllModels())
                    target.AddFile(fullPath);
            }
        }

        /// <summary>
        /// Called by the VO designers before they delete a generated file. See <see cref="AddFileNode"/>.
        /// </summary>
        public void DeleteFileNode(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                return;
            var fullPath = XSharpCommandLine.MakeFullPath(fileName, ProjectFolder);
            lock (modelGate)
            {
                XProject current;
                lock (gate)
                {
                    current = model;
                    if (current == null || !files.Remove(fullPath))
                        return;
                }
                foreach (var target in AllModels())
                    target.RemoveFile(fullPath);
            }
        }

        public void ClearIntellisenseErrors(string fileName) => errors.Clear(fileName);

        /// <summary>
        /// The code model errors and the errors and warnings of the last build (squiggles, XSharpErrorColorizer),
        /// like XSharpProjectNode.GetIntellisenseErrors in MPFproj.
        /// </summary>
        public List<IXErrorPosition> GetIntellisenseErrors(string filename) => errors.Get(filename);
        public void AddIntellisenseError(XError error) => errors.Add(error);

        #endregion

        /// <summary>
        /// Called by <see cref="BuildErrorLoggerProvider"/> after a build of <paramref name="configuration"/>.
        /// </summary>
        internal void SetBuildErrors(string configuration, IEnumerable<BuildErrorPosition> positions) =>
            errors.SetBuildErrors(configuration, positions);
    }
}
