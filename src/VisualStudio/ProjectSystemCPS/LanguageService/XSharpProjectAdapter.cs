//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
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
        private readonly IProjectThreadingService threading;
        private readonly IntellisenseErrorStore errors = new IntellisenseErrorStore();
        private readonly HashSet<string> files = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        private readonly HashSet<string> projectReferences = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        private readonly object gate = new object();

        private XProject model;
        private List<IDisposable> links;
        private IntellisenseErrorList errorList;
        private CommentTaskList taskList;
        private XParseOptions parseOptions = XParseOptions.Default;
        private bool parseOptionsFromCommandLine;
        private string rootNamespace = "";
        private string intermediateOutputPath = "";
        private string outputFile = "";
        private bool prefixClassesWithDefaultNamespace;

        [ImportingConstructor]
        public XSharpProjectAdapter(
            UnconfiguredProject project,
            IActiveConfiguredProjectSubscriptionService subscriptions,
            IProjectThreadingService threading)
        {
            this.project = project;
            this.subscriptions = subscriptions;
            this.threading = threading;
        }

        internal XProject Model => model;

        private string ProjectFolder => Path.GetDirectoryName(project.FullPath);

        #region IProjectDynamicLoadComponent

        public async Task LoadAsync()
        {
            if (!await EnsureSolutionIsOpenAsync())
            {
                XSettings.Information("XSharpProjectAdapter: X# solution model is not open, no code model for " + project.FullPath);
                return;
            }
            lock (gate)
            {
                if (model != null)
                    return;
                model = new XProject(this);
                model.ProjectWalkComplete += OnProjectWalkComplete;
                model.FileWalkComplete += OnFileWalkComplete;
            }
            XSettings.Information("XSharpProjectAdapter: created code model for " + project.FullPath);
            // Forms added with "Add New Item" open in the shadow designer instead of the code editor
            await threading.JoinableTaskFactory.SwitchToMainThreadAsync();
            ShadowDesigner.NewFormDesignerRedirect.EnsureAdvised(threading.JoinableTaskFactory);
            errorList = new IntellisenseErrorList(threading.JoinableTaskFactory, DisplayName);
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
                subscriptions.ProjectBuildRuleSource.SourceBlock.LinkTo(
                    DataflowBlockSlim.CreateActionBlock<IProjectVersionedValue<IProjectSubscriptionUpdate>>(OnDesignTimeBuildChanged),
                    linkOptions,
                    ruleNames: new[] { CommandLineRule }),
            };
        }

        public Task UnloadAsync()
        {
            XProject oldModel;
            lock (gate)
            {
                if (links != null)
                {
                    foreach (var link in links)
                        link.Dispose();
                    links = null;
                }
                if (model != null)
                {
                    model.ProjectWalkComplete -= OnProjectWalkComplete;
                    model.FileWalkComplete -= OnFileWalkComplete;
                }
                oldModel = model;
                model = null;
                errors.Changed = null;
                errorList?.Dispose();
                errorList = null;
                taskList?.Dispose();
                taskList = null;
                files.Clear();
                projectReferences.Clear();
            }
            oldModel?.Close();
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

        private void OnSourceItemsChanged(IProjectVersionedValue<IProjectSubscriptionUpdate> update)
        {
            try
            {
                var added = new List<string>();
                var removed = new List<string>();
                lock (gate)
                {
                    if (model == null)
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
                    foreach (var file in removed)
                    {
                        if (files.Remove(file))
                            model.RemoveFile(file);
                    }
                    foreach (var file in added)
                    {
                        if (files.Add(file))
                            model.AddFile(file);
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
                lock (gate)
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

        private void OnDesignTimeBuildChanged(IProjectVersionedValue<IProjectSubscriptionUpdate> update)
        {
            try
            {
                if (!update.Value.ProjectChanges.TryGetValue(CommandLineRule, out var change) || !change.Difference.AnyChanges)
                    return;
                var arguments = change.After.Items.Keys.ToList();
                if (arguments.Count == 0)
                {
                    // A failed design-time build returns no arguments. Keep the previous options and references.
                    return;
                }
                var commandLine = XSharpCommandLine.Parse(arguments, ProjectFolder);
                var options = XParseOptions.FromVsValues(commandLine.ParseOptions);
                lock (gate)
                {
                    if (model == null)
                        return;
                    parseOptions = options;
                    parseOptionsFromCommandLine = true;
                    model.ResetParseOptions(options);
                    model.RefreshReferences(commandLine.References);
                    foreach (var reference in projectReferences)
                        model.AddProjectReference(reference);
                    model.ResolveReferences();
                }
                XSettings.Information($"XSharpProjectAdapter: {project.FullPath}: design-time build, {commandLine.ParseOptions.Count} options, {commandLine.References.Count} references, dialect {options.Dialect}");
                WalkProject();
            }
            catch (Exception e)
            {
                XSettings.Exception(e);
            }
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
                model.ResetParseOptions(parseOptions);
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
                model.RemoveProjectReference(removed);
            }
            foreach (var added in current.Where(r => !projectReferences.Contains(r)).ToList())
            {
                projectReferences.Add(added);
                model.AddProjectReference(added);
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

        private void WalkProject()
        {
            var current = model;
            if (current != null)
                ModelWalker.AddProject(current);
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
            lock (gate)
            {
                if (model != null && files.Add(fullPath))
                    model.AddFile(fullPath);
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
            lock (gate)
            {
                if (model != null && files.Remove(fullPath))
                    model.RemoveFile(fullPath);
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
