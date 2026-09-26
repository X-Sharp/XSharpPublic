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
            }
            XSettings.Information("XSharpProjectAdapter: created code model for " + project.FullPath);

            var linkOptions = new DataflowLinkOptions { PropagateCompletion = true };
            links = new List<IDisposable>
            {
                subscriptions.SourceItemsRuleSource.SourceBlock.LinkTo(
                    DataflowBlockSlim.CreateActionBlock<IProjectVersionedValue<IProjectSubscriptionUpdate>>(OnSourceItemsChanged),
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
                oldModel = model;
                model = null;
                files.Clear();
                projectReferences.Clear();
            }
            oldModel?.Close();
            return Task.CompletedTask;
        }

        /// <summary>
        /// The X# solution model (database) is opened by the X# packages when a solution opens. When a CPS
        /// project loads before these packages have been initialized, load the X# project package and wait.
        /// </summary>
        private async Task<bool> EnsureSolutionIsOpenAsync()
        {
            if (XSolution.IsOpen)
                return true;
            await threading.JoinableTaskFactory.SwitchToMainThreadAsync();
            if (ServiceProvider.GlobalProvider.GetService(typeof(SVsShell)) is IVsShell shell)
            {
                var packageGuid = new Guid(XSharpConstants.guidXSharpProjectPkgString);
                shell.LoadPackage(ref packageGuid, out _);
            }
            await TaskScheduler.Default;
            for (int i = 0; i < 100 && !XSolution.IsOpen; i++)
            {
                await Task.Delay(100);
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
            lock (gate)
            {
                return files.Contains(fileName);
            }
        }

        public void AddFileNode(string fileName)
        {
            // Called by the code model for files that it wants to see in the project (e.g. generated files).
            // Under CPS the project file is the source of truth; adding items is done through CPS (WP-6).
        }

        public void DeleteFileNode(string fileName)
        {
            // See AddFileNode
        }

        public void ClearIntellisenseErrors(string fileName) => errors.Clear(fileName);
        public List<IXErrorPosition> GetIntellisenseErrors(string filename) => errors.Get(filename);
        public void AddIntellisenseError(XError error) => errors.Add(error);

        #endregion
    }
}
