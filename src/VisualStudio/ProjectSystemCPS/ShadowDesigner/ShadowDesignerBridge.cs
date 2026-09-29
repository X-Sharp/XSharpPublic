//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Moved from ProjectPackage, which suppresses VSTHRD010 for the whole project: the UI thread
// requirements of this code are handled explicitly (ThreadHelper) and were not rewritten.
#pragma warning disable VSTHRD010
using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Versioning;
using System.Threading.Tasks;
using EnvDTE80;
using Microsoft.CSharp;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OperationProgress;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using XSharp.CodeDom;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.ProjectSystem.ShadowDesigner
{
    /// <summary>
    /// Entry point for the "View Designer on a SDK-style .prg" bridge. VS's out-of-process
    /// WinForms Designer (used for .NET Core/SDK-style projects) has no extensibility point
    /// for third-party languages, so a .prg can never be opened in it directly. This bridges
    /// the gap: parse the real .prg/.Designer.prg with the real X# parser, merge into one
    /// CodeCompileUnit, generate C# via the stock BCL CSharpCodeProvider, write/refresh a
    /// real, hidden, auto-generated companion C# project next to the real one, and open ITS
    /// Designer view (whose owning IVsHierarchy is a real C# project, so the out-of-process
    /// Designer's project-type gate passes).
    ///
    /// XSharpCodeParser/XSharpCodeDomHelper/XProject are called directly -- no reflection is
    /// needed to reach them. The bridge only needs the path of the .prg and the X# code model
    /// (XProject) of its project. Entry points: the CPS commands XSharpShadowDesignerDefaultActionCommand and
    /// XSharpShadowDesignerViewFormCommand (MPFproj no longer loads SDK-style projects).
    ///
    /// Also exposes <see cref="TryResolveCompanionPaths"/>, used by
    /// <see cref="EventHandlerSync"/> and <see cref="DesignerChangesSync"/> to locate an
    /// already-open companion project's files deterministically, recomputed fresh each time
    /// from the real .prg's own class name.
    /// </summary>
    public static class ShadowDesignerBridge
    {
        /// <summary>
        /// True when <paramref name="project"/> is loaded by the CPS project system (its IXSharpProject is the
        /// XSharpProjectAdapter). Lets the X# project package decide without knowing the CPS types.
        /// </summary>
        public static bool IsCpsProject(XProject project) => project?.ProjectNode is LanguageService.XSharpProjectAdapter;

        /// <summary>
        /// True when <paramref name="prgPath"/> is a .prg with a matching .Designer.prg next to it -- the signal
        /// for a form/user control in SDK-style projects, which rarely have an explicit SubType.
        /// </summary>
        public static bool HasDesignerFile(string prgPath)
        {
            if (string.IsNullOrEmpty(prgPath) ||
                !string.Equals(Path.GetExtension(prgPath), ".prg", StringComparison.OrdinalIgnoreCase) ||
                prgPath.EndsWith(".designer.prg", StringComparison.OrdinalIgnoreCase))
            {
                return false;
            }
            string designerPrg = XSharpCodeDomHelper.BuildDesignerFileName(prgPath);
            return !string.IsNullOrEmpty(designerPrg) &&
                !string.Equals(designerPrg, prgPath, StringComparison.OrdinalIgnoreCase) &&
                File.Exists(designerPrg);
        }

        private static readonly string[] SharedFrameworkMarkers =
        {
            @"\dotnet\shared\",
            @"\dotnet\packs\",
            "Microsoft.WindowsDesktop.App",
            "Microsoft.NETCore.App",
            "Microsoft.AspNetCore.App",
        };

        /// <summary>
        /// Attempts to open the shadow Designer for <paramref name="mainPrgPath"/> (expected to
        /// be a SDK-style project's .prg file with a matching .Designer.prg), using the code model
        /// <paramref name="xProject"/> of its project. Call <see cref="EnsureReferencesAsync"/> first: this method
        /// does not wait for the references or build anything. Returns false with an
        /// error message on failure -- callers should fall back to whatever they'd otherwise
        /// have done (e.g. today's "does not support project" Designer error) rather than
        /// throwing, since a partially-set-up solution (mid-restore, no build yet) is a
        /// normal, recoverable condition, not a bug.
        /// </summary>
        public static async Task<(bool Ok, string Error)> TryOpenAsync(string mainPrgPath, XProject xProject)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            if (!TryPrepareCompanion(mainPrgPath, xProject, out var location, out string error))
            {
                return (false, error);
            }
            try
            {
                // The companion project includes its files through the SDK globs. When it was already loaded (another
                // form of this project was opened before), it only picks up newly written files after its file watcher
                // and a re-evaluation. Opening the designer before that fails with "... is not contained within a
                // project that supports code" -- wait until the project contains the files.
                await WaitForCompanionFilesAsync(location);
                var dte = await AsyncServiceProvider.GlobalProvider.GetServiceAsync(typeof(SDTE)) as DTE2;
                if (dte == null)
                {
                    return (false, "Could not obtain the DTE service.");
                }
                bool opened = SolutionWiring.TryOpenInDesigner(dte, location.CompanionDesignerCsPath, out error);
                if (!opened)
                {
                    Logger.Error($"ShadowDesignerBridge.TryOpenAsync: {error}");
                }
                return (opened, error);
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "ShadowDesignerBridge.TryOpenAsync");
                return (false, ex.ToString());
            }
        }

        // How long to wait for an already loaded companion project to contain newly written files
        private static readonly TimeSpan CompanionFilesTimeout = TimeSpan.FromSeconds(10);

        /// <remarks>
        /// Verified in VS: the companion project did not pick up new files through its globs even after 10 s.
        /// So the missing files are added explicitly (ProjectItems.AddFromFile); for a file that a glob of the
        /// SDK-style project already covers, CPS does not write an explicit item into the .csproj.
        /// </remarks>
        private static async Task WaitForCompanionFilesAsync(CompanionLocation location)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var project = FindHierarchy(location.CompanionCsprojPath) as IVsProject;
            if (project == null)
            {
                // Not loaded yet: a freshly added project (SolutionWiring.EnsureProjectInSolution) loads with its files
                return;
            }
            var files = new[] { location.CompanionFormCsPath, location.CompanionDesignerCsPath };
            var missing = files.Where(f => !IsInProject(project, f)).ToList();
            if (missing.Count == 0)
            {
                return;
            }
            try
            {
                var dte = await AsyncServiceProvider.GlobalProvider.GetServiceAsync(typeof(SDTE)) as DTE2;
                var dteProject = dte == null ? null : SolutionWiring.FindProjectByFullPath(dte, location.CompanionCsprojPath);
                foreach (var file in missing)
                {
                    Logger.Information($"ShadowDesignerBridge: adding {file} to the companion project");
                    dteProject?.ProjectItems.AddFromFile(file);
                }
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "ShadowDesignerBridge: could not add the files to the companion project");
            }
            var deadline = DateTime.UtcNow + CompanionFilesTimeout;
            while (!files.All(f => IsInProject(project, f)))
            {
                if (DateTime.UtcNow >= deadline)
                {
                    Logger.Information($"ShadowDesignerBridge: {location.CompanionDesignerCsPath} is not part of the companion project after {CompanionFilesTimeout.TotalSeconds} s, opening anyway");
                    return;
                }
                await Task.Delay(200);
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            }
        }

        private static bool IsInProject(IVsProject project, string file)
        {
            var priority = new VSDOCUMENTPRIORITY[1];
            return ErrorHandler.Succeeded(project.IsDocumentInProject(file, out int found, priority, out uint _)) && found != 0;
        }

        /// <summary>
        /// Parses the form, writes the companion project and makes sure it is part of the solution
        /// (everything except opening the designer, see <see cref="TryOpenAsync"/>).
        /// </summary>
        private static bool TryPrepareCompanion(string mainPrgPath, XProject xProject, out CompanionLocation location, out string error)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            location = null;
            try
            {
                if (xProject == null)
                {
                    error = "The project model is not available yet.";
                    return false;
                }

                string designerPrgPath = XSharpCodeDomHelper.BuildDesignerFileName(mainPrgPath);
                if (string.IsNullOrEmpty(designerPrgPath) || !File.Exists(designerPrgPath))
                {
                    error = $"No matching .Designer.prg found next to {mainPrgPath}.";
                    return false;
                }

                var dte = ServiceProvider.GlobalProvider.GetService(typeof(SDTE)) as DTE2;
                if (dte == null)
                {
                    error = "Could not obtain the DTE service.";
                    return false;
                }

                // The parser resolves System.Windows.Forms.Form & co. through the project's assembly references.
                // Shortly after the solution loaded they can still be queued (XProject.ResolveReferences runs at
                // most every 15 seconds); unresolved types make the parser drop the right-hand side of assignments
                // and CSharpCodeProvider then throws ArgumentNullException ("e") on the first open.
                ForceResolveUnprocessedReferences(xProject);
                XCodeCompileUnit mainUnit = ToXCodeCompileUnit(ParseFile(xProject, mainPrgPath, null));
                CodeTypeDeclaration firstClass = mainUnit.GetFirstClass();
                XCodeCompileUnit designerUnit = ToXCodeCompileUnit(ParseFile(xProject, designerPrgPath, firstClass));

                CodeCompileUnit mergedUnit = XSharpCodeDomHelper.MergeCodeCompileUnit(mainUnit, designerUnit);

                StripTrailingReturnFromInitializeComponent(mergedUnit);

                string shadowCSharp;
                using (var writer = new StringWriter())
                {
                    var csProvider = new CSharpCodeProvider();
                    var options = new CodeGeneratorOptions { BracingStyle = "C" };
                    csProvider.GenerateCodeFromCompileUnit(mergedUnit, writer, options);
                    shadowCSharp = writer.ToString();
                }

                CodeNamespace mergedNamespace = mergedUnit.Namespaces.Count > 0 ? mergedUnit.Namespaces[0] : null;
                CodeTypeDeclaration mergedType = (mergedNamespace != null && mergedNamespace.Types.Count > 0)
                    ? mergedNamespace.Types[0] : null;
                string namespaceName = mergedNamespace?.Name ?? "GeneratedShadow";
                string className = mergedType?.Name ?? Path.GetFileNameWithoutExtension(mainPrgPath);

                string stubCSharp = BuildStubCSharp(mergedNamespace, mergedType, namespaceName, className);

                var referencePaths = GetFilteredReferencePaths(xProject);

                // A background deletion from the previous solution close may still be retrying on this folder
                ShadowDesignerCleanup.CancelPendingDelete(CompanionProjectWriter.ComputeCompanionDir(xProject.FileName));
                var companion = CompanionProjectWriter.EnsureCompanionProject(
                    xProject.FileName, GetEvaluatedTargetFramework(xProject.FileName), referencePaths, shadowCSharp, stubCSharp, className);

                location = new CompanionLocation
                {
                    MainPrgPath = mainPrgPath,
                    DesignerPrgPath = designerPrgPath,
                    CompanionCsprojPath = companion.CsprojPath,
                    CompanionFormCsPath = CompanionProjectWriter.ComputeFormCsPath(xProject.FileName, className),
                    CompanionDesignerCsPath = companion.DesignerCsPath,
                };
                CompanionSaveWatcher.Watch(ServiceProvider.GlobalProvider, location);
                ShadowDesignerCleanup.Track(companion.CsprojPath);

                SolutionWiring.EnsureProjectInSolution(dte, companion.CsprojPath);
                error = null;
                return true;
            }
            catch (Exception ex)
            {
                error = ex.ToString();
                Logger.Exception(ex, "ShadowDesignerBridge.TryPrepareCompanion");
                return false;
            }
        }

        /// <summary>
        /// Result of <see cref="TryResolveCompanionPaths"/> -- everything the sync commands
        /// need to locate the real .prg pair and an already-existing companion project's
        /// files.
        /// </summary>
        public sealed class CompanionLocation
        {
            public string MainPrgPath { get; set; }
            public string DesignerPrgPath { get; set; }
            public string CompanionCsprojPath { get; set; }
            public string CompanionFormCsPath { get; set; }
            public string CompanionDesignerCsPath { get; set; }
        }

        /// <summary>
        /// Locates an already-open companion project's files for <paramref name="mainPrgPath"/>
        /// deterministically -- only a lightweight parse of the main .prg (to get the class
        /// name), no merge/generate/write. Does NOT create the companion project if it
        /// doesn't exist yet (callers should tell the user to run "Open Shadow Designer"
        /// first in that case, distinguishable via the companion .csproj not existing on
        /// disk).
        /// </summary>
        public static bool TryResolveCompanionPaths(string mainPrgPath, XProject xProject, out CompanionLocation location, out string error)
        {
            location = null;
            try
            {
                if (xProject == null)
                {
                    error = "Could not resolve the owning X# project.";
                    return false;
                }

                string designerPrgPath = XSharpCodeDomHelper.BuildDesignerFileName(mainPrgPath);
                if (string.IsNullOrEmpty(designerPrgPath) || !File.Exists(designerPrgPath))
                {
                    error = $"No matching .Designer.prg found next to {mainPrgPath}.";
                    return false;
                }

                XCodeCompileUnit mainUnit = ToXCodeCompileUnit(ParseFile(xProject, mainPrgPath, null));
                CodeTypeDeclaration firstClass = mainUnit.GetFirstClass();
                string className = firstClass?.Name ?? Path.GetFileNameWithoutExtension(mainPrgPath);

                var companionPaths = CompanionProjectWriter.ComputePaths(xProject.FileName, className);
                if (!File.Exists(companionPaths.CsprojPath))
                {
                    error = "No shadow companion project found -- run 'View Designer' on this file first.";
                    return false;
                }

                location = new CompanionLocation
                {
                    MainPrgPath = mainPrgPath,
                    DesignerPrgPath = designerPrgPath,
                    CompanionCsprojPath = companionPaths.CsprojPath,
                    CompanionFormCsPath = CompanionProjectWriter.ComputeFormCsPath(xProject.FileName, className),
                    CompanionDesignerCsPath = companionPaths.DesignerCsPath,
                };
                error = null;
                return true;
            }
            catch (Exception ex)
            {
                error = ex.ToString();
                return false;
            }
        }

        /// <summary>
        /// Coarse but reliable check: does the real .xsproj declare any
        /// &lt;PackageReference&gt; at all, and is the filtered (non-framework) reference
        /// list still empty? True only
        /// when a build is genuinely needed -- a project with no NuGet references at all
        /// (nothing to resolve) or one that's already built correctly both return false here.
        /// </summary>
        internal static bool IsMissingAnyPackageReference(XProject xProject)
        {
            var packageReferences = CompanionProjectWriter.ReadPackageReferences(xProject.FileName);
            if (packageReferences.Count == 0) return false;
            return GetFilteredReferencePaths(xProject).Count == 0;
        }

        // How long to wait for the project load (NuGet restore + design-time build) before offering a build
        private static readonly TimeSpan ProjectLoadTimeout = TimeSpan.FromSeconds(60);
        // How long to wait for the references after the load stage. The stage can already be complete when the NuGet
        // restore has not finished yet (the first design-time build ran without the packages; seen in VS), and the
        // design-time build after the restore follows a few seconds later.
        private static readonly TimeSpan ReferenceUpdateTimeout = TimeSpan.FromSeconds(20);
        // After a build (which restores) the design-time build with the references follows right away
        private static readonly TimeSpan ReferenceUpdateAfterBuildTimeout = TimeSpan.FromSeconds(3);

        /// <summary>
        /// Makes sure that the assembly references of the project's packages are known to the code model before the
        /// forms are parsed: without them any 3rd-party type reference silently corrupts the generated code instead
        /// of failing loudly (a multi-segment member-access chain like "oControl1:SomeProperty := x" collapses to a
        /// bare "oControl1 = x").
        /// </summary>
        /// <remarks>
        /// Under CPS the references come from the design-time build (XSharpProjectAdapter), which runs after the NuGet
        /// restore. When they are missing (shortly after the solution was opened, or while restoring) this waits
        /// asynchronously for the IntelliSense stage of the project load. Only when they are still missing, a build is
        /// offered (the build also restores); it runs asynchronously (IVsSolutionBuildManager), without the nested
        /// message pump of EnvDTE's BuildProject(WaitForBuildToFinish: true). Callers must not be awaited
        /// synchronously on the UI thread (as CPS does with command handlers): see XSharpShadowDesignerCommands.
        /// Returns false with an error when the user declined the build or it could not be started.
        /// </remarks>
        public static async Task<(bool Ok, string Error)> EnsureReferencesAsync(XProject xProject)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            if (!IsMissingAnyPackageReference(xProject))
            {
                return (true, null);
            }
            Logger.Information("ShadowDesignerBridge: package references not resolved yet, waiting for the project load");
            await WaitForReferencesAsync(xProject);
            if (!IsMissingAnyPackageReference(xProject))
            {
                return (true, null);
            }

            bool proceed = XCustomEditorSettings.AutoBuildForShadowDesigner;
            if (!proceed)
            {
                proceed = VsShellUtilities.ShowMessageBox(ServiceProvider.GlobalProvider,
                    "X# could not resolve the package references of this project for the Designer yet. " +
                    "Building the project restores and resolves them.\n\nBuild now?",
                    "X# WinForms Designer",
                    OLEMSGICON.OLEMSGICON_QUERY, OLEMSGBUTTON.OLEMSGBUTTON_OKCANCEL,
                    OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST) == 1; // IDOK
            }
            if (!proceed)
            {
                return (false, "Cancelled -- build the project manually, then try View Designer again.");
            }
            string buildError = await BuildAsync(xProject.FileName);
            if (buildError != null)
            {
                return (false, buildError);
            }
            // The build has restored: the design-time build with the references follows right after it (0.2 s in
            // VS). A short wait, also for a failed build, which can have restored as well (compile errors): the
            // references can still come, but when the restore failed 20 s would only delay the Designer.
            await WaitForReferencesAsync(xProject, ReferenceUpdateAfterBuildTimeout);
            // Like before: continue even when references are still missing (e.g. a failed build)
            return (true, null);
        }

        private static async Task WaitForReferencesAsync(XProject xProject, TimeSpan? timeout = null)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var statusBar = await AsyncServiceProvider.GlobalProvider.GetServiceAsync(typeof(SVsStatusbar)) as IVsStatusbar;
            statusBar?.SetText("X# WinForms Designer: waiting for the package references of " + Path.GetFileNameWithoutExtension(xProject.FileName) + "...");
            try
            {
                await WaitForReferencesCoreAsync(xProject, timeout ?? ReferenceUpdateTimeout);
            }
            finally
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                statusBar?.SetText("");
            }
        }

        private static async Task WaitForReferencesCoreAsync(XProject xProject, TimeSpan timeout)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            if (await AsyncServiceProvider.GlobalProvider.GetServiceAsync(typeof(SVsOperationProgressStatusService)) is IVsOperationProgressStatusService progress)
            {
                var stage = progress.GetStageStatusForSolutionLoad(CommonOperationProgressStageIds.Intellisense);
                if (stage != null && stage.IsInProgress)
                {
                    await Task.WhenAny(stage.WaitForCompletionAsync(), Task.Delay(ProjectLoadTimeout));
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                }
            }
            var deadline = DateTime.UtcNow + timeout;
            while (IsMissingAnyPackageReference(xProject) && DateTime.UtcNow < deadline)
            {
                await Task.Delay(250);
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            }
        }

        /// <summary>
        /// Builds the X# project through the solution build manager and waits asynchronously for the build to end.
        /// Returns an error message when the build could not be started, null otherwise (also for a failed build).
        /// </summary>
        private static async Task<string> BuildAsync(string projectFile)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var hierarchy = FindHierarchy(projectFile);
            if (hierarchy == null)
            {
                return $"Could not find an open project matching {projectFile} in the solution.";
            }
            if (!(await AsyncServiceProvider.GlobalProvider.GetServiceAsync(typeof(SVsSolutionBuildManager)) is IVsSolutionBuildManager2 buildManager))
            {
                return "Could not obtain the solution build manager.";
            }
            if (ErrorHandler.Succeeded(buildManager.QueryBuildManagerBusy(out int busy)) && busy != 0)
            {
                return "A build is already running -- try View Designer again when it has finished.";
            }
            var completion = new BuildCompletion();
            ErrorHandler.ThrowOnFailure(buildManager.AdviseUpdateSolutionEvents(completion, out uint cookie));
            try
            {
                int hr = buildManager.StartSimpleUpdateProjectConfiguration(hierarchy, null, null,
                    (uint)VSSOLNBUILDUPDATEFLAGS.SBF_OPERATION_BUILD, 0, 0);
                if (ErrorHandler.Failed(hr))
                {
                    return $"The build could not be started (0x{hr:X8}).";
                }
                bool succeeded = await completion.Task;
                Logger.Information($"ShadowDesignerBridge: build of {projectFile} finished, succeeded: {succeeded}");
                return null;
            }
            finally
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                buildManager.UnadviseUpdateSolutionEvents(cookie);
            }
        }

        /// <summary>
        /// Completes when the next solution build (the one started by BuildAsync) ends.
        /// </summary>
        private sealed class BuildCompletion : IVsUpdateSolutionEvents
        {
            private readonly TaskCompletionSource<bool> completion =
                new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously);

            public Task<bool> Task => completion.Task;

            public int UpdateSolution_Begin(ref int pfCancelUpdate) => VSConstants.S_OK;

            public int UpdateSolution_Done(int fSucceeded, int fModified, int fCancelCommand)
            {
                completion.TrySetResult(fSucceeded != 0);
                return VSConstants.S_OK;
            }

            public int UpdateSolution_StartUpdate(ref int pfCancelUpdate) => VSConstants.S_OK;

            public int UpdateSolution_Cancel()
            {
                completion.TrySetResult(false);
                return VSConstants.S_OK;
            }

            public int OnActiveProjectCfgChange(IVsHierarchy pIVsHierarchy) => VSConstants.S_OK;
        }

        private static IVsHierarchy FindHierarchy(string projectFile)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (!(ServiceProvider.GlobalProvider.GetService(typeof(SVsSolution)) is IVsSolution solution))
            {
                return null;
            }
            Guid ignored = Guid.Empty;
            if (ErrorHandler.Failed(solution.GetProjectEnum((uint)__VSENUMPROJFLAGS.EPF_LOADEDINSOLUTION, ref ignored, out IEnumHierarchies hierarchies)) || hierarchies == null)
            {
                return null;
            }
            var buffer = new IVsHierarchy[1];
            while (hierarchies.Next(1, buffer, out uint fetched) == VSConstants.S_OK && fetched == 1)
            {
                if (buffer[0] is IVsProject project &&
                    ErrorHandler.Succeeded(project.GetMkDocument(VSConstants.VSITEMID_ROOT, out string mkDocument)) &&
                    string.Equals(mkDocument, projectFile, StringComparison.OrdinalIgnoreCase))
                {
                    return buffer[0];
                }
            }
            return null;
        }

        /// <summary>
        /// The active target framework of the loaded X# project in short form (net8.0, net48, ...), from the
        /// hierarchy (VSHPROPID_TargetFrameworkMoniker): unlike the project file XML it covers
        /// &lt;TargetFrameworks&gt; and a TargetFramework from Directory.Build.props. Null when not available;
        /// the companion writer then reads the project file.
        /// </summary>
        private static string GetEvaluatedTargetFramework(string projectFile)
        {
            try
            {
                var hierarchy = FindHierarchy(projectFile);
                if (hierarchy == null ||
                    ErrorHandler.Failed(hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID4.VSHPROPID_TargetFrameworkMoniker, out object value)) ||
                    !(value is string moniker) || string.IsNullOrEmpty(moniker))
                {
                    return null;
                }
                return ToShortTargetFramework(new FrameworkName(moniker));
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "ShadowDesignerBridge.GetEvaluatedTargetFramework");
                return null;
            }
        }

        private static string ToShortTargetFramework(FrameworkName framework)
        {
            var version = framework.Version;
            switch (framework.Identifier)
            {
                case ".NETFramework":
                    // v4.8 -> net48, v4.7.2 -> net472
                    return "net" + version.ToString().Replace(".", "");
                case ".NETCoreApp":
                    return (version.Major >= 5 ? "net" : "netcoreapp") + version.Major + "." + version.Minor;
                default:
                    return null;
            }
        }

        /// <summary>
        /// The companion's Form1.cs-equivalent stub: the partial form class with the same base types and namespace
        /// imports as the generated designer code, generated by the same CSharpCodeProvider. Both partial
        /// declarations therefore name the same base class (a hard-coded Form gave CS0263 for user controls).
        /// </summary>
        private static string BuildStubCSharp(CodeNamespace mergedNamespace, CodeTypeDeclaration mergedType, string namespaceName, string className)
        {
            var ns = new CodeNamespace(namespaceName);
            if (mergedNamespace != null)
            {
                foreach (CodeNamespaceImport import in mergedNamespace.Imports)
                {
                    ns.Imports.Add(new CodeNamespaceImport(import.Namespace));
                }
            }
            var type = new CodeTypeDeclaration(className)
            {
                IsClass = true,
                IsPartial = true,
                TypeAttributes = TypeAttributes.Public,
            };
            if (mergedType != null)
            {
                foreach (CodeTypeReference baseType in mergedType.BaseTypes)
                {
                    type.BaseTypes.Add(baseType);
                }
            }
            ns.Types.Add(type);
            var unit = new CodeCompileUnit();
            unit.Namespaces.Add(ns);
            using (var writer = new StringWriter())
            {
                new CSharpCodeProvider().GenerateCodeFromCompileUnit(unit, writer, new CodeGeneratorOptions { BracingStyle = "C" });
                return writer.ToString();
            }
        }

        private static XCodeCompileUnit ToXCodeCompileUnit(CodeCompileUnit unit) =>
            unit is XCodeCompileUnit xccu ? xccu : new XCodeCompileUnit(unit);

        private static CodeCompileUnit ParseFile(XProject xProject, string path, CodeTypeDeclaration formClass)
        {
            var parser = formClass != null
                ? new XSharpCodeParser(xProject, formClass)
                : new XSharpCodeParser(xProject);
            parser.FileName = path;
            return parser.Parse(File.ReadAllText(path));
        }

        /// <summary>
        /// Removes trailing CodeMethodReturnStatement(s) from InitializeComponent -- the
        /// out-of-process Designer's own strict parser for that method rejects a trailing
        /// `return;`, which X#'s CodeDom model includes by default.
        /// </summary>
        private static void StripTrailingReturnFromInitializeComponent(CodeCompileUnit unit)
        {
            foreach (CodeNamespace ns in unit.Namespaces)
            {
                foreach (CodeTypeDeclaration type in ns.Types)
                {
                    foreach (CodeTypeMember member in type.Members)
                    {
                        if (member is CodeMemberMethod method && method.Name == "InitializeComponent")
                        {
                            while (method.Statements.Count > 0 &&
                                   method.Statements[method.Statements.Count - 1] is CodeMethodReturnStatement)
                            {
                                method.Statements.RemoveAt(method.Statements.Count - 1);
                            }
                        }
                    }
                }
            }
        }

        /// <summary>
        /// XProject.AssemblyReferences's own getter only actually processes newly-queued
        /// references at most once every 15 seconds (XProject's internal RefCheckTimeOut
        /// throttle, gated on a _lastRefCheck timestamp touched by ANY caller anywhere in the
        /// IDE, e.g. editor IntelliSense polling -- not something under this bridge's
        /// control). Confirmed via diagnostic logging: a reference already correctly written
        /// into the real .rsp by a build that just finished can still read back as "not
        /// there" immediately afterward, purely because of this timing gate, not because it
        /// was actually missing. Bypasses it the same way the research spike proved out
        /// (research/spikes/spike-vsix/RESULTS.md, AssemblyReferenceResolver
        /// .ForceResolveReferences) -- reflecting into the private
        /// ResolveUnprocessedAssemblyReferences() and calling it directly. Reflection is
        /// still needed here even though this code lives inside X#'s own product source
        /// (unlike everywhere else in this bridge): XProject lives in the separate
        /// XSharpModel assembly, and this specific method is private there, not just
        /// internal.
        /// </summary>
        private static void ForceResolveUnprocessedReferences(XProject xProject)
        {
            try
            {
                var method = xProject.GetType().GetMethod("ResolveUnprocessedAssemblyReferences",
                    System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
                method?.Invoke(xProject, null);
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "ForceResolveUnprocessedReferences: reflection failed");
            }
        }

        /// <summary>
        /// Reads xProject.AssemblyReferences (forcing resolution of anything still queued
        /// first, see ForceResolveUnprocessedReferences) and filters out shared-framework/
        /// runtime paths that come implicitly via the companion project's own
        /// UseWindowsForms=true SDK import.
        /// </summary>
        private static List<string> GetFilteredReferencePaths(XProject xProject)
        {
            ForceResolveUnprocessedReferences(xProject);
            var seenSimpleNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            var result = new List<string>();
            foreach (XAssembly asm in xProject.AssemblyReferences)
            {
                string path = asm?.FileName;
                if (string.IsNullOrEmpty(path)) continue;
                if (SharedFrameworkMarkers.Any(marker => path.IndexOf(marker, StringComparison.OrdinalIgnoreCase) >= 0))
                    continue;

                string simpleName = Path.GetFileNameWithoutExtension(path);
                if (!seenSimpleNames.Add(simpleName))
                    continue; // dedupe: transitive duplicates are common

                result.Add(path);
            }
            return result;
        }
    }
}
