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
using EnvDTE80;
using Microsoft.CSharp;
using Microsoft.VisualStudio;
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
    /// (XProject) of its project, so it serves both project systems: the CPS project system
    /// (XSharpShadowDesignerDefaultActionCommand, XSharpShadowDesignerViewFormCommand) and the MPFproj SDK project node (XSharpFileNode).
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
        /// <paramref name="xProject"/> of its project. <paramref name="refreshReferences"/> is called
        /// after a build that was needed to resolve the references (MPFproj reads them from the
        /// response file); CPS projects pass null, their references come from the design-time build. Returns false with an
        /// error message on failure -- callers should fall back to whatever they'd otherwise
        /// have done (e.g. today's "does not support project" Designer error) rather than
        /// throwing, since a partially-set-up solution (mid-restore, no build yet) is a
        /// normal, recoverable condition, not a bug.
        /// </summary>
        public static bool TryOpen(string mainPrgPath, XProject xProject, Action refreshReferences, out string error)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
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

                // A project that has never been built this session has no .rsp response file,
                // so XProject.AssemblyReferences has nothing to resolve -- any 3rd-party type
                // reference silently corrupts the generated code instead of failing loudly (a
                // multi-segment member-access chain like "oControl1:SomeProperty := x"
                // collapses to a bare "oControl1 = x"). Confirmed empirically that VS's own
                // automatic design-time ("Sync") build does NOT resolve 3rd-party/NuGet
                // references either (only plain framework reference-assembly paths show up) --
                // a real build is genuinely required, not just avoidable overhead. Ensure a
                // build has happened before parsing.
                if (IsMissingAnyPackageReference(xProject))
                {
                    if (!EnsureBuilt(dte, xProject, out error))
                    {
                        return false;
                    }
                    // Don't trust that BuildEnded(true)'s own RefreshReferences() call (fired
                    // from XSharpIDEBuildLogger's MSBuild logger callback) has already
                    // completed by the time the synchronous BuildProject(...) call above
                    // returned -- force a synchronous re-read right now instead.
                    refreshReferences?.Invoke();
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

                CompanionSaveWatcher.Watch(ServiceProvider.GlobalProvider, new CompanionLocation
                {
                    MainPrgPath = mainPrgPath,
                    DesignerPrgPath = designerPrgPath,
                    CompanionCsprojPath = companion.CsprojPath,
                    CompanionFormCsPath = CompanionProjectWriter.ComputeFormCsPath(xProject.FileName, className),
                    CompanionDesignerCsPath = companion.DesignerCsPath,
                });
                ShadowDesignerCleanup.Track(companion.CsprojPath);

                SolutionWiring.EnsureProjectInSolution(dte, companion.CsprojPath);
                bool opened = SolutionWiring.TryOpenInDesigner(dte, companion.DesignerCsPath, out error);
                if (!opened)
                {
                    Logger.Error($"ShadowDesignerBridge.TryOpen: {error}");
                }
                return opened;
            }
            catch (Exception ex)
            {
                error = ex.ToString();
                Logger.Exception(ex, "ShadowDesignerBridge.TryOpen");
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
        private static bool IsMissingAnyPackageReference(XProject xProject)
        {
            var packageReferences = CompanionProjectWriter.ReadPackageReferences(xProject.FileName);
            if (packageReferences.Count == 0) return false;
            return GetFilteredReferencePaths(xProject).Count == 0;
        }

        /// <summary>
        /// Builds the real project via VS's own build pipeline (EnvDTE SolutionBuild, not a
        /// separately-spawned dotnet.exe process) so XSharpIDEBuildLogger's BuildEnded hook
        /// fires normally and refreshes XProject.AssemblyReferences the same way a manual
        /// Build would. Prompts for confirmation first unless
        /// XCustomEditorSettings.AutoBuildForShadowDesigner is set (Tools > Options > X#
        /// Project System > Other Editor Options > Windows Forms Editor).
        /// </summary>
        private static bool EnsureBuilt(DTE2 dte, XProject xProject, out string error)
        {
            error = null;
            bool proceed = XCustomEditorSettings.AutoBuildForShadowDesigner;
            if (!proceed)
            {
                proceed = VsShellUtilities.ShowMessageBox(ServiceProvider.GlobalProvider,
                    "This project needs to be built at least once so X# can resolve its " +
                    "assembly references for the Designer.\n\nBuild now?",
                    "X# WinForms Designer",
                    OLEMSGICON.OLEMSGICON_QUERY, OLEMSGBUTTON.OLEMSGBUTTON_OKCANCEL,
                    OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST) == 1; // IDOK
            }
            if (!proceed)
            {
                error = "Cancelled -- build the project manually, then try View Designer again.";
                return false;
            }

            var project = SolutionWiring.FindProjectByFullPath(dte, xProject.FileName);
            if (project == null)
            {
                error = $"Could not find an open project matching {xProject.FileName} in the solution.";
                return false;
            }
            try
            {
                string configName = dte.Solution.SolutionBuild.ActiveConfiguration.Name;
                dte.Solution.SolutionBuild.BuildProject(configName, project.UniqueName, WaitForBuildToFinish: true);
                return true;
            }
            catch (Exception ex)
            {
                error = $"Build failed: {ex.Message}";
                return false;
            }
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
