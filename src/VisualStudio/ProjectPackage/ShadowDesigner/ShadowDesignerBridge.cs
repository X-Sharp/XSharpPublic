//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Community.VisualStudio.Toolkit;
using EnvDTE80;
using Microsoft.CSharp;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using XSharp.CodeDom;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.Project.ShadowDesigner
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
    /// This code lives inside X#'s own ProjectSystem, so XSharpCodeParser/
    /// XSharpCodeDomHelper/XProject are called directly -- no reflection is needed to reach
    /// them.
    ///
    /// Also exposes <see cref="TryResolveCompanionPaths"/>, used by
    /// <see cref="EventHandlerSync"/> and <see cref="DesignerChangesSync"/> to locate an
    /// already-open companion project's files deterministically, recomputed fresh each time
    /// from the real .prg's own class name.
    /// </summary>
    internal static class ShadowDesignerBridge
    {
        private static readonly string[] SharedFrameworkMarkers =
        {
            @"\dotnet\shared\",
            @"\dotnet\packs\",
            "Microsoft.WindowsDesktop.App",
            "Microsoft.NETCore.App",
            "Microsoft.AspNetCore.App",
        };

        /// <summary>
        /// Attempts to open the shadow Designer for <paramref name="fileNode"/> (expected to
        /// be a SDK-style project's .prg file with HasDesigner true). Returns false with an
        /// error message on failure -- callers should fall back to whatever they'd otherwise
        /// have done (e.g. today's "does not support project" Designer error) rather than
        /// throwing, since a partially-set-up solution (mid-restore, no build yet) is a
        /// normal, recoverable condition, not a bug.
        /// </summary>
        public static bool TryOpen(XSharpFileNode fileNode, out string error)
        {
            try
            {
                var projectNode = fileNode.ProjectMgr as XSharpProjectNode;
                if (projectNode == null)
                {
                    error = "Could not resolve the owning X# project.";
                    return false;
                }
                XProject xProject = projectNode.ProjectModel;
                if (xProject == null)
                {
                    error = "The project model is not available yet.";
                    return false;
                }

                string mainPrgPath = fileNode.Url;
                string designerPrgPath = XSharpCodeDomHelper.BuildDesignerFileName(mainPrgPath);
                if (string.IsNullOrEmpty(designerPrgPath) || !File.Exists(designerPrgPath))
                {
                    error = $"No matching .Designer.prg found next to {mainPrgPath}.";
                    return false;
                }

                var dte = fileNode.ProjectMgr.GetService(typeof(SDTE)) as DTE2;
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
                if (IsMissingAnyPackageReference(xProject) && !EnsureBuilt(dte, xProject, out error))
                {
                    return false;
                }

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

                var referencePaths = GetFilteredReferencePaths(xProject);

                var companion = CompanionProjectWriter.EnsureCompanionProject(
                    xProject.FileName, referencePaths, shadowCSharp, namespaceName, className);

                SolutionWiring.EnsureProjectInSolution(dte, companion.CsprojPath);
                return SolutionWiring.TryOpenInDesigner(dte, companion.DesignerCsPath, out error);
            }
            catch (Exception ex)
            {
                error = ex.ToString();
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
        /// Locates an already-open companion project's files for <paramref name="fileNode"/>
        /// deterministically -- only a lightweight parse of the main .prg (to get the class
        /// name), no merge/generate/write. Does NOT create the companion project if it
        /// doesn't exist yet (callers should tell the user to run "Open Shadow Designer"
        /// first in that case, distinguishable via the companion .csproj not existing on
        /// disk).
        /// </summary>
        public static bool TryResolveCompanionPaths(XSharpFileNode fileNode, out CompanionLocation location, out string error)
        {
            location = null;
            try
            {
                var projectNode = fileNode.ProjectMgr as XSharpProjectNode;
                XProject xProject = projectNode?.ProjectModel;
                if (xProject == null)
                {
                    error = "Could not resolve the owning X# project.";
                    return false;
                }

                string mainPrgPath = fileNode.Url;
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
                ThreadHelper.JoinableTaskFactory.Run(async () =>
                {
                    proceed = await VS.MessageBox.ShowConfirmAsync(
                        "X# WinForms Designer",
                        "This project needs to be built at least once so X# can resolve its " +
                        "assembly references for the Designer.\n\nBuild now?");
                });
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
        /// Reads xProject.AssemblyReferences (its public getter already triggers resolution,
        /// subject to XProject's own internal throttle/staleness rules) and filters out
        /// shared-framework/runtime paths that come implicitly via the companion project's
        /// own UseWindowsForms=true SDK import.
        /// </summary>
        private static List<string> GetFilteredReferencePaths(XProject xProject)
        {
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
