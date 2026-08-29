//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using EnvDTE;
using EnvDTE80;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Logger = XSharp.Project.Logger;

namespace XSharp.Project.ShadowDesigner
{
    /// <summary>
    /// The only class in this feature that touches EnvDTE/DTE2. Adds the companion project
    /// to the running solution and opens it in Designer view.
    /// </summary>
    internal static class SolutionWiring
    {
        /// <summary>Name of the Solution Folder companion projects are nested under, to keep
        /// them visually separated from the user's real projects in Solution Explorer.</summary>
        private const string ShadowDesignerFolderName = "Shadow Designer (generated)";

        /// <summary>
        /// Adds csprojPath to the running solution, nested inside a dedicated Solution Folder
        /// (created on first use) rather than at the solution root, unless a project with
        /// that exact full path is already present -- safe to call every time.
        /// </summary>
        public static void EnsureProjectInSolution(DTE2 dte, string csprojPath)
        {
            if (FindProjectByFullName(EnumerateProjects(dte.Solution), csprojPath) != null)
            {
                return;
            }

            // A stale/"zombie" hierarchy can be registered at this exact path without
            // EnvDTE's enumeration above finding it -- e.g. a .slnx left over from a session
            // that ended abnormally (still references a companion project/folder that no
            // longer exists on disk), which VS loads automatically on solution open, before
            // any of this bridge's code runs. Left in place, AddFromFile below throws
            // COMException 0x80041FE0 ("already a member of the solution"), and the Designer
            // itself may report "does not support project" on the very first double-click of
            // the session -- both confirmed via real repeated testing (see research/06-
            // document-binding-substitution.md). Self-heal instead of requiring the user to
            // hand-edit the .slnx: remove any existing hierarchy at this path first.
            RemoveStaleHierarchy(csprojPath);
            TryRemoveStaleProjectItem(dte, System.IO.Path.GetFileNameWithoutExtension(csprojPath));

            var folder = EnsureSolutionFolder(dte, ShadowDesignerFolderName);
            var solutionFolder = folder.Object as EnvDTE80.SolutionFolder;
            solutionFolder.AddFromFile(csprojPath);
        }

        /// <summary>
        /// Second removal attempt, tried regardless of whether RemoveStaleHierarchy succeeded
        /// -- IVsSolution.CloseSolutionElement can't be trusted to have actually removed a
        /// stale StubHierarchy. Works on the raw EnvDTE.ProjectItem inside the Solution
        /// Folder, not via its .SubProject (null for a broken reference, which is also why
        /// EnumerateProjects/FindProjectByFullName above can never find it).
        /// </summary>
        private static void TryRemoveStaleProjectItem(DTE2 dte, string expectedName)
        {
            foreach (EnvDTE.Project project in dte.Solution.Projects)
            {
                if (!IsSolutionFolder(project))
                {
                    continue;
                }
                for (int i = 1; i <= project.ProjectItems.Count; i++)
                {
                    EnvDTE.ProjectItem item = project.ProjectItems.Item(i);
                    if (item == null || !string.Equals(item.Name, expectedName, StringComparison.OrdinalIgnoreCase))
                    {
                        continue;
                    }
                    try
                    {
                        item.Remove();
                    }
                    catch (Exception ex)
                    {
                        Logger.Exception(ex, $"SolutionWiring.TryRemoveStaleProjectItem: Remove() failed for '{expectedName}'");
                    }
                    return;
                }
            }
        }

        /// <summary>
        /// Finds any IVsHierarchy already registered in the solution at csprojPath --
        /// searching every project, including ones nested in Solution Folders, via the same
        /// low-level IVsSolution API ShadowDesignerCleanup.CloseProjectElement uses to
        /// sidestep EnvDTE's issues with nested/broken projects -- and closes/removes it.
        /// Safe no-op if nothing matches or the solution service isn't available.
        /// </summary>
        private static void RemoveStaleHierarchy(string csprojPath)
        {
            if (!(((IServiceProvider)XSharpProjectPackage.XInstance)?.GetService(typeof(SVsSolution)) is IVsSolution vsSolution))
            {
                return;
            }

            Guid enumFlags = Guid.Empty;
            int hr = vsSolution.GetProjectEnum((uint)__VSENUMPROJFLAGS.EPF_ALLINSOLUTION, ref enumFlags, out IEnumHierarchies hierarchies);
            if (ErrorHandler.Failed(hr) || hierarchies == null)
            {
                return;
            }

            // Match by the project's simple NAME, not just its path -- confirmed via real
            // testing that a "(not found)"/broken project reference (e.g. from a stale .slnx
            // entry pointing at a since-deleted companion folder) surfaces as a
            // `Microsoft.VisualStudio.CommonIDE.Solutions.ProjectSystems.StubHierarchy` that
            // does NOT implement IVsProject at all (so GetMkDocument can't be called on it,
            // and this bridge has no real path to resolve for it anyway) -- but it still
            // reports its declared name via the plain IVsHierarchy property, which is all a
            // stub for a missing project file has left to go on.
            string expectedName = System.IO.Path.GetFileNameWithoutExtension(csprojPath);

            var buffer = new IVsHierarchy[1];
            while (hierarchies.Next(1, buffer, out uint fetched) == VSConstants.S_OK && fetched == 1)
            {
                var hierarchy = buffer[0];
                bool pathMatch = false;
                if (hierarchy is IVsProject vsProject &&
                    ErrorHandler.Succeeded(vsProject.GetMkDocument(VSConstants.VSITEMID_ROOT, out string mkDocument)))
                {
                    pathMatch = string.Equals(mkDocument, csprojPath, StringComparison.OrdinalIgnoreCase);
                }

                bool nameMatch = false;
                if (!pathMatch &&
                    ErrorHandler.Succeeded(hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_Name, out object nameObj)))
                {
                    nameMatch = string.Equals(nameObj as string, expectedName, StringComparison.OrdinalIgnoreCase);
                }

                if (pathMatch || nameMatch)
                {
                    // Plain 0 returns E_INVALIDARG for a StubHierarchy (a broken/"not found"
                    // reference) -- unlike a normal loaded project, it needs this explicit
                    // close option to be accepted. Whether or not this call itself succeeds,
                    // TryRemoveStaleProjectItem (the caller's next step) is the removal path
                    // actually confirmed to work for this case.
                    vsSolution.CloseSolutionElement((uint)__VSSLNCLOSEOPTIONS.SLNCLOSEOPT_DeleteProject, hierarchy, 0);
                    break;
                }
            }
        }

        /// <summary>Finds the top-level Solution Folder with the given name, creating it if
        /// it doesn't exist yet.</summary>
        private static EnvDTE.Project EnsureSolutionFolder(DTE2 dte, string folderName)
        {
            foreach (EnvDTE.Project project in dte.Solution.Projects)
            {
                if (string.Equals(project.Kind, ProjectKinds.vsProjectKindSolutionFolder, StringComparison.OrdinalIgnoreCase) &&
                    string.Equals(project.Name, folderName, StringComparison.OrdinalIgnoreCase))
                {
                    return project;
                }
            }
            var solution2 = (EnvDTE80.Solution2)dte.Solution;
            return solution2.AddSolutionFolder(folderName);
        }

        /// <summary>
        /// Attempts to open designerCsPath in Designer view. Returns false with an error
        /// message on failure instead of throwing -- a freshly-added companion project may
        /// still need a NuGet restore / design-time build to finish.
        /// </summary>
        public static bool TryOpenInDesigner(DTE2 dte, string designerCsPath, out string errorMessage)
        {
            try
            {
                dte.ItemOperations.OpenFile(designerCsPath, EnvDTE.Constants.vsViewKindDesigner);

                // VS reveals/expands a newly-opened document's containing Solution Explorer
                // node on its own timing, well after OpenFile returns -- confirmed NOT the
                // "Track Active Item in Solution Explorer" setting (still happened with that
                // setting already off), so this looks like a one-time "show me where I just
                // opened this" behavior independent of that setting. Rather than chase the
                // exact moment, poll-and-collapse on a background thread for a few seconds.
                CollapseSolutionExplorerNodeWithRetry(dte, ShadowDesignerFolderName);

                errorMessage = null;
                return true;
            }
            catch (Exception ex)
            {
                errorMessage = ex.Message;
                return false;
            }
        }

        private static void CollapseSolutionExplorerNodeWithRetry(DTE2 dte, string itemName)
        {
            // Intentionally fire-and-forget: best-effort background cleanup of a cosmetic
            // UI state, nothing to await it against (same pattern as
            // ShadowDesignerCleanup.DeleteWithRetry).
            _ = System.Threading.Tasks.Task.Run(async () =>
            {
                for (int i = 0; i < 10; i++)
                {
                    await System.Threading.Tasks.Task.Delay(300);
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    CollapseSolutionExplorerNode(dte, itemName);
                }
            });
        }

        /// <summary>
        /// Collapses the named top-level Solution Explorer node, if the window is currently
        /// available. Cosmetic only -- never lets a failure here affect callers. NOTE: even
        /// when this successfully sets UIHierarchyItems.Expanded to false (confirmed via
        /// diagnostic logging that the property read-back does flip), the actual WPF-rendered
        /// Solution Explorer tree was observed NOT following it visually for a Solution
        /// Folder node in testing -- a known category of disconnect between this legacy
        /// automation property and the modern tree's rendering. Left in as a harmless
        /// best-effort attempt (may work in other VS configurations) rather than removed.
        /// </summary>
        private static void CollapseSolutionExplorerNode(DTE2 dte, string itemName)
        {
            try
            {
                // UIHierarchy.GetItem needs a caption PATH rooted at the solution's own
                // display name (e.g. "MySolution\Shadow Designer (generated)"), not a bare
                // item name.
                string solutionName = System.IO.Path.GetFileNameWithoutExtension(dte.Solution.FullName);
                string path = $"{solutionName}\\{itemName}";

                var solutionExplorer = dte.ToolWindows.SolutionExplorer;
                var item = solutionExplorer.GetItem(path);
                if (item != null)
                {
                    item.UIHierarchyItems.Expanded = false;
                }
            }
            catch
            {
                // Best-effort UI cosmetic only.
            }
        }

        /// <summary>Finds the EnvDTE.Project matching the given full path, searching inside
        /// solution folders too.</summary>
        public static EnvDTE.Project FindProjectByFullPath(DTE2 dte, string fullPath)
        {
            return FindProjectByFullName(EnumerateProjects(dte.Solution), fullPath);
        }

        private static bool IsSolutionFolder(EnvDTE.Project project) =>
            string.Equals(project.Kind, ProjectKinds.vsProjectKindSolutionFolder, StringComparison.OrdinalIgnoreCase);

        private static IEnumerable<EnvDTE.Project> EnumerateProjects(EnvDTE.Solution solution)
        {
            foreach (EnvDTE.Project project in solution.Projects)
            {
                foreach (var p in EnumerateProjectAndFolders(project))
                {
                    yield return p;
                }
            }
        }

        private static IEnumerable<EnvDTE.Project> EnumerateProjectAndFolders(EnvDTE.Project project)
        {
            if (project == null) yield break;
            yield return project;

            if (IsSolutionFolder(project))
            {
                for (int i = 1; i <= project.ProjectItems.Count; i++)
                {
                    var subProject = project.ProjectItems.Item(i)?.SubProject;
                    if (subProject == null) continue;
                    foreach (var p in EnumerateProjectAndFolders(subProject))
                    {
                        yield return p;
                    }
                }
            }
        }

        private static EnvDTE.Project FindProjectByFullName(IEnumerable<EnvDTE.Project> projects, string fullName)
        {
            foreach (var project in projects)
            {
                string projectFullName;
                try { projectFullName = project.FullName; }
                catch (Exception) { continue; } // some project kinds throw on FullName access

                if (!string.IsNullOrEmpty(projectFullName) &&
                    string.Equals(projectFullName, fullName, StringComparison.OrdinalIgnoreCase))
                {
                    return project;
                }
            }
            return null;
        }
    }
}
