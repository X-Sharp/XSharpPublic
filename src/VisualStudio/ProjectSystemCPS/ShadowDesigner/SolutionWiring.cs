//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Moved from ProjectPackage, which suppresses VSTHRD010 for the whole project: the UI thread
// requirements of this code are handled explicitly (ThreadHelper) and were not rewritten.
#pragma warning disable VSTHRD010
using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace XSharp.ProjectSystem.ShadowDesigner
{
    /// <summary>
    /// Adds the companion project to the running solution and opens it in Designer view
    /// through the Visual Studio solution, hierarchy and document services.
    /// </summary>
    internal static class SolutionWiring
    {
        /// <summary>Name of the Solution Folder companion projects are nested under, to keep
        /// them visually separated from the user's real projects in Solution Explorer.</summary>
        internal const string ShadowDesignerFolderName = "Shadow Designer (generated)";

        /// <summary>
        /// Adds csprojPath to the running solution, nested inside a dedicated Solution Folder
        /// (created on first use) rather than at the solution root, unless a project with
        /// that exact full path is already present -- safe to call every time.
        /// </summary>
        public static void EnsureProjectInSolution(string csprojPath)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var solution = GetSolution();
            if (FindProjectByFullPath(csprojPath) != null)
            {
                return;
            }
            if (!(solution is IVsSolution6 solution6))
            {
                throw new InvalidOperationException("The solution service does not support adding companion projects.");
            }

            RemoveStaleHierarchy(solution, csprojPath);
            RemoveStaleProjectItem(solution, Path.GetFileNameWithoutExtension(csprojPath));

            var folder = EnsureSolutionFolder(solution, ShadowDesignerFolderName);
            ErrorHandler.ThrowOnFailure(solution6.AddExistingProject(csprojPath, folder, out _));
        }

        /// <summary>
        /// A broken project can remain as a folder item even after its stub hierarchy was closed.
        /// Remove that item from the solution folder without deleting anything from disk.
        /// </summary>
        private static void RemoveStaleProjectItem(IVsSolution solution, string expectedName)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            foreach (var folder in EnumerateSolutionFolders(solution))
            {
                ErrorHandler.ThrowOnFailure(folder.GetProperty(VSConstants.VSITEMID_ROOT,
                    (int)__VSHPROPID.VSHPROPID_FirstChild, out object child));
                while (TryGetItemId(child, out uint itemId))
                {
                    if (ErrorHandler.Succeeded(folder.GetProperty(itemId, (int)__VSHPROPID.VSHPROPID_Name, out object name)) &&
                        string.Equals(name as string, expectedName, StringComparison.OrdinalIgnoreCase))
                    {
                        if (!(folder is IVsHierarchyDeleteHandler deleteHandler))
                        {
                            throw new InvalidOperationException($"The solution folder cannot remove the stale project '{expectedName}'.");
                        }
                        uint operation = (uint)__VSDELETEITEMOPERATION.DELITEMOP_RemoveFromProject;
                        ErrorHandler.ThrowOnFailure(deleteHandler.QueryDeleteItem(operation, itemId, out int canDelete));
                        if (canDelete == 0)
                        {
                            throw new InvalidOperationException($"The stale project '{expectedName}' cannot be removed.");
                        }
                        ErrorHandler.ThrowOnFailure(deleteHandler.DeleteItem(operation, itemId));
                        return;
                    }
                    ErrorHandler.ThrowOnFailure(folder.GetProperty(itemId, (int)__VSHPROPID.VSHPROPID_NextSibling, out child));
                }
            }
        }

        private static void RemoveStaleHierarchy(IVsSolution solution, string csprojPath)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            string expectedName = Path.GetFileNameWithoutExtension(csprojPath);
            foreach (var hierarchy in EnumerateProjects(solution, __VSENUMPROJFLAGS.EPF_ALLINSOLUTION))
            {
                bool pathMatch = hierarchy is IVsProject project &&
                    ErrorHandler.Succeeded(project.GetMkDocument(VSConstants.VSITEMID_ROOT, out string path)) &&
                    string.Equals(path, csprojPath, StringComparison.OrdinalIgnoreCase);

                // A missing project's StubHierarchy has no IVsProject/path, only its declared name.
                bool nameMatch = !(hierarchy is IVsProject) &&
                    ErrorHandler.Succeeded(hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_Name, out object name)) &&
                    string.Equals(name as string, expectedName, StringComparison.OrdinalIgnoreCase);
                if (pathMatch || nameMatch)
                {
                    int hr = solution.CloseSolutionElement((uint)__VSSLNCLOSEOPTIONS.SLNCLOSEOPT_DeleteProject, hierarchy, 0);
                    if (ErrorHandler.Failed(hr))
                    {
                        Logger.Information($"SolutionWiring: could not close stale hierarchy '{csprojPath}' (0x{hr:X8}); trying its folder item.");
                    }
                    return;
                }
            }
        }

        /// <summary>Finds the top-level Solution Folder with the given name, creating it if
        /// it doesn't exist yet.</summary>
        private static IVsHierarchy EnsureSolutionFolder(IVsSolution solution, string folderName)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var folder = FindSolutionFolder(solution, folderName);
            if (folder != null)
            {
                return folder;
            }
            Guid projectType = SolutionFolderType;
            Guid hierarchyId = typeof(IVsHierarchy).GUID;
            IntPtr project = IntPtr.Zero;
            try
            {
                ErrorHandler.ThrowOnFailure(solution.CreateProject(ref projectType, null, null, folderName,
                    (uint)__VSCREATEPROJFLAGS.CPF_CLONEFILE, ref hierarchyId, out project));
                if (project == IntPtr.Zero)
                {
                    throw new InvalidOperationException($"Could not create the solution folder '{folderName}'.");
                }
                return (IVsHierarchy)Marshal.GetObjectForIUnknown(project);
            }
            finally
            {
                if (project != IntPtr.Zero)
                {
                    Marshal.Release(project);
                }
            }
        }

        /// <summary>
        /// Attempts to open designerCsPath in Designer view. Returns false with an error
        /// message on failure instead of throwing -- a freshly-added companion project may
        /// still need a NuGet restore / design-time build to finish.
        /// </summary>
        public static bool TryOpenInDesigner(string designerCsPath, out string errorMessage)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            try
            {
                VsShellUtilities.OpenDocument(ServiceProvider.GlobalProvider, designerCsPath,
                    VSConstants.LOGVIEWID.Designer_guid, out _, out _, out _);

                // VS reveals/expands a newly-opened document's containing Solution Explorer
                // node on its own timing, well after opening returns -- confirmed NOT the
                // "Track Active Item in Solution Explorer" setting (still happened with that
                // setting already off), so this looks like a one-time "show me where I just
                // opened this" behavior independent of that setting. Rather than chase the
                // exact moment, poll-and-collapse on a background thread for a few seconds.
                CollapseSolutionExplorerNodeWithRetry(ShadowDesignerFolderName);

                errorMessage = null;
                return true;
            }
            catch (Exception ex)
            {
                errorMessage = ex.Message;
                return false;
            }
        }

        private static void CollapseSolutionExplorerNodeWithRetry(string itemName)
        {
            // VS may reveal the document's node after OpenDocument returns.
            _ = System.Threading.Tasks.Task.Run(async () =>
            {
                for (int i = 0; i < 10; i++)
                {
                    await System.Threading.Tasks.Task.Delay(300);
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    CollapseSolutionExplorerNode(itemName);
                }
            });
        }

        /// <summary>Collapses the generated folder without affecting document opening on failure.</summary>
        private static void CollapseSolutionExplorerNode(string itemName)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            try
            {
                var folder = FindSolutionFolder(GetSolution(), itemName);
                if (folder is IVsUIHierarchy uiHierarchy)
                {
                    var window = VsShellUtilities.GetUIHierarchyWindow(ServiceProvider.GlobalProvider,
                        VSConstants.StandardToolWindows.SolutionExplorer);
                    if (window != null)
                    {
                        ErrorHandler.ThrowOnFailure(window.ExpandItem(uiHierarchy, VSConstants.VSITEMID_ROOT, EXPANDFLAGS.EXPF_CollapseFolder));
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "SolutionWiring: could not collapse the generated solution folder");
            }
        }

        /// <summary>Finds a loaded project by its full path, including projects in solution folders.</summary>
        public static IVsHierarchy FindProjectByFullPath(string fullPath)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            foreach (var hierarchy in EnumerateProjects(GetSolution(), __VSENUMPROJFLAGS.EPF_LOADEDINSOLUTION))
            {
                if (hierarchy is IVsProject project &&
                    ErrorHandler.Succeeded(project.GetMkDocument(VSConstants.VSITEMID_ROOT, out string path)) &&
                    string.Equals(path, fullPath, StringComparison.OrdinalIgnoreCase))
                {
                    return hierarchy;
                }
            }
            return null;
        }

        private static readonly Guid SolutionFolderType = new Guid("2150E333-8FDC-42A3-9474-1A3956D46DE8");

        private static IVsSolution GetSolution()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            return ServiceProvider.GlobalProvider.GetService(typeof(SVsSolution)) as IVsSolution
                ?? throw new InvalidOperationException("Could not obtain the solution service.");
        }

        private static IVsHierarchy FindSolutionFolder(IVsSolution solution, string name)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            foreach (var folder in EnumerateSolutionFolders(solution))
            {
                if (ErrorHandler.Succeeded(folder.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_Name, out object value)) &&
                    string.Equals(value as string, name, StringComparison.OrdinalIgnoreCase))
                {
                    folder.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_ParentHierarchy, out object parent);
                    if (!(parent is IVsHierarchy parentHierarchy) ||
                        ErrorHandler.Failed(parentHierarchy.GetGuidProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_TypeGuid, out Guid parentType)) ||
                        parentType != SolutionFolderType)
                    {
                        return folder;
                    }
                }
            }
            return null;
        }

        private static IEnumerable<IVsHierarchy> EnumerateSolutionFolders(IVsSolution solution) =>
            EnumerateProjects(solution, __VSENUMPROJFLAGS.EPF_ALLPROJECTS | __VSENUMPROJFLAGS.EPF_MATCHTYPE, SolutionFolderType);

        private static IEnumerable<IVsHierarchy> EnumerateProjects(IVsSolution solution, __VSENUMPROJFLAGS flags, Guid type = default)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            ErrorHandler.ThrowOnFailure(solution.GetProjectEnum((uint)flags, ref type, out IEnumHierarchies hierarchies));
            var projects = new List<IVsHierarchy>();
            var buffer = new IVsHierarchy[1];
            int hr;
            while ((hr = hierarchies.Next(1, buffer, out uint fetched)) == VSConstants.S_OK && fetched == 1)
            {
                projects.Add(buffer[0]);
            }
            ErrorHandler.ThrowOnFailure(hr);
            return projects;
        }

        private static bool TryGetItemId(object value, out uint itemId)
        {
            itemId = value is int signed ? unchecked((uint)signed) : value is uint unsigned ? unsigned : VSConstants.VSITEMID_NIL;
            return itemId != VSConstants.VSITEMID_NIL;
        }
    }
}
