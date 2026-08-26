//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using EnvDTE;
using EnvDTE80;

namespace XSharp.Project.ShadowDesigner
{
    /// <summary>
    /// The only class in this feature that touches EnvDTE/DTE2. Adds the companion project
    /// to the running solution and opens it in Designer view.
    /// </summary>
    internal static class SolutionWiring
    {
        /// <summary>
        /// Adds csprojPath to the running solution unless a project with that exact full
        /// path is already present -- safe to call every time.
        /// </summary>
        public static void EnsureProjectInSolution(DTE2 dte, string csprojPath)
        {
            if (FindProjectByFullName(EnumerateProjects(dte.Solution), csprojPath) != null)
            {
                return;
            }
            dte.Solution.AddFromFile(csprojPath, Exclusive: false);
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
                errorMessage = null;
                return true;
            }
            catch (Exception ex)
            {
                errorMessage = ex.Message;
                return false;
            }
        }

        /// <summary>Finds the EnvDTE.Project matching the given full path, searching inside
        /// solution folders too.</summary>
        public static EnvDTE.Project FindProjectByFullPath(DTE2 dte, string fullPath)
        {
            return FindProjectByFullName(EnumerateProjects(dte.Solution), fullPath);
        }

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

            if (string.Equals(project.Kind, ProjectKinds.vsProjectKindSolutionFolder, StringComparison.OrdinalIgnoreCase))
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
