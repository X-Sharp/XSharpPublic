//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Concurrent;

namespace XSharpModel
{
    public static class XSolution
    {
        static readonly ConcurrentDictionary<string, XProject> xProjects;
        static XProject _orphanedFilesProject = null;

        static XSolution()
        {
            xProjects = new ConcurrentDictionary<string, XProject>(StringComparer.OrdinalIgnoreCase);
        }

        public static bool Add(string projectName, XProject project)
        {
            if (xProjects.ContainsKey(projectName))
            {
                return false;
            }
            return xProjects.TryAdd(projectName, project);
        }

        public static bool Add(XProject project)
        {
            string projectName = project.Name;
            return XSolution.Add(projectName, project);
        }

        public static bool Remove(string projectName)
        {
            if (xProjects.ContainsKey(projectName))
            {
                var project = xProjects[projectName];
                // Flag as unloaded to make sure that a running file walker
                // for the project gets aborted
                project.UnLoad();
                var result = xProjects.TryRemove(projectName, out project);
                SystemTypeController.UnloadUnusedAssemblies();
                return result;
            }
            return false;
        }

        public static bool Remove(XProject project)
        {
            if (project != null)
            {
                return Remove(project.Name);
            }
            return false;
        }


        public static XFile FindFile(string fileName)
        {
            XFile file = null;
            foreach (var item in xProjects)
            {
                var prj = item.Value;
                file = prj.FindFullPath(fileName);
                if (file != null)
                {
                    break;
                }
            }
            return file;
        }

        public static XFile FindFullPath(string fullPath)
        {
            XFile file = null;
            foreach (var project in xProjects)
            {
                XProject prj = project.Value;
                file = prj.FindFullPath(fullPath);
                if (file != null)
                {
                    break;
                }
            }
            return file;
        }

        public static XProject FindProject(string projectFile)
        {
            XProject project;
            projectFile = System.IO.Path.GetFileNameWithoutExtension(projectFile);
            if (xProjects.TryGetValue(projectFile, out project))
            {
                return project;
            }
            return null;
        }

        public static void CloseAll()
        {
            xProjects.Clear();
            SystemTypeController.Clear();
            if (_orphanedFilesProject != null)
            {
                if (xProjects.TryAdd(OrphanedFiles, _orphanedFilesProject))
                {
                    foreach (var asm in _orphanedFilesProject.AssemblyReferences)
                    {
                        SystemTypeController.LoadAssembly(asm.FileName);
                    }
                }
            }
        }

        public static XProject OrphanedFilesProject
        {
            get
            {
                if (_orphanedFilesProject == null)
                {
                    _orphanedFilesProject = new XProject(new OrphanedFilesProject());
                    var prj = _orphanedFilesProject.ProjectNode as OrphanedFilesProject;
                    prj.Project = _orphanedFilesProject;
                    if (xProjects.TryAdd(OrphanedFiles, _orphanedFilesProject))
                    {
                        prj.Project.AddAssemblyReference(typeof(string).Assembly.Location);
                    }
                }
                return _orphanedFilesProject;
            }
        }
        private const string OrphanedFiles = "(OrphanedFiles)";

        public static void FileClose(string fileName)
        {
            var file = FindFile(fileName);
            if (file.Project == _orphanedFilesProject)
            {
                _orphanedFilesProject.RemoveFile(fileName);
            }
        }

        public static void WalkFile(string fileName)
        {
            var file = FindFile(fileName);
            if (file != null)
            {
                ModelWalker.GetWalker().FileWalk(file);
            }
        }
    }
}
