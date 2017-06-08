//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    public static class XSolution
    {
        static Dictionary<string, XProject> xProjects;
        static XProject _orphanedFilesProject = null;

        static XSolution()
        {
            xProjects = new Dictionary<string, XProject>(StringComparer.OrdinalIgnoreCase);
        }

        public static bool Add(string projectName, XProject project)
        {
            if (xProjects.ContainsKey(projectName))
            {
                return false;
            }
            xProjects.Add(projectName, project);
            return true;
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
                // Flag as unloaded to make sure that a running filewalk 
                // for the project gets aborted
                project.Loaded = false;
                xProjects.Remove(projectName);
                return true;
            }
            return false;
        }

        public static bool Remove(XProject project)
        {
            if (project != null)
                return Remove(project.Name);
            return false;
        }


        public static XFile FindFile(string fileName)
        {
            XFile file = null;
            foreach (var prj in xProjects.Values)
            {
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
            foreach (KeyValuePair<string, XProject> project in xProjects)
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
                xProjects.Add(OrphanedFiles, _orphanedFilesProject);
                foreach (var asm in _orphanedFilesProject.AssemblyReferences)
                {
                    SystemTypeController.LoadAssembly(asm.FileName);
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
                    xProjects.Add(OrphanedFiles, _orphanedFilesProject);
                    prj.Project.AddAssemblyReference(typeof(System.String).Assembly.Location);
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
