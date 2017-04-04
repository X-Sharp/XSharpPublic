//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using LanguageService.SyntaxTree.Misc;
using LanguageService.SyntaxTree.Tree;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace XSharpModel
{
    public class ModelWalker
    {
        static ModelWalker _walker;
        static ModelWalker()
        {
        }
        static internal ModelWalker GetWalker()
        {
            if (_walker == null)
                _walker = new ModelWalker();
            return _walker;
        }

        private Queue<XProject> _projects;
        private Thread _WalkerThread;
        private ModelWalker()
        {
            _projects = new Queue<XProject>();
        }

        internal void AddProject(XProject xProject)
        {
            lock (this)
            {
                bool lAdd2Queue = true;
                foreach (var prj in _projects)
                {
                    if (String.Equals(prj.Name, xProject.Name, StringComparison.OrdinalIgnoreCase))
                    {
                        lAdd2Queue = false;
                        break;
                    }
                }
                if (lAdd2Queue)
                {
                    _projects.Enqueue(xProject);
                }
                if (!IsWalkerRunning)
                {
                    Walk();
                }
            }
        }


        internal bool IsWalkerRunning
        {
            get
            {
                try
                {
                    if (_WalkerThread == null)
                        return false;
                    return _WalkerThread.IsAlive;
                }
                catch (Exception e)
                {
                    Debug.WriteLine("Cannot check Background walker Thread : ");
                    Debug.WriteLine(e.Message);
                }
                return false;

            }
        }

        internal void Walk()
        {
            try
            {
                StopThread();
                ThreadStart ts = new ThreadStart(this.Walker);
                _WalkerThread = new Thread(ts);
                _WalkerThread.IsBackground = true;
                _WalkerThread.Priority = ThreadPriority.Highest;
                _WalkerThread.Name = "ModelWalker";
                _WalkerThread.Start();
            }
            catch (Exception e)
            {
                Debug.WriteLine("Cannot start Background walker Thread : ");
                Debug.WriteLine(e.Message);
            }
            return;
        }

        private void Walker()
        {
            XProject project = null;
            //
            do
            {
                // 
                lock (this)
                {
                    // need to continue ?
                    if (_projects.Count == 0)
                    {
                        break;
                    }
                    project = _projects.Dequeue();
                    project.ProjectNode.SetStatusBarText($"Start scanning project {project.Name}");
                    //
                }
                var aFiles = project.Files.ToArray();
                int iProcessed = 0;
                Parallel.ForEach(aFiles, file =>
                {
                    // Detect project unload
                    if (project.Loaded)
                    {
                        iProcessed += 1;
                        project.ProjectNode.SetStatusBarText(String.Format("Walking {0} : Processing File {1} ({2} of {3})", project.Name, file.Name, iProcessed, aFiles.Length));
                        FileWalk(file);
                    }
                });
                project.ProjectNode.SetStatusBarText("");
            } while (true);
        }

        internal void FileWalk( XFile file )
        {
            SourceWalker sw = new SourceWalker();
            //
            sw.File = file;
            try
            {
                sw.InitParse();
                sw.BuildModelOnly();
                //
            }
            catch (Exception)
            {
                // Push Exception away...
                ;
            }
        }

        internal void StopThread()
        {
            try
            {
                if (_WalkerThread == null)
                    return;
                if (_WalkerThread.IsAlive)
                {

                }
            }
            catch (Exception e)
            {
                Debug.WriteLine("Cannot stop Background walker Thread : ");
                Debug.WriteLine(e.Message);
            }
            _WalkerThread = null;
            return;
        }
    }


}
