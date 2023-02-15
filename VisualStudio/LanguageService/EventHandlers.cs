//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System.Linq;
using XSharpModel;

namespace XSharp.LanguageService
{
    /// <summary>
    /// This class is used to suspend and resume the background scanner upon certain Solution Events
    /// </summary>
    public class ModelScannerEvents
    {
        static string solutionFile;
        public static void Start()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                VS.Events.SolutionEvents.OnAfterOpenSolution += SolutionEvents_OnAfterOpenSolution;
                VS.Events.SolutionEvents.OnBeforeCloseSolution += SolutionEvents_OnBeforeCloseSolution;
                VS.Events.SolutionEvents.OnAfterCloseSolution += SolutionEvents_OnAfterCloseSolution;
                VS.Events.SolutionEvents.OnBeforeOpenProject += SolutionEvents_OnBeforeOpenProject;
                VS.Events.DocumentEvents.Closed += DocumentEvents_Closed;
#if DEBUG
                VS.Events.SolutionEvents.OnBeforeOpenSolution += SolutionEvents_OnBeforeOpenSolution;
                VS.Events.SolutionEvents.OnAfterOpenProject += SolutionEvents_OnAfterOpenProject;
                VS.Events.DocumentEvents.Opened += DocumentEvents_Opened;
#endif
                VS.Events.ShellEvents.ShutdownStarted += ShellEvents_ShutdownStarted;
                var sol = await VS.Solutions.GetCurrentSolutionAsync();
                if (sol is Solution)
                {
                    SolutionEvents_OnAfterOpenSolution(sol);
                }
            });

        }

        private static void DocumentEvents_Closed(string document)
        {
            // Remove document from OrphanedFilesProject
            // So it can be opened in normal project afterwards
            // when possible
            XSolution.WriteOutputMessage("DocumentEvents_Closed " + document ?? "(none)");
            var xfile = XSolution.FindFile(document);
            if (xfile != null && xfile.Project.Name == OrphanedFilesProject.OrphanName)
            {
                XSolution.OrphanedFilesProject.RemoveFile(document);
            }
        }


#if DEBUG
        private static void DocumentEvents_Opened(string document)
        {
            XSolution.WriteOutputMessage("DocumentEvents_Opened " + document ?? "(none)");
        }

        private static void SolutionEvents_OnBeforeOpenSolution(string obj)
        {
            XSolution.WriteOutputMessage("SolutionEvents_OnBeforeOpenSolution " + obj ?? "(none)");
        }
#endif

        private static void SolutionEvents_OnBeforeOpenProject(string obj)
        {
            XSolution.WriteOutputMessage("SolutionEvents_OnBeforeOpenProject " + obj ?? "(none)");
        }
#if DEBUG
        private static void SolutionEvents_OnAfterOpenProject(Community.VisualStudio.Toolkit.Project project)
        {
            XSolution.WriteOutputMessage("SolutionEvents_OnAfterOpenProject " + project.FullPath ?? "(none)");
        }
#endif
        private static void ShellEvents_ShutdownStarted()
        {
            XSolution.IsClosing = true;
            XSolution.IsShuttingDown = true;
            XSolution.Close();
        }

        /// <summary>
        /// Called at load time when solution has finished opening.
        /// </summary>
        /// <param name="pUnkReserved">reserved</param>
        /// <param name="fNewSolution">true if this is a new solution</param>
        /// <returns></returns>
        ///
        private static void SolutionEvents_OnAfterOpenSolution(SolutionItem obj)
        {
            ModelWalker.Suspend();
            if (obj is Solution sol)
            {
                solutionFile = sol.FullPath;
                if (!string.IsNullOrEmpty(solutionFile))
                {
                    XSolution.Open(solutionFile);
                }
                return;
            }
        }

        private static void SolutionEvents_OnBeforeCloseSolution()
        {
            bool hasXsProject = XSolution.Projects.Count > 0;
            XSharpXMLDocTools.Close();
            XSolution.IsClosing = true;
            XSolution.Close();
            // close OUR documents that are opened in design mode.
            if (!hasXsProject)
            {
                return;
            }
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var frames = await VS.Windows.GetAllDocumentWindowsAsync();
                if (frames != null)
                {
                    foreach (var frame in frames.ToList())
                    {
                        if (frame.Caption.EndsWith("]"))
                        {
                            // no need to save here. VS has shown a dialog with the dirty files already
                            await frame.CloseFrameAsync(FrameCloseOption.NoSave);
                        }
                    }
                }
            });

            return;
        }

        private static void SolutionEvents_OnAfterCloseSolution()
        {
            XSolution.IsClosing = false;
        }
    }
}


