//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using XSharpModel;
using File = System.IO.File;


namespace XSharp.LanguageService
{
    /// <summary>
    /// This class is used to suspend and resume the background scanner upon certain Solution Events
    /// </summary>
    public class ModelScannerEvents
    {
        string solutionFile;

        static ModelScannerEvents events = null;

        public static void Start()
        {
            if (events == null)
                events = new ModelScannerEvents();
        }

        #region ctors
        public ModelScannerEvents()
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
                XSharpModel.ModelWalker.Suspend();
            });

        }

        private void DocumentEvents_Closed(string document)
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
        private void DocumentEvents_Opened(string document)
        {
            XSolution.WriteOutputMessage("DocumentEvents_Opened " + document ?? "(none)");
        }

        private void SolutionEvents_OnBeforeOpenSolution(string obj)
        {
            // we do not see this for the first solution that is opened
            // because we are usually not loaded then
            XSolution.WriteOutputMessage("SolutionEvents_OnBeforeOpenSolution " + obj ?? "(none)");
        }
#endif

        private void SolutionEvents_OnBeforeOpenProject(string obj)
        {

            XSolution.WriteOutputMessage("SolutionEvents_OnBeforeOpenProject " + obj ?? "(none)");
            checkProjectFile(obj);
        }
#if DEBUG
        private void SolutionEvents_OnAfterOpenProject(Community.VisualStudio.Toolkit.Project project)
        {
            XSolution.WriteOutputMessage("SolutionEvents_OnAfterOpenProject " + project.FullPath ?? "(none)");
            //if (project.IsXSharp())
            //{
            //var xProject = XSolution.FindProject(project.FullPath);
            //if (xProject != null)
            //{
            //    addProjectFiles(xProject, project);
            //    if (XSolution.IsOpen)
            //    {
            //        ModelWalker.AddProject(xProject);
            //        ModelWalker.Walk();
            //    }
            //}
            //}
        }
#endif

        private void ShellEvents_ShutdownStarted()
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
        private void SolutionEvents_OnAfterOpenSolution(SolutionItem obj)
        {
            // Restart scanning. Was suspended on opening of project system
            // or closing of previous solution
            if (obj != null)
            {

                // first check to see if there are any projects in the solution that have
                // not been loaded
                solutionFile = obj.FullPath;
                if (!string.IsNullOrEmpty(solutionFile))
                {
                    XSolution.Open(solutionFile);
                }
                return;
            }
        }



        private void SolutionEvents_OnBeforeCloseSolution()
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

        private void SolutionEvents_OnAfterCloseSolution()
        {
            XSolution.IsClosing = false;
        }

        static bool hasEnvironmentvariable = false;
        static ModelScannerEvents()
        {
            hasEnvironmentvariable = !String.IsNullOrEmpty(System.Environment.GetEnvironmentVariable("XSharpMsBuildDir"));
        }
        const string oldText = @"$(MSBuildExtensionsPath)\XSharp";
        const string newText = @"$(XSharpMsBuildDir)";
        const string MsTestGuid = @"{3AC096D0-A1C2-E12C-1390-A8335801FDAB};";

        private void checkProjectFile(string fileName)
        {
            if (fileName != null && fileName.ToLower().EndsWith("xsproj") && File.Exists(fileName))
            {
                string xml = File.ReadAllText(fileName);
                var original = Path.ChangeExtension(fileName, ".original");
                bool changed = false;
                if (hasEnvironmentvariable)
                {
                    var pos = xml.IndexOf(oldText, StringComparison.OrdinalIgnoreCase);
                    if (pos >= 0)
                    {
                        while (pos > 0)
                        {
                            xml = xml.Substring(0, pos) + newText + xml.Substring(pos + oldText.Length);
                            pos = xml.IndexOf(oldText, StringComparison.OrdinalIgnoreCase);
                        }
                        DeleteFileSafe(original);
                        File.Copy(fileName, original);
                        DeleteFileSafe(fileName);
                        File.WriteAllText(fileName, xml);
                        changed = true;
                    }
                }
                var testpos = xml.IndexOf(MsTestGuid, StringComparison.OrdinalIgnoreCase);
                if (testpos >= 0)
                {
                    var left = xml.Substring(0, testpos);
                    var right = xml.Substring(testpos + MsTestGuid.Length);
                    if (!changed)
                    {
                        DeleteFileSafe(original);
                        File.Copy(fileName, original);
                    }
                    xml = left + right;
                    DeleteFileSafe(fileName);
                    File.WriteAllText(fileName, xml);
                    changed = true;
                }
                if (changed)
                {
                    XSolution.ChangedProjectFiles.Add(fileName, original);
                }
            }
        }
        public static bool DeleteFileSafe(string fileName)
        {
            try
            {
                if (File.Exists(fileName))
                {
                    File.SetAttributes(fileName, FileAttributes.Normal);
                    File.Delete(fileName);

                }
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
                return false;
            }
            return true;

        }
        #endregion

    }
}
internal static class CVTProjectExtensions
{
    internal static bool IsXSharp(this Project project)
    {
        if (project != null)
        {
            var path = project.FullPath;
            var ext = System.IO.Path.GetExtension(path).ToLower();
            return ext == ".xsproj" || ext == ".xsprj";
        }
        return false;
    }
}

