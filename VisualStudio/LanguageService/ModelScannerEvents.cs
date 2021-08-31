//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using Microsoft.VisualStudio;
using IServiceProvider = System.IServiceProvider;
using System.IO;
using System.Threading.Tasks;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell.Interop;
using XSharpModel;
using File = System.IO.File;
using Shell = Microsoft.VisualStudio.Shell;
using System.Linq;

namespace XSharp.LanguageService
{
    /// <summary>
    /// This class is used to suspend and resume the background scanner upon certain Solution Events
    /// </summary>
    public class ModelScannerEvents 
    {
        List<string> projectfiles;
        string solutionFile;
        static Dictionary<string,string> changedProjectfiles;

        static ModelScannerEvents events = null;

        public static void Start()
        {
            if (events == null)
                events = new ModelScannerEvents();
        }

        static public IDictionary<string, string> ChangedProjectFiles => changedProjectfiles;
        #region ctors
        public ModelScannerEvents()
        {
            Shell.ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await Shell.ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                VS.Events.SolutionEvents.OnAfterOpenSolution += SolutionEvents_OnAfterOpenSolution;
                VS.Events.SolutionEvents.OnBeforeCloseSolution += SolutionEvents_OnBeforeCloseSolution;
                VS.Events.SolutionEvents.OnAfterCloseSolution += SolutionEvents_OnAfterCloseSolution;
                VS.Events.SolutionEvents.OnBeforeOpenProject += SolutionEvents_OnBeforeOpenProject;
                VS.Events.ShellEvents.ShutdownStarted += ShellEvents_ShutdownStarted;
                projectfiles = new List<string>();
                changedProjectfiles = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
                XSharpModel.ModelWalker.Suspend();
            });

        }

        private void SolutionEvents_OnBeforeOpenProject(string obj)
        {
            checkProjectFile(obj);
        }

        private void DebuggerEvents_EnterRunMode(object sender, EventArgs e)
        {
            throw new NotImplementedException();
        }

        private void ShellEvents_ShutdownStarted()
        {
            XSharpModel.XSolution.IsClosing = true;
            XSharpModel.XSolution.Close();
        }

        private async Task EnumChildrenAsync(SolutionItem obj, List<string> projects)
        {
            //System.Diagnostics.Debug.WriteLine(obj.Type.ToString() + " " + obj.Name);
            try
            {
                foreach (var child in obj.Children)
                {
                    switch (child.Type)
                    {
                        case SolutionItemType.Project:
                            var fileName = child.FullPath;
                            projects.Add(fileName);
                            var project = (Community.VisualStudio.Toolkit.Project) child;
                            var isXS = await project.IsKindAsync(GuidStrings.guidXSharpProjectFactoryString);
                            if (isXS)
                            {
                                var xProject = XSolution.FindProject(fileName);
                                addProjectFiles(xProject, project);
                            }
                            else
                            {
                                ; // do not enumerate the files in other project types
                            }
                            break;
                        case SolutionItemType.PhysicalFile:
                            await EnumChildrenAsync(child, projects);
                            break;
                        case SolutionItemType.PhysicalFolder:
                            await EnumChildrenAsync(child, projects);
                            break;
                        case SolutionItemType.MiscProject:
                            await EnumChildrenAsync(child, projects);
                            break;
                        case SolutionItemType.VirtualProject:
                            await EnumChildrenAsync(child, projects);
                            break;
                        case SolutionItemType.Solution:
                            await EnumChildrenAsync(child, projects);
                            break;
                        case SolutionItemType.SolutionFolder:
                            await EnumChildrenAsync(child, projects);
                            break;
                        case SolutionItemType.Unknown:
                            await EnumChildrenAsync(child, projects);
                            break;
                    }
                }
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(obj.Type.ToString() + " " + e.Message);
            }
            return;
        }

        void addProjectFiles(XProject project, SolutionItem obj)
        {
            foreach (var child in obj.Children)
            {
                var fileName = child.FullPath;
                switch (child.Type)
                {
                    case SolutionItemType.PhysicalFile:
                        project.AddFile(fileName);
                        // Add nested files
                        addProjectFiles(project, child);
                        break;
                    case SolutionItemType.PhysicalFolder:
                        // Add nested files
                        addProjectFiles(project, child);
                        break;
                }
            }
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
            solutionFile = obj.FullPath;
            var projects = new List<string>();
            Shell.ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var sol = await VS.Solutions.GetCurrentSolutionAsync();
                await EnumChildrenAsync(obj, projects);
            });

            if (string.IsNullOrEmpty(solutionFile))
            {
                if (projects.Count > 0)
                {
                    // open a project without solution.
                    // assume solution name is the same as project name with different extension
                    solutionFile = Path.ChangeExtension(projects[0], ".sln");
                }

            }
            if (!string.IsNullOrEmpty(solutionFile))
            {
                XSharpModel.XSolution.Open(solutionFile);
                //XSharpProjectPackage.XInstance.SetCommentTokens();
            }
            projectfiles.Clear();
            return;
        }

  

        private void  SolutionEvents_OnBeforeCloseSolution()
        {
            bool hasXsProject = false;
            Shell.ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var vsProjects = await VS.Solutions.GetAllProjectsAsync();
                foreach (var prj in vsProjects)
                {
                    hasXsProject= await prj.IsKindAsync(GuidStrings.guidXSharpProjectFactoryString);
                    if (hasXsProject)
                    {
                        break;
                    }
                }
            });
            // close OUR documents that are opened in design mode.
            if (! hasXsProject)
            {
                return;
            }
            Shell.ThreadHelper.JoinableTaskFactory.Run(async delegate
            {

                XSharpModel.XSolution.IsClosing = true;
                XSharpModel.XSolution.Close();
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
            XSharpModel.XSolution.Close();
            XSharp.LanguageService.XSharpXMLDocTools.Close();
            XSharpModel.XSolution.IsClosing = false;
        }

        static bool hasEnvironmentvariable = false;
        static ModelScannerEvents()
        {
            hasEnvironmentvariable = !String.IsNullOrEmpty(System.Environment.GetEnvironmentVariable("XSharpMsBuildDir"));
        }
        const string oldText = @"$(MSBuildExtensionsPath)\XSharp";
        const string newText = @"$(XSharpMsBuildDir)";
        const string MsTestGuid = @"{3AC096D0-A1C2-E12C-1390-A8335801FDAB};";

        private void checkProjectFile(string pszFileName)
        {
            if (pszFileName != null && pszFileName.ToLower().EndsWith("xsproj") && File.Exists(pszFileName))
            {
                projectfiles.Add(pszFileName);
                string xml = File.ReadAllText(pszFileName);
                var original = Path.ChangeExtension(pszFileName, ".original");
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
                        File.Copy(pszFileName, original);
                        DeleteFileSafe(pszFileName);
                        File.WriteAllText(pszFileName, xml);
                        changed = true;
                    }
                }
                var testpos = xml.IndexOf(MsTestGuid, StringComparison.OrdinalIgnoreCase);
                if (testpos >= 0)
                {
                    var left = xml.Substring(0, testpos);
                    var right = xml.Substring(testpos + MsTestGuid.Length);
                    if (! changed)
                    {
                        DeleteFileSafe(original);
                        File.Copy(pszFileName, original);
                    }
                    xml = left + right;
                    DeleteFileSafe(pszFileName);
                    File.WriteAllText(pszFileName, xml);
                    changed = true;
                }
                if (changed)
                {
                    changedProjectfiles.Add(pszFileName, original);
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

