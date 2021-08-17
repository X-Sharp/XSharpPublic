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

using Microsoft.VisualStudio.Shell;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell.Interop;

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
			ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                VS.Events.SolutionEvents.OnAfterOpenSolution += SolutionEvents_OnAfterOpenSolution;
                VS.Events.SolutionEvents.OnBeforeCloseSolution += SolutionEvents_OnBeforeCloseSolution;
                VS.Events.SolutionEvents.OnAfterCloseSolution += SolutionEvents_OnAfterCloseSolution;
                VS.Events.SolutionEvents.OnBeforeOpenProject += SolutionEvents_OnBeforeOpenProject;
                VS.Events.SolutionEvents.OnAfterOpenProject += SolutionEvents_OnAfterOpenProject;
                VS.Events.ShellEvents.ShutdownStarted += ShellEvents_ShutdownStarted;
                projectfiles = new List<string>();
                changedProjectfiles = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
                XSharpModel.ModelWalker.Suspend();
            });

        }

        private void SolutionEvents_OnAfterOpenProject(object sender, Microsoft.VisualStudio.Shell.Events.OpenProjectEventArgs e)
        {
            //SolutionItem project = null;
            //ThreadHelper.JoinableTaskFactory.Run(async delegate
            //{
            //    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            //    project = await SolutionItem.FromHierarchyAsync(e.Hierarchy, 1) ;
            //});
            //if (project != null)
            //{
            //    var dirs = new List<SolutionItem>();
            //    dirs.Add(project);
            //    var files = new List<SolutionItem>();
            //    while (dirs.Count > 0)
            //    {
            //        var dirsarray = dirs.ToArray();
            //        dirs.Clear();
            //        foreach (var dir in dirsarray)
            //        {
            //            foreach (var item in dir.Children)
            //            {
            //                if (item.Type == NodeType.PhysicalFile)
            //                {
            //                    files.Add(item);
            //                }
            //                else if (item.Type == NodeType.PhysicalFolder)
            //                {
            //                    dirs.Add(item);
            //                }
            //            }
            //        }
            //    }
            //}
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

        /// <summary>
        /// Called at load time when solution has finished opening.
        /// </summary>
        /// <param name="pUnkReserved">reserved</param>
        /// <param name="fNewSolution">true if this is a new solution</param>
        /// <returns></returns>
        private void SolutionEvents_OnAfterOpenSolution(object sender, Microsoft.VisualStudio.Shell.Events.OpenSolutionEventArgs e)
        {
            // Restart scanning. Was suspended on opening of project system
            // or closing of previous solution
            IVsSolution solution = null;


            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                
                solution = await  VS.Services.GetSolutionAsync();
                solution.GetSolutionInfo(out var _, out var file, out var _);
                solutionFile = file;
                if (solution != null && e.IsNewSolution)
                {
                    solution.SaveSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_ForceSave, null, 0);
                }
                var hierarchies = solution.GetAllProjectHierarchys();
                var list = new List<SolutionItem>();
                var fileType = VSConstants.GUID_ItemType_PhysicalFile.ToString("B").ToUpper();
                var folderType = VSConstants.GUID_ItemType_PhysicalFolder.ToString("B").ToUpper();
                var virtualType = VSConstants.GUID_ItemType_VirtualFolder.ToString("B").ToUpper();
                foreach (IVsHierarchy hierarchy in hierarchies)
                {
                    if (hierarchy.IsCapabilityMatch("XSHARP"))
                    {

                        hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_ExtObject, out var objProj);
                        var project = objProj as EnvDTE.Project;
                        System.Diagnostics.Debug.WriteLine(project.FullName);
                        var children = new List<EnvDTE.ProjectItem>();
                        var files = new List<string>();
                        var folders = new List<string>();
                        addItems(project.ProjectItems, files, folders, virtualType, folderType, fileType);
                        foreach (var folder in folders)
                        {
                            System.Diagnostics.Debug.WriteLine("folder " + folder);
                        }
                        foreach (var f in files)
                        {
                            System.Diagnostics.Debug.WriteLine("file    " + f);
                        }
                    }
                }
            });
            
            if (string.IsNullOrEmpty(solutionFile))
            {
                if (projectfiles.Count > 0)
                {
                    // open a project without solution.
                    // assume solution name is the same as project name with different extension
                    solutionFile = Path.ChangeExtension(projectfiles[0], ".sln");
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

        void addItems(EnvDTE.ProjectItems items, List<string> files, List<string> folders, string virtualType, string folderType, string fileType)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            foreach (var item in items)
            {
                var projectItem = item as EnvDTE.ProjectItem;
                // skip virtual folders, such as references
                if (projectItem != null)
                {
                    //if (string.Compare(projectItem.Kind, virtualType, true) == 0)
                    //    continue;
                    if (projectItem.ProjectItems.Count > 0)
                    {
                        addItems(projectItem.ProjectItems, files, folders, virtualType, folderType, fileType);
                    }
                    for (short i = 0; i < projectItem.FileCount; i++)
                    {
                        if (projectItem.Kind.ToUpper() == fileType)
                        {
                            files.Add(projectItem.FileNames[i]);
                        }
                        if (projectItem.Kind.ToUpper() == folderType)
                        {
                            folders.Add(projectItem.FileNames[i]);
                        }
                    };
                }
                else
                {
                    ;
                }
            }
        }


        private void  SolutionEvents_OnBeforeCloseSolution(object sender, EventArgs e)
        {
            // close OUR documents that are opened in design mode.
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {

                XSharpModel.XSolution.IsClosing = true;
                XSharpModel.XSolution.Close();

                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                var d = await VS.GetServiceAsync<EnvDTE.DTE, EnvDTE.DTE>();

                if (d == null)
                {
                    return;
                }
                EnvDTE80.DTE2 dte = d as EnvDTE80.DTE2;
                var docs = dte.Documents;
                var windows = new List<EnvDTE.Window>();
                foreach (EnvDTE.Document doc in docs)
                {
                    if (doc.FullName.ToLower().EndsWith(".prg"))
                    {
                        var wnd = doc.ActiveWindow;
                        if (wnd != null && wnd.Caption.EndsWith("]"))
                        {
                            windows.Add(wnd);
                        }
                    }
                }
                foreach (var wnd in windows)
                {
                    wnd.Document.Save();
                    wnd.Close();
                }

            });
            return;
        }
        private void SolutionEvents_OnAfterCloseSolution(object sender, EventArgs e)
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

        private void SolutionEvents_OnBeforeOpenProject(object sender, Microsoft.VisualStudio.Shell.Events.BeforeOpenProjectEventArgs e)
        {
            var pszFileName = e.Filename;
            if (pszFileName != null && pszFileName.ToLower().EndsWith("xsproj") && System.IO.File.Exists(pszFileName))
            {
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
            projectfiles.Add(pszFileName);
        }
        public static bool DeleteFileSafe(string fileName)
        {
            try
            {
                if (System.IO.File.Exists(fileName))
                {
                    System.IO.File.SetAttributes(fileName, FileAttributes.Normal);
                    System.IO.File.Delete(fileName);

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

