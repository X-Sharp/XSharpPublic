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

using Microsoft.VisualStudio.Project;
namespace XSharp.Project
{
    /// <summary>
    /// This class is used to suspend and resume the background scanner upon certain Solution Events
    /// </summary>
    class ModelScannerEvents : SolutionListener
    {
        List<string> projectfiles;
        static Dictionary<string,string> changedProjectfiles;

        static internal IDictionary<string, string> ChangedProjectFiles => changedProjectfiles;
        #region ctors
        public ModelScannerEvents(IServiceProvider serviceProvider)
            : base(serviceProvider)
        {
            projectfiles = new List<string>();
            changedProjectfiles = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            XSharpModel.ModelWalker.Suspend();
        }

        /// <summary>
        /// Called at load time when solution has finished opening.
        /// </summary>
        /// <param name="pUnkReserved">reserved</param>
        /// <param name="fNewSolution">true if this is a new solution</param>
        /// <returns></returns>
        public override int OnAfterOpenSolution(object pUnkReserved, int fNewSolution)
        {
            // Restart scanning. Was suspended on opening of project system
            // or closing of previous solution
            EnvDTE80.DTE2 dte = Microsoft.VisualStudio.Shell.Package.GetGlobalService(typeof(EnvDTE.DTE)) as EnvDTE80.DTE2;
            EnvDTE80.Solution2 solution = dte.Solution as EnvDTE80.Solution2;
            var solutionFile = solution.FullName;
            if (string.IsNullOrEmpty(solutionFile))
            {
                if (projectfiles.Count > 0)
                {
                    // open a project without solution.
                    // assume solution name is the same as project name with different extension
                    solutionFile = System.IO.Path.ChangeExtension(projectfiles[0], ".sln");
                }

            }
            if (! string.IsNullOrEmpty(solutionFile))
            {
                XSharpModel.XSolution.Open(solutionFile);
                XSharpProjectPackage.Instance.SetCommentTokens();
            }
            projectfiles.Clear();

            /*
            Code below to detect items in solution folders
            var projects = solution.Projects;
            var folder1 = new Guid("{66A26720-8FB5-11D2-AA7E-00C04F688DDE}"); // = Project Folder
            var folder2 = new Guid("{2150E333-8FDC-42A3-9474-1A3956D46DE8}"); // Solution Folder
            foreach (var prj in projects)
            {
                var project = (EnvDTE.Project)prj;
                var kind = new Guid(project.Kind);
                var name = project.FullName;
                if (kind == folder1 || kind == folder2)
                {
                    foreach (var item in project.ProjectItems)
                    {
                        var prjItem = (EnvDTE.ProjectItem)item;
                        if (prjItem.Object == null)
                        {
                            for (short i = 1; i <= prjItem.FileCount; i++)
                            {
                                var file = prjItem.FileNames[i];
                                Debug.WriteLine(file);
                            }
                        }
                    }
                }

            }
            */
            return VSConstants.S_OK;
        }

        public override int OnBeforeCloseSolution(object pUnkReserved)
        {
            XSharpModel.XSolution.IsClosing = true;
            XSharpModel.XSolution.Close();
            return VSConstants.S_OK;
        }
        public override int OnAfterCloseSolution(object reserved)
        {
            XSharpModel.XSolution.Close();
            XSharp.LanguageService.XSharpXMLDocTools.Close();
            XSharpModel.XSolution.IsClosing = false;
            return VSConstants.S_OK;
        }
        static bool hasEnvironmentvariable = false;
        static ModelScannerEvents()
        {
            hasEnvironmentvariable = !String.IsNullOrEmpty(System.Environment.GetEnvironmentVariable("XSharpMsBuildDir"));
        }
        const string oldText = @"$(MSBuildExtensionsPath)\XSharp";
        const string newText = @"$(XSharpMsBuildDir)";
        public override void OnBeforeOpenProject(ref Guid guidProjectID, ref Guid guidProjectType, string pszFileName)
        {
            if (hasEnvironmentvariable && pszFileName != null && pszFileName.ToLower().EndsWith("xsproj") && System.IO.File.Exists(pszFileName))
            {
                string xml = File.ReadAllText(pszFileName);
                var pos = xml.IndexOf(oldText, StringComparison.OrdinalIgnoreCase);
                if (pos >= 0)
                {
                    while (pos > 0)
                    {
                        xml = xml.Substring(0, pos) + newText + xml.Substring(pos + oldText.Length);
                        pos = xml.IndexOf(oldText, StringComparison.OrdinalIgnoreCase);
                    }
                    var original = Path.ChangeExtension(pszFileName, ".original");
                    Utilities.DeleteFileSafe(original);
                    File.Copy(pszFileName, original);
                    Utilities.DeleteFileSafe(pszFileName);
                    File.WriteAllText(pszFileName, xml);
                    changedProjectfiles.Add(pszFileName, original);
                }
            }
            projectfiles.Add(pszFileName);


        }

        #endregion

    }
}

