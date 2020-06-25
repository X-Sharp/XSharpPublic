//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using IServiceProvider = System.IServiceProvider;

using Microsoft.VisualStudio.Project;
namespace XSharp.Project
{
    /// <summary>
    /// This class is used to suspend and resume the background scanner upon certain Solution Events
    /// </summary>
    class ModelScannerEvents : SolutionListener
    {
        #region ctors
        public ModelScannerEvents(IServiceProvider serviceProvider)
            : base(serviceProvider)
        {
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
            XSharpModel.XSolution.Open(solutionFile);
            XSharpModel.XSolution.IsClosing = false;
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
            XSharpXMLDocTools.Close();
            XSharpModel.XSolution.IsClosing = false;
            return VSConstants.S_OK;
        }
        #endregion

    }
}

