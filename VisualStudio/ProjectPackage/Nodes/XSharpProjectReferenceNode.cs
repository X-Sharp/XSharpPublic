//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio;

namespace XSharp.Project
{
    /// <summary>
    /// Knows about special requirements for project to project references
    /// </summary>
    public class XSharpProjectReferenceNode : ProjectReferenceNode
    {
        public XSharpProjectReferenceNode(ProjectNode root, ProjectElement element)
           : base(root, element)
        {
            XSharpProjectNode project = root as XSharpProjectNode;
            if (project != null)
                project.AddURL(this.Url, this);

        }

        public XSharpProjectReferenceNode(ProjectNode root, string referencedProjectName, string projectPath, string projectReference)
           : base(root, referencedProjectName, projectPath, projectReference)
        {
            XSharpProjectNode project = root as XSharpProjectNode;
            if (project != null)
                project.AddURL(this.Url, this);

        }

        /// <summary>
        /// Checks if a reference can be added to the project.
        /// It calls base to see if the reference is not already there,
        /// and that it is not circular reference.
        /// If the target project is a a Python Project we can not add the project reference
        /// because this scenario is not supported.
        /// </summary>
        /// <param name="errorHandler">The error handler delegate to return</param>
        /// <returns>false if reference cannot be added, otherwise true</returns>
        /// 

        protected override bool CanAddReference(out CannotAddReferenceErrorMessage errorHandler, out ReferenceNode existingNode)
        {
            existingNode = null;

            //If source project has designer files of subtype form and if the target output (assembly) does not exists
            //show a dialog that tells the user to build the target project before the project reference can be added
            if (!File.Exists(this.ReferencedProjectOutputPath) && HasFormItems())
            {
                errorHandler = new CannotAddReferenceErrorMessage(ShowProjectReferenceErrorMessage2);
                return false;
            }

            //finally we must evaluate the the rules applied on the base class
            if (!base.CanAddReference(out errorHandler, out existingNode))
            {
                return false;
            }

            return true;
        }

        /// <summary>
        /// Evaluates all file node children of the project and returns true if anyone has subtype set to Form
        /// </summary>
        /// <returns>true if a Filenode with subtype Form is found</returns>
        private bool HasFormItems()
        {
            List<XSharpFileNode> nodes = new List<XSharpFileNode>();
            this.ProjectMgr.FindNodesOfType<XSharpFileNode>(nodes);
            foreach (XSharpFileNode node in nodes)
            {
                //Todo
                // if (node.FileType.IsWinFormSubType || node.FileType.IsWinUserControl)
                // {
                //   return true;
                //}

            }
            return false;
        }

        /// <summary>
        /// Gets a Project type string for a specified project instance guid
        /// </summary>
        /// <param name="projectGuid">Project instance guid.</param>
        /// <returns>The project type string</returns>
        private string GetProjectType(Guid projectGuid)
        {
            IVsHierarchy hierarchy = VsShellUtilities.GetHierarchy(this.ProjectMgr.Site, projectGuid);
            object projectType;
            ErrorHandler.ThrowOnFailure(hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_TypeName, out projectType));
            return projectType as string;
        }
        public override int ImageIndex
        {
            get
            {
                if (this.CanShowDefaultIcon())
                    return XSharpImageListIndex.Reference + XSharpProjectNode.imageOffset;
                else
                    return XSharpImageListIndex.DanglingReference + XSharpProjectNode.imageOffset;
            }
        }

        /// <summary>
        /// Shows Visual Studio message box with error message regarding project to project reference. Target Project must be built before
        /// adding the the project to project reference.
        /// The message box is not show in case the method has been called from automation
        /// </summary>
        private void ShowProjectReferenceErrorMessage2()
        {
            if (!Utilities.IsInAutomationFunction(this.ProjectMgr.Site))
            {
                string message = "Error referencing project";
                string title = string.Empty;
                OLEMSGICON icon = OLEMSGICON.OLEMSGICON_CRITICAL;
                OLEMSGBUTTON buttons = OLEMSGBUTTON.OLEMSGBUTTON_OK;
                OLEMSGDEFBUTTON defaultButton = OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST;
                VsShellUtilities.ShowMessageBox(this.ProjectMgr.Site, title, message, icon, buttons, defaultButton);
            }
        }
        #region Dispose Methods
        protected override void Dispose(bool disposing)
        {
            if (this.ProjectMgr is XSharpProjectNode)
            {
                XSharpProjectNode projectNode = (XSharpProjectNode)this.ProjectMgr;
                if (projectNode != null)
                    projectNode.RemoveURL(this);
            }
            base.Dispose(disposing);
        }
        #endregion

    }
}
