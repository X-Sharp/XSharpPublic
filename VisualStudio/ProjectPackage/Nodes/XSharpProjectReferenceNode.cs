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
using System.Diagnostics;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Imaging;

namespace XSharp.Project
{
    /// <summary>
    /// Knows about special requirements for project to project references
    /// </summary>
    [DebuggerDisplay("{Caption}")]
    public class XSharpProjectReferenceNode : ProjectReferenceNode
    {
        public XSharpProjectReferenceNode(ProjectNode root, ProjectElement element)
           : base(root, element)
        {
            AddProject();
        }

        public XSharpProjectReferenceNode(ProjectNode root, string referencedProjectName, string projectPath, string projectReference)
           : base(root, referencedProjectName, projectPath, projectReference)
        {
            AddProject();
        }

        private void AddProject()
        {
            XSharpProjectNode project = this.ProjectMgr as XSharpProjectNode;
            if (project != null)
                project.AddURL(this.Url, this);
            project.ProjectModel.AddProjectReference(this.Url);
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
            //finally we must evaluate the the rules applied on the base class
            if (!base.CanAddReference(out errorHandler, out existingNode))
            {
                return false;
            }
            return true;
        }
        public override void Remove(bool removeFromStorage)
        {
            XSharpProjectNode projectNode = (XSharpProjectNode)this.ProjectMgr;
            projectNode.ProjectModel.RemoveProjectReference(this.Url);
            base.Remove(removeFromStorage);
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
            ThreadHelper.ThrowIfNotOnUIThread();

            ErrorHandler.ThrowOnFailure(hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_TypeName, out projectType));
            return projectType as string;
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
            ProjectMgr = null;
            base.Dispose(disposing);
        }
        #endregion

    }
}
