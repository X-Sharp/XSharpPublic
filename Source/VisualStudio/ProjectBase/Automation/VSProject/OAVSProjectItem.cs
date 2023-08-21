/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
 * copy of the license can be found in the License.txt file at the root of this distribution. 
 * 
 * You must not remove this notice, or any other, from this software.
 *
 * ***************************************************************************/

using EnvDTE;
using Microsoft.VisualStudio.Shell;
using System;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.InteropServices;
using VSLangProj;

namespace Microsoft.VisualStudio.Project.Automation
{
    /// <summary>
    /// Represents a language-specific project item
    /// </summary>
    [SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "OAVS")]
    [ComVisible(true), CLSCompliant(false)]
    public class OAVSProjectItem : VSProjectItem
    {
        #region fields
        private FileNode fileNode;
        #endregion

        #region ctors
        internal OAVSProjectItem(FileNode fileNode)
        {
            this.FileNode = fileNode;
        }
        #endregion

        #region VSProjectItem Members

        public virtual EnvDTE.Project ContainingProject
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return fileNode.ProjectMgr.GetAutomationObject() as EnvDTE.Project;
            }
        }

        public virtual ProjectItem ProjectItem
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return fileNode.GetAutomationObject() as ProjectItem;
            }
        }

        public virtual DTE DTE
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return (DTE)this.fileNode.ProjectMgr.Site.GetService(typeof(DTE));
            }
        }

        public virtual void RunCustomTool()
        {
            this.FileNode.RunGenerator();
        }

        #endregion

        #region public properties
        /// <summary>
        /// File Node property
        /// </summary>
        internal FileNode FileNode
        {
            get
            {
                return fileNode;
            }
            set
            {
                fileNode = value;
            }
        }
        #endregion

    }
}
