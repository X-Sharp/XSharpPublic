//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;
using System.Drawing;
using System.IO;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using EnvDTE;
using VSLangProj;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Project;
using Microsoft.Windows.Design.Host;

using System.Collections.Generic;
using Microsoft.VisualStudio.Shell.Interop;
using XSharp.Project.WPF;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio;
using System.Diagnostics;
using MSBuild = Microsoft.Build.Evaluation;
using OleConstants = Microsoft.VisualStudio.OLE.Interop.Constants;

namespace XSharp.Project
{
    /// <summary>
    /// This class extends the ProjectNode in order to represent our project 
    /// within the hierarchy.
    /// </summary>
    [Guid("F1A46976-964A-4A1E-955D-E05F5DB8651F")]
    public class ProjectNodeX : ProjectNode//, IVsProjectSpecificEditorMap2
    {
        private bool showAllFilesEnabled;
        private MSBuild.Project userBuildProject;
        public ProjectNodeX() : base()
        {
        }

        /// <summary>
        /// Gets the project GUID.
        /// </summary>
        /// <value>The project GUID.</value>
        public override Guid ProjectGuid
        {
            get { return typeof(XSharpProjectFactory).GUID; }
        }

        /// <summary>
        /// Gets the type of the project.
        /// </summary>
        /// <value>The type of the project.</value>
        public override string ProjectType
        {
            get { return ""; }
        }

        /// <summary>
        /// Gets the path to the .user file
        /// </summary>
        private string UserFileName
        {
            get { return this.FileName + ProjectNode.PerUserFileExtension; }
        }
        /// <summary>
        /// Returns the MSBuild project associated with the .user file
        /// </summary>
        /// <value>The MSBuild project associated with the .user file.</value>
        public MSBuild.Project UserBuildProject
        {
            get
            {
                if (this.userBuildProject == null && File.Exists(this.UserFileName))
                {
                    this.CreateUserBuildProject();
                }

                return this.userBuildProject;
            }
        }

        public override int IsDirty(out int isDirty)
        {
            int res = base.IsDirty(out isDirty);
            if (isDirty == 0 && this.UserBuildProject != null)
            {
                isDirty = this.UserBuildProject.IsDirty ? 1 : 0;
            }
            return res;
        }

        public void CreateUserBuildProject()
        {
            this.userBuildProject = new MSBuild.Project(this.UserFileName);
        }

        /// <summary>
        /// Gets if the ShowAllFiles is enabled or not.
        /// </summary>
        /// <value>true if the ShowAllFiles option is enabled, false otherwise.</value>
        public bool ShowAllFilesEnabled
        {
            get
            {
                return this.showAllFilesEnabled;
            }
        }

        public object VulcanProjectFileConstants { get; private set; }

        protected internal int ToggleShowAllFiles()
        {
            if (this.ProjectMgr == null || this.ProjectMgr.IsClosed)
            {
                return (int)OleConstants.OLECMDERR_E_NOTSUPPORTED;
            }

            using (SupportMethods.NewWaitCursor())
            {
                this.showAllFilesEnabled = !this.showAllFilesEnabled; // toggle the flag

                if (this.showAllFilesEnabled)
                {
                    XSharpProjectMembers.AddNonMemberItems(this);
                }
                else
                {
                    XSharpProjectMembers.RemoveNonMemberItems(this);
                }
            }

            return VSConstants.S_OK;
        }
        #region Cache URLs and other canonicalnames
        protected Dictionary<string, HierarchyNode> URLNodes = new Dictionary<string, HierarchyNode>(1000);

        public string CleanURL(string url)
        {
            url = url.ToLowerInvariant();
            return url.TrimEnd('\\'); ;
        }
        public void RemoveURL(HierarchyNode node)
        {
            try
            {
                if (node.ItemNode != null)
                {
                    String url = node.Url;
                    RemoveURL(url);
                }
            }
            catch { }

        }

        public void RemoveURL(String url)
        {
            url = CleanURL(url);
            if (URLNodes.ContainsKey(url))
                URLNodes.Remove(url);

        }

        public void AddURL(String url, HierarchyNode node)
        {
            url = this.CleanURL(url);
            if (URLNodes.ContainsKey(url))
                URLNodes.Remove(url);
            URLNodes.Add(url, node);

        }


        public HierarchyNode FindURL(String url)
        {
            if (String.IsNullOrEmpty(url))
                return null;
            HierarchyNode node = null;
            url = CleanURL(url);
            URLNodes.TryGetValue(url, out node);
            return node;
        }
        public override int ParseCanonicalName(string name, out uint itemId)
        {

            if (String.Equals(name, this.Url, StringComparison.OrdinalIgnoreCase))
                itemId = this.ID;
            else
            {
                name = CleanURL(name);
                HierarchyNode node = null;
                if (URLNodes.TryGetValue(name, out node))
                {
                    itemId = node.ID;
                }
                else
                {
                    itemId = VSConstants.VSITEMID_NIL;
                }
            }
            return VSConstants.S_OK;

        }

        #endregion

        protected override void Reload()
        {
            base.Reload();

            // read .user file
            if (this.UserBuildProject != null)
            {
                // Read show all files flag
                string propertyValue = this.UserBuildProject.GetPropertyValue("ProjectView");
                if (String.Equals(propertyValue, "ShowAllFiles", StringComparison.OrdinalIgnoreCase))
                {
                    this.ToggleShowAllFiles();
                }
            }

        }



    }
}
