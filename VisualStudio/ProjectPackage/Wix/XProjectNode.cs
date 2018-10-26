//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Project;

using System.Collections.Generic;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio;
using MSBuild = Microsoft.Build.Evaluation;
using OleConstants = Microsoft.VisualStudio.OLE.Interop.Constants;
using VsCommands = Microsoft.VisualStudio.VSConstants.VSStd97CmdID;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;
using VsMenus = Microsoft.VisualStudio.Project.VsMenus;
using System.Diagnostics.CodeAnalysis;

namespace XSharp.Project
{
    /// <summary>
    /// This class extends the ProjectNode in order to represent our project
    /// within the hierarchy.
    /// Most of this code has been "borrowed" from the WixProjectNode class in Votive
    /// </summary>
    [Guid("0E55C091-DDDC-40CC-95FF-CCBCE51D35A9")]
    public abstract class XProjectNode : ProjectNode
    {

        public XProjectNode(): base()
        {
            URLNodes = new Dictionary<string, HierarchyNode>(1000, StringComparer.OrdinalIgnoreCase);
        }
        /// <summary>
        /// Initialize common project properties with default value if they are empty.
        /// </summary>
        /// <remarks>
        /// The following common project properties are set to default values: OutputName.
        /// </remarks>
        protected override void InitializeProjectProperties()
        {
            if (String.IsNullOrWhiteSpace(this.GetProjectProperty(XProjectFileConstants.OutputName)))
            {
                string projectName = Path.GetFileNameWithoutExtension(this.FileName);
                this.SetProjectProperty(XProjectFileConstants.OutputName, projectName);
            }
        }


        #region ShowAllFiles
        private bool showAllFilesEnabled;

        /// <summary>
        /// This method helps converting any non member node into the member one.
        /// </summary>
        /// <param name="node">Node to be added.</param>
        /// <returns>Returns the result of the conversion.</returns>
        /// <remarks>This method helps including the non-member items into the project when ShowAllFiles option is enabled.
        /// Normally, the project ignores "Add Existing Item" command if it is in ShowAllFiles mode and the non-member node
        /// exists for the item being added. Overridden to alter this behavior (now it includes the non-member node in the
        /// project)</remarks>
        protected override VSADDRESULT IncludeExistingNonMemberNode(HierarchyNode node)
        {
            IProjectSourceNode sourceNode = node as IProjectSourceNode;
            if (sourceNode != null && sourceNode.IsNonMemberItem)
            {
                if (sourceNode.IncludeInProject() == VSConstants.S_OK)
                {
                    return VSADDRESULT.ADDRESULT_Success;
                }
            }

            return base.IncludeExistingNonMemberNode(node);
        }

        /// <summary>
        /// Enables / Disables the ShowAllFileMode.
        /// </summary>
        /// <returns>S_OK if it's possible to toggle the state, OLECMDERR_E_NOTSUPPORTED if not</returns>
        protected internal override int ShowAllFiles()
        {
            int result = this.ToggleShowAllFiles();

            if (result != NativeMethods.S_OK)
            {
                return result;
            }

            if (this.userBuildProject == null)
            {
                this.CreateUserBuildProject();
            }
            // Save Project view in .user file.
            this.UserBuildProject.SetProperty(
                XProjectFileConstants.ProjectView,
                (this.showAllFilesEnabled ? XProjectFileConstants.ShowAllFiles : XProjectFileConstants.ProjectFiles));

            return result;
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

        protected internal int ToggleShowAllFiles()
        {
            if (this.ProjectMgr == null || this.ProjectMgr.IsClosed)
            {
                return (int)OleConstants.OLECMDERR_E_NOTSUPPORTED;
            }

            using (XHelperMethods.NewWaitCursor())
            {
                this.showAllFilesEnabled = !this.showAllFilesEnabled; // toggle the flag

                if (this.showAllFilesEnabled)
                {
                    XProjectMembers.AddNonMemberItems(this);
                }
                else
                {
                    XProjectMembers.RemoveNonMemberItems(this);
                }
            }

            return VSConstants.S_OK;
        }
        protected override void Reload()
        {
            base.Reload();

            // read .user file
            if (this.UserBuildProject != null)
            {
                // Read show all files flag
                string propertyValue = this.UserBuildProject.GetPropertyValue(XProjectFileConstants.ProjectView);
                if (String.Equals(propertyValue, XProjectFileConstants.ShowAllFiles, StringComparison.OrdinalIgnoreCase))
                {
                    this.ToggleShowAllFiles();
                }
            }

        }

        #endregion
        #region Cache URLs and other canonicalnames
        protected Dictionary<string, HierarchyNode> URLNodes ;

        public string CleanURL(string url)
        {
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

        public virtual void RemoveURL(String url)
        {
            url = CleanURL(url);
            if (URLNodes.ContainsKey(url))
                URLNodes.Remove(url);

        }

        public virtual void AddURL(String url, HierarchyNode node)
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
        /// <summary>
        /// Handle overwriting of an existing item in the hierarchy.
        /// </summary>
        /// <param name="existingNode">The node that exists.</param>
        protected override void OverwriteExistingItem(HierarchyNode existingNode)
        {
            IProjectSourceNode sourceNode = existingNode as IProjectSourceNode;
            if (sourceNode != null && sourceNode.IsNonMemberItem)
            {
                sourceNode.IncludeInProject();
            }
            else
            {
                base.OverwriteExistingItem(existingNode);
            }
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
        #region Query Methods

        /// <summary>
        /// Handles command status on the project node. If a command cannot be handled then the base should be called.
        /// </summary>
        /// <param name="cmdGroup">A unique identifier of the command group. The pguidCmdGroup parameter can be NULL to specify the standard group.</param>
        /// <param name="cmd">The command to query status for.</param>
        /// <param name="pCmdText">Pointer to an OLECMDTEXT structure in which to return the name and/or status information of a single command. Can be NULL to indicate that the caller does not require this information.</param>
        /// <param name="result">An out parameter specifying the QueryStatusResult of the command.</param>
        /// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
        [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#", Justification = "In the 2005 SDK, it's called guidCmdGroup and in the 2008 SDK it's cmdGroup")]
        protected override int QueryStatusOnNode(Guid cmdGroup, uint cmd, IntPtr pCmdText, ref QueryStatusResult result)
        {
            if (cmdGroup == VsMenus.guidStandardCommandSet97)
            {
                switch ((VsCommands)cmd)
                {
                    case VsCommands.Copy:
                        result = QueryStatusResult.NOTSUPPORTED;
                        return (int)OleConstants.OLECMDERR_E_NOTSUPPORTED;

                    case VsCommands.ProjectProperties:
                        // Sets the menu command text to 'ProjectName &Properties' where ProjectName is the name of this project
                        string propertiesMenuCommandText = Path.GetFileNameWithoutExtension(this.ProjectFile) + " &Properties";
                        NativeMethods.OLECMDTEXT.SetText(pCmdText, propertiesMenuCommandText);
                        result = QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED;
                        return (int)VSConstants.S_OK;

                }
            }

            if (this.QueryStatusOnProjectNode(cmdGroup, cmd, ref result))
            {
                return VSConstants.S_OK;
            }

            return base.QueryStatusOnNode(cmdGroup, cmd, pCmdText, ref result);
        }

        /// <summary>
        /// Handles menus originating from IOleCommandTarget.
        /// </summary>
        /// <param name="cmdGroup">Unique identifier of the command group</param>
        /// <param name="cmd">The command to be executed.</param>
        /// <param name="handled">Specifies whether the menu was handled.</param>
        /// <returns>A QueryStatusResult describing the status of the menu.</returns>
        [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#", Justification = "In the 2005 SDK, it's called guidCmdGroup and in the 2008 SDK it's cmdGroup.")]
        protected override QueryStatusResult QueryStatusCommandFromOleCommandTarget(Guid cmdGroup, uint cmd, out bool handled)
        {
            if (cmdGroup == VsMenus.guidStandardCommandSet97)
            {
                switch ((VsCommands)cmd)
                {
                    case VsCommands.Refresh:
                        handled = true;
                        return QueryStatusResult.ENABLED;

                    case VsCommands.ProjectProperties:
                        return base.QueryStatusCommandFromOleCommandTarget(cmdGroup, cmd, out handled);
                }
            }

            if (cmdGroup == VsMenus.guidStandardCommandSet2K)
            {
                switch ((VsCommands2K)cmd)
                {
                    case VsCommands2K.SLNREFRESH:
                    case (VsCommands2K)XVsConstants.CommandExploreFolderInWindows:
                        handled = true;
                        return QueryStatusResult.ENABLED | QueryStatusResult.SUPPORTED;

                }
            }

            return base.QueryStatusCommandFromOleCommandTarget(cmdGroup, cmd, out handled);
        }
        /// <summary>
        /// This is similar to QueryStatusOnNode method but it is internal so that others within the assembley can call
        /// it.
        /// </summary>
        /// <param name="guidCmdGroup">A unique identifier of the command group. The pguidCmdGroup parameter can be NULL to specify the standard group.</param>
        /// <param name="cmd">The command to query status for.</param>
        /// <param name="result">An out parameter specifying the QueryStatusResult of the command.</param>
        /// <returns>It returns true if succeeded, false otherwise.</returns>
        internal bool QueryStatusOnProjectNode(Guid guidCmdGroup, uint cmd, ref QueryStatusResult result)
        {
            if (guidCmdGroup == VsMenus.guidStandardCommandSet2K)
            {
                if ((VsCommands2K)cmd == VsCommands2K.SHOWALLFILES)
                {
                    result |= QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED;
                    if (this.showAllFilesEnabled)
                    {
                        result |= QueryStatusResult.LATCHED;
                    }

                    return true; // handled.
                }
            }

            return false; // not handled.
        }
        /// <summary>
        /// Gets the current project configuration.
        /// </summary>
        public XProjectConfig CurrentConfig
        {
            get
            {
                EnvDTE.Project automationObject = this.GetAutomationObject() as EnvDTE.Project;
                var name = new ConfigCanonicalName(Utilities.GetActiveConfigurationName(automationObject), Utilities.GetActivePlatformName(automationObject));
                return new XProjectConfig(this, name);
            }
        }

        /// <summary>
        /// Handles command execution.
        /// </summary>
        /// <param name="cmdGroup">Unique identifier of the command group</param>
        /// <param name="cmd">The command to be executed.</param>
        /// <param name="cmdexecopt">Values describe how the object should execute the command.</param>
        /// <param name="pvaIn">Pointer to a VARIANTARG structure containing input arguments. Can be NULL</param>
        /// <param name="pvaOut">VARIANTARG structure to receive command output. Can be NULL.</param>
        /// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
        [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "2#", Justification = "Suppressing to avoid conflict with style cop.")]
        [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#", Justification = "We cannot comply with FxCop and StyleCop simultaneously.")]
        protected override int ExecCommandOnNode(Guid cmdGroup, uint cmd, uint cmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            if (cmdGroup == VsMenus.guidStandardCommandSet2K)
            {
                switch ((VsCommands2K)cmd)
                {
                    case (VsCommands2K)XVsConstants.CommandExploreFolderInWindows:
                        XHelperMethods.ExploreFolderInWindows(this.ProjectFolder);
                        return VSConstants.S_OK;

                    case VsCommands2K.SLNREFRESH:
                        XHelperMethods.RefreshProject(this);
                        return VSConstants.S_OK;
                }
            }

            if (cmdGroup == VsMenus.guidStandardCommandSet97)
            {
                switch ((VsCommands)cmd)
                {
                    case VsCommands.Refresh:
                        XHelperMethods.RefreshProject(this);
                        return VSConstants.S_OK;
                    case VsCommands.F1Help:
                        // Prevent VS from showing keyword help
                        return VSConstants.S_OK;
                }
            }

            return base.ExecCommandOnNode(cmdGroup, cmd, cmdexecopt, pvaIn, pvaOut);
        }
        #endregion
        #region UserFile Handling
        private MSBuild.Project userBuildProject;
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
        //public override int IsDirty(out int isDirty)
        //{
        //    int res = base.IsDirty(out isDirty);
        //    if (isDirty == 0 && this.UserBuildProject != null)
        //    {
        //        isDirty = this.UserBuildProject.IsDirty ? 1 : 0;
        //    }
        //    return res;
        //}

        /// <summary>
        /// Creates an MSBuild project to be associated with the .user location specific build file.
        /// </summary>
        public void CreateUserBuildProject()
        {
            if (File.Exists(this.UserFileName))
            {
                // Create the project from an XmlReader so that this file is
                // not checked for being dirty when closing the project.
                // If loaded directly from the file, Visual Studio will display
                // a save changes dialog if any changes are made to the user
                // project since it will have been added to the global project
                // collection. Loading from an XmlReader will prevent the
                // project from being added to the global project collection
                // and thus prevent the save changes dialog on close.
                System.Xml.XmlReader xmlReader = System.Xml.XmlReader.Create(this.UserFileName);
                this.userBuildProject = new MSBuild.Project(xmlReader);
            }
            else
            {
                this.userBuildProject = new MSBuild.Project();
            }
        }
        /// <summary>
        /// Sets the configuration for the .user build file
        /// </summary>
        /// <param name="configCanonicalName">Configuration</param>
        //protected internal override void SetConfiguration(ConfigCanonicalName configCanonicalName)
        //{
        //    base.SetConfiguration(configCanonicalName);
        //}
        /// <summary>
        /// Closes the project node.
        /// </summary>
        /// <returns>A success or failure value.</returns>
        public override int Close()
        {
            int result = base.Close();

            if (this.UserBuildProject != null && this.UserBuildProject.IsDirty)
            {
                this.UserBuildProject.Save(this.UserFileName);
            }

            return result;
        }
        /// <summary>
        /// Called to save the project file
        /// </summary>
        /// <param name="fileToBeSaved">Name for the project file.</param>
        /// <param name="remember">Persist the dirty state after the save.</param>
        /// <param name="formatIndex">Format index</param>
        /// <returns>Native HRESULT</returns>
        [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#", Justification = "We cannot comply with FxCop and StyleCop simultaneously.")]
        [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "1#", Justification = "We cannot comply with FxCop and StyleCop simultaneously.")]
        [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "2#", Justification = "We cannot comply with FxCop and StyleCop simultaneously.")]
        public override int Save(string fileToBeSaved, int remember, uint formatIndex)
        {
            int result = base.Save(fileToBeSaved, remember, formatIndex);
            if (NativeMethods.S_OK == result && this.userBuildProject != null)
            {
                this.userBuildProject.Save(this.UserFileName);
            }

            return result;
        }

        /// <summary>
        /// Renames the project file
        /// </summary>
        /// <param name="newFile">The new name for the project file.</param>
        protected override void RenameProjectFile(string newFile)
        {
            string oldUserFileName = this.UserFileName;
            base.RenameProjectFile(newFile);

            if (this.UserBuildProject != null)
            {
                File.Move(oldUserFileName, this.UserFileName);
            }
        }
        #endregion

        #region support Methods

        /// <summary>
        /// Factory method for configuration provider
        /// </summary>
        /// <returns>Configuration provider created</returns>
        protected override ConfigProvider CreateConfigProvider()
        {
            return new XConfigProvider(this);
        }

        /// <summary>
        /// Creates and returns the ProjectElement for a file item.
        /// </summary>
        /// <param name="file">Path of the file.</param>
        /// <returns>ProjectElement for the file item.</returns>
        internal ProjectElement CreateMsBuildFileProjectElement(string file)
        {
            return this.AddFileToMsBuild(file);
        }

        /// <summary>
        /// Creates and returns the ProjectElement for a folder item.
        /// </summary>
        /// <param name="folder">Path of the folder.</param>
        /// <returns>ProjectElement for the folder item.</returns>
        internal ProjectElement CreateMsBuildFolderProjectElement(string folder)
        {
            return this.AddFolderToMsBuild(folder);
        }

        /// <summary>
        /// Executes an MSBuild target.
        /// </summary>
        /// <param name="target">Name of the MSBuild target to execute.</param>
        /// <returns>Result from executing the target (success/failure).</returns>
        protected override Microsoft.VisualStudio.Project.BuildResult InvokeMsBuild(string target)
        {
            XBuildMacroCollection.DefineSolutionProperties(this);
            XBuildMacroCollection.DefineProjectReferenceConfigurations(this);
            return base.InvokeMsBuild(target);
        }

        #endregion

        #region Project Properties
        /// <summary>
        /// Sets the value of an MSBuild project property.
        /// </summary>
        /// <param name="propertyName">The name of the property to change.</param>
        /// <param name="propertyValue">The value to assign the property.</param>
        public override void SetProjectProperty(string propertyName, string propertyValue)
        {
            this.SetProjectProperty(propertyName, propertyValue, null);
        }

        /// <summary>
        /// Sets the value of an MSBuild project property.
        /// </summary>
        /// <param name="propertyName">The name of the property to change.</param>
        /// <param name="propertyValue">The value to assign the property.</param>
        /// <param name="condition">The condition to use on the property. Corresponds to the Condition attribute of the Property element.</param>
        public void SetProjectProperty(string propertyName, string propertyValue, string condition)
        {
            XHelperMethods.VerifyStringArgument(propertyName, "propertyName");

            if (propertyValue == null)
            {
                propertyValue = String.Empty;
            }

            // see if the value is the same as what's already in the project so we
            // know whether to actually mark the project file dirty or not
            string oldValue = this.GetProjectProperty(propertyName, true);

            if (!String.Equals(oldValue, propertyValue, StringComparison.Ordinal))
            {
                // check out the project file
                if (this.ProjectMgr != null && !this.ProjectMgr.QueryEditProjectFile(false))
                {
                    throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED);
                }

                // Use condition!
                this.BuildProject.SetProperty(propertyName, propertyValue); //, condition);

                // refresh the cached values
                this.SetCurrentConfiguration();
                this.SetProjectFileDirty(true);
            }
            this.RaiseProjectPropertyChanged(propertyName, oldValue, propertyValue);
        }

        /// <summary>
        /// Converts the path to relative (if it is absolute) to the project folder.
        /// </summary>
        /// <param name="path">Path to be made relative.</param>
        /// <returns>Path relative to the project folder.</returns>
        public virtual string GetRelativePath(string path)
        {
            return XHelperMethods.GetRelativePath(this.ProjectFolder, path);
        }

        #endregion

    }
}
