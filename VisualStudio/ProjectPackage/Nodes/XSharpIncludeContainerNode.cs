using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System;
using System.Runtime.InteropServices;
using VsCommands = Microsoft.VisualStudio.VSConstants.VSStd97CmdID;
using VsMenus = Microsoft.VisualStudio.Project.VsMenus;

namespace XSharp.Project
{
    /// <summary>
    /// IncludeFile container node
    /// </summary>
    public class XSharpIncludeContainerNode : HierarchyNode
    {
        XSharpProjectNode _project;
        public XSharpIncludeContainerNode(XSharpProjectNode project) : base(project)
        {
            _project = project;
            this.IsExpanded = false;
        }
        protected override bool SupportsIconMonikers => true;
        protected override ImageMoniker GetIconMoniker(bool open)
        {
            if (open)
                return KnownMonikers.LinkedFolderOpened;
            else
                return KnownMonikers.LinkedFolderClosed;
        }

        public override string Url => string.Empty;

        public override string Caption => "Include Files";

        public override Guid ItemTypeGuid => VSConstants.GUID_ItemType_VirtualFolder;

        public override int SortPriority => DefaultSortOrderNode.ProjectIncludeFiles;

        protected override NodeProperties CreatePropertiesObject()
        {
            return new XSharpVirtualFolderNodeProperties(this);

        }

    }

    /// <summary>
    /// IncludeFile container node
    /// </summary>
    public class XSharpIncludeFileNode : HierarchyNode
    {
        string _fileName;
        string _caption;
        public XSharpIncludeFileNode(XSharpProjectNode project, string fileName) : base(project)
        {
            _fileName = fileName;
            _caption = System.IO.Path.GetFileName(_fileName);
        }
        protected override void DeleteFromStorage(string path)
        {
            return;
        }

        public override string Url => _fileName;
        public override string Caption => _caption;

        public override Guid ItemTypeGuid => VSConstants.GUID_ItemType_PhysicalFile;

        protected override int SetEditLabel(string label, string relativePath)
        {
            return VSConstants.S_FALSE;
        }

        protected override bool SupportsIconMonikers => true;
        protected override ImageMoniker GetIconMoniker(bool open)
        {
            return KnownMonikers.SymlinkFile;
        }

        protected override NodeProperties CreatePropertiesObject()
        {
            var xprops = new IncludeFileNodeProperties(this);
            return xprops;
        }
        protected override void DoDefaultAction()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var view = await VS.Documents.OpenAsync(this.Url);
            });
        }
        public override int MenuCommandId
        {
            get { return VsMenus.IDM_VS_CTXT_NOCOMMANDS; }
        }


    }
    [CLSCompliant(false), ComVisible(true)]
    public class IncludeFileNodeProperties : NodeProperties
    {
        #region properties

        [SRCategory(SR.Misc)]
        [LocDisplayName(SR.FileName)]
        [SRDescription(SR.FileNameDescription)]
        public string FileName
        {
            get
            {
                return this.Node.Caption;
            }
        }

        [SRCategory(SR.Misc)]
        [LocDisplayName(SR.FullPath)]
        [SRDescription(SR.FullPathDescription)]
        public string FullPath
        {
            get
            {
                return this.Node.Url;
            }
        }
        #endregion

        #region ctors
        public IncludeFileNodeProperties(HierarchyNode node)
            : base(node)
        {
        }
        #endregion

    }



}
