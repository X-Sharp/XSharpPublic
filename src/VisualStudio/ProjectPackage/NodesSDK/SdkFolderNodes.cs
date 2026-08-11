extern alias codeanalysis;

#if DEV17
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Project;

using VsCommands = Microsoft.VisualStudio.VSConstants.VSStd97CmdID;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;

using System;
using System.Diagnostics;

using EnvDTE;

namespace XSharp.Project
{
    class XSharpSdkFolderNode : XSharpFolderNode
    {
        public XSharpSdkFolderNode(XSharpProjectNode root, string folderName) :
            base(root, folderName, null, true)
        {

        }
        public override int MenuCommandId
        {
            // Make sure we do not shop copy/cut/delete etc. commands on this node
            get { return VsMenus.IDM_VS_CTXT_NOCOMMANDS; }
        }

        public override void Remove(bool removeFromStorage)
        {
            return;
        }
        protected override ImageMoniker GetIconMoniker(bool open) => KnownMonikers.Reference;
        protected override bool SupportsIconMonikers => true;
        protected override int SetEditLabel(string label, string relativePath)
        {
            return VSConstants.S_FALSE;
        }

        /// <summary>
        /// Handle the guidVSStd16 "Add ... Reference..." commands for the nodes inside the
        /// "Dependencies" tree that expose one of the reference group context menus.
        /// </summary>
        /// <remarks>
        /// FolderNode.QueryStatusOnNode returns OLECMDERR_E_UNKNOWNGROUP for every command group
        /// other than the 97 and 2K sets without calling its base, so this has to happen before
        /// base is called. Which of the commands actually shows up is decided by the menu that
        /// <see cref="MenuCommandId"/> returns.
        /// </remarks>
        protected override int QueryStatusOnNode(Guid cmdGroup, uint cmd, IntPtr pCmdText, ref QueryStatusResult result)
        {
            if (cmdGroup == XSharpSdkProjectNode.VsStd16 && this.ProjectMgr is XSharpSdkProjectNode sdk)
            {
                var hr = sdk.QueryStatusReferenceCommand(cmd, ref result);
                if (hr == VSConstants.S_OK)
                    return hr;
            }
            return base.QueryStatusOnNode(cmdGroup, cmd, pCmdText, ref result);
        }

        protected override int ExecCommandOnNode(Guid cmdGroup, uint cmd, uint cmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            if (cmdGroup == XSharpSdkProjectNode.VsStd16 && this.ProjectMgr is XSharpSdkProjectNode sdk)
            {
                var hr = sdk.ExecReferenceCommand(cmd);
                if (hr != VSConstants.E_NOTIMPL)
                    return hr;
            }
            return base.ExecCommandOnNode(cmdGroup, cmd, cmdexecopt, pvaIn, pvaOut);
        }
    }
    class XSharpSdkProjectsNode : XSharpSdkFolderNode
    {
        public XSharpSdkProjectsNode(XSharpProjectNode root) :
            base(root, "Projects")
        {
        }
        public override int SortPriority => DefaultSortOrderNode.ProjectsNode;

        // The "Projects" group menu of the dependencies tree. Carries "Add Project Reference...".
        public override int MenuCommandId => VsMenus.IDM_VS_CTXT_PROJECTREFERENCE_GROUP;
    }
    class XSharpSdkAssembliesNode : XSharpSdkFolderNode
    {
        public XSharpSdkAssembliesNode(XSharpProjectNode root) :
            base(root, "Assemblies")
        {
        }
        public override int SortPriority => DefaultSortOrderNode.AssembliesNode;

        // The "Assemblies" group menu of the dependencies tree. Carries "Add Assembly Reference...".
        public override int MenuCommandId => VsMenus.IDM_VS_CTXT_REFERENCE_GROUP;
    }
    [DebuggerDisplay("Frameworks {Parent?.Caption,nq}")]
    class XSharpSdkFrameworksNode : XSharpSdkFolderNode
    {
        public XSharpSdkFrameworksNode(XSharpProjectNode root) :
            base(root, "Frameworks")
        {
        }
        public override int SortPriority => DefaultSortOrderNode.TargetFrameworksNode;
    }
    [DebuggerDisplay("{Caption,nq}")]
    class XSharpTargetFrameworkReferenceNode : XSharpSdkFolderNode
    {
        public XSharpTargetFrameworkReferenceNode(XSharpProjectNode root, string frameworkName) :
            base(root, frameworkName)
        {
        }
        protected override ImageMoniker GetIconMoniker(bool open) => KnownMonikers.DotNETFrameworkDependency;

    }
}
#endif
