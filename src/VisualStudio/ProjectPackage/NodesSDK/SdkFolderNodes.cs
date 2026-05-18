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
    }
    class XSharpSdkProjectsNode : XSharpSdkFolderNode
    {
        public XSharpSdkProjectsNode(XSharpProjectNode root) :
            base(root, "Projects")
        {
        }
        public override int SortPriority => DefaultSortOrderNode.ProjectsNode;
    }
    class XSharpSdkAssembliesNode : XSharpSdkFolderNode
    {
        public XSharpSdkAssembliesNode(XSharpProjectNode root) :
            base(root, "Assemblies")
        {
        }
        public override int SortPriority => DefaultSortOrderNode.AssembliesNode;
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
