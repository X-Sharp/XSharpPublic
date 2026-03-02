#if DEV17
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Project;

using System.Diagnostics;

namespace XSharp.Project
{
    class XSharpSdkFolderNode : XSharpFolderNode
    {
        public XSharpSdkFolderNode(XSharpProjectNode root, string folderName) :
            base(root, folderName, null, true)
        {

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
