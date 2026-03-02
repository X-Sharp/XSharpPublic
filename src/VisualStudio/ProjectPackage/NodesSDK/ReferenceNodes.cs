using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Project
{
    class XSharpSDKAssemblyReferenceNode : XSharpAssemblyReferenceNode
    {
        public XSharpSDKAssemblyReferenceNode(ProjectNode root, string assemblyPath)
           : base(root, assemblyPath)
        {

        }
        public XSharpSDKAssemblyReferenceNode(ProjectNode root, ProjectElement element)
         : base(root, element)
        {

        }
        protected override void BindReferenceData()
        {
            var sdkproject = this.ProjectMgr as XSharpSdkProjectNode;
            var old = sdkproject.SuspendBuild;
            sdkproject.SuspendBuild = true;
            base.BindReferenceData();
            sdkproject.SuspendBuild = old;
        }
    }

    class XSharpSDKProjectReferenceNode : XSharpProjectReferenceNode
    {
        public XSharpSDKProjectReferenceNode(ProjectNode root, string referencedProjectName, string projectPath, string projectReference)
            : base(root, referencedProjectName,projectPath,projectReference)
        {
        }
        public XSharpSDKProjectReferenceNode(ProjectNode root, ProjectElement element)
         : base(root, element)
        {
            ClearElement(element);
        }
        protected override void BindReferenceData()
        {
            var sdkproject = this.ProjectMgr as XSharpSdkProjectNode;
            var old = sdkproject.SuspendBuild;
            sdkproject.SuspendBuild = true;
            base.BindReferenceData();
            ClearElement(this.ItemNode);
            sdkproject.SuspendBuild = old;
        }
        void ClearElement(ProjectElement element)
        {
            if (element.Item != null)
            {
                // Check to see if we have the Guid and Name in the ProjectElement
                var guid = element.GetMetadata(ProjectFileConstants.Project);
                var name = element.GetMetadata(ProjectFileConstants.Name);
                var priv = element.GetMetadata(ProjectFileConstants.Private);
                var project = (XSharpProjectNode)this.ProjectMgr;
                if (!string.IsNullOrEmpty(guid + priv + name))
                {
                    element.Item.RemoveMetadata(ProjectFileConstants.Project);
                    element.Item.RemoveMetadata(ProjectFileConstants.Private);
                    element.Item.RemoveMetadata(ProjectFileConstants.Name);
                    project.SetProjectFileDirty(true);
                }
            }
        }
    }

    class XSharpSDKComReferenceNode : XSharpComReferenceNode
    {
        public XSharpSDKComReferenceNode(ProjectNode root, VSCOMPONENTSELECTORDATA selectorData, string wrapperTool)
           : base(root, selectorData, wrapperTool)
        {
        }
        public XSharpSDKComReferenceNode(ProjectNode root, ProjectElement element)
         : base(root, element)
        {
        }
        protected override void BindReferenceData()
        {
            var sdkproject = this.ProjectMgr as XSharpSdkProjectNode;
            var old = sdkproject.SuspendBuild;
            sdkproject.SuspendBuild = true;
            base.BindReferenceData();
            sdkproject.SuspendBuild = old;
        }
    }

}
