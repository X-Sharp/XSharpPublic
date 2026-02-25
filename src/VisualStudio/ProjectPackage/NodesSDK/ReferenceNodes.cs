using Microsoft.VisualStudio.Project;
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
            sdkproject.SuspendBuild = true;
            base.BindReferenceData();
            sdkproject.SuspendBuild = false;
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
        }
        protected override void BindReferenceData()
        {
            var sdkproject = this.ProjectMgr as XSharpSdkProjectNode;
            sdkproject.SuspendBuild = true;
            base.BindReferenceData();
            sdkproject.SuspendBuild = false;
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
            sdkproject.SuspendBuild = true;
            base.BindReferenceData();
            sdkproject.SuspendBuild = false;
        }
    }

}
