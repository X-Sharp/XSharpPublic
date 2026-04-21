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
            var sdkProject = this.ProjectMgr as XSharpSdkProjectNode;
            var old = sdkProject.SuspendBuild;
            sdkProject.SuspendBuild = true;
            try
            {
                base.BindReferenceData();
            }
            finally
            {
                sdkProject.SuspendBuild = old;
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
            CleanMetadata();
        }
        private string _projectName = null;


        protected override string ReferencedProjectName
        {
            get
            {
                if (!string.IsNullOrEmpty(_projectName))
                    return _projectName;
                _projectName = this.ItemNode.Item.EvaluatedInclude;
                _projectName = System.IO.Path.GetFileNameWithoutExtension(_projectName);
                return _projectName;
            }

            set
            {
                base.ReferencedProjectName = value;
                CleanMetadata();
            }
        }

        protected override void BindReferenceData()
        {
            var sdkProject = this.ProjectMgr as XSharpSdkProjectNode;
            var old = sdkProject.SuspendBuild;
            sdkProject.SuspendBuild = true;
            try
            {
                base.BindReferenceData();
                this.CleanMetadata();
            }
            finally
            {
                sdkProject.SuspendBuild = old;
            }
        }
        string[] elements = new string[]
        {
            ProjectFileConstants.Project,
            ProjectFileConstants.Private,
            ProjectFileConstants.Name
        };

        void CleanMetadata()
        {
            foreach (var element in elements)
            {
                if (this.ItemNode.Item.HasMetadata(element))
                    this.ItemNode.Item.RemoveMetadata(element);

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
            var sdkProject = this.ProjectMgr as XSharpSdkProjectNode;
            var old = sdkProject.SuspendBuild;
            sdkProject.SuspendBuild = true;
            try
            {
                base.BindReferenceData();
            }
            finally
            {
                sdkProject.SuspendBuild = old;
            }
        }
    }

}
