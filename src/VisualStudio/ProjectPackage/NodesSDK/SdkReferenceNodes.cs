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
    }

    internal class XSharpSDKRefProps
    {
        internal string Guid;
        internal string Name;
        internal string ReferenceSpecification;
    }
    class XSharpSDKProjectReferenceNode : XSharpProjectReferenceNode
    {

        public XSharpSDKProjectReferenceNode(ProjectNode root, string referencedProjectName, string projectPath, string projectReference)
            : base(root, referencedProjectName,projectPath,projectReference)
        {
            _props = new XSharpSDKRefProps();
            _props.ReferenceSpecification = projectReference;
            _props.Guid = projectReference;
            if (_props.Guid.Contains("|"))
                _props.Guid = _props.Guid.Substring(0, _props.Guid.IndexOf('|'));
            _props.Name = referencedProjectName;
        }
        public XSharpSDKProjectReferenceNode(ProjectNode root, ProjectElement element)
         : base(root, element)
        {
            _props = new XSharpSDKRefProps();
            SaveProperties();
        }

        internal void SaveProperties()
        {
            if (this.ItemNode != null && this.ItemNode.Item != null)
            {
                _props.Guid = this.ItemNode.Item.GetMetadataValue(ProjectFileConstants.Project);
                _props.Name = this.ItemNode.Item.GetMetadataValue(ProjectFileConstants.Name);
            }
        }

        internal void SetProperties(XSharpSDKRefProps props)
        {
            if (this.ItemNode != null && this.ItemNode.Item != null && _props != null)
            {
                this.ItemNode.Item.SetMetadataValue(ProjectFileConstants.Project, props.Guid);
                this.ItemNode.Item.SetMetadataValue(ProjectFileConstants.Name, props.Name);
            }
        }

        internal void RestoreProperties()
        {
            SetProperties(_props);
        }
        internal void RemoveProperties()
        {
            if (this.ItemNode != null && this.ItemNode.Item != null)
            {
                this.ItemNode.Item.RemoveMetadata(ProjectFileConstants.Project);
                this.ItemNode.Item.RemoveMetadata(ProjectFileConstants.Name);
                this.ItemNode.Item.RemoveMetadata(ProjectFileConstants.Private);
            }
        }
        private string _projectName = null;

        private XSharpSDKRefProps _props;

        internal XSharpSDKRefProps ReferenceProperties => _props;
        protected override string ReferencedProjectName
        {
            get
            {
                if (!string.IsNullOrEmpty(_projectName))
                    return _projectName;
                if (this.ItemNode != null && this.ItemNode.Item != null)
                {
                    _projectName = this.ItemNode.Item.EvaluatedInclude;
                    _projectName = System.IO.Path.GetFileNameWithoutExtension(_projectName);
                }
                else
                    _projectName = System.IO.Path.GetFileNameWithoutExtension(this.Url);
                return _projectName;
            }

            set
            {
                base.ReferencedProjectName = value;
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
            }
            finally
            {
                sdkProject.SuspendBuild = old;
            }
        }
        string[] elements = new string[]
        {
            ProjectFileConstants.Private,
            ProjectFileConstants.Name
        };

        void CleanMetadata()
        {
            foreach (var element in elements)
            {
                if (this.ItemNode != null &&
                    this.ItemNode.Item != null &&
                    this.ItemNode.Item.HasMetadata(element))
                {
                    this.ItemNode.Item.RemoveMetadata(element);
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
