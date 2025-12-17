using Microsoft.Build.Evaluation;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Project
{
    internal class XSharpSdkProjectNode : XSharpProjectNode
    {
        public XSharpSdkProjectNode(XSharpProjectPackage package) : base(package)
        {

        }
        public override void SetBuildProject(Microsoft.Build.Evaluation.Project newBuildProject)
        {
            base.SetBuildProject(newBuildProject);
            if (this.ProjectIDGuid == Guid.Empty)
                this.ProjectIDGuid = Guid.NewGuid();
            _targetFrameworks = new List<string>();
            if (newBuildProject == null)
                return;

        }
        List<string> pendingReferences = new List<string>();
        protected override List<String> RefreshReferencesFromResponseFile()
        {
            var refs = base.RefreshReferencesFromResponseFile();
            if (IsNetCoreApp)
            {
                if (refs.Count != 0)
                {
                    pendingReferences = refs;
                }
                AddPendingReferences();
            }
            return refs;
        }

        internal bool IsNetCoreApp
        {
            get
            {
                var targetFramework = this.GetProjectProperty("TargetFrameworkIdentifier", false) ?? "";
                const string NetStandardPrefix = ".NetStandard";
                const string NetCorePrefix = ".NetCore";
                return targetFramework.StartsWith(NetCorePrefix, StringComparison.OrdinalIgnoreCase) ||
                       targetFramework.StartsWith(NetStandardPrefix, StringComparison.OrdinalIgnoreCase);
            }
        }
        private List<string> _targetFrameworks = null;
        public List<string> TargetFrameworks => _targetFrameworks;
        public override bool IsSdkProject => true;

        private XSharpSDKReferenceContainerNode dependenciesNode;
        protected override ReferenceContainerNode CreateReferenceContainerNode()
        {
            dependenciesNode = new XSharpSDKReferenceContainerNode(this);
            return dependenciesNode;
        }

        private void AddPendingReferences()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    dependenciesNode.DeleteDependencies();
                    // add dependencies from the TargetFramework
                    foreach (var item in pendingReferences)
                    {
                        var node = new XSharpDependencyNode(this, item);
                        dependenciesNode.AddChild(node);
                    }
                });
            pendingReferences.Clear();
        }
    }
    public class XSharpDependencyNode : HierarchyNode
    {
        string path;
        public XSharpDependencyNode(XSharpProjectNode root, string filePath) :
            base(root)
        {
            path = filePath;
        }
        public override string Caption
        {
            get
            {
                return System.IO.Path.GetFileNameWithoutExtension(path);
            }
        }
        override public string Url
        {
            get
            {
                return path;
            }
        }
        protected override bool SupportsIconMonikers => true;
        protected override ImageMoniker GetIconMoniker(bool open)
        {
            return KnownMonikers.ReferencePrivate;
        }
        override public Guid ItemTypeGuid
        {
            get
            {
                return VSConstants.GUID_ItemType_VirtualFolder;
            }
        }
        //override protected NodeProperties CreatePropertiesObject()
        //{
        //    return new XSharpDependencyNodeProperties(this);
        //}

    }
    //class XSharpDependencyNodeProperties : XSharpVirtualFolderNodeProperties
    //{
    //    public XSharpDependencyNodeProperties(XSharpDependencyNode node) : base(node)
    //    {
    //    }
    //    public string Path
    //    {
    //        get
    //        {
    //            return ((XSharpDependencyNode)this.Node).Url;
    //        }
    //    }
    //}

    class XSharpFrameworkReferenceNode : XSharpFolderNode
    {
        public XSharpFrameworkReferenceNode(XSharpProjectNode root, string frameworkName) :
            base(root, frameworkName, null, true)
        {
        }
        protected override ImageMoniker GetIconMoniker(bool open)
        {
            return KnownMonikers.Framework;
        }
        protected override bool SupportsIconMonikers => true;


    }
    class XSharpSDKReferenceContainerNode : XSharpReferenceContainerNode
    {
        private XSharpFolderNode frameworkNode = null;

        public XSharpSDKReferenceContainerNode(XSharpProjectNode root) : base(root)
        {
            CreateFrameworkReferenceNode();
        }
        public override string Caption
        {
            get
            {
                return "Dependencies";
            }
        }


        private void CreateFrameworkReferenceNode()
        {

            var items = this.ProjectMgr.BuildProject.GetItems("FrameworkReference");
            if (items.Count() == 0)
                return;

            frameworkNode = new XSharpFrameworkReferenceNode((XSharpProjectNode) this.ProjectMgr, items.First().EvaluatedInclude);
            base.AddChild(frameworkNode);

        }
        public override void AddChild(HierarchyNode node)
        {
            if (this.frameworkNode != null && node is XSharpDependencyNode)
            {
                this.frameworkNode.AddChild(node);
            }
            else
            {
                base.AddChild(node);
            }
        }

        public override void RemoveChild(HierarchyNode node)
        {
            if (this.frameworkNode != null && node is XSharpDependencyNode)
            {
                this.frameworkNode.RemoveChild(node);
            }
            else
            {
                base.RemoveChild(node);
            }

        }

        public void DeleteDependencies()
        {
            var nodes = new List< XSharpDependencyNode >();
            this.FindNodesOfType(nodes);
            foreach (var child in nodes)
            {
                this.RemoveChild(child);
                child.Dispose();
            }
        }
    }
}
