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
            return KnownMonikers.Reference;
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
    class XSharpSDKReferenceContainerNode : XSharpReferenceContainerNode
    {
        public XSharpSDKReferenceContainerNode(XSharpProjectNode root) : base(root)
        {
        }
        public override string Caption
        {
            get
            {
                return "Dependencies";
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
