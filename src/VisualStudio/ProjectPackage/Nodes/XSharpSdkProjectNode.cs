using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;

using System;
using System.Collections.Generic;
using System.Linq;

using XSharpModel;

namespace XSharp.Project
{
    internal class XSharpSdkProjectNode : XSharpProjectNode
    {

        private void XSharpSdkProjectNode_OnProjectPropertyChanged(object sender, ProjectPropertyChangedArgs e)
        {
            if (string.Compare(e.PropertyName, XSharpProjectFileConstants.TargetFramework, true) == 0)
            {
                ;
            }
            if (string.Compare(e.PropertyName, XSharpProjectFileConstants.TargetFrameworks, true) == 0)
            {
                ;
            }
            if (string.Compare(e.PropertyName, XSharpProjectFileConstants.XTargetFrameworks, true) == 0)
            {
                ;
            }
        }

        public XSharpSdkProjectNode(XSharpProjectPackage package) : base(package)
        {
            _targetFrameworks = new List<string>();
            OnProjectPropertyChanged += XSharpSdkProjectNode_OnProjectPropertyChanged;

        }
        internal bool IsMultiTargeting
        {
            get
            {
                return _targetFrameworks.Count > 1;
            }
        }
        internal string CheckFrameworks()
        {
            var frameworks = this.GetProjectProperty(XSharpProjectFileConstants.TargetFrameworks, false);
            if (string.IsNullOrEmpty(frameworks))
            {
                frameworks = this.GetProjectProperty(XSharpProjectFileConstants.XTargetFrameworks, false);
            }
            var framework = this.GetProjectProperty(XSharpProjectFileConstants.TargetFramework, false);
            if (!string.IsNullOrEmpty(frameworks))
            {
                var splits = frameworks.Split(new char[] { ';' }, StringSplitOptions.RemoveEmptyEntries);
                foreach (var item in splits)
                {
                    _targetFrameworks.Add(item);
                }
            }
            else
            {
                _targetFrameworks.Add(framework);
            }
            return frameworks;
        }

        public override void SetBuildProject(Microsoft.Build.Evaluation.Project newBuildProject)
        {
            base.SetBuildProject(newBuildProject);
            if (this.ProjectIDGuid == Guid.Empty)
                this.ProjectIDGuid = Guid.NewGuid();
            if (newBuildProject == null)
            {
                _targetFrameworks.Clear();
                return;
            }
            if (_targetFrameworks.Count == 0)
            {
                var frameworks = CheckFrameworks();
                if (frameworks != null)
                    SetSingleTargetFramework();
            }
        }

        private void RestoreOriginalTargetFrameworks()
        {
            var frameworks = this.GetProjectProperty(XSharpProjectFileConstants.XTargetFrameworks, false);
            if (frameworks != null)
            {
                SetProjectProperty(XSharpProjectFileConstants.TargetFrameworks, frameworks);
                RemoveProjectProperty(XSharpProjectFileConstants.TargetFramework);
                RemoveProjectProperty(XSharpProjectFileConstants.XTargetFrameworks);
                this.BuildProject.Save();
            }
        }
        private void SetSingleTargetFramework()
        {
            var frameworks = CheckFrameworks();
            RemoveProjectProperty(XSharpProjectFileConstants.TargetFrameworks);
            if (_targetFrameworks.Count > 1)
            {
                SetProjectProperty(XSharpProjectFileConstants.XTargetFrameworks, frameworks);
                SetProjectProperty(XSharpProjectFileConstants.TargetFramework, _targetFrameworks[0]);
            }
            else if (_targetFrameworks.Count == 1)
            {
                RemoveProjectProperty(XSharpProjectFileConstants.TargetFrameworks);
                SetProjectProperty(XSharpProjectFileConstants.TargetFramework, _targetFrameworks[0]);
            }
            this.BuildProject.Save();
        }

        protected override int UnloadProject()
        {
            if (IsMultiTargeting)
            {
                RestoreOriginalTargetFrameworks();
                this.BuildProject.Save();
            }
            var result = base.UnloadProject();
            return result;

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
        // Do not store FolderNodes in SDK projects
        public override int Save(string fileToBeSaved, int remember, uint formatIndex)
        {
            var folderNodes = new List<FolderNode>();
            this.FindNodesOfType(folderNodes);
            foreach (var node in folderNodes)
            {
                if (!(node is XSharpFrameworkReferenceNode) && node.ItemNode != null
                    && node.ItemNode.Item != null)
                    this.BuildProject.RemoveItem(node.ItemNode.Item);
            }
            this.RemoveProjectProperty("ProjectGuid");
            return base.Save(fileToBeSaved, remember, formatIndex);
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

            frameworkNode = new XSharpFrameworkReferenceNode((XSharpProjectNode)this.ProjectMgr, items.First().EvaluatedInclude);
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
            var nodes = new List<XSharpDependencyNode>();
            this.FindNodesOfType(nodes);
            foreach (var child in nodes)
            {
                this.RemoveChild(child);
                child.Dispose();
            }
        }
    }
}
