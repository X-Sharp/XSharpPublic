using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;


using MSBuild = Microsoft.Build.Evaluation;

namespace XSharp.Project
{
    internal class XSharpSdkProjectNode : XSharpProjectNode
    {

        class VirtualBuildProject : MSBuild.Project
        {
            public VirtualBuildProject(string fileName) : base(fileName)
            {
                ;
            }
        }

        [DebuggerDisplay("{Name,nq} - {TargetFramework,nq}")]
        internal class SdkSubProjectInfo
        {
            public string TargetFramework { get; set; }
            public string Name => ParentProject.Caption;
            public XSharpSdkProjectNode ParentProject { get; set; } = null;
            //public ProjectInstance ProjectInstance { get; set; } = null;
            //public XProject ProjectModel { get; set; } = null;
            public SdkSubProjectInfo(string targetFramework, XSharpSdkProjectNode parentProject)
            {
                TargetFramework = targetFramework;
                ParentProject = parentProject;
                var fileName = parentProject.BuildProject.FullPath;
                //var projectModel = XSharpModel.XSolution.FindProject(fileName, targetFramework);
                //if (projectModel == null)
                //{
                //    projectModel = new XProject(parentProject,targetFramework, fileName);
                //    ProjectModel = projectModel;
                //    projectModel.FileWalkComplete += parentProject.OnFileWalkComplete;
                //    ProjectModel.ProjectWalkComplete += parentProject.OnProjectWalkComplete;

                //}
            }

        }

        private List<SdkSubProjectInfo> _subProjects = new List<SdkSubProjectInfo>();
        internal List<SdkSubProjectInfo> SubProjects => _subProjects;
        internal SdkSubProjectInfo ActiveSubProject { get; set; } = null;

        private MSBuild.Project MainProject { get; set; } = null;
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

        public override string Caption
        {
            get
            {
                var caption = base.Caption;
                if (_targetFrameworks.Count > 1)
                {
                    caption = $"{caption} ({ActiveSubProject.TargetFramework})";
                }
                return caption;
            }
        }

        public string BaseName  => base.Caption;

        internal bool SelectSubProject(SdkSubProjectInfo info)
        {
            if (info != null)
            {
                ThreadHelper.JoinableTaskFactory.Run(async delegate
                {

                    ActiveSubProject = info;
                    SetProjectProperty(XSharpProjectFileConstants.TargetFramework, info.TargetFramework);
                    SetProjectProperty(XSharpProjectFileConstants.ActiveTargetFramework, info.TargetFramework);
                    var fileName = this.BuildProject.FullPath;
                    var projects = await VS.Solutions.GetAllProjectsAsync();
                    var prj = projects.FirstOrDefault(p => string.Compare(p.FullPath, fileName, true) == 0);
                    if (prj == null)
                        return false;
                    await prj.LoadAsync();
                    this.OnPropertyChanged(this, (int)__VSHPROPID.VSHPROPID_Caption, 0);
                    return true;
                });
            }
            return false;
        }

        public XSharpSdkProjectNode(XSharpProjectPackage package) : base(package)
        {
            _targetFrameworks = new List<string>();
            OnProjectPropertyChanged += XSharpSdkProjectNode_OnProjectPropertyChanged;

        }
        internal string CheckFrameworks()
        {
            MainProject = this.BuildProject;
            string framework = null;
            string frameworks = null;
            bool single = false;
            this.CreateUserBuildProject();
            _targetFrameworks.Clear();
            // First check for single TargetFramework
            framework = this.GetProjectProperty(XSharpProjectFileConstants.TargetFramework, false);
            if (string.IsNullOrEmpty(framework))
            {
                framework = this.GetProjectProperty(XSharpProjectFileConstants.ActiveTargetFramework);
            }
            single = !string.IsNullOrEmpty(framework);
            frameworks = this.GetProjectProperty(XSharpProjectFileConstants.TargetFrameworks, false);
            if (string.IsNullOrEmpty(frameworks))
            {
                frameworks = this.GetProjectProperty(XSharpProjectFileConstants.XTargetFrameworks, false);
            }
            // When we found a single framework, then that will become the active framework


            if (string.IsNullOrEmpty(frameworks))
            {
                single = true;
            }
            if (single)
            {
                if (string.IsNullOrEmpty(framework))
                {
                    framework = "net48";
                }
                _targetFrameworks.Add(framework);
                var subProject = new SdkSubProjectInfo(framework, this);
                _subProjects.Add(subProject);
                ActiveSubProject = subProject;
            }
            if (!string.IsNullOrEmpty(frameworks))
            {
                var splits = frameworks.Split(new char[] { ';' }, StringSplitOptions.RemoveEmptyEntries);
                _targetFrameworks.AddRange(splits);
                foreach (var fw in splits)
                {
                    if (string.Compare(fw, framework, true) == 0)
                    {
                        // this was already added
                        continue;
                    }
                    var subProject = new SdkSubProjectInfo(fw, this);
                    _subProjects.Add(subProject);
                }
                ActiveSubProject = _subProjects.First();
            }
            _frameworks = frameworks;
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
            SetProjectFileDirty(false);
        }

        private void SaveTargetFrameworks()
        {
            SetProjectProperty(XSharpProjectFileConstants.ActiveTargetFramework, ActiveSubProject.TargetFramework);
            if (TargetFrameworks.Count > 1)
            {
                // Store the active project in the XTargetFramework property
                RemoveProjectProperty(XSharpProjectFileConstants.TargetFramework);
                RemoveProjectProperty(XSharpProjectFileConstants.XTargetFrameworks);
                SetProjectProperty(XSharpProjectFileConstants.TargetFrameworks, _frameworks);
            }
            else
            {
                // Store the active project in the TargetFramework property
                SetProjectProperty(XSharpProjectFileConstants.TargetFramework, ActiveSubProject.TargetFramework);
                RemoveProjectProperty(XSharpProjectFileConstants.TargetFrameworks);
                RemoveProjectProperty(XSharpProjectFileConstants.XTargetFrameworks);
            }
        }
        private void SetSingleTargetFramework()
        {
            RemoveProjectProperty(XSharpProjectFileConstants.TargetFrameworks);
            SetProjectProperty(XSharpProjectFileConstants.XTargetFrameworks, _frameworks);
            SetProjectProperty(XSharpProjectFileConstants.TargetFramework, _targetFrameworks[0]);
        }

        List<string> sdkReferences = new List<string>();
        protected override List<string> RefreshReferences()
        {
            var refs = base.RefreshReferences();
            var sdkrefs = base._sdkReferences;
            if (IsNetCoreApp)
            {
                AddPendingReferences(sdkrefs);
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
        private string _frameworks = null;
        private List<string> _targetFrameworks = null;
        public List<string> TargetFrameworks => _targetFrameworks;
        public override bool IsSdkProject => true;

        private XSharpDependenciesContainerNode dependenciesNode;
        protected override ReferenceContainerNode CreateReferenceContainerNode()
        {
            dependenciesNode = new XSharpDependenciesContainerNode(this);
            return dependenciesNode;
        }

        public override int Close()
        {
            this.SaveTargetFrameworks();
            return base.Close();
        }

        // Do not store FolderNodes in SDK projects
        public override int Save(string fileToBeSaved, int remember, uint formatIndex)
        {
            var folderNodes = new List<FolderNode>();
            var dirty = this.IsProjectFileDirty;
            this.FindNodesOfType(folderNodes);
            foreach (var node in folderNodes)
            {
                if (!(node is XSharpFrameworkReferenceNode) && node.ItemNode != null
                    && node.ItemNode.Item != null)
                    this.BuildProject.RemoveItem(node.ItemNode.Item);
            }
            this.RemoveProjectProperty("ProjectGuid");
            this.SetProjectFileDirty(dirty);
            return base.Save(fileToBeSaved, remember, formatIndex);
        }

        private void AddPendingReferences(List<string> newReferences)
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    foreach (var frameworkNode in dependenciesNode.FrameworkNodes)
                    {
                        var nodes = new List<XSharpDependencyNode>();
                        var isExpanded = frameworkNode.IsExpanded;
                        frameworkNode.FindNodesOfType(nodes);

                        var toDelete = new List<XSharpDependencyNode>();
                        var toAdd = new List<string>();


                        foreach (var reference in sdkReferences)
                        {
                            if (!newReferences.Contains(reference, StringComparer.OrdinalIgnoreCase))
                            {
                                var oldnode = nodes.Find(n => n.Url.ToLower() == reference.ToLower());
                                toDelete.Add(oldnode);
                            }
                        }
                        // add dependencies from the TargetFramework
                        foreach (var reference in newReferences)
                        {
                            if (!sdkReferences.Contains(reference, StringComparer.OrdinalIgnoreCase))
                            {
                                toAdd.Add(reference);
                            }
                        }
                        // delete nodes that are no longer needed
                        foreach (var node in toDelete)
                        {
                            frameworkNode.RemoveChild(node);
                            node.Dispose();
                        }
                        // add new nodes
                        foreach (var item in toAdd)
                        {
                            var node = new XSharpDependencyNode(this, item);
                            frameworkNode.AddChild(node);
                        }
                        sdkReferences.Clear();
                        sdkReferences.AddRange(newReferences);
                        frameworkNode.IsExpanded = isExpanded;

                    }
                });

        }
    }
    public class XSharpFrameworkNode : XSharpDependencyNode
    {
        public XSharpFrameworkNode(XSharpProjectNode root, string filePath) :
            base(root, filePath)
        {

        }
        protected override ImageMoniker GetIconMoniker(bool open)
        {
            return KnownMonikers.Framework;
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
            return KnownMonikers.Dependancy;
        }
        override public Guid ItemTypeGuid
        {
            get
            {
                return VSConstants.GUID_ItemType_VirtualFolder;
            }
        }


    }
    class XSharpFrameworkReferenceNode : XSharpFolderNode
    {
        public XSharpFrameworkReferenceNode(XSharpProjectNode root) :
            base(root, "Frameworks", null, true)
        {
        }
        protected override ImageMoniker GetIconMoniker(bool open)
        {
            return KnownMonikers.Framework;
        }
        protected override bool SupportsIconMonikers => true;

        protected override int SetEditLabel(string label, string relativePath)
        {
            return VSConstants.S_FALSE;
        }
    }
    class XSharpTargetFrameworkReferenceNode : XSharpFolderNode
    {
        public XSharpTargetFrameworkReferenceNode(XSharpProjectNode root, string frameworkName) :
            base(root, frameworkName, null, true)
        {
        }
        protected override ImageMoniker GetIconMoniker(bool open)
        {
            return KnownMonikers.Framework;
        }
        protected override bool SupportsIconMonikers => true;

        protected override int SetEditLabel(string label, string relativePath)
        {
            return VSConstants.S_FALSE;
        }
    }
    class XSharpDependenciesContainerNode : XSharpReferenceContainerNode
    {
        internal List<XSharpFolderNode> FrameworkNodes => frameworkNodes;

        private List<XSharpFolderNode> frameworkNodes;
        public XSharpDependenciesContainerNode(XSharpProjectNode root) : base(root)
        {
            // Create the FrameworkReference node
            frameworkNodes = new List<XSharpFolderNode>();
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
            var project = this.ProjectMgr as XSharpSdkProjectNode;
            if (project.SubProjects.Count == 1)
            {
                var node = new XSharpFrameworkReferenceNode((XSharpProjectNode)this.ProjectMgr);
                frameworkNodes.Add(node);
                this.AddChild(node);
            }
            else
            {
                foreach (var subProject in project.SubProjects)
                {
                    var targetNode = new XSharpTargetFrameworkReferenceNode((XSharpProjectNode)this.ProjectMgr, subProject.TargetFramework);
                    this.AddChild(targetNode);
                    var node = new XSharpFrameworkReferenceNode((XSharpProjectNode)this.ProjectMgr);
                    frameworkNodes.Add(node);
                    targetNode.AddChild(node);
                }
            }
        }
        public override void AddChild(HierarchyNode node)
        {
            base.AddChild(node);
        }

        public override void RemoveChild(HierarchyNode node)
        {
             base.RemoveChild(node);
        }

        public void DeleteDependencies(string targetframework)
        {

        //    var nodes = new List<XSharpDependencyNode>();
        //    frameworkNode.FindNodesOfType(nodes);
        //    foreach (var child in nodes)
        //    {
        //        frameworkNode.RemoveChild(child);
        //        child.Dispose();
        //    }
        }
    }
}
