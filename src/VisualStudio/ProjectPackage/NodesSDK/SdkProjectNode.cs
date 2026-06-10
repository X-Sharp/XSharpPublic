extern alias codeanalysis;

#if DEV17
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

using VsCommands = Microsoft.VisualStudio.VSConstants.VSStd97CmdID;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;

using XSharpModel;

using MSBuild = Microsoft.Build.Evaluation;

using Microsoft.Build.Execution;

using System.Xml;
using System.IO;

using EnvDTE80;

using VSLangProj80;


namespace XSharp.Project
{
    internal class XSharpSdkProjectNode : XSharpProjectNode
    {
        internal bool SuspendBuild = false;
        [DebuggerDisplay("{Name,nq} - {TargetFramework,nq}")]
        internal class SdkSubProjectInfo
        {
            public string TargetFramework { get; set; }
            public string Name => ParentProject.Caption;
            public XSharpSdkProjectNode ParentProject { get; set; } = null;
            public XSharpTargetFrameworkReferenceNode TargetFrameworkReferenceNode { get; set; } = null;
            public XSharpSdkFrameworksNode FrameworksNode { get; set; } = null;
            //public ProjectInstance ProjectInstance { get; set; } = null;
            public XProject ProjectModel { get; set; } = null;
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

        protected override IVsReferenceManagerUser VsReferenceManager => new SdkProjectNodeReferenceManager(this);

        private List<SdkSubProjectInfo> _subProjects = new List<SdkSubProjectInfo>();
        internal List<SdkSubProjectInfo> SubProjects => _subProjects;
        internal SdkSubProjectInfo ActiveSubProject { get; set; } = null;

        private MSBuild.Project MainProject { get; set; } = null;
        private void XSharpSdkProjectNode_OnProjectPropertyChanged(object sender, ProjectPropertyChangedArgs e)
        {
            if (string.Compare(e.PropertyName, XSharpProjectFileConstants.TargetFramework, true) == 0)
            {
                CheckFrameworks();
            }
            if (string.Compare(e.PropertyName, XSharpProjectFileConstants.TargetFrameworks, true) == 0)
            {
                CheckFrameworks();
            }
            if (string.Compare(e.PropertyName, XSharpProjectFileConstants.XTargetFrameworks, true) == 0)
            {
                CheckFrameworks();
            }
            if (string.Compare(e.PropertyName, XSharpProjectFileConstants.ActiveTargetFramework, true) == 0)
            {
                CheckFrameworks();
            }
        }

        private OAXSharpSdkProject _automationObject;
        public override object GetAutomationObject()
        {
            if (_automationObject == null)
                _automationObject = new OAXSharpSdkProject(this);
            return _automationObject;
        }


        public override void OnItemAdded(HierarchyNode parent, HierarchyNode child)
        {
            base.OnItemAdded(parent, child);
            if (child is XSharpFileNode xfile)
            {
                // check for duplicate file items
                var item = xfile.ItemNode?.Item;
                if (item != null && !item.IsImported)
                {
                    this.BuildProject.ReevaluateIfNecessary();
                    var items = this.BuildProject.Items.Where(i => i.EvaluatedInclude == item.EvaluatedInclude && i.ItemType == item.ItemType && i.IsImported);
                    if (items.Count() > 0)
                    {
                        // we have a duplicate, remove the item that is not deleted from the project file
                        this.BuildProject.RemoveItem(item);
                    }
                }
            }
        }
        public override void OnItemDeleted()
        {
            base.OnItemDeleted();
            this.BuildProject.ReevaluateIfNecessary();
        }

        protected override void ProcessReferences()
        {
            base.ProcessReferences();
            RefreshReferences();
        }

        public string BaseName => base.Caption;

        internal bool SelectSubProject(SdkSubProjectInfo info)
        {
            if (info != null)
            {

                ActiveSubProject = info;
                SetProjectProperty(XSharpProjectFileConstants.TargetFramework, info.TargetFramework);
                SetProjectProperty(XSharpProjectFileConstants.ActiveTargetFramework, info.TargetFramework);

                //this.DoReload(false);
                this.OnPropertyChanged(this, (int)__VSHPROPID6.VSHPROPID_Subcaption, 0);
                VS.Commands.ExecuteAsync("Project.SetAsStartupProject").FireAndForget();
                return true;

            }
            return false;
        }


        public override object GetProperty(int propId)
        {

            switch ((__VSHPROPID6)propId)
            {
                case __VSHPROPID6.VSHPROPID_Subcaption:
                    if (this.SubProjects.Count > 1 && this.ActiveSubProject != null)
                    {
                        return this.ActiveSubProject.TargetFramework;
                    }
                    break;
            }
            return base.GetProperty(propId);
        }


        public XSharpSdkProjectNode(XSharpProjectPackage package) : base(package)
        {
            _targetFrameworks = new List<string>();
            OnProjectPropertyChanged += XSharpSdkProjectNode_OnProjectPropertyChanged;
            this.virtualProjectGuid = true;

        }
        internal string CheckFrameworks()
        {
            MainProject = this.BuildProject;
            string framework = null;
            string oldframework = null;
            string frameworks = null;
            bool single = false;
            bool mustSwitch = false;
            _targetFrameworks.Clear();
            _subProjects.Clear();
            // First check for single TargetFramework
            framework = this.GetProjectProperty(XSharpProjectFileConstants.TargetFramework, false);
            if (string.IsNullOrEmpty(framework))
            {
                framework = this.GetProjectProperty(XSharpProjectFileConstants.ActiveTargetFramework);
            }
            oldframework = framework;
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
            else
            {
                if (frameworks != null && framework != null && !frameworks.ToLower().Contains(framework.ToLower()))
                {
                    single = false;
                    framework = null;
                    mustSwitch = true;
                }
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
                foreach (var fw in splits)
                {
                    if (_targetFrameworks.Contains(fw))
                    {
                        // this was already added
                        continue;
                    }
                    _targetFrameworks.Add(fw);
                    var subProject = new SdkSubProjectInfo(fw, this);
                    _subProjects.Add(subProject);
                }
                ActiveSubProject = _subProjects.First();
            }
            _frameworks = frameworks;
            if (mustSwitch)
            {
                string message = "You have removed the active targetframework from the targetframeworks property.\n"
               + "Any unsaved changes within the project will be automatically saved.\n\n"
               + "The project will then be reopened with the new targetframrwork.\n\n"
               + "Are you sure you want to change the Target Framework for this project?";
                if (!VS.MessageBox.ShowConfirm(message))
                {
                    _targetFrameworks.Add(oldframework);
                    var subProject = new SdkSubProjectInfo(oldframework, this);
                    _subProjects.Add(subProject);
                    ActiveSubProject = subProject;
                    return frameworks + ";" + oldframework;
                }
                this.BeforeSave();
                this.BuildProject.Save();
                SelectSubProject(ActiveSubProject);
            }
            return frameworks;
        }

        public override void SetBuildProject(MSBuild.Project newBuildProject)
        {
            base.SetBuildProject(newBuildProject);
            if (this.ProjectIDGuid == Guid.Empty)
                this.SetProjectGuidFromProjectFile();
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
            if (newBuildProject.IsDirty)
            {
                this.BeforeSave();
                newBuildProject.Save();
            }


        }

        private void SaveTargetFrameworks()
        {
            if (TargetFrameworks.Count > 1)
            {
                // Store the active project in the XTargetFramework property
                SetProjectProperty(XSharpProjectFileConstants.ActiveTargetFramework, ActiveSubProject.TargetFramework);
                RemoveProjectProperty(XSharpProjectFileConstants.TargetFramework);
                RemoveProjectProperty(XSharpProjectFileConstants.XTargetFrameworks);
                SetProjectProperty(XSharpProjectFileConstants.TargetFrameworks, _frameworks);
            }
            else
            {
                // Store the active project in the TargetFramework property
                RemoveProjectProperty(XSharpProjectFileConstants.ActiveTargetFramework);
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

        internal bool IsNetCoreApp
        {
            get
            {
                var targetFramework = this.GetProjectProperty(ProjectFileConstants.TargetFrameworkIdentifier, false) ?? "";
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

        /// <summary>
        /// Return list of guids of property pages that are independent of the configuration
        /// </summary>
        /// <returns>List of pages GUIDs.</returns>
        protected override Guid[] GetConfigurationIndependentPropertyPages()
        {
            return new Guid[]
            {
                typeof(XSharpGeneralPropertyPage).GUID,
                typeof(XSharpLanguagePropertyPage).GUID,
                typeof(XSharpDialectPropertyPage).GUID,
                typeof(XSharpPackagePropertyPage).GUID,
                typeof(XSharpGlobalUsingsPropertiesPage).GUID
            };
        }

        protected override BuildSubmission DoMSBuildSubmission(BuildKind buildKind, string target, ref ProjectInstance projectInstance, MSBuildCoda uiThreadCallback)
        {
            var result = base.DoMSBuildSubmission(buildKind, target, ref projectInstance, uiThreadCallback);
            ProcessOptions(projectInstance, target);
            return result;
        }
        private List<string> _commandLineArguments = new List<string>();
        protected List<string> _sdkReferences = new List<string>();
        protected List<string> _allReferenceAssemblies = new List<string>();
        void ProcessOptions(ProjectInstance projectInstance, string target)
        {
            Logger.Information($"Build:  Invocation Result for target '{target}'");
            if (projectInstance != null)
            {
                var commandLineArguments = new List<string>();
                var sdkReferences = new List<string>();
                var allReferenceAssemblies = new List<string>();
                //Logger.Information($"Build:  Properties for projectInstance {projectInstance.FullPath}");
                //foreach (var item in projectInstance.Properties)
                //{
                //    Logger.Information($"Prop: {item.Name} {item.EvaluatedValue}");
                //}
                Logger.Information($"Build:  Items for projectInstance {projectInstance.FullPath}");
                var items = projectInstance.Items.ToArray();
                foreach (var item in items)
                {
                    var file = item.EvaluatedInclude.Replace("/", "\\"); ;
                    switch (item.ItemType.ToLower())
                    {
                        case "_explicitreference":
                        case "reference":
                            if (System.IO.File.Exists(file))
                                allReferenceAssemblies.AddUnique(file);
                            break;
                        case "referencepath":
                            if (System.IO.File.Exists(file))
                                allReferenceAssemblies.AddUnique(file);
                            if (item.GetMetadataValue("NugetSourceType")?.ToLower() == "package")
                            {
                                Logger.Information($"Build:  Item: Package{item.ItemType} {file}");
                                continue;
                            }
                            if (item.HasMetadata("FrameworkReferenceName"))
                                sdkReferences.AddUnique(file);
                            break;
                        case "xsccommandlineargs":
                            commandLineArguments.AddUnique(item.EvaluatedInclude);
                            break;
                        case "resolvedframeworkreference":
                            break;
                        default:
                            continue;
                    }

                    Logger.Information($"Build:  Item: {item.ItemType} {item.EvaluatedInclude}");
                }
                if (commandLineArguments.Count > 0)
                {
                    _commandLineArguments = commandLineArguments;
                }
                if (sdkReferences.Count > 0)
                {
                    _sdkReferences = sdkReferences;
                }
                if (allReferenceAssemblies.Count > 0)
                {
                    _allReferenceAssemblies = allReferenceAssemblies;
                }

            }
        }


        public override Guid[] GetReferencePages()
        {
            if (IsNetCoreApp)
            {
                return new[] {
                          VSConstants.ProjectReferenceProvider_Guid,
                          VSConstants.FileReferenceProvider_Guid,
                    };
            }
            return base.GetReferencePages();
        }


        protected Guid VsStd16 = new Guid("8F380902-6040-4097-9837-D3F40E66F908");
        protected const uint idAddAssemblyReference = (uint) VSConstants.VSStd16CmdID.AddAssemblyReference;
        protected const uint idAddCOMReference = (uint)VSConstants.VSStd16CmdID.AddComReference;
        protected const uint idAddProjectReference = (uint)VSConstants.VSStd16CmdID.AddProjectReference;
        protected const uint idAddSharedProjectReference = (uint)VSConstants.VSStd16CmdID.AddSharedProjectReference;
        protected const uint idAddSDKReference = (uint)VSConstants.VSStd16CmdID.AddSdkReference;


        protected override QueryStatusResult QueryStatusCommandFromOleCommandTarget(Guid cmdGroup, uint cmd, out bool handled)
        {
            handled = false;
            if (cmdGroup == Microsoft.VisualStudio.Project.VsMenus.guidStandardCommandSet2K)
            {
                switch ((VsCommands2K)cmd)
                {
                    case VsCommands2K.ADDREFERENCE:
                        handled = true;
                        return QueryStatusResult.NOTSUPPORTED | QueryStatusResult.INVISIBLE;
                }
            }
            return base.QueryStatusCommandFromOleCommandTarget (cmdGroup, cmd, out handled);
        }
        protected override int QueryStatusOnNode(Guid cmdGroup, uint cmd, IntPtr pCmdText, ref QueryStatusResult result)
        {
            if (cmdGroup == Microsoft.VisualStudio.Project.VsMenus.guidStandardCommandSet2K)
            {

                switch ((VsCommands2K)cmd)
                {
                    case VsCommands2K.ADDREFERENCE:
                        result |= QueryStatusResult.NOTSUPPORTED | QueryStatusResult.INVISIBLE;
                        return VSConstants.S_OK;
                }
            }
            else if (cmdGroup == VsStd16)
            {
                switch (cmd)
                {
                    case idAddProjectReference:
                        result |= QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED;
                        return VSConstants.S_OK;

                    case idAddAssemblyReference:
                    case idAddCOMReference:
                        if (this.IsNetCoreApp)
                        {
                            result |= QueryStatusResult.NOTSUPPORTED | QueryStatusResult.INVISIBLE;
                        }
                        else
                        {
                            result |= QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED;
                        }
                        return VSConstants.S_OK;

                    case idAddSharedProjectReference:
                    case idAddSDKReference:
                        result |= QueryStatusResult.NOTSUPPORTED | QueryStatusResult.INVISIBLE;
                        return VSConstants.S_OK;
                }
            }
            return base.QueryStatusOnNode(cmdGroup, cmd, pCmdText, ref result);
        }

        protected override int ExecCommandOnNode(Guid cmdGroup, uint cmd, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            if (cmdGroup == VsStd16)
            {
                switch (cmd)
                {
                    case idAddProjectReference:
                        return this.AddProjectReference();

                    case idAddAssemblyReference when  !this.IsNetCoreApp:
                        return this.AddAssemblyReference();

                    case idAddCOMReference when !this.IsNetCoreApp:
                        return this.AddCOMReference();

                }
            }
            return base.ExecCommandOnNode(cmdGroup, cmd, nCmdexecopt, pvaIn, pvaOut);
        }


        protected override List<string> RefreshReferences()
        {
            // find the resource file and read the lines with /reference
            List<string> references;
            references = new List<string>();
            references.AddRange(_allReferenceAssemblies);
            var sdkrefs = _sdkReferences;
            AddPendingReferences(sdkrefs, this.ActiveSubProject);

            var rsprefs = base.RefreshReferencesFromResponseFile();
            foreach ( var reference in rsprefs)
            {
                references.AddUnique(reference);
            }
            ProjectModel.RefreshReferences(references);
            return references;
        }

        public override ProjectOptions GetProjectOptions(ConfigCanonicalName configCanonicalName)
        {
            XSharpProjectOptions xoptions;
            if (this.options != null)
            {
                xoptions = this.options as XSharpProjectOptions;
                if (xoptions.ConfigCanonicalName != configCanonicalName)
                {
                    InvalidateOptions();
                }
            }
            options = base.GetProjectOptions(configCanonicalName);
            xoptions = this.options as XSharpProjectOptions;
            xoptions.ConfigCanonicalName = configCanonicalName;
            foreach (var cmd in _commandLineArguments)
            {
                if (cmd.StartsWith("/define:"))
                {
                    xoptions.DefinedPreprocessorSymbols.Clear();
                    var constants = cmd.Substring(8);

                    foreach (string s in constants.Replace(" \t\r\n", "").Split(';'))
                    {
                        options.DefinedPreprocessorSymbols.Add(s);
                    }
                }
                xoptions.BuildCommandLine();
            }
            return this.options;
        }

        public override int Save(string fileToBeSaved, int remember, uint formatIndex)
        {
            var nodes = GetSdkProjectReferences();
            var result = base.Save(fileToBeSaved, remember, formatIndex);
            foreach (var node in nodes)
            {
                node.RestoreProperties();
            }
            return result;
        }

        void Clean()
        {
            var folderNodes = new List<FolderNode>();
            bool dirty = false;
            this.FindNodesOfType(folderNodes);
            foreach (var node in folderNodes)
            {
                if (!(node is XSharpSdkFolderNode) && node.ItemNode != null
                    && node.ItemNode.Item != null)
                {
                    try
                    {
                    this.BuildProject.RemoveItem(node.ItemNode.Item);
                    Logger.Information($"Clean: Removed folder node {node.Caption} from project {this.Caption}");
                    dirty = true;
                    }
                    catch
                    {

                    }
                }
            }
            var referenceNodes = new List<XSharpSDKProjectReferenceNode>();
            this.FindNodesOfType(referenceNodes);
            foreach (var node in referenceNodes)
            {
                node.RemoveProperties();
            }
            if (this.GetProjectProperty(ProjectFileConstants.ProjectGuid) != null)
            {
                Logger.Information($"Clean: Removed project Guid from project{this.Caption}");
                this.RemoveProjectProperty(ProjectFileConstants.ProjectGuid);
            }
            if (this.BuildProject.IsDirty || dirty)
            {
                this.BuildProject.Save();
            }
        }
        public override void BeforeSave()
        {
            base.BeforeSave();
            this.SaveTargetFrameworks();
            this.Clean();
        }

        private void AddPendingReferences(List<string> newReferences, SdkSubProjectInfo active)
        {
            HierarchyNode frameworkNode = null;
            if (SubProjects.Count > 1)
                frameworkNode = active.TargetFrameworkReferenceNode;
            else
                frameworkNode = active.FrameworksNode;
            if (frameworkNode == null)
            {
                return;
            }
            var isExpanded = frameworkNode.IsExpanded;
            var nodes = new List<XSharpDependencyNode>();
            frameworkNode.FindNodesOfType(nodes);

            var toDelete = new List<XSharpDependencyNode>();
            var toAdd = new List<string>();
            // Check if existing reference needs to be removed
            foreach (var reference in sdkReferences)
            {
                var name = reference;
                if (newReferences.Find(r => string.Equals(r, name, StringComparison.OrdinalIgnoreCase)) != null)
                {
                    var oldnode = nodes.Find(n => n.Url.ToLower() == reference.ToLower());
                    toDelete.Add(oldnode);
                }
            }
            // add dependencies from the TargetFramework
            foreach (var reference in newReferences)
            {
                var name = reference.ToLower();
                if (sdkReferences.Find( r => r.ToLower() == name) == null)
                {
                    toAdd.Add(reference);
                }
            }
            // delete nodes that are no longer needed
            foreach (var node in toDelete)
            {
                if (node is object)
                {
                    frameworkNode.RemoveChild(node);
                    node.Dispose();
                }
            }
            // add new nodes
            this.SuspendBuild = true;

            foreach (var item in toAdd)
            {
                var node = new XSharpDependencyNode(this, item);
                frameworkNode.AddChild(node);
            }
            this.SuspendBuild = false;
            sdkReferences.Clear();
            sdkReferences.AddRange(newReferences);
            frameworkNode.IsExpanded = isExpanded;
        }
    }
    internal class XSharpFrameworkNode : XSharpDependencyNode
    {
        public XSharpFrameworkNode(XSharpProjectNode root, string filePath) :
            base(root, filePath)
        {

        }
        protected override ImageMoniker GetIconMoniker(bool open) => KnownMonikers.DotNETFrameworkDependency;
    }

    internal class XSharpDependencyNode : XSharpAssemblyReferenceNode
    {
        public XSharpDependencyNode(XSharpProjectNode root, string filePath) :
            base(root, filePath)
        {
            // We do not want to store these dependencies in the project file, so we remove them from the BuildProject
            if (this.ItemNode != null && this.ItemNode.Item != null)
                root.BuildProject.RemoveItem(this.ItemNode.Item);
        }
        public override bool EmbedInteropTypes { get => false; set { }}


        protected override ImageMoniker GetIconMoniker(bool open) => KnownMonikers.DotNETFrameworkDependency;
        protected override void ResolveAssemblyReference()
        {
            if (this.ProjectMgr is XSharpSdkProjectNode sdk && sdk.SuspendBuild)
            {
                return;
            }

            base.ResolveAssemblyReference();
        }
        protected override void BindReferenceData()
        {
            if (this.ProjectMgr is XSharpSdkProjectNode sdk && sdk.SuspendBuild)
            {
                return;
            }
            base.BindReferenceData();
        }
        override public Guid ItemTypeGuid => VSConstants.GUID_ItemType_VirtualFolder;
        protected override int QueryStatusOnNode(Guid cmdGroup, uint cmd, IntPtr pCmdText, ref QueryStatusResult result)
        {
            if (cmdGroup == Microsoft.VisualStudio.Project.VsMenus.guidStandardCommandSet97)
            {
                switch ((VsCommands)cmd)
                {
                    case VsCommands.Remove:
                    case VsCommands.Delete:
                        result |= QueryStatusResult.SUPPORTED | QueryStatusResult.INVISIBLE;
                        return 0;
                }
            }
            return base.QueryStatusOnNode(cmdGroup, cmd, pCmdText, ref result);
        }
    }

}

#endif
