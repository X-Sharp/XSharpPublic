//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#undef USEPROJECTVERSION
using Community.VisualStudio.Toolkit;
using EnvDTE;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;
using VSLangProj;
using XSharp.CodeDom;
using XSharp.Project.WPF;
using XSharpModel;
using XSharp.Settings;
using File = System.IO.File;
using MBC = Microsoft.Build.Construction;
using MSBuild = Microsoft.Build.Evaluation;
using VsParser = global::LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis;
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
//using XSharp.LanguageService;

namespace XSharp.Project
{
    /// <summary>
    /// This class extends the ProjectNode in order to represent our project
    /// within the hierarchy.
    /// </summary>
    [DebuggerDisplay("{Caption}")]
    [Guid("F1A46976-964A-4A1E-955D-E05F5DB8651F")]
    public partial class XSharpProjectNode : XProjectNode, IVsSingleFileGeneratorFactory, IXSharpProject,
        /*IVsDesignTimeAssemblyResolution, */IVsProject5, IProjectTypeHelper, IXsProjectDesigner
    , IVsReferenceManagerUser
    {
        static List<XSharpProjectNode> nodes = new List<XSharpProjectNode>();

        static IDictionary<string, string> dependencies;
        static IDictionary<string, string> _changedProjectFiles;

        static XSharpProjectNode()
        {
            // first the extension to look for, second the extension that can be the parent
            dependencies = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            dependencies.Add(".designer.prg", ".prg");
            dependencies.Add(".xaml.prg", ".xaml");
            dependencies.Add(".vh", ".prg");
            dependencies.Add(".xh", ".prg");
            dependencies.Add(".resx", ".prg");
            try
            {
                imageList = Utilities.GetImageList(typeof(XSharpProjectNode).Assembly.GetManifestResourceStream("XSharp.Project.Resources.XSharpProjectImageList.bmp"));
            }
            catch (Exception)
            {
            }
            _changedProjectFiles = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        }
        internal static IDictionary<string, string> ChangedProjectFiles => _changedProjectFiles;
        internal static XSharpProjectNode[] AllProjects => nodes.ToArray();

        #region Constants
        internal const string ProjectTypeName = XSharpConstants.LanguageName;
        #endregion

        #region Fields
        private XSharpProjectPackage package;
        private static ImageList imageList;
        private VSLangProj.VSProject vsProject;
        bool isLoading = false;
        XSharpModel.XProject projectModel;
        Dictionary<string, string> _cachedProjectProperties;


        //private Microsoft.VisualStudio.Designer.Interfaces.IVSMDCodeDomProvider codeDomProvider;
        #endregion

        #region Constructors

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpProjectNode"/> class.
        /// </summary>
        /// <param name="package">Value of the project package for initialize internal package field.</param>
        public XSharpProjectNode(XSharpProjectPackage package)
        {
            this.package = package;
            _cachedProjectProperties = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            this.OnProjectPropertyChanged += XSharpProjectNode_OnProjectPropertyChanged;
            InitializeImageList();
            InitializeCATIDs();

            // Used by (at least) the AddFromTemplate in order (for eg) to have Form1.Designer.Prg depending on Form1.prg
            this.CanFileNodesHaveChilds = true;

            this.CanProjectDeleteItems = true;

            // Gets or sets whether the project uses the Project Designer Editor or the property page frame to edit project properties.
            // True : New WPF way
            // False: C++-Like Property pages
            this.SupportsProjectDesigner = true;
            lock (nodes)
            {
                nodes.Add(this);
            }
        }

        internal void ClearCache()
        {
            _cachedProjectProperties.Clear();
        }

        public bool HasProjectProperty(string propertyName)
        {
            return !string.IsNullOrEmpty(GetProjectProperty(propertyName));
        }

        public bool GetLogicProjectProperty(string propertyName)
        {
            var prop = GetProjectProperty(propertyName);
            return prop != null && string.Compare(prop, "true",true) == 0;
        }

        public override string GetProjectProperty(string propertyName)
        {
            if (_cachedProjectProperties.ContainsKey(propertyName))
                return _cachedProjectProperties[propertyName];
            var result = base.GetProjectProperty(propertyName);
            if (result != null)
                _cachedProjectProperties[propertyName] = result;
            return result;
        }

        public override void SetProjectProperty(string propertyName, string propertyValue)
        {
            base.SetProjectProperty(propertyName, propertyValue);
            if (propertyValue == null && _cachedProjectProperties.ContainsKey(propertyName))
                _cachedProjectProperties.Remove(propertyName);
            else if (propertyValue != null)
                _cachedProjectProperties[propertyName] = propertyValue;
        }

        private void XSharpProjectNode_OnProjectPropertyChanged(object sender, ProjectPropertyChangedArgs e)
        {
            if (string.Compare(e.PropertyName, XSharpProjectFileConstants.Dialect, true) == 0)
            {
                var prop = e.NewValue;
                if (!Enum.TryParse(prop, true, out _dialect))
                {
                    _dialect = VsParser.XSharpDialect.Core;
                }
                _dialectIsCached = true;
            }
            _cachedProjectProperties[e.PropertyName] = e.NewValue;
            this.ClearOptions();
        }


        protected override void OnFileChanged(string url)
        {
            //Logger.LogMessage("FileChangedOnDisk " + e.FileName);
            if (IsXamlFile(url) || IsCodeFile(url))
            {
                XFile file = this.ProjectModel.FindXFile(url);
                if (file != null)
                {
                    this.ProjectModel.WalkFile(file, true);
                }
            }
        }

        private void Newtask_Navigate(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            lock (this)
            {
#if DEV17
                var task = (Microsoft.VisualStudio.Shell.TaskListItem)sender;
#else
                var task = (Microsoft.VisualStudio.Shell.Task)sender;
#endif
                XSettings.OpenDocument(task.Document, task.Line, task.Column, false);
            }
        }


        /// <summary>
        /// Provide mapping from our browse objects and automation objects to our CATIDs
        /// CATID (Category ID) objects are used to extend the properties that appear in the Properties window for projects and project items.
        /// </summary>
        private void InitializeCATIDs()
        {
            // The following properties classes are specific to XSharp so we can use their GUIDs directly
            //
            AddCATIDMapping(typeof(XSharpProjectNodeProperties), typeof(XSharpProjectNodeProperties).GUID);
            AddCATIDMapping(typeof(XSharpFileNodeProperties), typeof(XSharpFileNodeProperties).GUID);
            AddCATIDMapping(typeof(OAXSharpProject), typeof(OAXSharpProject).GUID);
            AddCATIDMapping(typeof(OAProject), typeof(OAProject).GUID);

            // This one we use the same as file nodes since both refer to files
            AddCATIDMapping(typeof(FileNodeProperties), typeof(XSharpFileNodeProperties).GUID);

            // Because our property page pass itself as the object to display in its grid,
            // we need to make it have the same CATID as the browse object of the project node
            // so that filtering is possible.
            AddCATIDMapping(typeof(XSharpGeneralPropertyPage), typeof(XSharpGeneralPropertyPage).GUID);

            //// We could also provide CATIDs for references and the references container node, if we wanted to.
            AddCATIDMapping(typeof(XSharpBuildPropertyPage), typeof(XSharpBuildPropertyPage).GUID);
            AddCATIDMapping(typeof(XSharpBuildEventsPropertyPage), typeof(XSharpBuildEventsPropertyPage).GUID);
            AddCATIDMapping(typeof(XSharpLanguagePropertyPage), typeof(XSharpLanguagePropertyPage).GUID);
            AddCATIDMapping(typeof(XSharpDialectPropertyPage), typeof(XSharpDialectPropertyPage).GUID);
            AddCATIDMapping(typeof(XSharpDebugPropertyPage), typeof(XSharpDebugPropertyPage).GUID);
        }
        #endregion

        #region Properties

        internal bool IsLoading => isLoading;
        /// <summary>
        /// Gets or sets the image list.
        /// </summary>
        /// <value>The image list.</value>
        public static ImageList ImageList
        {
            get
            {
                return imageList;
            }
            set
            {
                imageList = value;
            }
        }
        /// <summary>
        /// Gets the XSharpPackage instance for this project.
        /// </summary>
        /// <remarks>The SDK2008 base ProjectNode class has a Package property defined,
        /// but we can't use it (with the  single codebase) because it isn't in SDK2005.</remarks>
        internal XSharpProjectPackage XSharpPackage
        {
            get
            {
                return this.package;
            }
        }

        protected internal VSLangProj.VSProject VSProject
        {
            get
            {
                if (vsProject == null)
                {
                    vsProject = new OAXSharpVSProject(this);
                }

                return vsProject;
            }
        }

        internal IVsHierarchy InteropSafeHierarchy
        {
            get
            {
                IntPtr unknownPtr = Utilities.QueryInterfaceIUnknown(this);
                if (IntPtr.Zero == unknownPtr)
                {
                    return null;
                }
                ThreadHelper.ThrowIfNotOnUIThread();
                IVsHierarchy hier = Marshal.GetObjectForIUnknown(unknownPtr) as IVsHierarchy;
                return hier;
            }
        }


        #endregion

        #region Overriden implementation

        internal static bool InContextMenu = false;

        protected override int ShowContextMenu(int menuId, Guid menuGroup, POINTS points)
        {
            InContextMenu = true;
            var result = base.ShowContextMenu(menuId, menuGroup, points);
            InContextMenu = false;
            return result;
        }
        /// <summary>
        /// Gets the project GUID.
        /// </summary>
        /// <value>The project GUID.</value>
        public override Guid ProjectGuid
        {
            get { return XSharpConstants.guidXSharpProjectFactory; }
        }
        public override string ProjectGuidString
        {
            get { return XSharpConstants.guidXSharpProjectFactoryStringCurly; }
        }

        /// <summary>
        /// Gets the type of the project.
        /// </summary>
        /// <value>The type of the project.</value>
        public override string ProjectType
        {
            get { return ProjectTypeName; }
        }

        /// <summary>
        /// Return an imageindex
        /// </summary>
        /// <value></value>
        /// <returns></returns>
        public override int ImageIndex
        {
            get
            {
                return imageOffset;
            }
        }

        public override object Object
        {
            get
            {
                return this.VSProject;
            }
        }

        /// <summary>
        /// Override this method if you have your own project specific
        /// subclass of ProjectOptions
        /// </summary>
        /// <returns>This method returns a new instance of the ProjectOptions base class.</returns>
        public override ProjectOptions CreateProjectOptions()
        {
            var xoptions = new XSharpProjectOptions(this);
            base.options = xoptions;
            if (projectModel != null)
                projectModel.ResetParseOptions(null);
            return options;
        }

        internal void InvalidateOptions()
        {
            this.options = null;
            //this.OutputFile = null;
            this._outputPath = null;

        }
        public override void SetProjectFileDirty(bool value)
        {
            base.SetProjectFileDirty(value);
            InvalidateOptions();
        }
        public override void OnItemAdded(HierarchyNode parent, HierarchyNode child)
        {
            base.OnItemAdded(parent, child);
            if (child is XSharpFileNode xfile)
            {
                xfile.SetSpecialPropertiesEx();
            }
        }
        public override ProjectOptions GetProjectOptions(ConfigCanonicalName configCanonicalName)
        {

            if (this.options != null)
            {
                var xoptions = this.options as XSharpProjectOptions;
                if (xoptions.ConfigCanonicalName != configCanonicalName)
                {
                    InvalidateOptions();
                }
            }
            if (this.options == null)
            {
                ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    if (this.options == null)
                    {
                        this.options = base.GetProjectOptions(configCanonicalName);
                        var xoptions = this.options as XSharpProjectOptions;
                        xoptions.ConfigCanonicalName = configCanonicalName;
                        xoptions.BuildCommandLine();
                    }

                });
            }
            return this.options;
        }

        public override void PrepareBuild(ConfigCanonicalName config, bool cleanBuild)
        {
            // Do not prepare the build when we are not completely loaded. This
            // speeds up loading a bit
            base.PrepareBuild(config, cleanBuild);
        }
        public void RemoveProjectProperty(string name)
        {
            var prop = this.BuildProject.GetProperty(name);
            if (prop != null && !prop.IsImported)
            {
                this.BuildProject.RemoveProperty(prop);
            }
        }


        public override string GetProjectProperty(string propertyName, bool resetCache, bool unevaluated = false)
        {
            if (BuildProject != null)
            {
                return base.GetProjectProperty(propertyName, resetCache, unevaluated);
            }
            return null;
        }

        public __VSPROJOUTPUTTYPE GetOutPutType()
        {
            string outputTypeAsString = this.GetProjectProperty("OutputType", false);
            switch (outputTypeAsString.ToLower())
            {
                case "winexe":
                    return __VSPROJOUTPUTTYPE.VSPROJ_OUTPUTTYPE_WINEXE;
                case "library":
                    return __VSPROJOUTPUTTYPE.VSPROJ_OUTPUTTYPE_LIBRARY;
                case "exe":
                    return __VSPROJOUTPUTTYPE.VSPROJ_OUTPUTTYPE_EXE;
                case "winmdobj":
                    return __VSPROJOUTPUTTYPE.VSPROJ_OUTPUTTYPE_WINMDOBJ;
                case "appcontainerexe":
                    return __VSPROJOUTPUTTYPE.VSPROJ_OUTPUTTYPE_APPCONTAINEREXE;
            }
            return __VSPROJOUTPUTTYPE.VSPROJ_OUTPUTTYPE_NONE;
        }
        public override object GetProperty(int propId)
        {
            switch (propId)
            {
                case unchecked((int)VSConstants.VSITEMID_ROOT):
                    return this;
                case (int)__VSHPROPID.VSHPROPID_DefaultNamespace:
                    return this.RootNameSpace;
                case (int)__VSHPROPID5.VSHPROPID_OutputType:
                    return (uint)GetOutPutType();
                case (int)__VSHPROPID2.VSHPROPID_DesignerHiddenCodeGeneration:
                case (int)__VSHPROPID3.VSHPROPID_WebReferenceSupported:
                case (int)__VSHPROPID3.VSHPROPID_ServiceReferenceSupported:
                case (int)__VSHPROPID3.VSHPROPID_SupportsHierarchicalUpdate:
                case (int)__VSHPROPID3.VSHPROPID_SupportsLinqOverDataSet:
                case (int)__VSHPROPID3.VSHPROPID_SupportsNTierDesigner:
                case (int)__VSHPROPID6.VSHPROPID_ShowAllProjectFilesInProjectView:
                    return true;
                case (int)__VSHPROPID6.VSHPROPID_NuGetPackageProjectTypeContext:
                    return "XSharp.ProjectSystem";
                //case (int)__VSHPROPID6.VSHPROPID_Subcaption:
                //case (int)__VSHPROPID7.VSHPROPID_ShortSubcaption:
                //    return "X#";
                case (int)__VSHPROPID7.VSHPROPID_CanBuildQuickCheck:
                case (int)__VSHPROPID7.VSHPROPID_CanDebugLaunchQuickCheck:
                    return _VSQuickCheckAnswer.QCA_Always;
                // Added for NuGet Support
                case (int)__VSHPROPID8.VSHPROPID_ProjectCapabilitiesChecker:
                    if (_checker == null)
                    {
                        _checker = new XSharpProjectCapabilitiesPresenceChecker();
                    }
                    return _checker;

                // Test ?
                case (int)__VSHPROPID5.VSHPROPID_TargetPlatformIdentifier:
                    return "Windows";
            }
            return base.GetProperty(propId);
        }
        static private XSharpProjectCapabilitiesPresenceChecker _checker;

        private object automationobject;
        /// <summary>
        /// Returns an automation object representing this node
        /// </summary>
        /// <returns>The automation object</returns>
        public override object GetAutomationObject()
        {
            if (automationobject == null)
            {
                automationobject = new OAXSharpProject(this);
            }
            return automationobject;
        }

        /// <summary>
        /// Creates the file node.
        /// </summary>
        /// <param name="item">The project element item.</param>
        /// <returns></returns>
        public override FileNode CreateFileNode(ProjectElement item)
        {
            Utilities.ArgumentNotNull("item", item);

            XSharpFileNode node = new XSharpFileNode(this, item, item.IsVirtual);
            if (!item.IsVirtual)
            {
                // No extra services for excluded items
                var provider = node.OleServiceProvider;

                // Use the CreateServices of the Project
                provider.AddService(typeof(EnvDTE.Project), new OleServiceProvider.ServiceCreatorCallback(this.CreateServices), false);
                // Use the CreateServices of the Node
                provider.AddService(typeof(ProjectItem), node.ServiceCreator, false);
                // Use the CreateServices of the Project
                provider.AddService(typeof(VSProject), new OleServiceProvider.ServiceCreatorCallback(this.CreateServices), false);
                //provider.AddService(typeof(VSProject), this.VSProject, false);

                // Not just for source items. Also for ResX and Settings
                provider.AddService(typeof(SVSMDCodeDomProvider), new XSharpVSMDProvider(node), false);
            }
            return node;
        }

        /// <summary>
        /// Creates and returns the folder node object for projects.
        /// </summary>
        /// <param name="path">Folder path.</param>
        /// <param name="element">MSBuild element.</param>
        /// <returns>Returns newly created Folder Node object.</returns>
        [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "1#")]
        protected override FolderNode CreateFolderNode(string path, ProjectElement element)
        {
            Utilities.ArgumentNotNull("element", element);
            FolderNode folderNode;
            if (String.Compare(Path.GetDirectoryName(path), "properties", StringComparison.OrdinalIgnoreCase) == 0)
            {
                folderNode = new XSharpPropertiesFolderNode(this, path, element);
            }
            else
            {
                folderNode = new XSharpFolderNode(this, path, element, element.IsVirtual);
            }
            return folderNode;
        }


        protected override ConfigProvider CreateConfigProvider()
        {
            return new XSharpConfigProvider(this);
        }

        /// <summary>
        /// Create dependent file node based on an msbuild item
        /// </summary>
        /// <param name="item">msbuild item</param>
        /// <returns>dependent file node</returns>
        public override FileNode CreateDependentFileNode(ProjectElement item)
        {
            XSharpDependentFileNode newNode = new XSharpDependentFileNode(this, item, item.IsVirtual);
            string include = item.GetMetadata(ProjectFileConstants.Include);

            var provider = newNode.OleServiceProvider;

            provider.AddService(typeof(EnvDTE.Project), GetAutomationObject(), false);
            provider.AddService(typeof(EnvDTE.ProjectItem), newNode.GetAutomationObject(), false);
            provider.AddService(typeof(VSLangProj.VSProject), this.VSProject, false);

            if (IsCodeFile(include) && item.ItemName == "Compile")
                newNode.OleServiceProvider.AddService(typeof(SVSMDCodeDomProvider),
                    new XSharpVSMDProvider(newNode), false);
            if (newNode.FileType == XFileType.ManagedResource)
            {
                newNode.Generator = null;
            }

            return newNode;
        }

        private Guid guidPublishPage = Guid.Parse("CC4014F5-B18D-439C-9352-F99D984CCA85");

        /// <summary>
        /// Return list of guids of all property pages
        /// </summary>
        /// <returns>List of pages GUIDs.</returns>
        protected override Guid[] GetPriorityProjectDesignerPages()
        {
            Guid[] result = new Guid[]
                {
                typeof(XSharpGeneralPropertyPage).GUID,
                typeof(XSharpLanguagePropertyPage).GUID,
                typeof(XSharpDialectPropertyPage).GUID,
                typeof(XSharpBuildPropertyPage).GUID,
                typeof(XSharpBuildEventsPropertyPage).GUID,
                typeof(XSharpDebugPropertyPage).GUID
                };
            return result;
        }

        /// <summary>
        /// Return list of guids of property pages that are independent of the configuration
        /// </summary>
        /// <returns>List of pages GUIDs.</returns>
        protected override Guid[] GetConfigurationIndependentPropertyPages()
        {
            Guid[] result = new Guid[]
                {
                typeof(XSharpGeneralPropertyPage).GUID,
                typeof(XSharpLanguagePropertyPage).GUID,
                 typeof(XSharpDialectPropertyPage).GUID,
                };
            return result;
        }

        /// <summary>
        /// Return list of guids of property pages that are dependent of the configuration
        /// </summary>
        /// <returns>List of pages GUIDs.</returns>
        protected override Guid[] GetConfigurationDependentPropertyPages()
        {
            Guid[] result = new Guid[]
            {
                typeof(XSharpBuildPropertyPage).GUID,
                typeof(XSharpDebugPropertyPage).GUID,
                typeof(XSharpBuildEventsPropertyPage).GUID,
            };
            return result;
        }

        private HierarchyNode _GetAppDesignerFolder(bool createIfNotExist)
        {
            HierarchyNode node = null;

            List<XSharpPropertiesFolderNode> folderNodes = new List<XSharpPropertiesFolderNode>();
            FindNodesOfType<XSharpPropertiesFolderNode>(folderNodes);

            if (folderNodes.Count > 0) // really only should be 0 or 1
            {
                node = folderNodes[0];
            }
            else if (createIfNotExist)
            {
                HierarchyNode child = CreateFolderNodes("Properties");

                if (child != null)
                {
                    ((XSharpPropertiesFolderNode)child).CreateDirectory("Properties");
                }

                node = _GetAppDesignerFolder(false);
            }
            return node;
        }
        /// <summary>
        /// Allows you to query the project for special files and optionally create them.
        /// </summary>
        /// <param name="fileId">__PSFFILEID of the file</param>
        /// <param name="flags">__PSFFLAGS flags for the file</param>
        /// <param name="itemid">The itemid of the node in the hierarchy</param>
        /// <param name="fileName">The file name of the special file.</param>
        /// <returns></returns>
        public override int GetFile(int fileId, uint flags, out uint itemid, out string fileName)
        {
            bool fCreateInPropertiesFolder = false;
            HierarchyNode propsFolder = null;
            string props = "Properties";
            bool createIfNotExist = (((__PSFFLAGS)flags) & __PSFFLAGS.PSFF_CreateIfNotExist) != 0;
            if (fileId == (int)__PSFFILEID2.PSFFILEID_AppDesigner)       // Special case, this returns a folder instead of a file
            {
                var propertiesNode = _GetAppDesignerFolder(createIfNotExist);
                itemid = 0;
                fileName = string.Empty;
                if (propertiesNode != null)
                {
                    itemid = propertiesNode.ID;
                    fileName = propertiesNode.Url;
                }
            }
            else
            {
                fileName = _GetFilenameForSpecialFiles(fileId, out fCreateInPropertiesFolder);
                string fullPath = Path.Combine(ProjectFolder, fileName);

                if (fCreateInPropertiesFolder)
                {
                    propsFolder = this.FindURL(Path.Combine(ProjectFolder, props));
                    if (propsFolder == null)
                    {
                        propsFolder = CreateFolderNode(props);
                        AddChild(propsFolder);
                    }
                    fileName = props + "\\" + fileName;
                    fullPath = Path.Combine(ProjectFolder, fileName);

                }
                HierarchyNode fileNode = this.FindURL(fullPath);
                if (fileNode == null && createIfNotExist)
                {
                    // Create a zero-length file if does not exist already.
                    //
                    if (!File.Exists(fullPath))
                    {
                        File.WriteAllText(fullPath, string.Empty);
                    }
                    fileNode = CreateFileNode(fileName);
                    if (fCreateInPropertiesFolder && propsFolder != null)
                    {
                        propsFolder.AddChild(fileNode);
                    }
                    else
                    {
                        AddChild(fileNode);
                    }
                    if (fileId == (int)__PSFFILEID2.PSFFILEID_AssemblyResource)
                    {
                        fileNode.ItemNode.SetMetadata("CustomTool", "ResXFileCodeGenerator");
                    }
                }

                itemid = fileNode != null ? fileNode.ID : uint.MaxValue;
                if ((flags & (uint)__PSFFLAGS.PSFF_FullPath) != 0)
                    fileName = fullPath;
            }
            return VSConstants.S_OK;
        }

        private string _GetFilenameForSpecialFiles(int fileId, out bool fCreateInPropertiesFolder)
        {
            string fileName = "";
            fCreateInPropertiesFolder = false;

            switch (fileId)
            {
                case (int)__PSFFILEID.PSFFILEID_AppConfig:
                    fileName = "app.config";
                    break;
                case (int)__PSFFILEID.PSFFILEID_Licenses:
                    fileName = "licenses.licx";
                    fCreateInPropertiesFolder = true;
                    break;
                case (int)__PSFFILEID2.PSFFILEID_WebSettings:
                    fileName = "web.config";
                    break;
                case (int)__PSFFILEID2.PSFFILEID_AppManifest:
                    fileName = "app.manifest";
                    break;
                case (int)__PSFFILEID2.PSFFILEID_AppSettings:
                    fileName = "Settings.settings";
                    fCreateInPropertiesFolder = true;
                    break;
                case (int)__PSFFILEID2.PSFFILEID_AssemblyResource:
                    fileName = "Resources.resx";
                    fCreateInPropertiesFolder = true;
                    break;
                case (int)__PSFFILEID2.PSFFILEID_AssemblyInfo:
                    fileName = "AssemblyInfo.prg";
                    fCreateInPropertiesFolder = true;
                    break;
                case (int)__PSFFILEID3.PSFFILEID_AppXaml:
                    fileName = "App.Xaml";
                    break;
                case (int)__PSFFILEID4.PSFFILEID_WcfServiceReferencesConfig:
                    fileName = "ServiceReference.config";
                    break;
                //case (int)__PSFFILEID5.PSFFILEID_AppxManifest:
                //    fileName = "App.manifest";
                //    break;
                default:
                    break;
            }
            return fileName;
        }

        /// <summary>
        /// Adds the file from template.
        /// </summary>
        /// <param name="source">The source template.</param>
        /// <param name="target">The target file.</param>
        public override void AddFileFromTemplate(string source, string target)
        {
            if (!File.Exists(source))
            {
                throw new FileNotFoundException(string.Format("Template file not found: {0}", source));
            }
            var type = XFileTypeHelpers.GetFileType(target);
            if (type == XFileType.Resource)
            {
                string path = Path.GetDirectoryName(target);
                if (!Directory.Exists(path))
                    Directory.CreateDirectory(path);
                Utilities.CopyFileSafe(source, target);
                return;
            }
            // The class name is based on the new file name
            string className = Path.GetFileNameWithoutExtension(target);
            string namespce = this.FileTemplateProcessor.GetFileNamespace(target, this);

            this.FileTemplateProcessor.AddReplace("%className%", className);
            this.FileTemplateProcessor.AddReplace("%namespace%", namespce);
            try
            {
                this.FileTemplateProcessor.UntokenFile(source, target);

                this.FileTemplateProcessor.Reset();
            }
            catch (Exception e)
            {
                throw new FileLoadException("Failed to add template file to project", target, e);
            }
        }
        internal bool WizardIsRunning { get; set; } = false;
        public override VSADDRESULT RunWizard(HierarchyNode parentNode, string itemName, string wizardToRun, IntPtr dlgOwner)
        {
            WizardIsRunning = true;
            var result = base.RunWizard(parentNode, itemName, wizardToRun, dlgOwner);
            WizardIsRunning = false;
            return result;
        }


        protected override HierarchyNode AddDependentFileNode(IDictionary<String, MSBuild.ProjectItem> subitems, string key)
        {
            Utilities.ArgumentNotNull("subitems", subitems);

            MSBuild.ProjectItem item = subitems[key];
            subitems.Remove(key);

            HierarchyNode newNode;
            HierarchyNode parent = null;
            bool inSameFolder = true;
            string dependentOf = item.GetMetadataValue(ProjectFileConstants.DependentUpon);
            // strip path when it is available
            if (dependentOf.IndexOf(System.IO.Path.DirectorySeparatorChar) >= 0)
            {
                // if the path in the parent is the same as the path of the item then we delete it
                // if it is different then the path must be a relative path from the project dir
                string dependentPath = Path.GetDirectoryName(this.GetRelativePath(dependentOf));
                string childPath = Path.GetDirectoryName(this.GetRelativePath(key));
                if (string.Equals(dependentPath, childPath, StringComparison.OrdinalIgnoreCase))
                {
                    dependentOf = System.IO.Path.GetFileName(dependentOf);
                    item.SetMetadataValue(ProjectFileConstants.DependentUpon, dependentOf);
                    SetProjectFileDirty(true);
                }
                else
                {
                    inSameFolder = false;
                }
            }
            else if (key.IndexOf(System.IO.Path.DirectorySeparatorChar) >= 0)
            {
                // dependentUpon in main folder, child in subfolder
                // or both in the subfolder
                string childPath = Path.Combine(this.ProjectFolder, System.IO.Path.GetDirectoryName(key));
                // check to see if they are in the same folder or not
                string dependentFileName = Path.Combine(childPath, dependentOf);
                inSameFolder = File.Exists(dependentFileName);
            }

            Debug.Assert(String.Compare(dependentOf, key, StringComparison.OrdinalIgnoreCase) != 0, "File dependent upon itself is not valid. Ignoring the DependentUpon metadata");
            if (subitems.ContainsKey(dependentOf))
            {
                // The parent item is an other subitem, so recurse into this method to add the parent first
                parent = AddDependentFileNode(subitems, dependentOf);
            }
            else
            {
                // See if the parent node already exist in the hierarchy
                // Please note that for the dependent node the folder is not included
                // so we take the folder from the child node and find the parentnode in the same folder
                uint parentItemID;
                string path = Path.Combine(this.ProjectFolder, item.EvaluatedInclude);
                if (inSameFolder)
                {
                    path = System.IO.Path.GetDirectoryName(path);
                    path = Path.Combine(path, dependentOf);
                }
                else
                {
                    path = Path.Combine(this.ProjectFolder, dependentOf);
                }
                this.ParseCanonicalName(path, out parentItemID);
                if (parentItemID != (uint)VSConstants.VSITEMID.Nil)
                    parent = this.NodeFromItemId(parentItemID);
                if (parent == null)
                {
                    var found = new List<String>();
                    var filename = Path.GetFileName(dependentOf);
                    // see if we can find it in our UrlNodes collection

                    foreach (var pair in URLNodes)
                    {
                        if (pair.Key.EndsWith(filename, StringComparison.OrdinalIgnoreCase))
                        {
                            found.Add(pair.Key);
                        }
                    }
                    if (found.Count == 1)
                    {
                        parent = URLNodes[found[0]];
                    }
                }
                //Debug.Assert(parent != null, "File dependent upon a non existing item or circular dependency. Ignoring the DependentUpon metadata");
                if (parent == null)
                {
                    VS.MessageBox.Show($"Cannot set dependency from \"{item.EvaluatedInclude}\" to \"{dependentOf}\"\r\nCannot find \"{dependentOf}\" in the project hierarchy");
                }
            }

            // If the parent node was found we add the dependent item to it otherwise we add the item ignoring the "DependentUpon" metatdata
            if (parent != null)
                newNode = this.AddDependentFileNodeToNode(item, parent);
            else
                newNode = this.AddIndependentFileNode(item);

            return newNode;
        }

        protected override HierarchyNode AddNewFileNodeToHierarchyCore(HierarchyNode parentNode, string fileName, string linkPath)
        {
            // We have to take care of Dependant Files here
            // So any .Designer.prg, or .Xaml.Prg is depending from a parent which has the same prefix name
            // then we must set that parent as parentNode;
            Utilities.ArgumentNotNull("parentNode", parentNode);
            // Check if we can find the Parent
            // to do we take the name until the first DOT (form.designer.prg belongs to form.prg, and form.voform.vnfrm belongs to form.prg)
            // but remove the path first because there may also be a dot in the path
            var originalfileName = fileName;
            var path = System.IO.Path.GetDirectoryName(fileName);
            fileName = System.IO.Path.GetFileName(fileName);
            int dotPos = fileName.IndexOf(".");
            string extension = System.IO.Path.GetExtension(originalfileName);
            if (dotPos > 0)
            {
                extension = fileName.Substring(dotPos);
            }
            var parentFile = fileName.Substring(0, fileName.Length - extension.Length);
            parentFile = Path.Combine(path, parentFile);
            var parentExtension = ".prg";
            if (dependencies.ContainsKey(extension))
            {
                parentExtension = dependencies[extension];
                HierarchyNode newParent = parentNode.FindChild(parentFile + parentExtension);
                if (newParent != null)
                {
                    parentNode = newParent;
                    // Ok, is it a XSharp node or something else ?
                    var xsharpParent = parentNode as XSharpFileNode;
                    if (xsharpParent != null)
                    {
                        xsharpParent.UpdateHasDesigner();
                    }
                }
            }
            // there are other possible parents. For Example Window1.prg is the parent of Window1.Windows.vnfrm
            // In this case the children are a VOBinary, Header or NativeResource
            switch (XFileTypeHelpers.GetFileType(fileName))
            {
                case XFileType.Header:
                case XFileType.NativeResource:
                case XFileType.VOFieldSpec:
                case XFileType.VOForm:
                case XFileType.VODBServer:
                case XFileType.VOMenu:
                case XFileType.VOOrder:
                case XFileType.VOIndex:
                    // dependent file
                    HierarchyNode newParent = parentNode.FindChild(parentFile + parentExtension);
                    if (newParent != null)
                    {
                        parentNode = newParent;
                    }
                    break;
            }
            var newNode = base.AddNewFileNodeToHierarchyCore(parentNode, originalfileName, linkPath);
            if (newNode is XSharpFileNode)
            {
                var xNode = newNode as XSharpFileNode;
                xNode.DetermineSubType();
            }
            return newNode;
        }

        protected override Microsoft.VisualStudio.Project.ProjectElement AddFileToMsBuild(string file)
        {

            string itemPath = PackageUtilities.MakeRelativeIfRooted(file, this.BaseURI);
            string itemType = XSharpFileType.GetProjectItemType(itemPath);
            return this.CreateMsBuildFileItem(itemPath, itemType);
        }

        /// <summary>
        /// Determines whether the given file is a resource file (resx file).
        /// </summary>
        /// <param name="fileName">Name of the file to be evaluated.</param>
        /// <returns>true if the file is a resx file, otherwise false.</returns>
        public override bool IsEmbeddedResource(string fileName)
        {
            if (XFileTypeHelpers.GetFileType(fileName) == XFileType.ManagedResource)
                return true;
            return false;
        }


        /// <summary>
        /// Evaluates if a file is an XSharp code file based on is extension
        /// </summary>
        /// <param name="strFileName">The filename to be evaluated</param>
        /// <returns>true if is a code file</returns>
        public override bool IsCodeFile(string strFileName)
        {
            // Don't check errors here
            var type = XFileTypeHelpers.GetFileType(strFileName);
            return type == XFileType.SourceCode;
        }

        public bool IsXamlFile(string strFileName)
        {
            // Don't check errors here
            var type = XFileTypeHelpers.GetFileType(strFileName);
            return type == XFileType.XAML;
        }
        /// <summary>
        /// Called by the project to know if the item is a file (that is part of the project)
        /// or an intermediate file used by the MSBuild tasks/targets
        /// Override this method if your project has more types or different ones
        /// </summary>
        /// <param name="type">Type name</param>
        /// <returns>True = items of this type should be included in the project</returns>
        protected override bool IsItemTypeFileType(string type)
        {
            if (String.Compare(type, ProjectFileConstants.Compile, StringComparison.OrdinalIgnoreCase) == 0          // prg
                || String.Compare(type, ProjectFileConstants.None, StringComparison.OrdinalIgnoreCase) == 0          // none
                || String.Compare(type, ProjectFileConstants.Resource, StringComparison.OrdinalIgnoreCase) == 0           // resource file
                || String.Compare(type, ProjectFileConstants.EmbeddedResource, StringComparison.OrdinalIgnoreCase) == 0  // resx
                || String.Compare(type, ProjectFileConstants.Page, StringComparison.OrdinalIgnoreCase) == 0          // xaml page/window
                || String.Compare(type, ProjectFileConstants.ApplicationDefinition, StringComparison.OrdinalIgnoreCase) == 0     // xaml application definition
                || String.Compare(type, ProjectFileConstants.NativeResource, StringComparison.OrdinalIgnoreCase) == 0           // rc file
                || String.Compare(type, ProjectFileConstants.VOBinary, StringComparison.OrdinalIgnoreCase) == 0           // vobinary file
                )
            {
                return true;
            }

            // we don't know about this type, ask the base class.
            return base.IsItemTypeFileType(type);
        }

        public override void OnAfterProjectOpen(object sender, AfterProjectFileOpenedEventArgs e)
        {
            base.OnAfterProjectOpen(sender, e);
            XSharpProjectPackage.XInstance.SetCommentTokens();

            // initialize the parser

            if (this.isLoading)
            {
                // Run the background Walker/Listener, to fill the Model
                this.isLoading = false;
                foreach (var url in this.URLNodes.Keys)
                {
                    if (!IsXSharpProjectFile(url) && this.BuildProject != null)
                    {
                        var xnode = this.URLNodes[url] as XSharpFileNode;
                        if (xnode != null && !xnode.IsNonMemberItem)
                        {
                            if (File.Exists(url))
                            {
                                //this.ProjectModel.AddFile(url);
                                base.ObserveItem(url);
                            }
                        }
                    }
                }
                //this.ProjectModel.Walk();
            }
        }


        public override void Load(string filename, string location, string name, uint flags, ref Guid iidProject, out int canceled)
        {
            // check for incomplete conditions
            this.isLoading = true; // gets reset in OnAfterProjectOpen
            base.Load(filename, location, name, flags, ref iidProject, out canceled);

            // WAP ask the designer service for the CodeDomProvider corresponding to the project node.
            this.OleServiceProvider.AddService(typeof(SVSMDCodeDomProvider), new OleServiceProvider.ServiceCreatorCallback(this.CreateServices), false);
            this.OleServiceProvider.AddService(typeof(System.CodeDom.Compiler.CodeDomProvider), new OleServiceProvider.ServiceCreatorCallback(this.CreateServices), false);
#if XSHARPLIBRARY
            // This will call the callback in PojectPackage
            IXSharpLibraryManager libraryManager = Site.GetService(typeof(IXSharpLibraryManager)) as IXSharpLibraryManager;
            // Be sure we have External/system types for Intellisense
            //UpdateAssemblyReferencesModel();
            ThreadHelper.ThrowIfNotOnUIThread();
            if (null != libraryManager)
            {
                libraryManager.RegisterHierarchy(this.InteropSafeHierarchy, this.ProjectModel, this);
            }
#endif

            CreateListManagers();


            // Add EventHandler to handle adding / removing a Reference
            VSProject.Events.ReferencesEvents.ReferenceAdded += ReferencesEvents_ReferenceAdded;
            VSProject.Events.ReferencesEvents.ReferenceRemoved += ReferencesEvents_ReferenceRemoved;
            VSProject.Events.ReferencesEvents.ReferenceChanged += ReferencesEvents_ReferenceChanged;
        }


        #endregion


        public override bool IsProjectItemType(MSBuild.ProjectItem item)
        {
            return XSharpFileType.IsProjectItemType(item);
        }

        XSharpIncludeContainerNode includeNode = null;
        protected void CreateIncludeFileFolder()
        {
            if (includeNode == null)
            {
                includeNode = new XSharpIncludeContainerNode(this);
                this.AddChild(includeNode);
            }
        }
        protected void DeleteIncludeFileFolder()
        {
            if (includeNode != null)
            {
                includeNode.Remove(false);
                includeNode = null;
            }
        }


        #region PackageReferences


        protected override void ProcessReferences()
        {
            // Nuget package references are added as child to the Reference Node.
            base.ProcessReferences();
            this.RegisterReferencesWithModel();
            this.LoadPackageReferences();
        }
        internal void RegisterReferencesWithModel()
        {
            var refContainer = GetReferenceContainer() as XSharpReferenceContainerNode;
            foreach (var child in refContainer.EnumReferences())
            {
                if (child is XSharpAssemblyReferenceNode xasm)
                {
                    ProjectModel.AddAssemblyReference(xasm.AssemblyPath);
                }
                else if (child is XSharpProjectReferenceNode projref)
                {
                    var url = projref.Url;
                    if (IsXSharpProjectFile(url))
                    {
                        this.ProjectModel.AddProjectReference(url);
                    }
                    else if (IsOtherProjectFile(url))
                    {
                        this.ProjectModel.AddStrangerProjectReference(url);
                    }
                }
            }
        }
        internal void RegisterFilesWithModel()
        {
            foreach (var item in URLNodes)
            {
                if (item.Value is FileNode file)
                {
                    this.ProjectModel.AddFile(item.Key);
                }
            }
        }


        internal void LoadPackageReferences()
        {
            var packageContainer = PackageReferenceContainerNode;
            var referenceContainerNode = GetReferenceContainer() as HierarchyNode;
            if (packageContainer == null)
            {
                packageContainer = new XSharpPackageReferenceContainerNode(this);
                referenceContainerNode.AddChild(packageContainer);
            }
            packageContainer.LoadReferencesFromBuildProject(this);
        }

        public XSharpPackageReferenceContainerNode PackageReferenceContainerNode =>
            FindChild(XSharpPackageReferenceContainerNode.PackageReferencesNodeVirtualName) as XSharpPackageReferenceContainerNode;

        public virtual XSharpPackageReferenceNode CreatePackageReferenceNode(string name)
        {
            ProjectElement item = CreateMsBuildFileItem(name, "PackageReference");
            return new XSharpPackageReferenceNode(this, item);
        }
#endregion

#region References Management Events

        private void ReferencesEvents_ReferenceRemoved(VSLangProj.Reference pReference)
        {
            if ((pReference.Type == prjReferenceType.prjReferenceTypeAssembly) ||
                (pReference.Type == prjReferenceType.prjReferenceTypeActiveX))
            {
                if (!String.IsNullOrEmpty(pReference.Path))
                    ProjectModel.RemoveAssemblyReference(pReference.Path);
            }
        }

        private void ReferencesEvents_ReferenceAdded(VSLangProj.Reference pReference)
        {
            ProjectModel.AddAssemblyReference(pReference.Path);
            //
            ProjectModel.ResolveReferences();
        }

        private void ReferencesEvents_ReferenceChanged(VSLangProj.Reference pReference)
        {
            if ((pReference.Type == prjReferenceType.prjReferenceTypeAssembly) ||
                (pReference.Type == prjReferenceType.prjReferenceTypeActiveX))
            {
                if (!String.IsNullOrEmpty(pReference.Path))
                    ProjectModel.UpdateAssemblyReference(pReference.Path);
            }
        }
#endregion


#region Private implementation

        private void CreateListManagers()
        {
            if (_errorListManager == null)
            {
                _errorListManager = ErrorListManager.RegisterProject(this);
            }
            if (_taskListManager == null)
            {
                _taskListManager = TaskListManager.RegisterProject(this);
            }
        }


        private void InitializeImageList()
        {
            imageOffset = this.ImageHandler.ImageList.Images.Count;

            foreach (Image img in ImageList.Images)
            {
                this.ImageHandler.AddImage(img);
            }
        }
        /// <summary>
        /// Factory method for reference container node
        /// </summary>
        /// <returns>ReferenceContainerNode created</returns>
        protected override ReferenceContainerNode CreateReferenceContainerNode()
        {
            return new XSharpReferenceContainerNode(this);
        }

        private object CreateServices(Type serviceType)
        {
            object service = null;
            if (typeof(VSLangProj.VSProject) == serviceType)
            {
                service = this.VSProject;
            }
            else if (typeof(EnvDTE.Project) == serviceType)
            {
                service = this.GetAutomationObject();
            }
            //else if (typeof(SVSMDCodeDomProvider) == serviceType)
            //{
            //    service = this.CodeDomProvider;
            //}
            //else if (typeof(System.CodeDom.Compiler.CodeDomProvider) == serviceType)
            //{
            //    service = this.CodeDomProvider.CodeDomProvider;
            //}
            else if (typeof(IVsSingleFileGeneratorFactory) == serviceType)
            {
                service = new SingleFileGeneratorFactory(this.ProjectGuid, this.Site);
            }


            return service;
        }



        protected override Microsoft.VisualStudio.Project.BuildResult InvokeMsBuild(string target)
        {
            if (String.Equals(target, "Clean", StringComparison.OrdinalIgnoreCase))
            {
                if (buildLogger != null)
                {
                    buildLogger.Clear();

                }
            }
            Logger.Debug($"InvokeMsBuild Target: {target}, Project: {Url}");
            return base.InvokeMsBuild(target);
        }


        protected override NodeProperties CreatePropertiesObject()
        {
            return new XSharpProjectNodeProperties(this);
        }
#endregion


        public XSharpModel.XProject ProjectModel
        {
            get
            {
                // Already Initialized ?
                if (projectModel == null)
                {
                    // Already in the Solution ?
                    projectModel = XSharpModel.XSolution.FindProject(this.Url);
                }
                // Neither ? Ok, Create
                if (projectModel == null)
                {
                    projectModel = new XProject(this);
                    projectModel.FileWalkComplete += OnFileWalkComplete;
                    ProjectModel.ProjectWalkComplete += OnProjectWalkComplete;
                }
                return projectModel;
            }

            private set
            {
                projectModel = value;
            }
        }
        internal void ReloadProjectModel()
        {
            projectModel = null;
            this.RegisterReferencesWithModel();
            this.RegisterFilesWithModel();
            this.RefreshIncludeFiles();
            ModelWalker.AddProject(this.ProjectModel);
        }

        private void OnProjectWalkComplete(XProject xProject)
        {
            var tasks = this.ProjectModel.GetCommentTasks();
#if DEV17
            var list = new List<TaskItem>();
#else
            var list = new List<Task>();
#endif
            if (_taskListManager != null)
            {
                _taskListManager.Clear();
                foreach (var task in tasks)
                {
                    _taskListManager.AddItem(task, this.ProjectIDGuid);
                }
                _taskListManager.Refresh();
            }
            RefreshIncludeFiles();

        }

        private void RefreshIncludeFiles()
        {
            if (XSettings.HideIncludes)
            {
                DeleteIncludeFileFolder();
                return;
            }
            var oldEvents = this.EventTriggeringFlag;
            bool hasChanged = false;
            try
            {

                var currentChildren = new Dictionary<string, HierarchyNode>(StringComparer.OrdinalIgnoreCase);
                if (includeNode != null)
                {
                    var child = includeNode.FirstChild;
                    while (child != null)
                    {
                        currentChildren.Add(child.Url, child);
                        child = child.NextSibling;
                    }
                }
                var newIncludes = ProjectModel.IncludeFiles;
                CreateIncludeFileFolder();
                if (currentChildren.Count == 0)
                {
                    this.EventTriggeringFlag = EventTriggering.DoNotTriggerHierarchyEvents;
                }
                if (newIncludes.Count > 0)
                {
                    foreach (var fileName in newIncludes)
                    {
                        if (currentChildren.ContainsKey(fileName))
                        {
                            currentChildren.Remove(fileName);
                        }
                        else
                        {
                            var newChild = new XSharpIncludeFileNode(this, fileName);
                            includeNode.AddChild(newChild);
                            hasChanged = true;
                        }
                    }
                }
                // delete includes that are no longer needed
                if (currentChildren.Count > 0 && includeNode != null)
                {
                    foreach (var file in currentChildren)
                    {
                        var node = file.Value;
                        node.OnItemDeleted();
                        hasChanged = true;
                        includeNode.RemoveChild(node);
                    }
                }
            }
            finally
            {
                this.EventTriggeringFlag = oldEvents;
                ThreadUtilities.runSafe(() =>
                {
                    if (hasChanged)
                    {
                        this.OnItemsAppended(includeNode);
                    }

                });
            }
        }
        private void OnFileWalkComplete(XFile xfile)
        {
            var tasks = this.ProjectModel.GetCommentTasks();
#if DEV17
            var list = new List<TaskListItem>();
#else
            var list = new List<Task>();
#endif
            _taskListManager.Clear();
            foreach (var task in tasks)
            {
                _taskListManager.AddItem(task, this.ProjectIDGuid);
            }
            _taskListManager.Refresh();
            RefreshIncludeFiles();
        }

        public override void AddURL(String url, HierarchyNode node)
        {
            //
            base.AddURL(url, node);
            // We can arrive from
            // XSharpFileNode
            // XSharpFolderNode
            // XSharpProjectReference
            // So, we will add files only (currently) => Don't forget RemoveURL
            if (IsXSharpProjectFile(url))
            {
                this.ProjectModel.AddProjectReference(url);
            }
            else if (IsOtherProjectFile(url))
            {
                this.ProjectModel.AddStrangerProjectReference(url);
            }
            else
            {
                var xnode = node as XSharpFileNode;
                if (xnode != null && !xnode.IsNonMemberItem)
                {
                    if (File.Exists(url))
                    {
                        this.ProjectModel.AddFile(url);
                        if (IsXamlFile(url))
                        {
                            base.ObserveItem(url);
                        }
                    }
                }
            }
        }


        /// <summary>
        /// Used in XProject to enumerate all projects in the current Solution, looking for a project using its name.
        /// </summary>
        /// <param name="sProject"></param>
        /// <returns>The EnvDTE.Project found, or null</returns>
        ///

        private static IList<EnvDTE.Project> GetSolutionFolderProjects(EnvDTE.Project solutionFolder)
        {
            return ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                List<EnvDTE.Project> list = new List<EnvDTE.Project>();
                for (var i = 1; i <= solutionFolder.ProjectItems.Count; i++)
                {
                    var subProject = solutionFolder.ProjectItems.Item(i).SubProject;
                    if (subProject == null)
                    {
                        continue;
                    }

                    // If this is another solution folder, do a recursive call, otherwise add

                    if (subProject.Kind.ToUpper() == EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder.ToUpper())
                    {
                        list.AddRange(GetSolutionFolderProjects(subProject));
                    }
                    else
                    {
                        list.Add(subProject);
                    }
                }
                return list;
            });
        }

        private List<EnvDTE.Project> GetSolutionProjects()
        {
            List<EnvDTE.Project> list = new List<EnvDTE.Project>();
            EnvDTE.DTE dte = null;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                var tmp = this.Site.GetService(typeof(EnvDTE.DTE));
                if (tmp != null)
                {
                    dte = (EnvDTE.DTE)tmp;

                    foreach (EnvDTE.Project p in dte.Solution.Projects)
                    {
                        if (p == null || p.Properties == null) // unloaded ?
                        {
                            continue;
                        }
                        if (p.Kind.ToUpper() == EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder.ToUpper())
                        {
                            list.AddRange(GetSolutionFolderProjects(p));
                        }
                        else
                        {
                            list.Add(p);
                        }
                    }
                }
            });
            return list;
        }

        public Object FindProject(string sProject)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            foreach (var p in GetSolutionProjects())
            {
                string name = "";
                try
                {
                    name = p.FullName;
                }
                catch (Exception e)
                {
                    Debug.WriteLine(e.Message);
                }
                if (name.Equals(sProject, StringComparison.InvariantCultureIgnoreCase))
                {
                    return p;
                }
            }
            return null;
        }

        public override void RemoveURL(String url)
        {
            if (!_closing)
            {
                //
                // We should remove the external projects entries
                if (IsXSharpProjectFile(url))
                {
                    this.ProjectModel.RemoveProjectReference(url);
                }
                else if (IsOtherProjectFile(url))
                {
                    this.ProjectModel.RemoveStrangerProjectReference(url);
                }
                else
                {
                    var node = this.FindChild(url) as XSharpFileNode;
                    if (node != null && !node.IsNonMemberItem)
                    {
                        if (IsXamlFile(url) || node.IsDependent)
                        {
                            base.StopObservingItem(url);
                        }
                        this.ProjectModel.RemoveFile(url);
                    }
                }
            }
            base.RemoveURL(url);
        }

        public override int ReopenItem(uint itemId, ref Guid editorType, string physicalView, ref Guid logicalView, IntPtr docDataExisting, out IVsWindowFrame frame)
        {
            frame = null;
            // suppress opening winforms in design mode when the file walk is not ready
            // because the type lookup will not work then
            if (physicalView == "Design" && !projectModel.FileWalkCompleted)
                return VSConstants.E_FAIL;
            ThreadHelper.ThrowIfNotOnUIThread();
            return base.ReopenItem(itemId, ref editorType, physicalView, ref logicalView, docDataExisting, out frame);
        }


        /// <summary>
        /// Check if fullpath points to a XSharp Project file.
        /// </summary>
        /// <param name="fullPath"></param>
        /// <returns></returns>
        private bool IsXSharpProjectFile(string fullPath)
        {
            return fullPath.EndsWith(".xsproj", StringComparison.OrdinalIgnoreCase)
                || fullPath.EndsWith(".xsprj", StringComparison.OrdinalIgnoreCase);
        }

        /// <summary>
        /// Check if fullpath points to a file, whose extension ends with "proj" so it might be project file.
        /// </summary>
        /// <param name="fullPath"></param>
        /// <returns></returns>
        private bool IsOtherProjectFile(string fullPath)
        {
            if (fullPath.EndsWith("proj", StringComparison.OrdinalIgnoreCase))
            {
                if (File.Exists(fullPath))
                {
                    var source = File.ReadAllText(fullPath);
                    return source.IndexOf("<Project", StringComparison.OrdinalIgnoreCase) != -1;
                }
            }
            return false;
        }
        internal void BuildStarted()
        {

        }

        internal void BuildEnded(bool didCompile)
        {
            if (didCompile)
            {
                RefreshReferencesFromResponseFile();
            }
        }



#region IXSharpProject Interface

        bool _enforceSelf = false;
        public bool EnforceSelf
        {
            get => _enforceSelf;
            set => _enforceSelf = value;
        }
        public void RunInForeGroundThread(Action a)
        {

            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                a();
            });
        }

        public string SynchronizeKeywordCase(string code, string fileName)
        {
            if (XEditorSettings.KeywordCase == KeywordCase.None)
                return code;
            // we also normalize the line endings
            code = code.Replace("\n", "");
            code = code.Replace("\r", "\r\n");

            var file = XSolution.FindFullPath(fileName);
            var sb = new StringBuilder();
            XSharpParseOptions parseoptions;
            if (file != null)
            {
                parseoptions = file.Project.ParseOptions;
            }
            else
                parseoptions = XSharpParseOptions.Default;
            ITokenStream tokenStream;
            var reporter = new XSharp.CodeDom.ErrorIgnorer();
            bool ok = XSharp.Parser.VsParser.Lex(code, fileName, parseoptions, reporter, out tokenStream, out _);
            var stream = tokenStream as BufferedTokenStream;
            var tokens = stream.GetTokens();
            foreach (var token in tokens)
            {
                if (XSharpLexer.IsKeyword(token.Type))
                {
                    sb.Append(XLiterals.FormatKeyword(token.Text));
                }
                else
                {
                    sb.Append(token.Text);
                }
            }
            return sb.ToString();
        }






        public string IntermediateOutputPath
        {
            get
            {
                if (this.BuildProject != null)
                {
                    var result = this.BuildProject.GetPropertyValue("IntermediateOutputPath");
                    if (!Path.IsPathRooted(result))
                        result = Path.Combine(this.ProjectFolder, result);
                    return result;
                }
                return "";
            }
        }




        string _prefix = null;

        public bool PrefixClassesWithDefaultNamespace
        {
            get
            {
                if (_prefix == null)
                {
                    lock (this)
                    {
                        if (_prefix == null)
                            _prefix = GetProjectProperty("NS");
                        if (_prefix == null)
                            _prefix = "false";
                    }

                }
                return _prefix.ToLower() == "true";
            }
            internal set
            {
                _prefix = value.ToString().ToLower();
            }

        }


#endregion


        protected override void Reload()
        {
            this.isLoading = true; // gets reset in OnAfterProjectOpen
            Logger.Information("Reload");
            base.Reload();
            CreateListManagers();
            this.CreateIncludeFileFolder();
            if (ResetDependencies())
            {
                this.BuildProject.Save();
            }
            RefreshIncludeFiles();
        }

        private void RefreshReferencesFromResponseFile()
        {
            // find the resource file and read the lines with /reference
            string tempPath = System.IO.Path.GetTempPath();
            string file = Path.Combine(tempPath, "LastXSharpResponseFile.Rsp");
            if (File.Exists(file))
            {
                var response = File.ReadAllText(file);
                response = response.Replace("\r", "");
                response = response.Replace("\n", "");
                var lines = response.Split(new char[] { '/' }, StringSplitOptions.RemoveEmptyEntries);
                var references = new List<string>();
                foreach (var line in lines)
                {
                    if (line.StartsWith("reference:"))
                    {
                        var reffile = line.Substring(10).Trim();
                        if (reffile[0] == '"')
                            reffile = reffile.Substring(1, reffile.Length - 2);
                        references.Add(reffile);

                    }
                }
                ProjectModel.RefreshReferences(references);
            }
        }

        internal void UpdateReferencesInProjectModel()
        {
            // find all the assembly references

            var target = "FindReferenceAssembliesForReferences";
            var buildResult = this.Build(target);
            if (buildResult.IsSuccessful)
            {
                var items = buildResult.ProjectInstance.GetItems("ReferencePath");
                var references = new List<string>();
                foreach (var item in items)
                {
                    references.Add(item.EvaluatedInclude);
                }
                ProjectModel.RefreshReferences(references);
            }
        }


        public override int Save(string fileToBeSaved, int remember, uint formatIndex)
        {
            this.UpdateProjectVersion();
            return base.Save(fileToBeSaved, remember, formatIndex);
        }

        protected virtual internal bool ResetDependencies()
        {
            bool bMoved = false;
            List<Tuple<XSharpFileNode, String, String>> FilesToMove = new List<Tuple<XSharpFileNode, String, String>>();
            foreach (KeyValuePair<string, HierarchyNode> pair in URLNodes)
            {
                XSharpFileNode vnode = pair.Value as XSharpFileNode;
                if (vnode != null)
                {
                    string parent = vnode.GetParentName();
                    if (!string.IsNullOrEmpty(parent))
                    {
                        if (!(vnode.Parent is XSharpFileNode))
                        {
                            FilesToMove.Add(new Tuple<XSharpFileNode, string, string>(vnode, parent, vnode.SubType));
                        }
                    }
                }

            }
            foreach (var element in FilesToMove)
            {

                var NodeToMove = element.Item1;
                var parentName = element.Item2;
                var subType = element.Item3;
                var parentNode = FindURL(parentName);
                if (parentNode != null && parentNode is XSharpFileNode parent)
                {
                    parent.AddDependant(NodeToMove);
                    NodeToMove.SubType = subType;
                    bMoved = true;
                }
            }
            return bMoved;
        }

        protected override bool AddFilesFromProjectReferences(HierarchyNode targetNode, string[] projectReferences, uint dropEffect)
        {
            bool bOk = base.AddFilesFromProjectReferences(targetNode, projectReferences, dropEffect);
            if (bOk)
            {
                ResetDependencies();
            }
            return bOk;
        }
#region IProjectTypeHelper
        public IXTypeSymbol ResolveExternalType(string name, IList<string> usings)
        {
            switch (name.ToLower())
            {
                case "object":
                case "system.object":
                    name = KnownTypes.SystemObject;
                    break;
                case "void":
                case "system.void":
                    name = KnownTypes.SystemVoid;
                    break;
                case "boolean":
                case "system.boolean":
                    name = KnownTypes.SystemBoolean;
                    break;
                case "string":
                case "system.string":
                    name = KnownTypes.SystemString;
                    break;
            }
            var model = this.ProjectModel;
            var myusings = new List<string>();
            myusings.AddRange(usings);
            myusings.AddUnique("System");
            return model.FindSystemType(name, myusings);
        }

        public XSourceTypeSymbol ResolveXType(string name, IList<string> usings)
        {
            var model = this.ProjectModel;
            XSourceTypeSymbol result = model.Lookup(name, usings);
            if (result != null)
                return result;
            return result;
        }


        public XSourceTypeSymbol ResolveReferencedType(string name, IList<string> usings)
        {
            // identical but may be used by others
            return ResolveXType(name, usings);
        }


#endregion
#region IVsSingleFileGeneratorFactory
        IVsSingleFileGeneratorFactory factory = null;

        // Note that in stead of using the SingleFileGeneratorFactory we can also do everything here based on
        // the info we know about common SFGs.
        // See for more info ReadMeCodeGen.TXT in the root of the project
        // We could create our own subclass of SingleFileGeneratorFactory and implement GetGeneratorInformation with a fixed table of
        // known generators.
        private bool createIVsSingleFileGeneratorFactory()
        {
            if (factory == null)
            {
                factory = new SingleFileGeneratorFactory(this.ProjectGuid, this.Site);
            }
            return factory != null;
        }
        public int GetDefaultGenerator(string wszFilename, out string pbstrGenProgID)
        {
            string temp = String.Empty;
            int result = VSConstants.S_FALSE;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                if (createIVsSingleFileGeneratorFactory())
                    result = factory.GetDefaultGenerator(wszFilename, out temp);
            });
            pbstrGenProgID = temp;
            return result;
        }

        public int CreateGeneratorInstance(string wszProgId, out int pbGeneratesDesignTimeSource, out int pbGeneratesSharedDesignTimeSource, out int pbUseTempPEFlag, out IVsSingleFileGenerator ppGenerate)
        {
            pbGeneratesDesignTimeSource = 0;
            pbGeneratesSharedDesignTimeSource = 0;
            pbUseTempPEFlag = 0;
            ppGenerate = null;
            ThreadHelper.ThrowIfNotOnUIThread();
            if (createIVsSingleFileGeneratorFactory())
                return factory.CreateGeneratorInstance(wszProgId, out pbGeneratesDesignTimeSource, out pbGeneratesSharedDesignTimeSource, out pbUseTempPEFlag, out ppGenerate);
            return VSConstants.S_FALSE;
        }

        public int GetGeneratorInformation(string wszProgId, out int pbGeneratesDesignTimeSource, out int pbGeneratesSharedDesignTimeSource, out int pbUseTempPEFlag, out Guid pguidGenerator)
        {
            pbGeneratesDesignTimeSource = 0;
            pbGeneratesSharedDesignTimeSource = 0;
            pbUseTempPEFlag = 0;
            pguidGenerator = Guid.Empty;
            ThreadHelper.ThrowIfNotOnUIThread();
            if (createIVsSingleFileGeneratorFactory())
                return factory.GetGeneratorInformation(wszProgId, out pbGeneratesDesignTimeSource, out pbGeneratesSharedDesignTimeSource, out pbUseTempPEFlag, out pguidGenerator);
            return VSConstants.S_FALSE;

        }
#endregion

#region IVsDesignTimeAssemblyResolution

        //private DesignTimeAssemblyResolution designTimeAssemblyResolution;
        private ConfigCanonicalName _config = new ConfigCanonicalName("Debug", "AnyCPU");

        public override void SetConfiguration(ConfigCanonicalName config)
        {
            _config = config;
            bool invalidate = false;
            if (this.options is XSharpProjectOptions xopts)
            {
                if (xopts.ConfigCanonicalName != config)
                    invalidate = true;
            }
            base.SetConfiguration(config);
            if (invalidate)
            {
                InvalidateOptions();
            }
            //if (this.designTimeAssemblyResolution == null)
            //{
            //    this.designTimeAssemblyResolution = new DesignTimeAssemblyResolution();
            //}
            //this.designTimeAssemblyResolution.Initialize(this);

        }
        public int GetTargetFramework(out string ppTargetFramework)
        {
            ppTargetFramework = this.TargetFrameworkMoniker.FullName;
            return VSConstants.S_OK;
        }

        //public int ResolveAssemblyPathInTargetFx(string[] prgAssemblySpecs, uint cAssembliesToResolve, VsResolvedAssemblyPath[] prgResolvedAssemblyPaths, out uint pcResolvedAssemblyPaths)
        //{
        //    if (prgAssemblySpecs == null || cAssembliesToResolve == 0 || prgResolvedAssemblyPaths == null)
        //    {
        //        throw new ArgumentException("One or more of the arguments are invalid.");
        //    }

        //    pcResolvedAssemblyPaths = 0;

        //    try
        //    {
        //        var results = designTimeAssemblyResolution.Resolve(prgAssemblySpecs.Take((int)cAssembliesToResolve));
        //        results.CopyTo(prgResolvedAssemblyPaths, 0);
        //        pcResolvedAssemblyPaths = (uint)results.Length;
        //    }
        //    catch (Exception ex)
        //    {
        //        return Marshal.GetHRForException(ex);
        //    }

        //    return VSConstants.S_OK;
        //}

#endregion
#region TableManager
        //internal ITableManagerProvider tableManagerProvider { get; private set; }
        ErrorListManager _errorListManager = null;
        TaskListManager _taskListManager = null;


        XSharpIDEBuildLogger buildLogger = null;

        protected override IDEBuildLogger CreateBuildLogger(IVsOutputWindowPane output, TaskProvider taskProvider, IVsHierarchy hierarchy)
        {
            buildLogger = new XSharpIDEBuildLogger(output, this.TaskProvider, hierarchy);
            buildLogger.ProjectNode = this;
            buildLogger.ErrorString = ErrorString;
            buildLogger.WarningString = WarningString;
            CreateListManagers();
            buildLogger.ErrorListManager = _errorListManager;
            return buildLogger;
        }

        public override BuildResult Build(string target)
        {
            return Build(_config, target);
        }

        protected override void SetBuildConfigurationProperties(ConfigCanonicalName config)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            Logger.Debug("SetBuildConfigurationProperties " + config.ToString());
            var xoptions = this.options as XSharpProjectOptions;
            if (xoptions != null && xoptions.ConfigCanonicalName == config)
                return;
            base.SetBuildConfigurationProperties(config);
        }
        public void ClearIntellisenseErrors(string fileName)
        {
            _errorListManager.DeleteIntellisenseErrorsFromFile(fileName);
        }
        public void AddIntellisenseError(string file, int line, int column, int length, string errCode, string message, DiagnosticSeverity sev)
        {
            _errorListManager.AddIntellisenseError(file, line, column, length, errCode, message, sev.ToMessageSeverity());
        }

        public List<IXErrorPosition> GetIntellisenseErrorPos(string fileName)
        {
            return _errorListManager.GetIntellisenseErrorPos(fileName);
        }


        public void ShowIntellisenseErrors()
        {
            _errorListManager.Refresh();
            return;
        }

#endregion


        public void AddFileNode(string strFileName)
        {
            var node = this.FindChild(strFileName);
            if (node == null)
            {
                var element = this.AddFileToMsBuild(strFileName);
                var newNode = new XSharpFileNode(this, element);
                string parent = newNode.GetParentName();
                if (parent != null)
                {
                    var parentNode = this.FindChild(parent);
                    parentNode.AddChild(newNode);
                }
                else
                    this.AddChild(newNode);

            }
        }
        public void DeleteFileNode(string strFileName)
        {
            var node = this.FindChild(strFileName);
            if (node != null)
            {
                this.RemoveChild(node);
            }
        }
        public bool HasFileNode(string strFileName)
        {
            return this.FindChild(strFileName) != null;
        }

        protected override void Dispose(bool disposing)
        {
            projectModel = null;
            base.Dispose(disposing);
            lock (nodes)
            {
                nodes.Remove(this);
            }
        }
        private bool _dialectIsCached = false;
        private VsParser.XSharpDialect _dialect;

        public VsParser.XSharpDialect Dialect
        {
            get
            {
                if (_dialectIsCached)
                    return _dialect;

                var prop = GetProjectProperty(XSharpProjectFileConstants.Dialect);
                if (!Enum.TryParse(prop, true, out _dialect))
                {
                    _dialect = VsParser.XSharpDialect.Core;
                }
                _dialectIsCached = true;
                return _dialect;
            }

        }

        /// <summary>
        /// Clear cached Config and ParseOptions
        /// </summary>
        internal void ClearOptions()
        {
            this.CachedConfig = null;
            this.CachedOptions = null;

        }
        internal void GetParseOptions()
        {
            this.ClearOptions();
            this.CachedConfig = this.CurrentConfig;
            this.CachedOptions = this.ParseOptions;

        }

        internal ProjectConfig CachedConfig;
        public override ProjectConfig CurrentConfig
        {
            get
            {
                if (CachedConfig == null)
                {
                    CachedConfig = base.CurrentConfig;
                }
                return CachedConfig;
            }
        }

        public void CreateParseOptions()
        {
            var xoptions = CreateProjectOptions() as XSharpProjectOptions;
            if (xoptions != null)
            {
                xoptions.BuildCommandLine();
            }
        }
         internal XSharpParseOptions CachedOptions;
        public XSharpParseOptions ParseOptions
        {
            get
            {
                if (CachedOptions != null)
                    return CachedOptions;
                if (this.IsClosed || XSolution.IsClosing)
                    return XSharpParseOptions.Default;
                try
                {
                    var config = this.CurrentConfig;
                    if (config != null)
                    {
                        var xoptions = GetProjectOptions(config.ConfigCanonicalName) as XSharpProjectOptions;
                        if (xoptions != null)
                        {
                            if (xoptions.ParseOptions == null)
                                xoptions.BuildCommandLine();
                            CachedOptions = xoptions.ParseOptions;
                            return CachedOptions;
                        }
                    }
                }
                catch (Exception e)
                {
                    Logger.Exception(e, "Retrieving Parse Options");
                }
                return XSharpParseOptions.Default;
            }
        }
        bool _closing = false;
        public override int Close()
        {

            // First remove the Navigation Data
            //
            ThreadHelper.ThrowIfNotOnUIThread();
            Logger.Debug("Close " + this.ProjectFile);
            if (this.Site != null)
            {
#if XSHARPLIBRARY
                IXSharpLibraryManager libraryManager = (IXSharpLibraryManager)Site.GetService(typeof(IXSharpLibraryManager));
                if (libraryManager != null)
                {
                    libraryManager.UnregisterHierarchy(this.InteropSafeHierarchy);
                }
#endif
            }
            // CleanUp the CodeModel
            if (projectModel != null)
            {
                projectModel.Close();
            }
            _closing = true;
            var res = base.Close();

            if (buildLogger != null)
            {
                buildLogger.Clear();
            }
            ErrorListManager.RemoveProject(this);
            if (_errorListManager != null)
            {
                _errorListManager.Refresh();
                _errorListManager = null;
            }
            TaskListManager.RemoveProject(this);
            if (_taskListManager != null)
            {
                _taskListManager.Clear();
                _taskListManager.Refresh();
                _taskListManager = null;
            }
            URLNodes.Clear();
            return res;
        }

        protected override void RenameProjectFile(string newFile)
        {

            base.RenameProjectFile(newFile);
            var model = this.ProjectModel;
            if (model != null)
            {
                model.Rename(newFile);
            }

        }

        const string config = "$(Configuration)";
        const string configPlatform = "$(Configuration)|$(Platform)";
        const string conditionDebug = "'$(Configuration)|$(Platform)' == 'Debug|AnyCPU'";
        const string conditionRelease = "'$(Configuration)|$(Platform)' == 'Release|AnyCPU'";
        const string XSharpMsBuildDir = @"$(XSharpMsBuildDir)";
        const string importDefaultPropsFull = XSharpMsBuildDir + @"\XSharp.Default.props";
        const string importDefaultPropsShort = @"XSharp.Default.props";
        const string importPropsShort = @"XSharp.props";
        const string importTargetsShort = @"XSharp.targets";
        const string importTargetsFull = XSharpMsBuildDir + @"\XSharp.targets";
        const string PostBuildEvent = nameof(PostBuildEvent);
        const string PreBuildEvent = nameof(PreBuildEvent);
        const string RunPostBuildEvent = nameof(RunPostBuildEvent);
        const string XSharpProjectVersion = nameof(XSharpProjectVersion);
        const string IncludePaths = nameof(IncludePaths);
        const string Nostandarddefs = nameof(Nostandarddefs);
        const string DocumentationFile = nameof(DocumentationFile);
        const string XSharpProjectExtensionsPath = nameof(XSharpProjectExtensionsPath);
        public override int UpgradeProject(uint grfUpgradeFlags)
        {
            bool silent;
            int result;
            bool dialectVO = false;
            bool ok = true;
            ThreadHelper.ThrowIfNotOnUIThread();
            silent = (__VSUPGRADEPROJFLAGS)grfUpgradeFlags == __VSUPGRADEPROJFLAGS.UPF_SILENTMIGRATE;
            if (ChangedProjectFiles.ContainsKey(this.Url))
            {
                var original = ChangedProjectFiles[this.Url];
                ChangedProjectFiles.Remove(this.Url);
                var changedSource = File.ReadAllText(this.Url);
                if (File.Exists(original))
                {
                    Utilities.DeleteFileSafe(this.Url);
                    File.Move(original, this.Url);
                }
                if (!this.QueryEditProjectFile(true))
                {
                    return VSConstants.S_FALSE;
                }
                Utilities.DeleteFileSafe(this.Url);
                File.WriteAllText(Url, changedSource);
                ok = false;
            }
#if USEPROJECTVERSION
            //we have added a projectversion property to makes checks easier in the future
            if (ok)
            {
                var vers = this.BuildProject.Properties.Where(p => string.Compare(p.Name, ProjectVersion, StringComparison.OrdinalIgnoreCase) == 0).FirstOrDefault();
                if (vers != null)
                {
                    ok = vers.UnevaluatedValue == Constants.FileVersion;
                    var version = vers.UnevaluatedValue.Split('.');
                    if (version.Length == 4)
                    {
                        if (Int32.TryParse(version[0], out var ivers1))
                        {
                            ok = ivers1 >= 2;
                        }
                        if (ok && ivers1 == 2 && Int32.TryParse(version[1], out var ivers2))
                        {
                            ok = (ivers2 >= 6);
                        }
                        if (ok)
                        {
                            return VSConstants.S_OK;
                        }
                    }
                }
            }
#else

#endif
            StringWriter backup = new StringWriter();
            BuildProject.Save(backup);
            var str = backup.ToString();
            // do not touch SDK style projects !
            if (str.IndexOf("<Project Sdk=", StringComparison.OrdinalIgnoreCase) > 0)
            {
                return VSConstants.S_OK;
            }
            var str2 = str.ReplaceEx( "anycpu", "AnyCPU", StringComparison.OrdinalIgnoreCase);
            if (str2 != str)
            {
                ok = false;
                str = str2;
            }
            // check to see required elements
            int len = str.Length;
            // /prebuildevent appears more than once.
            //if (str.ToLower().Replace("/prebuildevent", "").Length < str.Length - "/prebuildevent".Length)
            //    ok = false;
            if (ok && str.IndexOf("'=='", StringComparison.OrdinalIgnoreCase) >= 0)
            {
                ok = false;
            }
            if (ok && str.IndexOf("'Debug|AnyCPU'", StringComparison.OrdinalIgnoreCase) == -1)
            {
                ok = false;
            }
            if (ok && str.IndexOf("'Release|AnyCPU'", StringComparison.OrdinalIgnoreCase) == -1)
            {
                ok = false;
            }
            if (ok && str.IndexOf("<DocumentationFile>false", StringComparison.OrdinalIgnoreCase) != -1)
            {
                ok = false;
            }
            if (ok && str.IndexOf("<DocumentationFile>true", StringComparison.OrdinalIgnoreCase) != -1)
            {
                ok = false;
            }
            if (ok && str.IndexOf(importDefaultPropsFull, StringComparison.OrdinalIgnoreCase) == -1)
            {
                ok = false;
            }
            // XSharp.Props should no longer be there
            if (ok && str.IndexOf(importPropsShort, StringComparison.OrdinalIgnoreCase) >= 0)
            {
                ok = false;
            }

            if (ok)
            {
                int iTargets = str.IndexOf(importTargetsShort, StringComparison.OrdinalIgnoreCase);
                if (iTargets == -1)
                {
                    ok = false;
                }
                else
                {
                    // prebuild and postbuild must be after Targets
                    int iPrebuild = str.IndexOf(PreBuildEvent, StringComparison.OrdinalIgnoreCase);
                    int iPostbuild = str.IndexOf(PostBuildEvent, StringComparison.OrdinalIgnoreCase);
                    int iRunPostBuild = str.IndexOf(RunPostBuildEvent, StringComparison.OrdinalIgnoreCase);
                    if (iPrebuild > -1 && iPrebuild < iTargets)
                        ok = false;
                    else if (iPostbuild > -1 && iPrebuild < iTargets)
                        ok = false;
                    else if (iRunPostBuild > -1 && iPrebuild < iTargets)
                        ok = false;
                }
            }
            if (str.IndexOf("XSharp.VO", StringComparison.OrdinalIgnoreCase) >= 0 && str.IndexOf("XSharp.RT", StringComparison.OrdinalIgnoreCase) == -1)
            {
                if (str.IndexOf("<Dialect>VO</Dialect>", StringComparison.OrdinalIgnoreCase) >= 0 ||
                    str.IndexOf("<Dialect>Vulcan</Dialect>", StringComparison.OrdinalIgnoreCase) >= 0)
                {
                    ok = false;
                    dialectVO = true;
                }
            }
            if (!ok)
            {
                if (!this.QueryEditProjectFile(true))
                {
                    result = VSConstants.S_FALSE;
                }
                else
                {
                    FixProjectFile(BuildProject.FullPath, dialectVO);
                    base.UpgradeProject(grfUpgradeFlags);
                    result = VSConstants.S_OK;
                }
            }
            else
            {
                result = base.UpgradeProject(grfUpgradeFlags);
            }
            return result;
        }

        private bool updateProperty(Microsoft.Build.Construction.ProjectPropertyElement prop)
        {
            try
            {
                if (string.Equals(prop.Name, DocumentationFile, StringComparison.OrdinalIgnoreCase))
                {
                    string sValue = prop.Value;
                    if (string.Equals(sValue, "true", StringComparison.OrdinalIgnoreCase))
                    {
                        var prop2 = this.BuildProject.Properties.Where(p => p.Name.ToLower() == "assemblyname").FirstOrDefault();
                        if (!String.IsNullOrEmpty(prop2?.UnevaluatedValue))
                            prop.Value = System.IO.Path.ChangeExtension(prop2.UnevaluatedValue, ".Xml");
                        else
                            prop.Value = "";
                        return true;
                    }
                    else if (string.Equals(sValue, "false", StringComparison.OrdinalIgnoreCase))
                    {
                        prop.Value = "";
                        return true;
                    }
                }
            }
            catch (Exception e)
            {
                if (System.Diagnostics.Debugger.IsAttached)
                    System.Diagnostics.Debug.WriteLine("Error updating documentationfle: " + e.Message);
            }
            return false;
        }

        private void UpdateProjectVersion()
        {
#if USEPROJECTVERSION
            var xml = BuildProject.Xml;
            var groups = xml.PropertyGroups.ToList();
            var grp = groups.Where(g => g.Condition.Trim().Length == 0).FirstOrDefault();
            if (grp != null)
            {
                addProperty(grp, ProjectVersion, Constants.FileVersion);
            }
#else
            this.RemoveProjectProperty(XSharpProjectVersion);
            this.RemoveProjectProperty(ProjectFileConstants.TargetPath);
#endif
        }

        private void FixProjectFile(string filename, bool dialectVO)
        {
            bool changed = false;
            var xml = BuildProject.Xml;
            var groups = xml.PropertyGroups.ToList();
            var groupDict = new Dictionary<string, MBC.ProjectPropertyGroupElement>();
            var debugInclude = "";
            var debugNoStdDefs = "";
            foreach (var group in groups.Where(g => g.Condition.Trim().Length > 0))
            {
                string condition = group.Condition;
                if (condition.IndexOf(config, StringComparison.OrdinalIgnoreCase) > -1 &&
                    condition.IndexOf(configPlatform, StringComparison.OrdinalIgnoreCase) == -1)
                {
                    if (condition.IndexOf("'Debug", StringComparison.OrdinalIgnoreCase) > 0)
                        condition = conditionDebug;
                    else
                        condition = conditionRelease;
                    changed = true;
                }
                if (condition.Contains("'=='"))
                {
                    condition = condition.Replace("'=='", "' == '");
                    changed = true;

                }
                var newcondition = System.Text.RegularExpressions.Regex.Replace(condition, "anycpu", "AnyCPU", System.Text.RegularExpressions.RegexOptions.IgnoreCase).Trim();
                if (condition != newcondition)
                {
                    changed = true;
                    group.Condition = newcondition;
                }
                if (!groupDict.ContainsKey(group.Condition))
                {
                    groupDict.Add(group.Condition, group);
                }
                foreach (var prop in group.Properties.ToArray())
                {
                    switch (prop.Name.ToLower())
                    {
                        case "includepaths":
                            if (string.IsNullOrEmpty(debugInclude))
                                debugInclude = prop.Value;
                            group.RemoveChild(prop);
                            changed = true;
                            break;
                        case "nostandarddefs":
                            if (string.IsNullOrEmpty(debugNoStdDefs))
                                debugNoStdDefs = prop.Value;
                            group.RemoveChild(prop);
                            changed = true;
                            break;
                    }
                }
            }
            // remove the first of each condition combination from the list
            foreach (var group in groupDict.Values)
            {
                groups.Remove(group);
            }
            // now the collection only has groups that are not the "first" of their kind.
            // now move the elements from groups after 1st 2 groups that have the same condition
            // with the exception of the build events
            foreach (var group in groups.Where(g => g.Condition.Trim().Length > 0))
            {
                var firstGroup = groupDict[group.Condition.Trim()];
                var propsToMove = new List<MBC.ProjectPropertyElement>();
                foreach (var prop in group.Properties.ToArray())
                {
                    var name = prop.Name;
                    switch (name.ToLower())
                    {
                        case "prebuildevent":
                        case "postbuildevent":
                        case "runpostbuildevent":
                            continue;
                        default:
                            break;
                    }
                    bool found = false;
                    foreach (var current in firstGroup.Properties)
                    {
                        if (String.Compare(current.Name, name, true) == 0)
                        {
                            found = true;
                            current.Value = prop.Value;
                            group.RemoveChild(prop);
                            changed = true;
                        }
                    }
                    if (!found)
                    {
                        group.RemoveChild(prop);
                        firstGroup.AddProperty(prop.Name, prop.Value);
                        changed = true;
                    }
                }

            }
            foreach (var group in groupDict.Values)
            {
                foreach (var prop in group.Properties)
                {
                    if (updateProperty(prop))
                        changed = true;
                }
            }
            // Add ProjectVersion and IncludePaths & NoStandardDefs to first Propertygroup without condition.
            var grp = groups.Where(g => g.Condition.Trim().Length == 0).FirstOrDefault();
            if (grp != null)
            {
                if (!String.IsNullOrEmpty(debugInclude))
                {
                    if (addProperty(grp, IncludePaths, debugInclude))
                        changed = true;
                }
                if (!String.IsNullOrEmpty(debugNoStdDefs))
                {
                    if (addProperty(grp, Nostandarddefs, debugNoStdDefs))
                        changed = true;
                }
                if (BuildProject.Xml.RawXml.ToString().IndexOf("$(" + XSharpProjectExtensionsPath, StringComparison.OrdinalIgnoreCase) >= 0)
                {
                    if (addProperty(grp, XSharpProjectExtensionsPath, XSharpMsBuildDir + "\\"))
                        changed = true;
                }
                else
                {
                    if (removeProperty(grp, XSharpProjectExtensionsPath))
                        changed = true;
                }
            }

            MBC.ProjectImportElement iTargets = null;
            changed = moveImports(ref iTargets, filename) || changed;
            changed = moveBuildEvents(iTargets) || changed;
            if (dialectVO)
            {
                changed = addReferences() || changed;
            }
            if (changed)
            {
                if (this.QueryEditProjectFile(true))
                {
                    this.UpdateProjectVersion();
                    Utilities.DeleteFileSafe(filename);
                    BuildProject.Xml.Save(filename);
                    BuildProject.ReevaluateIfNecessary();
                }
            }
        }

        private bool addProperty(MBC.ProjectPropertyGroupElement grp, string Name, string Value)
        {
            bool changed = false;
            var prop = grp.Properties.Where(p => String.Equals(p.Name, Name, StringComparison.OrdinalIgnoreCase)).FirstOrDefault();
            if (prop == null)
            {
                grp.AddProperty(Name, Value);
                changed = true;
            }
            else if (prop.Value != Value && !String.IsNullOrEmpty(Value))
            {
                prop.Value = Value;
                changed = true;
            }
            return changed;
        }
        private bool removeProperty(MBC.ProjectPropertyGroupElement grp, string Name)
        {
            bool changed = false;
            var prop = grp.Properties.Where(p => String.Equals(p.Name, Name, StringComparison.OrdinalIgnoreCase)).FirstOrDefault();
            if (prop != null)
            {
                grp.RemoveChild(prop);
                changed = true;
            }
            return changed;
        }

        private bool addReferences()
        {
            MBC.ProjectItemElement voRef = null;
            bool hasRT = false;
            bool changed = false;

            foreach (var grp in BuildProject.Xml.ItemGroups)
            {
                foreach (var item in grp.Items)
                {
                    if (item.ItemType.ToLower() == "reference")
                    {
                        if (item.Include.IndexOf("XSharp.VO", StringComparison.OrdinalIgnoreCase) >= 0)
                            voRef = item;
                        if (item.Include.IndexOf("XSharp.RT", StringComparison.OrdinalIgnoreCase) >= 0)
                            hasRT = true;
                    }

                }
            }
            if (!hasRT && voRef != null)
            {
                var grp = voRef.Parent;
                var newitem = BuildProject.Xml.AddItem("Reference", voRef.Include.Replace("XSharp.VO", "XSharp.RT"));
                foreach (var child in voRef.Metadata)
                {
                    newitem.AddMetadata(child.Name, child.Value.Replace("XSharp.VO", "XSharp.RT"));
                }
                changed = true;
            }
            return changed;
        }
        private bool moveImports(ref MBC.ProjectImportElement iTargets, string filename)
        {
            bool hasImportDefaultProps = false;
            bool hasImportProps = false;
            bool hasImportTargets = false;
            bool changed = false;
            MBC.ProjectImportElement iProps = null;
            foreach (var import in BuildProject.Xml.Imports)
            {
                var prj = import.Project;
                if (prj.IndexOf(importDefaultPropsShort, StringComparison.OrdinalIgnoreCase) >= 0)
                {
                    hasImportDefaultProps = true;
                    if (prj != importDefaultPropsFull)
                    {
                        import.Project = importDefaultPropsFull;
                        changed = true;
                    }
                }
                if (prj.IndexOf(importPropsShort, StringComparison.OrdinalIgnoreCase) >= 0)
                {
                    hasImportProps = true;
                    iProps = import;
                }
                if (prj.IndexOf(importTargetsShort, StringComparison.OrdinalIgnoreCase) >= 0)
                {
                    hasImportTargets = true;
                    iTargets = import;
                    if (prj != importTargetsFull)
                    {
                        import.Project = importTargetsFull;
                        changed = true;
                    }

                }
            }
            if (!hasImportDefaultProps || !hasImportTargets)
            {
                VS.MessageBox.Show($"Important <Imports> tags are missing in your projectfile: {filename}, your project will most likely not compile.");
            }
            if (hasImportProps && hasImportTargets)
            {
                // we must change the original XSharp.Props to XSharp.Targets and remove the original import for XSharp.Targets
                iProps.Project = importTargetsFull;
                BuildProject.Xml.RemoveChild(iTargets);
                iTargets = iProps;
                changed = true;
            }
            return changed;
        }
        private bool moveBuildEvents(MBC.ProjectImportElement iTargets)
        {
            // Check for Prebuild and Postbuild
            // must be after iTargets
            string prebuildValue = "";
            string postbuildValue = "";
            string runpostbuildValue = "";
            bool moveBuildEvents = false;
            bool hasBuildEvents = false;
            bool changed = false;
            foreach (var group in BuildProject.Xml.PropertyGroups)
            {
                bool groupHasCondition = !string.IsNullOrEmpty(group.Condition);
                MBC.ProjectPropertyElement prebuild = null;
                MBC.ProjectPropertyElement postbuild = null;
                MBC.ProjectPropertyElement runpostbuild = null;
                bool found = false;
                foreach (var c in group.Children)
                {
                    var p = c as MBC.ProjectPropertyElement;
                    if (p != null)
                    {
                        if (string.Equals(p.Name, PreBuildEvent, StringComparison.OrdinalIgnoreCase))
                        {
                            prebuild = p;
                            found = true;
                        }
                        if (string.Equals(p.Name, PostBuildEvent, StringComparison.OrdinalIgnoreCase))
                        {
                            prebuild = p;
                            found = true;
                        }
                        if (string.Equals(p.Name, RunPostBuildEvent, StringComparison.OrdinalIgnoreCase))
                        {
                            runpostbuild = p;
                            found = true;
                        }
                    }
                }
                // we do not want prebuild and postbuild in a group without condition
                // and the group should be after the import XSharp.Targets
                if (found)
                {
                    if (groupHasCondition)
                    {
                        hasBuildEvents = true;
                        if (iTargets != null && group.Location.Line < iTargets.Location.Line)
                        {
                            BuildProject.Xml.RemoveChild(group);
                            BuildProject.Xml.InsertAfterChild(group, BuildProject.Xml.LastChild);
                            changed = true;
                        }
                    }
                    else
                    {
                        moveBuildEvents = true;
                        if (prebuild != null)
                        {
                            prebuildValue = prebuild.Value;
                            group.RemoveChild(prebuild);
                        }
                        if (postbuild != null)
                        {
                            postbuildValue = postbuild.Value;
                            group.RemoveChild(postbuild);
                        }
                        if (runpostbuild != null)
                        {
                            runpostbuildValue = runpostbuild.Value;
                            group.RemoveChild(runpostbuild);
                        }
                        if (group.Children.Count == 0)
                        {
                            group.Parent.RemoveChild(group);
                        }
                    }
                }
            }
            if (moveBuildEvents || !hasBuildEvents)
            {
                // add property groups for prebuild and postbuild events
                var group = BuildProject.Xml.CreatePropertyGroupElement();
                BuildProject.Xml.InsertAfterChild(group, BuildProject.Xml.LastChild);
                group.Condition = conditionDebug;
                group.AddProperty(PreBuildEvent, prebuildValue);
                group.AddProperty(PostBuildEvent, postbuildValue);
                group.AddProperty(RunPostBuildEvent, runpostbuildValue);
                group = BuildProject.Xml.CreatePropertyGroupElement();
                BuildProject.Xml.InsertAfterChild(group, BuildProject.Xml.LastChild);
                group.Condition = conditionRelease;
                group.AddProperty(PreBuildEvent, prebuildValue);
                group.AddProperty(PostBuildEvent, postbuildValue);
                group.AddProperty(RunPostBuildEvent, runpostbuildValue);
                changed = true;
            }
            return changed;

        }



#region IVsProject5
        public int IsDocumentInProject2(string pszMkDocument, out int pfFound, out int pdwPriority2, out uint pitemid)
        {
            var node = this.FindURL(pszMkDocument);
            if (node != null)
            {
                pfFound = 1;
                pitemid = node.ID;
                if (node is XSharpFileNode)
                {
                    var FNode = node as XSharpFileNode;
                    if (FNode.IsNonMemberItem)
                    {
                        pdwPriority2 = (int)__VSDOCUMENTPRIORITY2.DP2_NonMember;
                    }
                    else
                    {
                        pdwPriority2 = (int)__VSDOCUMENTPRIORITY2.DP2_Standard;
                    }

                }
                else
                {
                    pdwPriority2 = (int)__VSDOCUMENTPRIORITY2.DP2_Standard;
                }
            }
            else
            {
                pfFound = 0;
                pdwPriority2 = 0;
                pitemid = 0;
            }
            return VSConstants.S_OK;
        }


        public bool DocumentInsertLine(string fileName, int line, string text)
        {
            XSharpFileNode node = this.FindURL(fileName) as XSharpFileNode;
            if (node != null)
            {
                return node.DocumentInsertLine(line, text);
            }
            return false;
        }

        public string DocumentGetText(string fileName, ref bool isOpen)
        {
            isOpen = XSettings.IsDocumentOpen(fileName);
            if (isOpen)
            {
                XSharpFileNode node = this.FindURL(fileName) as XSharpFileNode;
                if (node != null)
                {
                    return node.DocumentGetText();
                }
            }
            return "";
        }

        public bool DocumentSetText(string fileName, string text)
        {
            XSharpFileNode node = this.FindURL(fileName) as XSharpFileNode;
            if (node != null)
            {
                return node.DocumentSetText(text);
            }
            return false;
        }

#endregion

    }




    class XSharpProjectCapabilitiesPresenceChecker : IVsBooleanSymbolPresenceChecker
    {

        public bool HasChangedSince(ref object versionObject)
        {
            // If your project capabilities do not change over time while the project is open,
            // you may simply `return false;` from your `HasChangedSince` method.
            return false;
        }

        public const string AssemblyReferences = nameof(AssemblyReferences);
        public const string CSharp = nameof(CSharp);
        public const string DeclaredSourceItems = nameof(DeclaredSourceItems);
        public const string DotNet = ".NET";
        public const string Managed = nameof(Managed);
        public const string PackageReferences = nameof(PackageReferences);
        public const string Publish = nameof(Publish);
        public const string UserSourceItems = nameof(UserSourceItems);
        public const string WindowsXAML = nameof(WindowsXAML);
        public const string WindowsXaml = nameof(WindowsXaml);
        public const string WPF = nameof(WPF);
        public const string XSharp = nameof(XSharp);

        // Known unsupported capabilities
        public const string AspNetCore = nameof(AspNetCore);
        public const string BuildAndroidTarget = nameof(BuildAndroidTarget);
        public const string BuildiOSProject = nameof(BuildiOSProject);
        public const string CPS = nameof(CPS);
        public const string DependenciesTree = nameof(DependenciesTree);
        public const string DependencyPackageManagement = nameof(DependencyPackageManagement);
        public const string DNX = nameof(DNX);
        public const string DotNetCoreWeb = nameof(DotNetCoreWeb);
        public const string DynamicFileNesting = nameof(DynamicFileNesting);
        public const string HideConnectedServicesNode = nameof(HideConnectedServicesNode);
        public const string PHP = nameof(PHP);
        public const string Python = nameof(Python);
        public const string SharedAssetsProject = nameof(SharedAssetsProject);
        public const string TestContainer = nameof(TestContainer);
        public const string VB = nameof(VB);
        public const string VisualC = nameof(VisualC);
        public const string WapProj = nameof(WapProj);
        public const string Web = nameof(Web);
        public const string WebsiteProject = nameof(WebsiteProject);
        public const string WindowsXAMLAppxPackage = nameof(WindowsXAMLAppxPackage);
        public const string WindowsXAMLEnableOverview = nameof(WindowsXAMLEnableOverview);
        public const string MicrosoftVisualStudioConnectedServicesVirtualNode = "Microsoft.VisualStudio.ConnectedServices.VirtualNode";


        public bool IsSymbolPresent(string symbol)
        {
            switch (symbol)
            {
                case AssemblyReferences:
                //case CSharp:
                case DeclaredSourceItems:
                case DotNet:
                case Managed:
                //case PackageReferences:
                case Publish:
                case UserSourceItems:
                case WindowsXAML:
                case WindowsXaml:
                case WPF:
                case XSharp:
                    return true;
                case PackageReferences:
                    // If we return true then sometimes builds with solutionwide restores will start very slowly
                    return false;
                case AspNetCore:
                case BuildAndroidTarget:
                case BuildiOSProject:
                case CPS:
                case DependenciesTree:
                case DependencyPackageManagement:
                case DNX:
                case DotNetCoreWeb:
                case DynamicFileNesting:
                case HideConnectedServicesNode:
                case MicrosoftVisualStudioConnectedServicesVirtualNode:
                case PHP:
                case Python:
                case SharedAssetsProject:
                case TestContainer:
                case VB:
                case VisualC:
                case WapProj:
                case Web:
                case WebsiteProject:
                case WindowsXAMLAppxPackage:
                case WindowsXAMLEnableOverview:
                    return false;
            }
            // intentionally no default so we can set a breakpoint and see what other capabilities
            // might be needed
            return false;
        }
    }
}
