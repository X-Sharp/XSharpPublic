//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Drawing;
using System.IO;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using EnvDTE;
using VSLangProj;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Project;
using Microsoft.Windows.Design.Host;

using System.Collections.Generic;
using Microsoft.VisualStudio.Shell.Interop;
using XSharp.Project.WPF;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio;
using System.Diagnostics;
using MSBuild = Microsoft.Build.Evaluation;
using System.Diagnostics.CodeAnalysis;
using Microsoft.VisualStudio.Shell.TableManager;
using System.ComponentModel.Composition;

using XSharpModel;
using System.Linq;
using Microsoft.VisualStudio.TextManager.Interop;
using LanguageService.CodeAnalysis;
using LanguageService.CodeAnalysis.XSharp;
using XSharp.CodeDom;
using MBC = Microsoft.Build.Construction;

namespace XSharp.Project
{
    /// <summary>
    /// This class extends the ProjectNode in order to represent our project
    /// within the hierarchy.
    /// </summary>
    [Guid("F1A46976-964A-4A1E-955D-E05F5DB8651F")]
    public class XSharpProjectNode : XProjectNode, IVsSingleFileGeneratorFactory, IXSharpProject,
        IVsDesignTimeAssemblyResolution, IVsProject5, IProjectTypeHelper
    //, IVsReferenceManagerUser
    {

        static Dictionary<string, string> dependencies;
        static XSharpProjectNode()
        {
            // first the extension to look for, second the extension that can be the parent
            dependencies = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            dependencies.Add(".designer.prg", ".prg");
            dependencies.Add(".xaml.prg", ".xaml");
            dependencies.Add(".vh", ".prg");
            try
            {
                imageList = Utilities.GetImageList(typeof(XSharpProjectNode).Assembly.GetManifestResourceStream("XSharp.Project.Resources.XSharpProjectImageList.bmp"));
            }
            catch (Exception)
            {
            }
        }

        internal enum XSharpProjectImageName
        {
            Project = 0,
        }

        #region Constants
        internal const string ProjectTypeName = "XSharp";
        #endregion

        #region Fields
        private XSharpProjectPackage package;
        internal static int imageOffset;
        private static ImageList imageList;
        private VSLangProj.VSProject vsProject;
        IErrorList errorList = null;
        bool isLoading = false;
        private FileChangeManager filechangemanager = null;


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

            InitializeImageList();

            InitializeCATIDs();

            // Used by (at least) the AddFromTemplate in order (for eg) to have Form1.Designer.Prg depending on Form1.prg
            this.CanFileNodesHaveChilds = true;

            this.CanProjectDeleteItems = true;

            // Gets or sets whether the project uses the Project Designer Editor or the property page frame to edit project properties.
            // True : New WPF way
            // False: C++-Like Property pages
            this.SupportsProjectDesigner = true;

            object errlist = ((IServiceProvider)this.package).GetService(typeof(SVsErrorList));
            errorList = (IErrorList)errlist;

        }

        private void Filechangemanager_FileChangedOnDisk(object sender, FileChangedOnDiskEventArgs e)
        {
            //System.Diagnostics.Trace.WriteLine("FileChangedOnDisk " + System.IO.Path.GetFileName(e.FileName));
            if (IsXamlFile(e.FileName) || IsCodeFile(e.FileName))
            {
                XFile file = this.ProjectModel.FindFullPath(e.FileName);
                if (file != null)
                {
                    this.ProjectModel.WalkFile(file);
                }
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
            AddCATIDMapping(typeof(XSharpDebugPropertyPage), typeof(XSharpDebugPropertyPage).GUID);
        }
        #endregion

        #region Properties
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
                IVsHierarchy hier = Marshal.GetObjectForIUnknown(unknownPtr) as IVsHierarchy;
                return hier;
            }
        }

        // Gets the output file name depending on current OutputType.
        // View GeneralProperyPage
        string _outputFile;
        string _configName;
        public string OutputFile
        {
            get
            {
                if (_outputFile == null && _configName != CurrentConfig.ConfigName)
                {
                    _outputFile = this.GetProjectProperty(ProjectFileConstants.TargetPath);
                    _configName = CurrentConfig.ConfigName;
                }
                return _outputFile;
            }
            internal set
            {
                _outputFile = value;
            }
        }
        #endregion

        #region Overriden implementation
        /// <summary>
        /// Gets the project GUID.
        /// </summary>
        /// <value>The project GUID.</value>
        public override Guid ProjectGuid
        {
            get { return typeof(XSharpProjectFactory).GUID; }
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
                return imageOffset + (int)XSharpProjectImageName.Project;
            }
        }

        internal override object Object
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
            base.options = new XSharpProjectOptions(this);
            return options;
        }

        public override ProjectOptions GetProjectOptions(ConfigCanonicalName configCanonicalName)
        {
            if (this.options != null)
            {
                var xoptions = this.options as XSharpProjectOptions;
                if (xoptions.ConfigCanonicalName != configCanonicalName)
                {
                    this.options = null;
                }
            }
            if (this.options == null)
            {
                this.options = base.GetProjectOptions(configCanonicalName);
                var xoptions = this.options as XSharpProjectOptions;
                xoptions.ConfigCanonicalName = configCanonicalName;
                xoptions.BuildCommandLine();
            }
            return this.options;
        }
        public override string GetProjectProperty(string propertyName, bool resetCache)
        {
            if (BuildProject != null)
            {
                return base.GetProjectProperty(propertyName, resetCache);
            }
            return null;
        }
        public __VSPROJOUTPUTTYPE GetOutPutType()
        {
            string outputTypeAsString = this.ProjectMgr.GetProjectProperty("OutputType", false);
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
                    return GetOutPutType();
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
                    return new XSharpProjectCapabilitiesPresenceChecker();

                // Test ?
                case (int)__VSHPROPID5.VSHPROPID_TargetPlatformIdentifier:
                    return "Windows";
            }
            return base.GetProperty(propId);
        }

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

                if (node.IsXAML || node.IsForm)
                {
                    provider.AddService(typeof(DesignerContext), node.ServiceCreator, false);
                }
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
        protected internal override FolderNode CreateFolderNode(string path, ProjectElement element)
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

            provider.AddService(typeof(EnvDTE.Project), ProjectMgr.GetAutomationObject(), false);
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
                    ShowMessageBox($"Cannot set dependency from \"{item.EvaluatedInclude}\" to \"{dependentOf}\"\r\nCannot find \"{dependentOf}\" in the project hierarchy");
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
            string parentFile = Path.Combine(path, fileName.Substring(0, dotPos));
            string extension = fileName.Substring(dotPos);
            //
            if (dependencies.ContainsKey(extension))
            {
                //
                HierarchyNode newParent = parentNode.FindChild(parentFile + dependencies[extension]);
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
                    HierarchyNode newParent = parentNode.FindChild(parentFile + ".prg");
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
                || String.Compare(type, XSharpProjectFileConstants.NativeResource, StringComparison.OrdinalIgnoreCase) == 0           // rc file
                || String.Compare(type, XSharpProjectFileConstants.VOBinary, StringComparison.OrdinalIgnoreCase) == 0           // vobinary file
                )
            {
                return true;
            }

            // we don't know about this type, ask the base class.
            return base.IsItemTypeFileType(type);
        }

        internal override void OnAfterProjectOpen(object sender, AfterProjectFileOpenedEventArgs e)
        {
            base.OnAfterProjectOpen(sender, e);

            // initialize the parser

            if (filechangemanager == null)
            {
                filechangemanager = new FileChangeManager(this.Site);
                filechangemanager.FileChangedOnDisk += Filechangemanager_FileChangedOnDisk;

            }
            if (this.isLoading)
            {
                // Run the background Walker/Listener, to fill the Model
                this.isLoading = false;
                foreach (var url in this.URLNodes.Keys)
                {
                    if (!IsProjectFile(url) && this.BuildProject != null)
                    {
                        var xnode = this.URLNodes[url] as XSharpFileNode;
                        if (xnode != null && !xnode.IsNonMemberItem)
                        {
                            if (File.Exists(url))
                            {
                                this.ProjectModel.AddFile(url);
                                // make sure generated code is updated when changed
                                if (xnode.IsDependent || IsXamlFile(url))
                                {
                                    filechangemanager.ObserveItem(url);
                                }
                            }
                        }
                    }
                }
                this.ProjectModel.Walk();
            }
        }


        public override void Load(string filename, string location, string name, uint flags, ref Guid iidProject, out int canceled)
        {
            // check for incomplete conditions
            base.Load(filename, location, name, flags, ref iidProject, out canceled);
            this.isLoading = true;

            // WAP ask the designer service for the CodeDomProvider corresponding to the project node.
            this.OleServiceProvider.AddService(typeof(SVSMDCodeDomProvider), new OleServiceProvider.ServiceCreatorCallback(this.CreateServices), false);
            this.OleServiceProvider.AddService(typeof(System.CodeDom.Compiler.CodeDomProvider), new OleServiceProvider.ServiceCreatorCallback(this.CreateServices), false);

            // This will call the Calback in PojectPackage
            IXSharpLibraryManager libraryManager = Site.GetService(typeof(IXSharpLibraryManager)) as IXSharpLibraryManager;
            if (null != libraryManager)
            {
                libraryManager.RegisterHierarchy(this.InteropSafeHierarchy, this.ProjectModel, this);
            }

            //If this is a WPFFlavor-ed project, then add a project-level DesignerContext service to provide
            //event handler generation (EventBindingProvider) for the XAML designer.
            this.OleServiceProvider.AddService(typeof(DesignerContext), new OleServiceProvider.ServiceCreatorCallback(this.CreateServices), false);

            CreateErrorListManager();

            // Be sure we have External/system types for Intellisense
            UpdateAssemblyReferencesModel();

            // Add EventHandler to handle adding / removing a Reference
            VSProject.Events.ReferencesEvents.ReferenceAdded += ReferencesEvents_ReferenceAdded;
            VSProject.Events.ReferencesEvents.ReferenceRemoved += ReferencesEvents_ReferenceRemoved;
            VSProject.Events.ReferencesEvents.ReferenceChanged += ReferencesEvents_ReferenceChanged;
        }


        #endregion

        #region References Management Events

        private void ReferencesEvents_ReferenceRemoved(Reference pReference)
        {
            if ((pReference.Type == prjReferenceType.prjReferenceTypeAssembly) ||
                (pReference.Type == prjReferenceType.prjReferenceTypeActiveX))
            {
                ProjectModel.RemoveAssemblyReference(pReference.Path);
            }
        }

        private void ReferencesEvents_ReferenceAdded(Reference pReference)
        {
            ProjectModel.AddAssemblyReference(pReference);
        }

        private void ReferencesEvents_ReferenceChanged(Reference pReference)
        {
            if ((pReference.Type == prjReferenceType.prjReferenceTypeAssembly) ||
                (pReference.Type == prjReferenceType.prjReferenceTypeActiveX))
            {
                ProjectModel.UpdateAssemblyReference(pReference.Path);
            }
        }
        #endregion


        #region Private implementation

        private void CreateErrorListManager()
        {
            if (_errorListManager == null)
            {
                _errorListManager = ErrorListManager.RegisterProject(errorList, this);
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
            else if (typeof(DesignerContext) == serviceType)
            {
                service = this.DesignerContext;
            }
            else if (typeof(IVsSingleFileGeneratorFactory) == serviceType)
            {
                service = new SingleFileGeneratorFactory(this.ProjectGuid, this.Site);
            }


            return service;
        }


        private DesignerContext _designerContext;
        protected internal Microsoft.Windows.Design.Host.DesignerContext DesignerContext
        {
            get
            {
                if (_designerContext == null)
                {
                    _designerContext = new DesignerContext();
                    //Set the RuntimeNameProvider so the XAML designer will call it when items are added to
                    //a design surface. Since the provider does not depend on an item context, we provide it at
                    //the project level.
                    _designerContext.RuntimeNameProvider = new XSharpRuntimeNameProvider();
                }
                return _designerContext;
            }
        }
        protected override Microsoft.VisualStudio.Project.BuildResult InvokeMsBuild(string target)
        {
            if (String.Equals(target, "Clean", StringComparison.OrdinalIgnoreCase))
            {
                if (logger != null)
                {
                    logger.Clear();

                }
            }
            return base.InvokeMsBuild(target);
        }


        protected override NodeProperties CreatePropertiesObject()
        {
            return new XSharpProjectNodeProperties(this);
        }
        #endregion


        XSharpModel.XProject projectModel;
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
                    projectModel = new XSharpModel.XProject(this);
                    // Set the backlink, so the walker can access the StatusBar
                    projectModel.ProjectNode = this;
                    //
                    UpdateAssemblyReferencesModel();
                    //
                    XSharpModel.XSolution.Add(projectModel);
                }
                return projectModel;
            }

            private set
            {
                projectModel = null;
            }
        }

        public void UpdateAssemblyReferencesModel()
        {
            // Add all references to the Type Controller
            ProjectModel.ClearAssemblyReferences();
            foreach (Reference reference in this.VSProject.References)
            {
                // Our project references should not be added as AssemblyReference
                if (reference is OAProjectReference)
                {
                    // no need to add, AddUrl already adds these projects
                }
                else
                {
                    // OAAssemblyReference or OACOMReference
                    ProjectModel.AddAssemblyReference(reference);
                }
            }
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
            if (IsProjectFile(url))
            {
                this.ProjectModel.AddProjectReference(url);
            }
            else if (IsStrangerProjectFile(url))
            {
                this.ProjectModel.AddStrangerProjectReference(url);
            }
            else if (this.IsProjectOpened)
            {
                var xnode = node as XSharpFileNode;
                if (xnode != null && !xnode.IsNonMemberItem)
                {
                    if (File.Exists(url))
                    {
                        this.ProjectModel.AddFile(url);
                        if (IsXamlFile(url))
                        {
                            if (filechangemanager != null)
                            {
                                filechangemanager.ObserveItem(url);
                            }

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
            List<EnvDTE.Project> list = new List<EnvDTE.Project>();
            for (var i = 1; i <= solutionFolder.ProjectItems.Count; i++)
            {
                var subProject = solutionFolder.ProjectItems.Item(i).SubProject;
                if (subProject == null)
                {
                    continue;
                }

                // If this is another solution folder, do a recursive call, otherwise add
                if (subProject.Kind == EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder)
                {
                    list.AddRange(GetSolutionFolderProjects(subProject));
                }
                else
                {
                    list.Add(subProject);
                }
            }
            return list;
        }

        private List<EnvDTE.Project> GetSolutionProjects()
        {
            List<EnvDTE.Project> list = new List<EnvDTE.Project>();
            EnvDTE.DTE dte = (EnvDTE.DTE)this.Site.GetService(typeof(EnvDTE.DTE));
            foreach (EnvDTE.Project p in dte.Solution.Projects)
            {
                if (p == null)
                {
                    continue;
                }

                if (p.Kind == EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder)
                {
                    list.AddRange(GetSolutionFolderProjects(p));
                }
                else
                {
                    list.Add(p);
                }
            }
            return list;
        }
        public EnvDTE.Project FindProject(String sProject)
        {
            foreach (var p in GetSolutionProjects())
            {
                if (p.FullName.Equals(sProject, StringComparison.InvariantCultureIgnoreCase))
                {
                    return p;
                }
            }
            return null;
        }

        public override void RemoveURL(String url)
        {
            if (_closing)
                return;
            //
            // We should remove the external projects entries
            if (IsProjectFile(url))
            {
                this.ProjectModel.RemoveProjectReference(url);
            }
            else if (IsStrangerProjectFile(url))
            {
                this.ProjectModel.RemoveStrangerProjectReference(url);
            }
            else
            {
                var node = this.FindChild(url) as XSharpFileNode;
                if (node != null && !node.IsNonMemberItem)
                {
                    if (filechangemanager != null)
                    {
                        if (IsXamlFile(url) ||
                            node.IsDependent)

                            filechangemanager.StopObservingItem(url);
                    }

                    this.ProjectModel.RemoveFile(url);
                }
            }
            base.RemoveURL(url);
        }


        /// <summary>
        /// Check if fullpath points to a XSharp Project file.
        /// </summary>
        /// <param name="fullPath"></param>
        /// <returns></returns>
        private bool IsProjectFile(string fullPath)
        {
            string cExt = Path.GetExtension(fullPath);
            return String.Equals(cExt, ".xsprj", StringComparison.OrdinalIgnoreCase)
                || String.Equals(cExt, ".xsproj", StringComparison.OrdinalIgnoreCase);
        }

        /// <summary>
        /// Check if fullpath points to a file, whose extension ends with "proj" so it might be project file.
        /// </summary>
        /// <param name="fullPath"></param>
        /// <returns></returns>
        private bool IsStrangerProjectFile(string fullPath)
        {
            string ext = Path.GetExtension(fullPath);
            return (ext.EndsWith("proj", StringComparison.OrdinalIgnoreCase));
        }

        #region IXSharpProject Interface
        protected IVsStatusbar statusBar;
        protected bool lTriedToGetStatusBar = false;
        public void SetStatusBarText(string msg)
        {
            if (statusBar == null && !lTriedToGetStatusBar)
            {
                statusBar = Site.GetService(typeof(SVsStatusbar)) as IVsStatusbar;
                lTriedToGetStatusBar = true;
            }

            if (statusBar != null)
            {
                statusBar.SetText(msg);
            }
        }
        public void SetStatusBarAnimation(bool onoff, short idAnimation)
        {
            try
            {
                if (statusBar == null && !lTriedToGetStatusBar)
                {
                    statusBar = Site.GetService(typeof(SVsStatusbar)) as IVsStatusbar;
                    lTriedToGetStatusBar = true;
                }

                if (statusBar != null)
                {
                    statusBar.Animation(onoff ? 1 : 0, idAnimation);
                }
            }
            catch //(Exception e)
            {
                //System.Diagnostics.Debug.WriteLine("Error showing animation " );
            }
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
        public void OpenElement(string file, int line, int column)
        {
            IVsWindowFrame window;
            IVsTextView textView;
            IVsUIHierarchy dummy1;
            uint dummy2;

            VsShellUtilities.OpenDocument(this.Site, file, VSConstants.LOGVIEWID_Code, out dummy1, out dummy2, out window, out textView);
            if ((window != null) && (textView != null))
            {
                window.Show();
                //
                TextSpan span = new TextSpan();
                span.iStartLine = line - 1;
                span.iStartIndex = column - 1;
                span.iEndLine = line - 1;
                span.iEndIndex = column - 1;
                //
                textView.SetCaretPos(span.iStartLine, span.iStartIndex);
                textView.EnsureSpanVisible(span);
                if (span.iStartLine > 5)
                    textView.SetTopLine(span.iStartLine - 5);
                else
                    textView.SetTopLine(0);
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
        string _rootNamespace = null;

        public string RootNameSpace
        {
            get
            {
                if (_rootNamespace == null)
                {
                    lock (this)
                    {
                        if (_rootNamespace == null)
                            _rootNamespace = GetProjectProperty(ProjectFileConstants.RootNamespace, false);
                    }
                }
                return _rootNamespace;
            }
            internal set
            {
                _rootNamespace = value;
            }
        }

        public bool DisableLexing => package.GetIntellisenseOptionsPage().DisableSyntaxColorization;
        public bool DisableParsing => package.GetIntellisenseOptionsPage().DisableEntityParsing;
        public bool DisableRegions  => package.GetIntellisenseOptionsPage().DisableRegions;

        public bool KeywordsUppercase => package.GetIntellisenseOptionsPage().KeywordCase == 1;

        #endregion


        protected override void Reload()
        {
            base.Reload();

            if (ResetDependencies())
            {
                this.BuildProject.Save();
            }

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
                    if (!String.IsNullOrEmpty(parent))
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
                if (parentNode != null && parentNode is XSharpFileNode)
                {
                    XSharpFileNode parent = (XSharpFileNode)parentNode;
                    parent.AddDependant(NodeToMove);
                    NodeToMove.SubType = subType;
                    bMoved = true;
                }
            }
            return bMoved;
        }

        internal override bool AddFilesFromProjectReferences(HierarchyNode targetNode, string[] projectReferences, uint dropEffect)
        {
            bool bOk = base.AddFilesFromProjectReferences(targetNode, projectReferences, dropEffect);
            if (bOk)
            {
                ResetDependencies();
            }
            return bOk;
        }
        #region IProjectTypeHelper
        public System.Type ResolveType(string name, IReadOnlyList<string> usings)
        {
            switch (name.ToLower())
            {
                case "object":
                case "system.object":
                    return typeof(object);
                case "void":
                case "system.void":
                    return typeof(void);
                case "boolean":
                case "system.boolean":
                    return typeof(Boolean);
                case "string":
                case "system.string":
                    return typeof(String);
            }
            var model = this.ProjectModel;
            var myusings = new List<string>();
            myusings.AddRange(usings);
            myusings.AddUnique("System");
            return model.FindSystemType(name, myusings);
        }

        public XType ResolveXType(string name, IReadOnlyList<string> usings)
        {
            var model = this.ProjectModel;
            //
            XType result = model.LookupFullName(name, true);
            if (result != null)
                return result;
            // try to find with explicit usings
            if (usings != null)
            {
                foreach (var usingName in usings)
                {
                    var fullname = usingName + "." + name;
                    result = model.LookupFullName(fullname, true);
                    if (result != null)
                        return result;
                }
            }
            return result;
        }


        public XType ResolveReferencedType(string name, IReadOnlyList<string> usings)
        {
            var model = this.ProjectModel;
            //
            XType result = model.LookupReferenced(name, true);
            if (result != null)
                return result;
            // try to find with explicit usings
            if (usings != null)
            {
                foreach (var usingName in usings)
                {
                    var fullname = usingName + "." + name;
                    result = model.LookupReferenced(fullname, true);
                    if (result != null)
                        return result;
                }
            }
            return result;
        }

        //public CodeElement ResolveStrangerType(string name, IReadOnlyList<string> usings)
        //{
        //    var model = this.ProjectModel;
        //    // First, easy way..Use the simple name
        //    CodeElement codeElt = model.LookupForStranger(name, true);
        //    if (codeElt == null)
        //    {
        //        // Search using the USING statements in the File that contains the var
        //        foreach (string usingStatement in usings)
        //        {
        //            String fqn = usingStatement + "." + name;
        //            codeElt = model.LookupForStranger(fqn, true);
        //            if (codeElt != null)
        //                break;
        //        }
        //    }
        //    return codeElt;
        //}

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
            pbstrGenProgID = String.Empty;
            if (createIVsSingleFileGeneratorFactory())
                return factory.GetDefaultGenerator(wszFilename, out pbstrGenProgID);
            return VSConstants.S_FALSE;
        }

        public int CreateGeneratorInstance(string wszProgId, out int pbGeneratesDesignTimeSource, out int pbGeneratesSharedDesignTimeSource, out int pbUseTempPEFlag, out IVsSingleFileGenerator ppGenerate)
        {
            pbGeneratesDesignTimeSource = 0;
            pbGeneratesSharedDesignTimeSource = 0;
            pbUseTempPEFlag = 0;
            ppGenerate = null;
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
            if (createIVsSingleFileGeneratorFactory())
                return factory.GetGeneratorInformation(wszProgId, out pbGeneratesDesignTimeSource, out pbGeneratesSharedDesignTimeSource, out pbUseTempPEFlag, out pguidGenerator);
            return VSConstants.S_FALSE;

        }
        #endregion

        #region IVsDesignTimeAssemblyResolution

        private DesignTimeAssemblyResolution designTimeAssemblyResolution;
        private ConfigCanonicalName _config = new ConfigCanonicalName("Debug", "AnyCPU");

        protected internal override void SetConfiguration(ConfigCanonicalName config)
        {
            _config = config;
            base.SetConfiguration(config);
            if (this.designTimeAssemblyResolution == null)
            {
                this.designTimeAssemblyResolution = new DesignTimeAssemblyResolution();
            }
            this.designTimeAssemblyResolution.Initialize(this);

        }
        public int GetTargetFramework(out string ppTargetFramework)
        {
            ppTargetFramework = this.ProjectMgr.TargetFrameworkMoniker.FullName;
            return VSConstants.S_OK;
        }

        public int ResolveAssemblyPathInTargetFx(string[] prgAssemblySpecs, uint cAssembliesToResolve, VsResolvedAssemblyPath[] prgResolvedAssemblyPaths, out uint pcResolvedAssemblyPaths)
        {
            if (prgAssemblySpecs == null || cAssembliesToResolve == 0 || prgResolvedAssemblyPaths == null)
            {
                throw new ArgumentException("One or more of the arguments are invalid.");
            }

            pcResolvedAssemblyPaths = 0;

            try
            {
                var results = designTimeAssemblyResolution.Resolve(prgAssemblySpecs.Take((int)cAssembliesToResolve));
                results.CopyTo(prgResolvedAssemblyPaths, 0);
                pcResolvedAssemblyPaths = (uint)results.Length;
            }
            catch (Exception ex)
            {
                return Marshal.GetHRForException(ex);
            }

            return VSConstants.S_OK;
        }

        #endregion
        #region TableManager
        //internal ITableManagerProvider tableManagerProvider { get; private set; }
        ErrorListManager _errorListManager = null;

        protected override void SetOutputLogger(IVsOutputWindowPane output)
        {
            base.SetOutputLogger(output);

        }
        XSharpIDEBuildLogger logger = null;

        internal override IDEBuildLogger CreateBuildLogger(IVsOutputWindowPane output, TaskProvider taskProvider, IVsHierarchy hierarchy)
        {
            logger = new XSharpIDEBuildLogger(output, this.TaskProvider, hierarchy);
            logger.ProjectNode = this;
            logger.ErrorString = ErrorString;
            logger.WarningString = WarningString;
            CreateErrorListManager();
            logger.ErrorListManager = _errorListManager;
            return logger;
        }

        public override BuildResult Build(string target)
        {
            return Build(_config, target);
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

        public bool IsVsBuilding
        {
            get
            {
                try
                {
                    if (this.Site != null)
                    {
                        return VsShellUtilities.IsSolutionBuilding(this.Site);
                    }
                }
                catch (Exception e)
                {
                    if (System.Diagnostics.Debugger.IsAttached)
                        System.Diagnostics.Debug.WriteLine("Error fetching IsVsBuilding: " + e.Message);
                }
                return false;
            }
        }
        #endregion
        public bool IsDocumentOpen(string documentName)
        {
            IVsUIHierarchy hier;
            uint itemid;
            IVsWindowFrame windowFrame;
            bool open = VsShellUtilities.IsDocumentOpen(this.Site, documentName, Guid.Empty, out hier, out itemid, out windowFrame);
            return open;
        }


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
        public XSharpParseOptions ParseOptions
        {
            get
            {
                var xoptions = GetProjectOptions(this.CurrentConfig.ConfigCanonicalName) as XSharpProjectOptions;
                return xoptions.ParseOptions;
            }
        }
        bool _closing = false;
        public override int Close()
        {

            // First remove the Navigation Data
            //
            if (this.Site != null)
            {

                IXSharpLibraryManager libraryManager = (IXSharpLibraryManager)Site.GetService(typeof(IXSharpLibraryManager));
                if (libraryManager != null)
                {
                    libraryManager.UnregisterHierarchy(this.InteropSafeHierarchy);
                }
            }
            // CleanUp the CodeModel
            XSharpModel.XSolution.Remove(projectModel);
            _closing = true;
            var res = base.Close();

            if (logger != null)
            {
                logger.Clear();
            }
            ErrorListManager.RemoveProject(this);
            _errorListManager = null;
            URLNodes.Clear();
            return res;
        }

        protected override void RenameProjectFile(string newFile)
        {

            var model = this.ProjectModel;
            if (model != null)
            {
                XSharpModel.XSolution.Remove(model);
            }
            base.RenameProjectFile(newFile);
            if (model != null)
            {
                XSharpModel.XSolution.Add(model);
            }
        }

        const string config = "$(Configuration)";
        const string configPlatform = "$(Configuration)|$(Platform)";
        const string conditionDebug = "'$(Configuration)|$(Platform)' == 'Debug|AnyCPU'";
        const string conditionRelease = "'$(Configuration)|$(Platform)' == 'Release|AnyCPU'";
        const string import1DefaultProps1 = @"$(MSBuildExtensionsPath)\XSharp\XSharp.Default.props";
        const string importDefaultProps2 = @"$(XSharpProjectExtensionsPath)XSharp.Default.props";
        const string importProps = @"XSharp.props";
        const string importTargets = @"XSharp.targets";
        const string importTargetsFull = @"$(MSBuildExtensionsPath)\XSharp\XSharp.targets";
        const string postBuildEvent = "PostBuildEvent";
        const string preBuildEvent = "PreBuildEvent";
        const string runPostBuildEvent = "RunPostBuildEvent";
        public override int UpgradeProject(uint grfUpgradeFlags)
        {
            bool silent;
            int result;
            
            silent = (__VSUPGRADEPROJFLAGS)grfUpgradeFlags == __VSUPGRADEPROJFLAGS.UPF_SILENTMIGRATE;
            StringWriter backup = new StringWriter();
            BuildProject.Save(backup);
            var str = backup.ToString();
            var str2 = System.Text.RegularExpressions.Regex.Replace(str, "anycpu", "AnyCPU", System.Text.RegularExpressions.RegexOptions.IgnoreCase);
            bool ok = true;
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
            if (ok && str.IndexOf("XSharpProjectExtensionsPath", StringComparison.OrdinalIgnoreCase) == -1)
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
            if (ok && str.IndexOf(import1DefaultProps1, StringComparison.OrdinalIgnoreCase) == -1 &&
                str.IndexOf(importDefaultProps2, StringComparison.OrdinalIgnoreCase) == -1)
            {
                ok = false;
            }
            // XSharp.Props should no longer be there
            if (ok && str.IndexOf(importProps, StringComparison.OrdinalIgnoreCase) >= 0)
            {
                ok = false;
            }
            if (ok)
            {
                int iTargets = str.IndexOf(importTargets, StringComparison.OrdinalIgnoreCase);
                if (iTargets == -1)
                {
                    ok = false;
                }
                else
                {
                    // prebuild and postbuild must be after Targets
                    int iPrebuild = str.IndexOf(preBuildEvent, StringComparison.OrdinalIgnoreCase);
                    int iPostbuild = str.IndexOf(postBuildEvent, StringComparison.OrdinalIgnoreCase);
                    int iRunPostBuild = str.IndexOf(runPostBuildEvent, StringComparison.OrdinalIgnoreCase);
                    if (iPrebuild > -1 && iPrebuild < iTargets)
                        ok = false;
                    else if (iPostbuild > -1 && iPrebuild < iTargets)
                        ok = false;
                    else if (iRunPostBuild > -1 && iPrebuild < iTargets)
                        ok = false;
                }
            }
            if (!ok)
            {
                FixProjectFile(BuildProject.FullPath);
                base.UpgradeProject(grfUpgradeFlags);
                result = VSConstants.S_OK;
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
                if (string.Equals(prop.Name, "documentationfile", StringComparison.OrdinalIgnoreCase))
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
        private void FixProjectFile(string filename)
        {
            bool changed = false;
            var xml = BuildProject.Xml;
            var groups = xml.PropertyGroups.ToList();
            var groupDict = new Dictionary<string, MBC.ProjectPropertyGroupElement>();

            foreach (var group in groups.Where(grp => grp.Condition.Trim().Length > 0))
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
                condition = System.Text.RegularExpressions.Regex.Replace(condition, "anycpu", "AnyCPU", System.Text.RegularExpressions.RegexOptions.IgnoreCase).Trim();
                group.Condition = condition;
                if (!groupDict.ContainsKey(condition))
                {
                    groupDict.Add(condition, group);
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
                var propsToDelete = new List<MBC.ProjectPropertyElement>();
                foreach (var prop in group.Properties)
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
                            propsToDelete.Add(prop);
                        }
                    }
                    if (!found)
                    {
                        propsToMove.Add(prop);
                    }
                }
                foreach (var prop in propsToDelete)
                {
                    group.RemoveChild(prop);
                    changed = true;
                }
                foreach (var prop in propsToMove)
                {
                    group.RemoveChild(prop);
                    firstGroup.AddProperty(prop.Name, prop.Value);
                    changed = true;
                }
                if (group.Properties.Count == 0)
                {
                    group.Parent.RemoveChild(group);
                    changed = true;
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
            // check for the XSharpProjectExtensionsPath property inside the first propertygroup
            bool ok = false;
            foreach (var child in xml.Properties)
            {
                if (child.Name == "XSharpProjectExtensionsPath")
                {
                    ok = true;
                    break;
                }
            }
            if (!ok)
            {
                var item = groups[0].AddProperty("XSharpProjectExtensionsPath", @"$(MSBuildExtensionsPath)\XSharp\");
            }
            MBC.ProjectImportElement iTargets = null;
            changed = moveImports(ref iTargets, filename) || changed;
            changed = moveBuildEvents(iTargets) || changed;
            if (changed)
            {
                File.Copy(filename, filename + ".bak", true);
                BuildProject.Xml.Save(filename);
                BuildProject.ReevaluateIfNecessary();
            }
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
                if (String.Equals(prj, import1DefaultProps1, StringComparison.OrdinalIgnoreCase))
                    hasImportDefaultProps = true;
                if (String.Equals(prj, importDefaultProps2, StringComparison.OrdinalIgnoreCase))
                    hasImportDefaultProps = true;
                if (prj.IndexOf(importProps, StringComparison.OrdinalIgnoreCase) >= 0)
                {
                    hasImportProps = true;
                    iProps = import;
                }
                if (prj.IndexOf(importTargets, StringComparison.OrdinalIgnoreCase) >= 0)
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
                ShowMessageBox($"Important <Imports> tags are missing in your projectfile: {filename}, your project will most likely not compile.");
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
                        if (string.Equals(p.Name, preBuildEvent, StringComparison.OrdinalIgnoreCase))
                        {
                            prebuild = p;
                            found = true;
                        }
                        if (string.Equals(p.Name, postBuildEvent, StringComparison.OrdinalIgnoreCase))
                        {
                            prebuild = p;
                            found = true;
                        }
                        if (string.Equals(p.Name, runPostBuildEvent, StringComparison.OrdinalIgnoreCase))
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
                group.AddProperty(preBuildEvent, prebuildValue);
                group.AddProperty(postBuildEvent, postbuildValue);
                group.AddProperty(runPostBuildEvent, runpostbuildValue);
                group = BuildProject.Xml.CreatePropertyGroupElement();
                BuildProject.Xml.InsertAfterChild(group, BuildProject.Xml.LastChild);
                group.Condition = conditionRelease;
                group.AddProperty(preBuildEvent, prebuildValue);
                group.AddProperty(postBuildEvent, postbuildValue);
                group.AddProperty(runPostBuildEvent, runpostbuildValue);
                changed = true;
            }
            return changed;

        }
        internal int ShowMessageBox(string message)
        {
            string title = string.Empty;
            OLEMSGICON icon = OLEMSGICON.OLEMSGICON_CRITICAL;
            OLEMSGBUTTON buttons = OLEMSGBUTTON.OLEMSGBUTTON_OK;
            OLEMSGDEFBUTTON defaultButton = OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST;

            return VsShellUtilities.ShowMessageBox(this.Site, message, title, icon, buttons, defaultButton);

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
            isOpen = IsDocumentOpen(fileName);
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
        /*
        public override int AddProjectReference()
        {
            var referenceManager = this.GetService(typeof(SVsReferenceManager)) as IVsReferenceManager;
            if (referenceManager != null)
            {
                var contextGuids = new[] {
                    VSConstants.AssemblyReferenceProvider_Guid,
                    VSConstants.ComReferenceProvider_Guid,
                    VSConstants.ProjectReferenceProvider_Guid,
                    VSConstants.FileReferenceProvider_Guid,
                };
                referenceManager.ShowReferenceManager(
                    this,
                    SR.GetString(SR.AddReferenceDialogTitle),
                    "VS.ReferenceManager",
                    contextGuids.First(),
                    false
                    );
                return VSConstants.S_OK;
            }
            else
            {
                return VSConstants.E_NOINTERFACE;
            }
        }
        #region IVsReferenceManagerUser Members

        void IVsReferenceManagerUser.ChangeReferences(uint operation, IVsReferenceProviderContext changedContext)
        {
            var op = (__VSREFERENCECHANGEOPERATION)operation;
            __VSREFERENCECHANGEOPERATIONRESULT result;

            try
            {
                if (op == __VSREFERENCECHANGEOPERATION.VSREFERENCECHANGEOPERATION_ADD)
                {
                    result = this.AddReferences(changedContext);
                }
                else
                {
                    result = this.RemoveReferences(changedContext);
                }
            }
            catch (InvalidOperationException e)
            {
                Debug.Fail(e.ToString());
                result = __VSREFERENCECHANGEOPERATIONRESULT.VSREFERENCECHANGEOPERATIONRESULT_DENY;
            }

            if (result == __VSREFERENCECHANGEOPERATIONRESULT.VSREFERENCECHANGEOPERATIONRESULT_DENY)
            {
                throw new InvalidOperationException();
            }
        }

        Array IVsReferenceManagerUser.GetProviderContexts()
        {
            return this.GetProviderContexts();
        }

        #endregion

        #region IvsReferenceManagerUser implementation
        protected virtual Array GetProviderContexts()
        {
            var referenceManager = this.GetService(typeof(SVsReferenceManager)) as IVsReferenceManager;

            var contextProviders = new[] {
                CreateAssemblyReferenceProviderContext(referenceManager),
                CreateCOMReferenceProviderContext(referenceManager),
                CreateProjectReferenceProviderContext(referenceManager),
                CreateFileReferenceProviderContext(referenceManager),
            };

            return contextProviders;
        }
        private IVsReferenceProviderContext CreateAssemblyReferenceProviderContext(IVsReferenceManager mgr)
        {
            var context = mgr.CreateProviderContext(VSConstants.AssemblyReferenceProvider_Guid) as IVsAssemblyReferenceProviderContext;
            context.TargetFrameworkMoniker = this.TargetFrameworkMoniker.ToString();
            context.
            var referenceContainer = this.GetReferenceContainer();
            var references = referenceContainer
                .EnumReferences()
                .OfType<AssemblyReferenceNode>();
            foreach (var reference in references)
            {
                var newReference = context.CreateReference() as IVsAssemblyReference;
                newReference.Name = reference.AssemblyName.Name;
                newReference.FullPath = reference.Url;
                context.AddReference(newReference);
            }

            return context as IVsReferenceProviderContext;
        }

        private IVsReferenceProviderContext CreateCOMReferenceProviderContext(IVsReferenceManager mgr)
        {
            var context = mgr.CreateProviderContext(VSConstants.ComReferenceProvider_Guid) as IVsComReferenceProviderContext;

            var referenceContainer = this.GetReferenceContainer();
            var references = referenceContainer
                .EnumReferences()
                .OfType<ComReferenceNode>();
            foreach (var reference in references)
            {
                var newReference = context.CreateReference() as IVsComReference;
                newReference.MajorVersion = (ushort) reference.MajorVersionNumber;
                newReference.MinorVersion = (ushort) reference.MinorVersionNumber;
                newReference.Guid = reference.TypeGuid;
                newReference.FullPath = reference.Url;
                context.AddReference(newReference);
            }

            return context as IVsReferenceProviderContext;
        }


        private IVsReferenceProviderContext CreateProjectReferenceProviderContext(IVsReferenceManager mgr)
        {
            var context = mgr.CreateProviderContext(VSConstants.ProjectReferenceProvider_Guid) as IVsProjectReferenceProviderContext;
            context.CurrentProject = this;

            var referenceContainer = this.GetReferenceContainer();
            var references = referenceContainer
                .EnumReferences()
                .OfType<ProjectReferenceNode>();
            foreach (var reference in references)
            {
                var newReference = context.CreateReference() as IVsProjectReference;
                newReference.Identity = reference.ReferencedProjectGuid.ToString("B");
                newReference.AlreadyReferenced = true;
                context.AddReference(newReference);
            }

            return context as IVsReferenceProviderContext;
        }

        private IVsReferenceProviderContext CreateFileReferenceProviderContext(IVsReferenceManager mgr)
        {
            var context = mgr.CreateProviderContext(VSConstants.FileReferenceProvider_Guid) as IVsFileReferenceProviderContext;

            context.BrowseFilter = AddReferenceExtensions.Replace('|', '\0') + "\0";
            return context as IVsReferenceProviderContext;
        }

        private __VSREFERENCECHANGEOPERATIONRESULT AddReferences(IVsReferenceProviderContext context)
        {
            var addedReferences = this.GetAddedReferences(context);

            var referenceContainer = this.GetReferenceContainer();
            foreach (var selectorData in addedReferences)
            {
                referenceContainer.AddReferenceFromSelectorData(selectorData);
            }

            return __VSREFERENCECHANGEOPERATIONRESULT.VSREFERENCECHANGEOPERATIONRESULT_ALLOW;
        }

        protected virtual IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedReferences(IVsReferenceProviderContext context)
        {
            var addedReferences = Enumerable.Empty<VSCOMPONENTSELECTORDATA>();

            if (context.ProviderGuid == VSConstants.ProjectReferenceProvider_Guid)
            {
                addedReferences = GetAddedReferences(context as IVsProjectReferenceProviderContext);
            }
            else if (context.ProviderGuid == VSConstants.FileReferenceProvider_Guid)
            {
                addedReferences = GetAddedReferences(context as IVsFileReferenceProviderContext);
            }
            else if (context.ProviderGuid == VSConstants.AssemblyReferenceProvider_Guid)
            {
                addedReferences = GetAddedReferences(context as IVsAssemblyReferenceProviderContext);
            }
            else if (context.ProviderGuid == VSConstants.ComReferenceProvider_Guid)
            {
                addedReferences = GetAddedReferences(context as IVsComReferenceProviderContext);
            }

            return addedReferences;
        }

        private __VSREFERENCECHANGEOPERATIONRESULT RemoveReferences(IVsReferenceProviderContext context)
        {
            var removedReferences = this.GetRemovedReferences(context);

            foreach (var refNode in removedReferences)
            {
                refNode.Remove(true ); // delete from storage
            }

            return __VSREFERENCECHANGEOPERATIONRESULT.VSREFERENCECHANGEOPERATIONRESULT_ALLOW;
        }

        protected virtual IEnumerable<ReferenceNode> GetRemovedReferences(IVsReferenceProviderContext context)
        {
            var removedReferences = Enumerable.Empty<ReferenceNode>();

            if (context.ProviderGuid == VSConstants.ProjectReferenceProvider_Guid)
            {
                removedReferences = GetRemovedReferences(context as IVsProjectReferenceProviderContext);
            }
            else if (context.ProviderGuid == VSConstants.FileReferenceProvider_Guid)
            {
                removedReferences = GetRemovedReferences(context as IVsFileReferenceProviderContext);
            }

            return removedReferences;
        }

        private IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedReferences(IVsProjectReferenceProviderContext context)
        {
            var selectedReferences = context
                .References
                .OfType<IVsProjectReference>()
                .Select(reference => new VSCOMPONENTSELECTORDATA()
                {
                    type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_Project,
                    bstrTitle = reference.Name,
                    bstrFile = new FileInfo(reference.FullPath).Directory.FullName,
                    bstrProjRef = reference.ReferenceSpecification,
                });

            return selectedReferences;
        }

        private IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedReferences(IVsAssemblyReferenceProviderContext context)
        {
            var selectedReferences = context
                .References
                .OfType<IVsAssemblyReference>()
                .Select(reference => new VSCOMPONENTSELECTORDATA()
                {
                    type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_ComPlus,
                    bstrTitle = reference.Name,
                    bstrFile = reference.FullPath,
                });

            return selectedReferences;
        }
        private IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedReferences(IVsComReferenceProviderContext context)
        {
            var selectedReferences = context
                .References
                .OfType<IVsComReference>()
                .Select(reference => new VSCOMPONENTSELECTORDATA()
                {
                    type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_Com2,
                    bstrTitle = reference.Name,
                    bstrFile = reference.FullPath,
                    wFileMajorVersion = reference.MajorVersion,
                    wFileMinorVersion = reference.MinorVersion,
                    guidTypeLibrary = reference.Guid
                });

            return selectedReferences;
        }


        private IEnumerable<ReferenceNode> GetRemovedReferences(IVsProjectReferenceProviderContext context)
        {
            var selectedReferences = context
                .References
                .OfType<IVsProjectReference>()
                .Select(asmRef => new Guid(asmRef.Identity));

            var referenceContainer = this.GetReferenceContainer();
            var references = referenceContainer
                .EnumReferences()
                .OfType<ProjectReferenceNode>()
                .Where(refNode => selectedReferences.Contains(refNode.ReferencedProjectGuid));

            return references;
        }

        private IEnumerable<VSCOMPONENTSELECTORDATA> GetAddedReferences(IVsFileReferenceProviderContext context)
        {
            var selectedReferences = context
                .References
                .OfType<IVsFileReference>()
                .Select(reference => new VSCOMPONENTSELECTORDATA()
                {
                    type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_File,
                    bstrFile = reference.FullPath,
                });

            return selectedReferences;
        }

        private IEnumerable<ReferenceNode> GetRemovedReferences(IVsFileReferenceProviderContext context)
        {
            var selectedReferences = context
                .References
                .OfType<IVsFileReference>()
                .Select(fileRef => fileRef.FullPath);

            var referenceContainer = this.GetReferenceContainer();
            var references = referenceContainer
                .EnumReferences()
                .OfType<ReferenceNode>()
                .Where(refNode => selectedReferences.Contains(refNode.Url));

            return references;
        }

        protected virtual string AddReferenceExtensions
        {
            get
            {
                return "Assembly files|*.dll";
            }
        }
        #endregion
    */
    }




    class XSharpProjectCapabilitiesPresenceChecker : IVsBooleanSymbolPresenceChecker
    {

        public bool HasChangedSince(ref object versionObject)
        {
            // If your project capabilities do not change over time while the project is open,
            // you may simply `return false;` from your `HasChangedSince` method.
            return false;
        }

        public bool IsSymbolPresent(string symbol)
        {
            switch (symbol.ToLower())
            {
                case "assemblyreferences":
                case "declaredsourceitems":
                case "usersourceitems":
                case "windowsxaml":
                case "csharp":
                    return true;
            }
            return false;
        }
    }
}


