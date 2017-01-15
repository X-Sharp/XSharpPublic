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
namespace XSharp.Project
{
    /// <summary>
    /// This class extends the ProjectNode in order to represent our project
    /// within the hierarchy.
    /// </summary>
    [Guid("F1A46976-964A-4A1E-955D-E05F5DB8651F")]
    public class XSharpProjectNode : XProjectNode, IVsSingleFileGeneratorFactory
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


        /// <summary>
        /// Provide mapping from our browse objects and automation objects to our CATIDs
        /// CATID (Category ID) objects are used to extend the properties that appear in the Properties window for projects and project items.
        /// </summary>
        private void InitializeCATIDs()
        {
            // The following properties classes are specific to Nemerle so we can use their GUIDs directly
            //
            AddCATIDMapping(typeof(XSharpProjectNodeProperties), typeof(XSharpProjectNodeProperties).GUID);
            AddCATIDMapping(typeof(XSharpFileNodeProperties), typeof(XSharpFileNodeProperties).GUID);
            //AddCATIDMapping(typeof(NemerleOAFileItem), typeof(NemerleOAFileItem).GUID);

            //// The following are not specific and as such we need a separate GUID
            //// (we simply used guidgen.exe to create new guids)
            ////
            //AddCATIDMapping(typeof(FolderNodeProperties), new Guid(NemerleConstants.FolderNodePropertiesGuidString));

            // This one we use the same as file nodes since both refer to files
            AddCATIDMapping(typeof(FileNodeProperties), typeof(XSharpFileNodeProperties).GUID);

            // Because our property page pass itself as the object to display in its grid,
            // we need to make it have the same CATID as the browse object of the project node
            // so that filtering is possible.
            AddCATIDMapping(typeof(XSharpGeneralPropertyPage), typeof(XSharpGeneralPropertyPage).GUID);

            //// We could also provide CATIDs for references and the references container node, if we wanted to.
            AddCATIDMapping(typeof(XSharpBuildPropertyPage), typeof(XSharpBuildPropertyPage).GUID);
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
                    vsProject = new OAVSProject(this);
                }

                return vsProject;
            }
        }

        // Gets the output file name depending on current OutputType.
        // View GeneralProperyPage
        public string OutputFile
        {
            get
            {
                string assemblyName = this.ProjectMgr.GetProjectProperty("AssemblyName", true);
                string outputTypeAsString = this.ProjectMgr.GetProjectProperty("OutputType", false);
                OutputType outputType = (OutputType)Enum.Parse(typeof(OutputType), outputTypeAsString);
                if (outputType == OutputType.Library)
                {
                    assemblyName += ".dll";
                }
                else
                {
                    assemblyName += ".dll";
                }
                return assemblyName;
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

        public override object GetProperty(int propId)
        {
            if (propId == (int) __VSHPROPID.VSHPROPID_DefaultNamespace)
            {
                return GetProjectProperty(ProjectFileConstants.RootNamespace, true);
            }

            if (propId == (int)__VSHPROPID2.VSHPROPID_DesignerHiddenCodeGeneration)
            {
                return __VSDESIGNER_HIDDENCODEGENERATION.VSDHCG_Declarations | __VSDESIGNER_HIDDENCODEGENERATION.VSDHCG_InitMethods;
            }
            switch ((__VSHPROPID3)propId)
            {
                case __VSHPROPID3.VSHPROPID_WebReferenceSupported:
                case __VSHPROPID3.VSHPROPID_ServiceReferenceSupported:
                case __VSHPROPID3.VSHPROPID_SupportsHierarchicalUpdate:
                case __VSHPROPID3.VSHPROPID_SupportsLinqOverDataSet:
                case __VSHPROPID3.VSHPROPID_SupportsNTierDesigner:
                    return true;

            }
            return base.GetProperty(propId);
        }


        /// <summary>
        /// Returns an automation object representing this node
        /// </summary>
        /// <returns>The automation object</returns>
        public override object GetAutomationObject()
        {
            return new OAXSharpProject(this);
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
            FolderNode folderNode = new XSharpFolderNode(this, path, element, element.IsVirtual);
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
            if (newNode.FileType == XSharpFileType.ManagedResource)
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
                typeof(XSharpLanguagePropertyPage).GUID
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
            };
            return result;
        }
        /// <summary>
        /// Allows you to query the project for special files and optionally create them.
        /// </summary>
        /// <param name="fileId">__PSFFILEID of the file</param>
        /// <param name="flags">__PSFFLAGS flags for the file</param>
        /// <param name="itemid">The itemid of the node in the hierarchy</param>
        /// <param name="fileName">The file name of the special file.</param>
        /// <returns></returns>
        public override int GetFile(int fileId, uint flags, out uint itemid, out string fileName) {
            bool fCreateInPropertiesFolder = false;

            switch(fileId) {
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
                    return base.GetFile(fileId, flags, out itemid, out fileName);
            }

            if(fCreateInPropertiesFolder) {
                string sPropFolder = ProjectFolder + "\\Properties";
                if(!System.IO.Directory.Exists(sPropFolder)){
                    System.IO.Directory.CreateDirectory(sPropFolder);
                }
                fileName = "Properties\\" + fileName;
            }

            HierarchyNode fileNode = FindChild(fileName);
            string fullPath = Path.Combine(ProjectFolder, fileName);
            if (fCreateInPropertiesFolder) {
                fullPath = Path.Combine(ProjectFolder , fileName);
                }

            if(fileNode == null && (flags & (uint)__PSFFLAGS.PSFF_CreateIfNotExist) != 0) {
                // Create a zero-length file if does not exist already.
                //
                if(!File.Exists(fullPath))
                    File.WriteAllText(fullPath, string.Empty);

                fileNode = CreateFileNode(fileName);
                if (fCreateInPropertiesFolder) {
                    var PropsFolder = FindChild("Properties");
                    if(PropsFolder == null) {
                        PropsFolder = CreateFolderNode("Properties");
                        AddChild(PropsFolder);
                    }
                    PropsFolder.AddChild(fileNode);
                }
                else {
                    AddChild(fileNode);
                }
            }

            itemid = fileNode != null ? fileNode.ID : 0;
             if((flags & (uint)__PSFFLAGS.PSFF_FullPath) != 0)
                fileName = fullPath;

            return VSConstants.S_OK;
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

            string dependentOf = item.GetMetadataValue(ProjectFileConstants.DependentUpon);
            // strip path when it is available
            if (dependentOf.IndexOf(System.IO.Path.DirectorySeparatorChar) >= 0)
            {
                // strip path out of dependendentUpon property
                dependentOf = System.IO.Path.GetFileName(dependentOf);
                item.SetMetadataValue(ProjectFileConstants.DependentUpon, dependentOf);
                SetProjectFileDirty(true);
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
                path = System.IO.Path.GetDirectoryName(path);
                path = Path.Combine(path, dependentOf);

                this.ParseCanonicalName(path, out parentItemID);
                if (parentItemID != (uint)VSConstants.VSITEMID.Nil)
                    parent = this.NodeFromItemId(parentItemID);
                //Debug.Assert(parent != null, "File dependent upon a non existing item or circular dependency. Ignoring the DependentUpon metadata");
                if (parent == null)
                {
                    string message = $"Cannot set dependency from \"{item.EvaluatedInclude}\" to \"{dependentOf}\"\r\nCannot find \"{dependentOf}\" in the project hierarchy";
                    string title = string.Empty;
                    OLEMSGICON icon = OLEMSGICON.OLEMSGICON_CRITICAL;
                    OLEMSGBUTTON buttons = OLEMSGBUTTON.OLEMSGBUTTON_OK;
                    OLEMSGDEFBUTTON defaultButton = OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST;

                    VsShellUtilities.ShowMessageBox(this.Site, message, title, icon, buttons, defaultButton);
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
            int dotPos = fileName.IndexOf(".");
            string parentFile = fileName.Substring(0, dotPos);
            string extension = fileName.Substring(dotPos).ToLower();
            //
            if ( dependencies.ContainsKey(extension) )
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
            switch (XFileType.GetFileType(fileName))
            {
                case XSharpFileType.Header:
                case XSharpFileType.NativeResource:
                case XSharpFileType.VOFieldSpec:
                case XSharpFileType.VOForm:
                case XSharpFileType.VODBServer:
                case XSharpFileType.VOMenu:
                case XSharpFileType.VOOrder:
                case XSharpFileType.VOIndex:
                    // dependent file
                    HierarchyNode newParent = parentNode.FindChild(parentFile + ".prg");
                    if (newParent != null)
                    {
                        parentNode = newParent;
                    }
                    break;
            }
            var newNode = base.AddNewFileNodeToHierarchyCore(parentNode, fileName,linkPath);
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
            string itemType = XFileType.GetItemType(itemPath);
            return this.CreateMsBuildFileItem(itemPath, itemType);
        }

        /// <summary>
        /// Determines whether the given file is a resource file (resx file).
        /// </summary>
        /// <param name="fileName">Name of the file to be evaluated.</param>
        /// <returns>true if the file is a resx file, otherwise false.</returns>
        public override bool IsEmbeddedResource(string fileName)
        {
            if (XFileType.GetFileType(fileName) == XSharpFileType.ManagedResource)
                return true;
            return false;
        }

        public bool IsVoBinary(string fileName) {
            return XFileType.IsVoBinary(fileName);
        }


        /// <summary>
        /// Evaluates if a file is an XSharp code file based on is extension
        /// </summary>
        /// <param name="strFileName">The filename to be evaluated</param>
        /// <returns>true if is a code file</returns>
        public override bool IsCodeFile(string strFileName)
        {
            // Don't check errors here
            if (string.IsNullOrEmpty(strFileName))
                return false;

            string ext = Path.GetExtension(strFileName);
            return
                string.Compare(ext, XSharpConstants.FileExtension1, StringComparison.OrdinalIgnoreCase) == 0 ||
                string.Compare(ext, XSharpConstants.FileExtension2, StringComparison.OrdinalIgnoreCase) == 0;
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
                || String.Compare(type, ProjectFileConstants.EmbeddedResource, StringComparison.OrdinalIgnoreCase) == 0  // resx
                || String.Compare(type, ProjectFileConstants.Page, StringComparison.OrdinalIgnoreCase) == 0          // xaml page/window
                || String.Compare(type, ProjectFileConstants.ApplicationDefinition, StringComparison.OrdinalIgnoreCase) == 0     // xaml application definition
                || String.Compare(type, XSharpProjectFileConstants.NativeResource, StringComparison.OrdinalIgnoreCase) == 0           // rc file
                || String.Compare(type, XSharpProjectFileConstants.VOBinary, StringComparison.OrdinalIgnoreCase) == 0           // vobinary file
                ) {
                return true;
            }

            // we don't know about this type, ask the base class.
            return base.IsItemTypeFileType( type );
        }


        public override void Load(string filename, string location, string name, uint flags, ref Guid iidProject, out int canceled)
        {
            base.Load(filename, location, name, flags, ref iidProject, out canceled);
            // WAP ask the designer service for the CodeDomProvider corresponding to the project node.
            this.OleServiceProvider.AddService(typeof(SVSMDCodeDomProvider), new OleServiceProvider.ServiceCreatorCallback(this.CreateServices), false);
            this.OleServiceProvider.AddService(typeof(System.CodeDom.Compiler.CodeDomProvider), new OleServiceProvider.ServiceCreatorCallback(this.CreateServices), false);

            //IPythonLibraryManager libraryManager = Site.GetService(typeof(IPythonLibraryManager)) as IPythonLibraryManager;
            //if (null != libraryManager)
            //{
            //    libraryManager.RegisterHierarchy(this.InteropSafeHierarchy);
            //}

            //If this is a WPFFlavor-ed project, then add a project-level DesignerContext service to provide
            //event handler generation (EventBindingProvider) for the XAML designer.
            this.OleServiceProvider.AddService(typeof(DesignerContext), new OleServiceProvider.ServiceCreatorCallback(this.CreateServices), false);

        }
        #endregion

        #region Private implementation
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
        protected override  ReferenceContainerNode CreateReferenceContainerNode()
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


        //private XSharpVSMDProvider codeDomProvider;
        ///// Retrieve the CodeDOM provider
        ///// </summary>
        //public Microsoft.VisualStudio.Designer.Interfaces.IVSMDCodeDomProvider CodeDomProvider
        //{
        //    get
        //    {
        //        if (codeDomProvider == null)
        //            codeDomProvider = new XSharpVSMDProvider( );
        //        return codeDomProvider;
        //    }
        //}

        protected override NodeProperties CreatePropertiesObject()
        {
            //return new XSharpProjectNodeProperties( this );
            return new XSharpProjectNodeProperties(this);
        }
        /*
               /// <summary>
               ///  IVsProjectSpecificEditorMap2 interface
               /// </summary>
               /// <param name="pszMkDocument"></param>
               /// <param name="pguidEditorType"></param>
               /// <returns></returns>
                public int GetSpecificEditorType(string pszMkDocument, out Guid pguidEditorType)
                {
                    switch (XSharpFileNode.GetFileType(pszMkDocument))
                    {
                        case XSharpFileType.VODBServer:
                            pguidEditorType = GuidStrings.guidVOServerEditorFactory;
                            break;
                        case XSharpFileType.VOFieldSpec:
                            pguidEditorType = GuidStrings.guidVOFieldSpecEditorFactory;
                            break;
                        case XSharpFileType.VOMenu:
                            pguidEditorType = GuidStrings.guidVOMenuEditorFactory;
                            break;
                        case XSharpFileType.VOForm:
                            pguidEditorType = GuidStrings.guidVOFormEditorFactory;
                            break;
                        case XSharpFileType.SourceCode:
                        case XSharpFileType.Header:
                        case XSharpFileType.PreprocessorOutput:
                            pguidEditorType = GuidStrings.guidSourcecodeEditorFactory;
                            break;
                        default:
                            pguidEditorType = Guid.Empty;
                            break;
                    }
                    return VSConstants.S_OK;
                }

                public int GetSpecificLanguageService(string pszMkDocument, out Guid pguidLanguageService)
                {
                    pguidLanguageService = Guid.Empty;
                    return VSConstants.S_OK;
                }

                public int GetSpecificEditorProperty(string pszMkDocument, int propid, out object pvar)
                {
                    pvar = true;
                    switch (XSharpFileNode.GetFileType(pszMkDocument))
                    {
                        case XSharpFileType.VODBServer:
                        case XSharpFileType.VOFieldSpec:
                        case XSharpFileType.VOMenu:
                        case XSharpFileType.VOForm:
                        case XSharpFileType.SourceCode:
                        case XSharpFileType.Header:
                        case XSharpFileType.PreprocessorOutput:
                        default:
                            if (propid == (int)__VSPSEPROPID.VSPSEPROPID_ProjectDefaultEditorName)
                            {
                                pvar = "XSharp Editor";
                            }
                            else if (propid == (int)__VSPSEPROPID.VSPSEPROPID_UseGlobalEditorByDefault)
                            {
                                pvar = false;
                            }
                            else
                            {
                                pvar = false;
                            }
                            break;
                    }
                    return VSConstants.S_OK;
                }

                public int SetSpecificEditorProperty(string pszMkDocument, int propid, object var)
                {
                    switch (XSharpFileNode.GetFileType(pszMkDocument))
                    {
                        case XSharpFileType.VODBServer:
                        case XSharpFileType.VOFieldSpec:
                        case XSharpFileType.VOMenu:
                        case XSharpFileType.VOForm:
                        case XSharpFileType.SourceCode:
                        case XSharpFileType.Header:
                        case XSharpFileType.PreprocessorOutput:
                        default:
                            break;
                    }
                    return VSConstants.S_OK;
                }

        */
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
            List< Tuple<XSharpFileNode,String,String>> FilesToMove = new List<Tuple<XSharpFileNode, String, String>>();
            foreach (KeyValuePair<string, HierarchyNode> pair in URLNodes)
            {
                XSharpFileNode vnode = pair.Value as XSharpFileNode;
                if (vnode != null)
                {
                    string parent = vnode.GetParentName();
                    if (! String.IsNullOrEmpty(parent))
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


        #region TableManager
        //internal ITableManagerProvider tableManagerProvider { get; private set; }
        ITableManager errorListTableManager;
        ErrorListProvider errorlistProvider;
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
            if (errorlistProvider != null)
            {
                errorListTableManager = null;
                errorlistProvider = null;
            }

            errorListTableManager = errorList.TableControl.Manager;
            errorlistProvider = new ErrorListProvider(errorListTableManager);
            logger.ErrorlistProvider = errorlistProvider;
            return logger;
        }
        public override int Close()
        {
            var res = base.Close();
            if (logger != null)
            {
                logger.Clear();
            }
            if (errorlistProvider != null)
            {
                errorlistProvider.Clear();
            }
            return res;
        }
    }

        #endregion
    }

