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

namespace XSharp.Project
{
    /// <summary>
    /// This class extends the ProjectNode in order to represent our project 
    /// within the hierarchy.
    /// </summary>
    [Guid("F1A46976-964A-4A1E-955D-E05F5DB8651F")]
    public class XSharpProjectNode : ProjectNode
    {
        #region Enum for image list
        internal enum XSharpProjectImageName
        {
            Project = 0,
        }
        #endregion

        #region Constants
        internal const string ProjectTypeName = "XSharp";
        #endregion

        #region Fields
        private XSharpProjectPackage package;
        internal static int imageOffset;
        private static ImageList imageList;
        private VSLangProj.VSProject vsProject;

        //private Microsoft.VisualStudio.Designer.Interfaces.IVSMDCodeDomProvider codeDomProvider;
        #endregion

        #region Constructors
        /// <summary>
        /// Initializes the <see cref="XSharpProjectNode"/> class.
        /// </summary>
        static XSharpProjectNode()
        {
            imageList = Utilities.GetImageList(typeof(XSharpProjectNode).Assembly.GetManifestResourceStream("XSharp.Project.Resources.XSharpProjectImageList.bmp"));
        }

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
        protected override void SetOutputLogger(IVsOutputWindowPane output)
        {
            //base.SetOutputLogger(output);
            // Create our logger, if it was not specified
            if (BuildLogger == null)
            {
                // Because we may be aggregated, we need to make sure to get the outer IVsHierarchy
                IntPtr unknown = IntPtr.Zero;
                IVsHierarchy hierarchy = null;
                try
                {
                    unknown = Marshal.GetIUnknownForObject(this);
                    hierarchy = Marshal.GetTypedObjectForIUnknown(unknown, typeof(IVsHierarchy)) as IVsHierarchy;
                }
                finally
                {
                    if (unknown != IntPtr.Zero)
                        Marshal.Release(unknown);
                }
                // Create the logger
                BuildLogger = new XSharpIDEBuildLogger(output, this.TaskProvider, hierarchy);
                //BuildLogger = new IDEBuildLogger(output, this.TaskProvider, hierarchy);

                // To retrieve the verbosity level, the build logger depends on the registry root 
                // (otherwise it will used an hard coded default)
                ILocalRegistry2 registry = this.GetService(typeof(SLocalRegistry)) as ILocalRegistry2;
                if (null != registry)
                {
                    string registryRoot;
                    registry.GetLocalRegistryRoot(out registryRoot);
                    XSharpIDEBuildLogger logger = this.BuildLogger as XSharpIDEBuildLogger;
                    //IDEBuildLogger logger = this.BuildLogger as IDEBuildLogger;
                    if (!String.IsNullOrEmpty(registryRoot) && (null != logger))
                    {
                        logger.BuildVerbosityRegistryRoot = registryRoot;
                        logger.ErrorString = this.ErrorString;
                        logger.WarningString = this.WarningString;
                    }
                }
            }
            else
            {
                ((XSharpIDEBuildLogger)this.BuildLogger).OutputWindowPane = output;
            }

            if (BuildEngine != null)
            {
                BuildEngine.UnregisterAllLoggers();
                BuildEngine.RegisterLogger(BuildLogger);
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
            if (item == null)
            {
                throw new ArgumentNullException("item");
            }

            XSharpFileNode node = new XSharpFileNode(this, item);

            var provider = node.OleServiceProvider;

            // Use the CreateServices of the Project
            provider.AddService(typeof(EnvDTE.Project), new OleServiceProvider.ServiceCreatorCallback(this.CreateServices), false);
            // Use the CreateServices of the Node
            provider.AddService(typeof(ProjectItem), node.ServiceCreator, false);
            // Use the CreateServices of the Project
            provider.AddService(typeof(VSProject), new OleServiceProvider.ServiceCreatorCallback(this.CreateServices), false);
            //provider.AddService(typeof(VSProject), this.VSProject, false);

            string include = item.GetMetadata(ProjectFileConstants.Include);
            if (!string.IsNullOrEmpty(include) && Path.GetExtension(include).Equals(".xaml", StringComparison.OrdinalIgnoreCase))
            {
                //Create a DesignerContext for the XAML designer for this file
                // Use the CreateServices of the Node
                provider.AddService(typeof(DesignerContext), node.ServiceCreator, false);
            }

            if (node.IsFormSubType)
            {
                // Use the CreateServices of the Node
                provider.AddService(typeof(DesignerContext), node.ServiceCreator, false);
            }

            if (this.IsCodeFile(include) && item.ItemName == "Compile")
                provider.AddService(typeof(SVSMDCodeDomProvider), new XSharpVSMDProvider(node), false);


            return node;
        }

        /// <summary>
        /// Create dependent file node based on an msbuild item
        /// </summary>
        /// <param name="item">msbuild item</param>
        /// <returns>dependent file node</returns>
        public override XSharpDependentFileNode CreateDependentFileNode(ProjectElement item)
        {
            XSharpDependentFileNode newNode = new XSharpDependentFileNode(this, item);
            string include = item.GetMetadata(ProjectFileConstants.Include);

            var provider = newNode.OleServiceProvider;

            provider.AddService(typeof(EnvDTE.Project), ProjectMgr.GetAutomationObject(), false);
            provider.AddService(typeof(EnvDTE.ProjectItem), newNode.GetAutomationObject(), false);
            provider.AddService(typeof(VSLangProj.VSProject), this.VSProject, false);

            if (IsCodeFile(include) && item.ItemName == "Compile")
                newNode.OleServiceProvider.AddService(typeof(SVSMDCodeDomProvider),
                    new XSharpVSMDProvider(newNode), false);

            return newNode;
        }

        private Guid guidPublishPage = Guid.Parse("CC4014F5-B18D-439C-9352-F99D984CCA85");
        private Guid guidSignPagePage = Guid.Parse("1e78f8db-6c07-4d61-a18f-7514010abd56");

        /// <summary>
        /// Generate new Guid value and update it with GeneralPropertyPage GUID.
        /// This is the Common Properties Dialog
        /// </summary>
        /// <returns>Returns the property pages that are independent of configuration.</returns>
        protected override Guid[] GetConfigurationIndependentPropertyPages()
        {
            Guid[] result = new Guid[]
                {
                typeof(XSharpGeneralPropertyPage).GUID,
                typeof(XSharpLanguagePropertyPage).GUID,
                guidSignPagePage
                };
            return result;
        }

        // These pages have a content which depends on the Configuration Release/Debug
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
        /// Overriding to provide project general property page.
        /// </summary>
        /// <returns>Returns the GeneralPropertyPage GUID value.</returns>
        protected override Guid[] GetPriorityProjectDesignerPages()
        {
            Guid[] result = new Guid[]
                {
                typeof(XSharpGeneralPropertyPage).GUID,
                typeof(XSharpLanguagePropertyPage).GUID,
                typeof(XSharpBuildPropertyPage).GUID,
                typeof(XSharpDebugPropertyPage).GUID,
                guidSignPagePage
                };
            return result;
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

        protected override void AddNewFileNodeToHierarchy(HierarchyNode parentNode, string fileName)
        {
            // We have to take care of Dependant Files here
            // So any .Designer.prg, or .Xaml.Prg is depending from a parent which has the same prefix name
            // then we must set that parent as parentNode;
            Dictionary<string, string> Dependencies = new Dictionary<string, string>();
            Dependencies.Add(".designer.prg", ".prg");
            Dependencies.Add(".xaml.prg", ".xaml");
            // Check if we can find the Parent
            int dotPos = fileName.IndexOf(".");
            string parentFile = fileName.Substring(0, dotPos);
            string extension = fileName.Substring(dotPos).ToLower();
            //
            if ( Dependencies.ContainsKey(extension) )
            {
                // 
                HierarchyNode newParent = parentNode.FindChild(parentFile + Dependencies[extension]);
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
                //
            }

            base.AddNewFileNodeToHierarchy(parentNode, fileName);
        }

        protected override Microsoft.VisualStudio.Project.ProjectElement AddFileToMsBuild(string file) {
            ProjectElement newItem;

            string itemPath = PackageUtilities.MakeRelativeIfRooted(file, this.BaseURI);
            Debug.Assert(!Path.IsPathRooted(itemPath), "Cannot add item with full path.");

            string ext = Path.GetExtension(file);

            if(IsCodeFile(itemPath)) {
                newItem = this.CreateMsBuildFileItem(itemPath, ProjectFileConstants.Compile);
                // HACK
                //newItem.SetMetadata(ProjectFileConstants.SubType, ProjectFileAttributeValue.Form);
            } else if(this.IsEmbeddedResource(itemPath)) {
                newItem = this.CreateMsBuildFileItem(itemPath, ProjectFileConstants.EmbeddedResource);
                newItem.SetMetadata(ProjectFileConstants.SubType, ProjectFileAttributeValue.Designer);
                newItem.SetMetadata(ProjectFileConstants.Generator, "ResXFileCodeGenerator");
            } else if(this.IsSettings(itemPath)) {
                newItem = this.CreateMsBuildFileItem(itemPath, ProjectFileConstants.None);
                newItem.SetMetadata(ProjectFileConstants.Generator, "SettingsSingleFileGenerator");
            } else if(this.IsXaml(itemPath)) {
                newItem = this.CreateMsBuildFileItem(itemPath, ProjectFileConstants.Page);
            } else {
                newItem = this.CreateMsBuildFileItem(itemPath, ProjectFileConstants.None);
                //newItem.SetMetadata(ProjectFileConstants.SubType, ProjectFileConstants.Content);
            }
            return newItem;
        }

        public bool IsSettings(string fileName)
        {
            if (String.Compare(Path.GetExtension(fileName), ".settings", StringComparison.OrdinalIgnoreCase) == 0)
                return true;
            return false;
        }
        public bool IsXaml(string fileName) {
            if(String.Compare(Path.GetExtension(fileName), ".xaml", StringComparison.OrdinalIgnoreCase) == 0)
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
            if (String.Compare(type, ProjectFileConstants.Page, StringComparison.OrdinalIgnoreCase) == 0          // xaml page/window
                || String.Compare(type, ProjectFileConstants.ApplicationDefinition, StringComparison.OrdinalIgnoreCase) == 0     // xaml application definition
                || String.Compare(type, XSharpConstants.NativeResource, StringComparison.OrdinalIgnoreCase) == 0           // rc file
                || String.Compare(type, XSharpConstants.VOBinary, StringComparison.OrdinalIgnoreCase) == 0           // vobinary file
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
            return new ProjectNodeProperties(this);
        }


        #endregion


    }
}
