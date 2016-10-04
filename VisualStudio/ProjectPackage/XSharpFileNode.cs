//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Project;
using System.Globalization;
using Microsoft.Windows.Design.Host;


using XSharp.Project.WPF;
using XSharp.CodeDom;
using System.Diagnostics;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using System.IO;

namespace XSharp.Project
{
    /// <summary>
    /// This class extends the FileNode in order to represent a file 
    /// within the hierarchy.
    /// </summary>
    public class XSharpFileNode : FileNode
    {
        #region Fields
        private OAXSharpFileItem automationObject;
        private bool isNonMemberItem;

        #endregion

        #region Constructors
        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpFileNode"/> class.
        /// </summary>
        /// <param name="root">The project node.</param>
        /// <param name="e">The project element node.</param>
        internal XSharpFileNode(ProjectNode root, ProjectElement e)
            : base(root, e)
        {
            //
            this.UpdateHasDesigner();
            this.UpdateItemType();
        }

        #endregion

        #region Overriden implementation
        /// <summary>
        /// Gets the automation object for the file node.
        /// </summary>
        /// <returns></returns>
        public override object GetAutomationObject()
        {
            if (automationObject == null)
            {
                automationObject = new OAXSharpFileItem(this.ProjectMgr.GetAutomationObject() as OAProject, this);
            }

            return automationObject;
        }

        protected internal override DocumentManager GetDocumentManager()
        {
            return new FileDocumentManager(this);
        }


        public bool IsFormSubType
        {
            get
            {
                string result = this.ItemNode.GetMetadata(ProjectFileConstants.SubType);
                if (!String.IsNullOrEmpty(result) && string.Compare(result, ProjectFileAttributeValue.Form, true, CultureInfo.InvariantCulture) == 0)
                    return true;
                else
                    return false;
            }
        }



        /// <summary>
        /// factory method for creating single file generators.
        /// </summary>
        /// <returns></returns>
        protected override ISingleFileGenerator CreateSingleFileGenerator()
        {
            return new XSharpSingleFileGenerator(this.ProjectMgr);
        }



        protected override NodeProperties CreatePropertiesObject()
        {

            NodeProperties properties;
            if (IsLink)
            {
                properties = new XSharpLinkedFileNodeProperties(this);
            }
            else if (IsNonMemberItem)
            {
                properties = new XSharpNonMemberProperties(this);
            }
            //else if (this.IsVOBinary)
            //{
                
            //    properties = new XSharpVOBinaryFileNodeProperties(this);
            //}
            else
            {
                XSharpFileNodeProperties xprops = new XSharpFileNodeProperties(this);
                xprops.IsDependent = IsDependent;
                xprops.OnCustomToolChanged += new EventHandler<HierarchyNodeEventArgs>(OnCustomToolChanged);
                xprops.OnCustomToolNameSpaceChanged += new EventHandler<HierarchyNodeEventArgs>(OnCustomToolNameSpaceChanged);
                properties = xprops;
            }
            return properties;
        }
        #endregion

        #region Private implementation
        internal OleServiceProvider.ServiceCreatorCallback ServiceCreator
        {
            get { return new OleServiceProvider.ServiceCreatorCallback(this.CreateServices); }
        }

        private object CreateServices(Type serviceType)
        {
            object service = null;
            if (typeof(EnvDTE.ProjectItem) == serviceType)
            {
                service = GetAutomationObject();
            }
            else if (typeof(DesignerContext) == serviceType)
            {
                service = this.DesignerContext;
            }
            return service;
        }

        /// <summary>
        /// This method is used to move dependant items from the main level (1st level below project node) to
        /// and make them children of the modules they belong to.
        /// </summary>
        /// <param name="child"></param>
        /// <returns></returns>
        internal bool AddDependant(HierarchyNode child)
        {
            // If the file is not a VulcanFileNode then drop it and create a new VulcanFileNode
            XSharpFileNode dependant;
            String fileName = child.Url;
            try
            {
                child.Remove(false);
            }
            catch (Exception e)
            {
                System.Diagnostics.Trace.WriteLine(e.Message);
            }
            dependant = (XSharpFileNode)ProjectMgr.CreateDependentFileNode(fileName);

            // Like the C# project system we do not put a path in front of the parent name, even when we are in a subfolder
            string parent = this.ItemNode.GetMetadata(ProjectFileConstants.Include);
            parent = Path.GetFileName(parent);
            if (!this.IsNonMemberItem)
                dependant.ItemNode.SetMetadata(ProjectFileConstants.DependentUpon, parent);
            // Make the item a dependent item
            dependant.HasParentNodeNameRelation = true;
            // Insert in the list of children
            dependant.NextSibling = this.FirstChild;
            this.FirstChild = dependant;
            ProjectMgr.OnItemsAppended(this);
            this.OnItemAdded(this, dependant);
            // Set parent and inherit the NonMember Status
            dependant.Parent = this;
            dependant.IsDependent = true;
            dependant.IsNonMemberItem = this.IsNonMemberItem;
            return true;
        }

        private VSXSharpCodeDomProvider _codeDomProvider;
        protected internal VSXSharpCodeDomProvider CodeDomProvider
        {
            get
            {
                if (_codeDomProvider == null)
                {
                    int tabSize = 1;
                    try
                    {
                        LanguageService.XSharpLanguageService lngServ = (LanguageService.XSharpLanguageService)ProjectMgr.GetService(typeof(LanguageService.XSharpLanguageService));
                        Microsoft.VisualStudio.Package.LanguagePreferences pref = lngServ.GetLanguagePreferences();
                        tabSize = pref.TabSize;
/*
                        EnvDTE.DTE dte = (EnvDTE.DTE)ProjectMgr.GetService(typeof(EnvDTE.DTE));
                        EnvDTE.Properties props;
                        props = dte.Properties[ "Text Editor" , "XSharp"];
                        foreach (EnvDTE.Property temp in props)
                        {
                            if (temp.Name.ToLower() == "tabsize")
                            {
                                tabSize = (int)temp.Value;
                            }
                        }
*/
                    }
                    catch (Exception ex )
                    {
                        string msg = ex.Message;
                    }
                    //
                    _codeDomProvider = new VSXSharpCodeDomProvider(this);
                    XSharpCodeDomProvider.TabSize = tabSize;
                }
                return _codeDomProvider;
            }
        }


        private DesignerContext _designerContext;
        protected internal virtual DesignerContext DesignerContext
        {
            get
            {
                if (_designerContext == null)
                {
                    XSharpFileNode xsFile = Parent.FindChild(this.Url.Replace(".xaml", ".xaml.prg")) as XSharpFileNode;
                    _designerContext = new DesignerContext();
                    //Set the EventBindingProvider for this XAML file so the designer will call it
                    //when event handlers need to be generated
                    _designerContext.EventBindingProvider = new XSharpEventBindingProvider(xsFile);
                }

                return _designerContext;
            }
        }

        /// <summary>
        /// Flag that indicates if this node is not a member of the project.
        /// </summary>
        /// <value>true if the item is not a member of the project build, false otherwise.</value>
        public bool IsNonMemberItem
        {
            get
            {
                return this.isNonMemberItem;
            }
            set
            {
                this.isNonMemberItem = value;
            }
        }

        internal String GetParentName()
        {
            // There needs to be a better way to handle this
            // CS uses a table with
            // Parent extension, Allowed child extensions
            // look at HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\VisualStudio\9.0\Projects\{FAE04EC0-301F-11d3-BF4B-00C04F79EFBC}\RelatedFiles
            // .xaml .xaml.cs
            // .cs   .designer.cs, .resx
            // .xsd  .cs, .xsc, .xss, .xsx
            // .resx files (like Resources.resx) seem to be handled differently..
            // we can hard code a similar table or read it from the registry like C# does. 
            // CS also defines a 'relationtype'. See the CS Project System source code.
            String path = Path.GetFileName(this.Url);
            int relationIndex = path.IndexOf(".");
            if (IsDependent)
            {
                switch (GetFileType(this.Url))
                {
                    case XSharpFileType.Header:
                    case XSharpFileType.ManagedResource:
                        path = Path.ChangeExtension(path, ".prg");
                        break;
                    case XSharpFileType.VODBServer:
                    case XSharpFileType.VOFieldSpec:
                    case XSharpFileType.VOForm:
                    case XSharpFileType.VOIndex:
                    case XSharpFileType.VOMenu:
                    case XSharpFileType.VOOrder:
                    case XSharpFileType.NativeResource:

                        if (relationIndex < 0)
                            return string.Empty;
                        path = path.Substring(0, relationIndex) + ".prg";
                        break;
                    default:
                        if (path.EndsWith(".designer.prg"))
                        {
                            path = path.Substring(0, relationIndex) + ".prg";
                        }
                        else if (path.EndsWith(".xaml.prg"))
                        {
                            path = path.Substring(0, relationIndex) + ".prg";
                        }
                        else
                        {
                            path = string.Empty;
                        }
                        break;
                }
                if (!String.IsNullOrEmpty(path))
                {
                    String dir = Path.GetDirectoryName(this.Url);
                    if (dir.EndsWith("\\resources"))
                    {
                        dir = dir.Substring(0, dir.Length - "\\resources".Length);
                    }
                    path = dir + "\\" + path;
                }
                return path;
            }
            return null;
        }

        #endregion

        public void UpdateHasDesigner()
        {
            string itemType = GetItemType(this.FileName);
            switch (itemType) {
                case XSharpConstants.VOBinary:
                case XSharpConstants.Settings:
                case ProjectFileConstants.Resource:
                case ProjectFileConstants.Page:
                case ProjectFileConstants.ApplicationDefinition:
                    HasDesigner = true;
                    break;
                default:
                    switch(SubType) {
                        case ProjectFileAttributeValue.Component:
                        case ProjectFileAttributeValue.Form:
                        case ProjectFileAttributeValue.UserControl:
                            HasDesigner = true;
                            break;
                        default:
                            HasDesigner = false;
                            break;
                    }
                    break;
            }
        }



        private void UpdateItemType()
        {
            string ext = Path.GetExtension(this.FileName).ToLower();
            string itemType = this.ItemNode.ItemName;
            //
            if (ext == ".xaml")
            {
                if ((String.Compare(itemType, ProjectFileConstants.Page, StringComparison.OrdinalIgnoreCase) != 0)
                  && (String.Compare(itemType, ProjectFileConstants.ApplicationDefinition, StringComparison.OrdinalIgnoreCase) != 0)
                  && (String.Compare(itemType, ProjectFileConstants.Resource, StringComparison.OrdinalIgnoreCase) != 0))
                {
                    this.ItemNode.ItemName = ProjectFileConstants.Page;
                }
            }
            else if (ext == ".prg" && String.IsNullOrEmpty(itemType))
            {
                this.ItemNode.ItemName = SR.Compile;
            }
            else
            {
                var ftype = GetFileType(this.FileName);
                switch (ftype)
                {
                    case XSharpFileType.ManagedResource:
                        this.ItemNode.ItemName = ProjectFileConstants.EmbeddedResource;
                        break;
                    //case XSharpFileType.Settings:
                    //    this.ItemNode.ItemName = ProjectFileConstants.None;
                    //    break;
                    default:
                        break;
                }
                //
            }
        }
        /// <summary>
        /// Returns the SubType of an XSharp FileNode. It is 
        /// </summary>
        public string SubType
        {
            get
            {
                return ItemNode.GetMetadata(ProjectFileConstants.SubType);
            }
            set
            {
                ItemNode.SetMetadata(ProjectFileConstants.SubType, value);
                // Don't forget to update...
                UpdateHasDesigner();
            }
        }

        /// <summary>
        /// Open a file depending on the SubType property associated with the file item in the project file
        /// </summary>
        protected override void DoDefaultAction()
        {
            var manager = (FileDocumentManager)this.GetDocumentManager();
            Debug.Assert(manager != null, "Could not get the FileDocumentManager");

            Guid viewGuid;

            if(HasDesigner) {
                viewGuid = VSConstants.LOGVIEWID.Designer_guid;
            } else if(GetItemType(this.FileName) == ProjectFileConstants.Compile) {
                viewGuid = VSConstants.LOGVIEWID.Code_guid;
            } else {
                viewGuid = VSConstants.LOGVIEWID.Primary_guid;
            }


            IVsWindowFrame frame;
            manager.Open(false, false, viewGuid, out frame, WindowFrameShowAction.Show);
        }

        public override int ImageIndex
        {
            get
            {
                if (!File.Exists(this.Url))
                {
                    return (int)ProjectNode.ImageName.MissingFile;
                }
                //
                int imageIndex = RetrieveImageIndex();
                if (imageIndex == -1)
                    imageIndex = base.ImageIndex;
                return imageIndex;
            }
        }

        private int RetrieveImageIndex()
        {
            int ret = -1;
            switch (GetFileType(this.Url))
            {
                case XSharpFileType.SourceCode:
                    ret = XSharpConstants.ImageListIndex.Source;
                    //
                    if (IsFormSubType)
                    {
                        ret = (int)ProjectNode.ImageName.WindowsForm;
                        return ret;
                    }
                    break;
                case XSharpFileType.Header:
                case XSharpFileType.PreprocessorOutput:

                    ret = XSharpConstants.ImageListIndex.Source;
                    break;
                case XSharpFileType.VOForm:
                    ret = XSharpConstants.ImageListIndex.Form;
                    break;
                case XSharpFileType.VOMenu:
                    ret = XSharpConstants.ImageListIndex.Menu;
                    break;
                case XSharpFileType.VODBServer:
                    ret = XSharpConstants.ImageListIndex.Server;
                    break;
                case XSharpFileType.VOFieldSpec:
                    ret = XSharpConstants.ImageListIndex.FieldSpec;
                    break;

            }
            if (ret != -1)
            {
                // Don't forget that we have two Images bitmap,
                // so add the offset retrieved at load time in the project
                ret += XSharpProjectNode.imageOffset;
            }
            //
            return ret;
        }
        static internal XSharpFileType GetFileType(string filename)
        {
            string ext = Path.GetExtension(filename);
            switch (ext)
            {
                case ".prg":
                case ".xs":
                    return XSharpFileType.SourceCode;
                case ".vh":
                case ".xh":
                    return XSharpFileType.Header;
                case ".xsfrm":
                case ".vnfrm":
                    return XSharpFileType.VOForm;
                case ".xsmnu":
                case ".vnmnu":
                    return XSharpFileType.VOMenu;
                case ".xsdbs":
                case ".vndbs":
                    return XSharpFileType.VODBServer;
                case ".xsfs":
                case ".vnfs":
                    return XSharpFileType.VOFieldSpec;
                case ".xaml":
                    return XSharpFileType.XAML;
                case ".settings":
                    return XSharpFileType.Settings;
                case ".resx":
                    return XSharpFileType.ManagedResource;
                case ".rc":
                    return XSharpFileType.NativeResource;
                default:
                    return XSharpFileType.Unknown;

            }
        }

        ////////////////////////////////////////////////////
        ////////////////////////////////////////////////////
        ////////////////////////////////////////////////////
        ////////////////////////////////////////////////////
        ////////////////////////////////////////////////////
        ////////////////////////////////////////////////////
        /*
        public override int SetProperty(int propid, object value)
        {
            int result;
            __VSHPROPID id = (__VSHPROPID)propid;
            switch (id)
            {
                case __VSHPROPID.VSHPROPID_ItemSubType:
                    this.SubType = (string)value;
                    result = VSConstants.S_OK;
                    break;

                default:
                    result = base.SetProperty(propid, value);
                    break;
            }

            return result;
        }

        public override object GetProperty(int propId)
        {
            __VSHPROPID id = (__VSHPROPID)propId;
            switch (id)
            {
                case __VSHPROPID.VSHPROPID_ItemSubType:
                    return this.SubType;
            }

            return base.GetProperty(propId);
        }

    */

        public static string GetItemType(string file)
        {
            switch (GetFileType(file))
            {
                case XSharpFileType.SourceCode:
                    return ProjectFileConstants.Compile;
                case XSharpFileType.NativeResource:
                    return XSharpConstants.NativeResource;
                case XSharpFileType.VOForm:
                case XSharpFileType.VODBServer:
                case XSharpFileType.VOFieldSpec:
                case XSharpFileType.VOMenu:
                case XSharpFileType.VOIndex:
                case XSharpFileType.VOOrder:
                    return XSharpConstants.VOBinary;
                case XSharpFileType.ManagedResource:
                    return ProjectFileConstants.Resource;
                case XSharpFileType.XAML:
                    return ProjectFileConstants.Page;
                default:
                    return ProjectFileConstants.None;
            }
        }

    }
}
