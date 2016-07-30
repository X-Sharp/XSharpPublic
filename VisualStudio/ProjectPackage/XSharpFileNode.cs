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

        protected override NodeProperties CreatePropertiesObject()
        {
            XSharpFileNodeProperties properties = new XSharpFileNodeProperties(this);
            properties.OnCustomToolChanged += new EventHandler<HierarchyNodeEventArgs>(OnCustomToolChanged);
            properties.OnCustomToolNameSpaceChanged += new EventHandler<HierarchyNodeEventArgs>(OnCustomToolNameSpaceChanged);
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
            //
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
                viewGuid = VSConstants.LOGVIEWID_Designer;
            } else if(GetItemType(this.FileName) == ProjectFileConstants.Compile) {
                viewGuid = VSConstants.LOGVIEWID_Code;
            } else {
                viewGuid = VSConstants.LOGVIEWID_Primary;
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
                case XSharpFileType.Settings:
                    return XSharpConstants.Settings;
                case XSharpFileType.XAML:
                    return ProjectFileConstants.Page;
                default:
                    return ProjectFileConstants.None;
            }
        }

    }
}
