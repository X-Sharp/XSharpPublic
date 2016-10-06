//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using Microsoft.VisualStudio.Project.Automation;
using System.Globalization;
using Microsoft.Windows.Design.Host;
using XSharp.Project.WPF;
using XSharp.CodeDom;
using System.Diagnostics;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell.Interop;
using System.IO;

namespace XSharp.Project
{
    /// <summary>
    /// This class extends the FileNode in order to represent a file
    /// within the hierarchy.
    /// </summary>
    public class XSharpFileNode : XFileNode
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
        internal XSharpFileNode(XSharpProjectNode root, ProjectElement element)
            : this(root, element, false)
        {
        }
        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpFileNode"/> class.
        /// </summary>
        /// <param name="root">The root <see cref="XSharpProjectNode"/> that contains this node.</param>
        /// <param name="element">The element that contains MSBuild properties.</param>
        /// <param name="isNonMemberItem">Flag that indicates if the file is not part of the project.</param>
        public XSharpFileNode(XSharpProjectNode root, ProjectElement element, bool isNonMemberItem)
            : base(root, element, isNonMemberItem)
        {
            this.UpdateHasDesigner();
            this.UpdateItemType();
            root.AddURL(this.Url, this);

        }

        #endregion
        #region Properties
        /// <summary>
        /// Gets an index into the default <b>ImageList</b> of the icon to show for this file.
        /// </summary>
        /// <value>An index into the default  <b>ImageList</b> of the icon to show for this file.</value>
        public override int ImageIndex
        {
            get
            {
                int ret = -1;
                if (this.IsNonMemberItem)
                {
                    ret = (int)ProjectNode.ImageName.ExcludedFile;
                }
                else if (!File.Exists(this.Url))
                {
                    ret = (int)ProjectNode.ImageName.MissingFile;
                }
                else
                {
                    switch (GetFileType(this.Url))
                    {
                        case XSharpFileType.SourceCode:
                            //
                            if (IsForm)
                            {
                                ret =  (int)ProjectNode.ImageName.WindowsForm;
                            }
                            else if (IsUserControl)
                            {
                                ret =  (int)ProjectNode.ImageName.WindowsForm;
                            }
                            else
                            {
                                ret = XSharpImageListIndex.Source + XSharpProjectNode.imageOffset;
                            }
                            break;
                        case XSharpFileType.Header:
                        case XSharpFileType.PreprocessorOutput:

                            ret = XSharpImageListIndex.Source + XSharpProjectNode.imageOffset;
                            break;
                        case XSharpFileType.VOForm:
                            ret = XSharpImageListIndex.Form + XSharpProjectNode.imageOffset;
                            break;
                        case XSharpFileType.VOMenu:
                            ret = XSharpImageListIndex.Menu + XSharpProjectNode.imageOffset;
                            break;
                        case XSharpFileType.VODBServer:
                            ret = XSharpImageListIndex.Server + XSharpProjectNode.imageOffset;
                            break;
                        case XSharpFileType.VOFieldSpec:
                            ret = XSharpImageListIndex.FieldSpec + XSharpProjectNode.imageOffset;
                            break;
                        default:
                            ret = base.ImageIndex;
                            break;

                    }
                }
                return ret;
            }
        }

        #endregion

  
        protected override int IncludeInProject()
        {
            int result = base.IncludeInProject();
            DetermineSubType();
            return result;
        }

        internal void DetermineSubType()
        {
            // Parse the contents of the file and see if we have a windows form or a windows control
            // (something that inherits from system.windows.forms.form or system.windows.forms.usercontrol
            // We should do this with proper parsing. For now we simply test the first word after the INHERIT keyword
            // and then parse and bind to see if we can find the first type in the file.
            if (GetFileType(this.Url) == XSharpFileType.SourceCode)
            {
                string SubType = "";
                string token = "INHERIT";
                string source = System.IO.File.ReadAllText(this.Url);
                int pos = source.IndexOf(token, StringComparison.OrdinalIgnoreCase);
                if (pos > 0)
                {
                    source = source.Substring(pos + token.Length, 250);
                    var words = source.Split(";\t \r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    if (words.Length > 0)
                    {
                        var parentclass = words[0].ToLower();
                        switch (parentclass)
                        {
                            case "form":
                            case "system.windows.forms.form":
                                SubType = ProjectFileAttributeValue.Form;
                                break;
                            case "usercontrol":
                            case "system.windows.forms.usercontrol":
                                SubType = ProjectFileAttributeValue.UserControl;
                                break;
                        }
                        if (SubType != null && this.ItemNode.GetMetadata(ProjectFileConstants.SubType) != SubType)
                        {
                            this.ItemNode.SetMetadata(ProjectFileConstants.SubType, SubType);
                            this.ItemNode.RefreshProperties();
                            this.UpdateHasDesigner();
                            this.ReDraw(UIHierarchyElement.Icon);
                        }
                    }
                }
            }
            return;
        }

        public void UpdateHasDesigner()
        {
            string itemType = GetItemType(this.FileName);
            switch (itemType) {
                case XSharpProjectFileConstants.VOBinary:
                case XSharpProjectFileConstants.Settings:
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
 




#region Dependent Items
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
            String path = Path.GetFileName(this.Url).ToLowerInvariant();
            int relationIndex = path.IndexOf(".");
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
        /// <summary>
        /// This method is used to move dependant items from the main level (1st level below project node) to
        /// and make them children of the modules they belong to.
        /// </summary>
        /// <param name="child"></param>
        /// <returns></returns>
        internal bool AddDependant(HierarchyNode child)
        {
            // If the file is not a XSharpFileNode then drop it and create a new XSharpFileNode
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

        #endregion
        #region ItemTypes


        internal static string GetItemType(string file)
        {
            return GetItemType(GetFileType(file));
        }
        internal static string GetItemType(XSharpFileType type)
        {
            switch (type)
            {
                case XSharpFileType.SourceCode:
                    return ProjectFileConstants.Compile;
                case XSharpFileType.NativeResource:
                    return XSharpProjectFileConstants.NativeResource;
                case XSharpFileType.VOForm:
                case XSharpFileType.VODBServer:
                case XSharpFileType.VOFieldSpec:
                case XSharpFileType.VOMenu:
                case XSharpFileType.VOIndex:
                case XSharpFileType.VOOrder:
                    return XSharpProjectFileConstants.VOBinary;
                case XSharpFileType.ManagedResource:
                    return ProjectFileConstants.Resource;
                case XSharpFileType.XAML:
                    return ProjectFileConstants.Page;
                default:
                    return ProjectFileConstants.None;
            }
        }

        internal override void SetSpecialProperties()
        {
            var type = GetFileType(this.Url);
            switch (type)
            {
                case XSharpFileType.ManagedResource:
                    this.ItemNode.SetMetadata(ProjectFileConstants.SubType, ProjectFileAttributeValue.Designer);
                    this.ItemNode.SetMetadata(ProjectFileConstants.Generator, "ResXFileCodeGenerator");
                    break;
                case XSharpFileType.Settings:
                    this.ItemNode.SetMetadata(ProjectFileConstants.Generator, "SettingsSingleFileGenerator");
                    break;
            }

        }

        private bool hasSubType(string value)
        {
            string result = SubType;
            return !String.IsNullOrEmpty(result) && String.Equals(result, value, StringComparison.OrdinalIgnoreCase) ;

        }
        public bool IsForm
        {
            get
            {
                return hasSubType(ProjectFileAttributeValue.Form);
            }
        }
        public bool IsUserControl
        {
            get
            {
                return hasSubType(ProjectFileAttributeValue.UserControl);
            }
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
        private void UpdateItemType()
        {
            string itemType = this.ItemNode.ItemName;
            var ftype = GetFileType(this.FileName);
            switch (ftype)
            {
            case XSharpFileType.XAML:
                // do not change the type when not needed
                if (String.Equals(itemType, ProjectFileConstants.Page, StringComparison.OrdinalIgnoreCase))
                {
                    break;
                }
                else if (String.Equals(itemType, ProjectFileConstants.ApplicationDefinition, StringComparison.OrdinalIgnoreCase))
                {
                    break;
                }
                else if (String.Equals(itemType, ProjectFileConstants.Resource, StringComparison.OrdinalIgnoreCase))
                {
                    break;
                }
                this.ItemNode.ItemName = ProjectFileConstants.Page;
                break;
            case XSharpFileType.SourceCode:
                if (String.IsNullOrEmpty(itemType))
                {
                    this.ItemNode.ItemName = SR.Compile;
                }
                break;
            case XSharpFileType.ManagedResource:
                if (!String.Equals(itemType, ProjectFileConstants.EmbeddedResource, StringComparison.OrdinalIgnoreCase))
                {
                    this.ItemNode.ItemName = ProjectFileConstants.EmbeddedResource;
                }
                break;
            //case XSharpFileType.Settings:
            //    this.ItemNode.ItemName = ProjectFileConstants.None;
            //    break;
            default:
                break;
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

		#endregion

		#region Code Generation and Code Parsing
        /// <summary>
        /// factory method for creating single file generators.
        /// </summary>
        /// <returns></returns>
        protected override ISingleFileGenerator CreateSingleFileGenerator()
        {
            return new XSharpSingleFileGenerator(this.ProjectMgr);
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

        #region Overriden implementation
        /// <summary>
        /// Creates an object derived from <see cref="NodeProperties"/> that will be used to expose
        /// properties specific for this object to the property browser.
        /// </summary>
        /// <returns>A new <see cref="WixFileNodeProperties"/> object.</returns>
        protected override NodeProperties CreatePropertiesObject()
        {
            if (IsNonMemberItem)
            {
                return new XSharpFileNodeNonMemberProperties(this);
            }
            else if (!String.IsNullOrEmpty(this.ItemNode.GetMetadata("Link")))
            {
                return new XSharpLinkedFileNodeProperties(this);
            }
            //else if (this.IsVOBinary)
            //{

            //    return new XSharpVOBinaryFileNodeProperties(this);
            //}
            else
            {
                XSharpFileNodeProperties xprops = new XSharpFileNodeProperties(this);
                xprops.IsDependent = IsDependent;
                xprops.OnCustomToolChanged += new EventHandler<HierarchyNodeEventArgs>(OnCustomToolChanged);
                xprops.OnCustomToolNameSpaceChanged += new EventHandler<HierarchyNodeEventArgs>(OnCustomToolNameSpaceChanged);
                return xprops;
            }
        }


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
        protected override void Dispose(bool disposing)
        {
            if (this.ProjectMgr is XSharpProjectNode)
            {
                XSharpProjectNode projectNode = (XSharpProjectNode)this.ProjectMgr;
                if (projectNode != null)
                    projectNode.RemoveURL(this);
            }
            base.Dispose(disposing);
        }

        /// <summary>
        /// Open a file depending on the SubType property associated with the file item in the project file
        /// </summary>
        protected override void DoDefaultAction()
        {
            var manager = (FileDocumentManager)this.GetDocumentManager();
            Debug.Assert(manager != null, "Could not get the FileDocumentManager");

            Guid viewGuid;

            if (HasDesigner)
            {
                viewGuid = VSConstants.LOGVIEWID.Designer_guid;
            }
            else if (GetItemType(this.FileName) == ProjectFileConstants.Compile)
            {
                viewGuid = VSConstants.LOGVIEWID.Code_guid;
            }
            else
            {
                viewGuid = VSConstants.LOGVIEWID.Primary_guid;
            }


            IVsWindowFrame frame;
            manager.Open(false, false, viewGuid, out frame, WindowFrameShowAction.Show);
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
        #endregion
    }
}
