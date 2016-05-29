//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System;
using Microsoft.VisualStudioTools.Project.Automation;
using Microsoft.VisualStudioTools.Project;
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
    internal class XSharpFileNode : FileNode
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
        }
        #endregion

        #region Overriden implementation
        /// <summary>
        /// Gets the automation object for the file node.
        /// </summary>
        /// <returns></returns>
        public override object GetAutomationObject()
        {
            if(automationObject == null)
            {
                automationObject = new OAXSharpFileItem(this.ProjectMgr.GetAutomationObject() as OAProject, this);
            }

            return automationObject;
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
            if(typeof(EnvDTE.ProjectItem) == serviceType)
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
                    _codeDomProvider = new VSXSharpCodeDomProvider( this );
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
                    XSharpFileNode xsFile = Parent.FindChild( this.Url.Replace(".xaml", ".xaml.prg") ) as XSharpFileNode;
                    _designerContext = new DesignerContext();
                    //Set the EventBindingProvider for this XAML file so the designer will call it
                    //when event handlers need to be generated
                    _designerContext.EventBindingProvider = new XSharpEventBindingProvider( xsFile );
                }

                return _designerContext;
            }
        }


        #endregion

        public void UpdateHasDesigner()
        {
            switch (SubType)
            {
                case ProjectFileAttributeValue.Component:
                case ProjectFileAttributeValue.Form:
                case ProjectFileAttributeValue.UserControl:
                    HasDesigner = true;
                    break;
                default:
                    HasDesigner = false;
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

        /// <summary>
        /// Open a file depending on the SubType property associated with the file item in the project file
        /// </summary>
        protected override void DoDefaultAction()
        {
            var manager = (FileDocumentManager)this.GetDocumentManager();
            Debug.Assert(manager != null, "Could not get the FileDocumentManager");

            Guid viewGuid = HasDesigner ? VSConstants.LOGVIEWID_Designer : VSConstants.LOGVIEWID_Code;

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
            string ext = Path.GetExtension(this.Url);
            switch (ext)
            {
                case ".prg":
                case ".xs":
                    ret = XSharpConstants.ImageListIndex.Source;
                    //
                    if (IsFormSubType)
                    {
                        ret = (int)ProjectNode.ImageName.WindowsForm;
                        return ret;
                    }
                    break;
                case ".vh":
                case ".xh":
                case ".ppo":
                    ret = XSharpConstants.ImageListIndex.Source;
                    break;
            }
            //
            if (ret != -1)
            {
                // Don't forget that we have two Images bitmap,
                // so add the offset retrieved at load time in the project
                ret += XSharpProjectNode.imageOffset;
            }
            //
            return ret;
        }
    }
}
