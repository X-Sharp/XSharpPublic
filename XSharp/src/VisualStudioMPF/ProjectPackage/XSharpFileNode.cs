/********************************************************************************************

Copyright (c) Microsoft Corporation 
All rights reserved. 

Microsoft Public License: 

This license governs use of the accompanying software. If you use the software, you 
accept this license. If you do not accept the license, do not use the software. 

1. Definitions 
The terms "reproduce," "reproduction," "derivative works," and "distribution" have the 
same meaning here as under U.S. copyright law. 
A "contribution" is the original software, or any additions or changes to the software. 
A "contributor" is any person that distributes its contribution under this license. 
"Licensed patents" are a contributor's patent claims that read directly on its contribution. 

2. Grant of Rights 
(A) Copyright Grant- Subject to the terms of this license, including the license conditions 
and limitations in section 3, each contributor grants you a non-exclusive, worldwide, 
royalty-free copyright license to reproduce its contribution, prepare derivative works of 
its contribution, and distribute its contribution or any derivative works that you create. 
(B) Patent Grant- Subject to the terms of this license, including the license conditions 
and limitations in section 3, each contributor grants you a non-exclusive, worldwide, 
royalty-free license under its licensed patents to make, have made, use, sell, offer for 
sale, import, and/or otherwise dispose of its contribution in the software or derivative 
works of the contribution in the software. 

3. Conditions and Limitations 
(A) No Trademark License- This license does not grant you rights to use any contributors' 
name, logo, or trademarks. 
(B) If you bring a patent claim against any contributor over patents that you claim are 
infringed by the software, your patent license from such contributor to the software ends 
automatically. 
(C) If you distribute any portion of the software, you must retain all copyright, patent, 
trademark, and attribution notices that are present in the software. 
(D) If you distribute any portion of the software in source code form, you may do so only 
under this license by including a complete copy of this license with your distribution. 
If you distribute any portion of the software in compiled or object code form, you may only 
do so under a license that complies with this license. 
(E) The software is licensed "as-is." You bear the risk of using it. The contributors give 
no express warranties, guarantees or conditions. You may have additional consumer rights 
under your local laws which this license cannot change. To the extent permitted under your 
local laws, the contributors exclude the implied warranties of merchantability, fitness for 
a particular purpose and non-infringement.

********************************************************************************************/

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
                    ret = XSharpConstants.ImageListIndex.Source;
                    //
                    if (IsFormSubType)
                    {
                        ret = (int)ProjectNode.ImageName.WindowsForm;
                        return ret;
                    }
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
