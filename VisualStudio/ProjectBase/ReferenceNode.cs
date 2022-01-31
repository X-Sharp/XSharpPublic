/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
 * copy of the license can be found in the License.txt file at the root of this distribution.
 *
 * You must not remove this notice, or any other, from this software.
 *
 * ***************************************************************************/

using System;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Text;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using OleConstants = Microsoft.VisualStudio.OLE.Interop.Constants;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;
using XSharpModel;
using Community.VisualStudio.Toolkit;

namespace Microsoft.VisualStudio.Project
{
    [CLSCompliant(false), ComVisible(true)]
    public abstract class ReferenceNode : HierarchyNode
    {
        protected delegate void CannotAddReferenceErrorMessage();

        #region ctors
        /// <summary>
        /// constructor for the ReferenceNode
        /// </summary>
        protected ReferenceNode(ProjectNode root, ProjectElement element)
            : base(root, element)
        {
            this.ExcludeNodeFromScc = true;
        }

        /// <summary>
        /// constructor for the ReferenceNode
        /// </summary>
        protected ReferenceNode(ProjectNode root)
            : base(root)
        {
            this.ExcludeNodeFromScc = true;
        }

        #endregion

      public bool Resolved
      {
         get
         {
            return CanShowDefaultIcon();
         }
      }
        #region overridden properties
        public override int MenuCommandId
        {
            get { return VsMenus.IDM_VS_CTXT_REFERENCE; }
        }

        public override Guid ItemTypeGuid
        {
            get { return Guid.Empty; }
        }

        public override string Url
        {
            get
            {
                return String.Empty;
            }
        }

        public override string Caption
        {
            get
            {
                return String.Empty;
            }
        }
        public bool EmbedInteropTypes
        {
            get
            {
                bool value;
                bool.TryParse(this.ItemNode.GetMetadata(ProjectFileConstants.EmbedInteropTypes), out value);
                return value;
            }

            set
            {
                ThreadHelper.ThrowIfNotOnUIThread();

                if (value)
                    this.ItemNode.SetMetadata(ProjectFileConstants.EmbedInteropTypes, value.ToString());
                else
                    this.ItemNode.SetMetadata(ProjectFileConstants.EmbedInteropTypes, null);
            }
        }
        #endregion

        #region overridden methods
        protected override NodeProperties CreatePropertiesObject()
        {
         	return null; //  new ReferenceNodeProperties( this );
        }

        /// <summary>
        /// Get an instance of the automation object for ReferenceNode
        /// </summary>
        /// <returns>An instance of Automation.OAReferenceItem type if succeeded</returns>
        public override object GetAutomationObject()
        {
            if(this.ProjectMgr == null || this.ProjectMgr.IsClosed)
            {
                return null;
            }

            return new Automation.OAReferenceItem(this.ProjectMgr.GetAutomationObject() as Automation.OAProject, this);
        }

        /// <summary>
        /// Disable inline editing of Caption of a ReferendeNode
        /// </summary>
        /// <returns>null</returns>
        public override string GetEditLabel()
        {
            return null;
        }


        public override object GetIconHandle(bool open)
        {
            int offset = (this.CanShowDefaultIcon() ? (int)ProjectNode.ImageName.Reference : (int)ProjectNode.ImageName.DanglingReference);
            return this.ProjectMgr.ImageHandler.GetIconHandle(offset);
        }

        /// <summary>
        /// This method is called by the interface method GetMkDocument to specify the item moniker.
        /// </summary>
        /// <returns>The moniker for this item</returns>
        public override string GetMkDocument()
        {
            return this.Url;
        }

        /// <summary>
        /// Not supported.
        /// </summary>
        protected override int ExcludeFromProject()
        {
            return (int)OleConstants.OLECMDERR_E_NOTSUPPORTED;
        }

        /// <summary>
        /// References node cannot be dragged.
        /// </summary>
        /// <returns>A stringbuilder.</returns>
        protected internal override StringBuilder PrepareSelectedNodesForClipBoard()
        {
            return null;
		}

		protected override void DoDefaultAction()
		{
            ThreadHelper.ThrowIfNotOnUIThread();

            this.ShowObjectBrowser();
		}

        protected override int QueryStatusOnNode(Guid cmdGroup, uint cmd, IntPtr pCmdText, ref QueryStatusResult result)
        {
            if(cmdGroup == VsMenus.guidStandardCommandSet2K)
            {
                if((VsCommands2K)cmd == VsCommands2K.QUICKOBJECTSEARCH)
                {
                    result |= QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED;
                    return VSConstants.S_OK;
                }
            }
            else
            {
                return (int)OleConstants.OLECMDERR_E_UNKNOWNGROUP;
            }
            return base.QueryStatusOnNode(cmdGroup, cmd, pCmdText, ref result);
        }

        protected override int ExecCommandOnNode(Guid cmdGroup, uint cmd, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            if (cmdGroup == VsMenus.guidStandardCommandSet2K)
            {
                if((VsCommands2K)cmd == VsCommands2K.QUICKOBJECTSEARCH)
                {
                    return this.ShowObjectBrowser();
                }
            }

            return base.ExecCommandOnNode(cmdGroup, cmd, nCmdexecopt, pvaIn, pvaOut);
        }

        #endregion

        #region  methods


        /// <summary>
        /// Links a reference node to the project and hierarchy.
        /// </summary>
		public virtual /*void*/ ReferenceNode AddReference()  //
		{

            ReferenceNode existingNode = null;  /// returns existing node or null if this node has been newly added

			ReferenceContainerNode referencesFolder = this.ProjectMgr.FindChild(ReferenceContainerNode.ReferencesNodeVirtualName) as ReferenceContainerNode;
			Debug.Assert(referencesFolder != null, "Could not find the References node");

			CannotAddReferenceErrorMessage referenceErrorMessageHandler = null;

			if (!this.CanAddReference(out referenceErrorMessageHandler, out existingNode ))
			{
				if(referenceErrorMessageHandler != null)
				{
					referenceErrorMessageHandler.DynamicInvoke(new object[] { });
				}
				return existingNode;
			}
			// Link the node to the project file.
			this.BindReferenceData();

			// At this point force the item to be refreshed
			this.ItemNode.RefreshProperties();

			referencesFolder.AddChild(this);

            return existingNode;
		}

        /// <summary>
        /// Refreshes a reference by re-resolving it and redrawing the icon.
        /// </summary>
        internal virtual void RefreshReference()
        {
            this.ResolveReference();
            ThreadHelper.ThrowIfNotOnUIThread();

            this.ReDraw(UIHierarchyElement.Icon);
        }

        /// <summary>
        /// Resolves references.
        /// </summary>
        protected virtual void ResolveReference()
        {

        }

        /// <summary>
        /// Validates that a reference can be added.
        /// </summary>
        /// <param name="errorHandler">A CannotAddReferenceErrorMessage delegate to show the error message.</param>
        /// <returns>true if the reference can be added.</returns>
		protected virtual bool CanAddReference(out CannotAddReferenceErrorMessage errorHandler, out ReferenceNode existingNode )
		{
			// When this method is called this reference has not yet been added to the hierarchy, only instantiated.
			errorHandler = null;
			if (this.IsAlreadyAdded( out existingNode ))
			{
                   errorHandler = null;

                    return false;
		    }

			return true;
		}
        protected virtual bool CanAddReference(out CannotAddReferenceErrorMessage errorHandler)
        {
			ReferenceNode existingNode;
			return CanAddReference(out errorHandler, out existingNode);
		}
        /// <summary>
        /// Checks if a reference is already added. The method parses all references and compares the Url.
        /// </summary>
        /// <returns>true if the assembly has already been added.</returns>
        protected bool IsAlreadyAdded()
        {
            ReferenceNode existingReference;
            return IsAlreadyAdded(out existingReference);
        }

        /// <summary>
        /// Checks if a reference is already added. The method parses all references and compares the Url.
        /// </summary>
        /// <param name="existingEquivalentNode">The existing reference, if one is found.</param>
        /// <returns>true if the assembly has already been added.</returns>
        protected internal virtual bool IsAlreadyAdded(out ReferenceNode existingEquivalentNode)
        {
            ReferenceContainerNode referencesFolder = this.ProjectMgr.FindChild(ReferenceContainerNode.ReferencesNodeVirtualName) as ReferenceContainerNode;
            Debug.Assert(referencesFolder != null, "Could not find the References node");

            for(HierarchyNode n = referencesFolder.FirstChild; n != null; n = n.NextSibling)
            {
                ReferenceNode referenceNode = n as ReferenceNode;
                if(null != referenceNode)
                {
                    // We check if the Url of the assemblies is the same.
                    if(NativeMethods.IsSamePath(referenceNode.Url, this.Url))
                    {
                        existingEquivalentNode = referenceNode;
                        return true;
                    }
                }
            }

            existingEquivalentNode = null;
            return false;
        }

		/// <summary>
		/// Shows Add Reference error dialog
		/// </summary>
		/// <param name="message">Message to be displayed.</param>
		protected void ShowReferenceErrorMessage(string message)
		{
			string title = string.Empty;
            VS.MessageBox.ShowError(title, message);
		}

		/// <summary>
		/// Gets the Guid to use to set VSOJBECTINFO.pguidLib for the call to IVsObjBrowser.NavigateTo
		/// </summary>
		protected virtual Guid GetBrowseLibraryGuid()
		{
			// Previous default: VSConstants.guidCOMPLUSLibrary;
			return Guid.Empty;
		}
        /// Shows the Object Browser
        /// </summary>
        /// <returns></returns>
        protected virtual int ShowObjectBrowser()
        {
            if(String.IsNullOrEmpty(this.Url) || !System.IO.File.Exists(this.Url))
            {
                return (int)OleConstants.OLECMDERR_E_NOTSUPPORTED;
            }

            // Request unmanaged code permission in order to be able to creaet the unmanaged memory representing the guid.
            new SecurityPermission(SecurityPermissionFlag.UnmanagedCode).Demand();

			Guid guid = GetBrowseLibraryGuid();
            IntPtr ptr = System.Runtime.InteropServices.Marshal.AllocCoTaskMem(guid.ToByteArray().Length);

            System.Runtime.InteropServices.Marshal.StructureToPtr(guid, ptr, false);
            int returnValue = VSConstants.S_OK;
            try
            {
                VSOBJECTINFO[] objInfo = new VSOBJECTINFO[1];
                ThreadHelper.ThrowIfNotOnUIThread();

                objInfo[0].pguidLib = ptr;
                objInfo[0].pszLibName = this.Url;

                IVsObjBrowser objBrowser = this.ProjectMgr.Site.GetService(typeof(SVsObjBrowser)) as IVsObjBrowser;
                Assumes.Present(objBrowser);

                ErrorHandler.ThrowOnFailure(objBrowser.NavigateTo(objInfo, 0));
            }
            catch(COMException e)
            {
                XSettings.LogException(e, "ShowObjectBrowser");
                returnValue = e.ErrorCode;
            }
            finally
            {
                if(ptr != IntPtr.Zero)
                {
                    System.Runtime.InteropServices.Marshal.FreeCoTaskMem(ptr);
                }
            }

            return returnValue;
        }

        protected override bool CanDeleteItem(__VSDELETEITEMOPERATION deleteOperation)
        {
            if(deleteOperation == __VSDELETEITEMOPERATION.DELITEMOP_RemoveFromProject)
            {
                return true;
            }
            return false;
        }

        protected abstract void BindReferenceData();

        #endregion
		#region private methods
		private void ShowReferenceAlreadyExistMessage()
		{
			string message = String.Format(CultureInfo.CurrentCulture, SR.GetString(SR.ReferenceAlreadyExists, CultureInfo.CurrentUICulture), this.Caption);
            ThreadHelper.ThrowIfNotOnUIThread();

            ShowReferenceErrorMessage(message);
		}

		#endregion
	}
}
