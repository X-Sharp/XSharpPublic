//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using XSharp.Project.WPF;
using System.Diagnostics.CodeAnalysis;
using Microsoft.VisualStudio;

using OleConstants = Microsoft.VisualStudio.OLE.Interop.Constants;
using VsCommands = Microsoft.VisualStudio.VSConstants.VSStd97CmdID;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;


namespace XSharp.Project
{
    public class XSharpDependentFileNode : XSharpFileNode
    {
        #region ctor

        /// <summary>
        /// Constructor for the XSharpDependentFileNode
        /// </summary>
        /// <param name="root">Root of the hierarchy</param>
        /// <param name="e">Associated project element</param>
        public XSharpDependentFileNode(XSharpProjectNode root, ProjectElement e)
            : base(root, e, false)
        {
        }

        /// <summary>
        /// Constructor for the XSharpDependentFileNode
        /// </summary>
        /// <param name="root">Root of the hierarchy</param>
        /// <param name="e">Associated project element</param>
        public XSharpDependentFileNode(XSharpProjectNode root, ProjectElement e, bool isNonMemberItem)
            : base(root, e,isNonMemberItem)
        {
            HasParentNodeNameRelation = false;
            IsDependent = true;
        }

        #endregion

        #region overridden methods
        /// <summary>
        /// Disable rename
        /// </summary>
        /// <param name="label">new label</param>
        /// <returns>E_NOTIMPLE in order to tell the call that we do not support rename</returns>
        //public override string GetEditLabel()
        //{
        //    throw new NotImplementedException();
        //}
        protected override bool CanRenameItem()
        {
            return false;
        }

        /// <summary>
        /// Disable certain commands for dependent file nodes
        /// </summary>
        protected override int QueryStatusOnNode(Guid cmdGroup, uint cmd, IntPtr pCmdText, ref QueryStatusResult result)
        {
            if (cmdGroup == Microsoft.VisualStudio.Project.VsMenus.guidStandardCommandSet97)
            {
                switch ((VsCommands)cmd)
                {
                    case VsCommands.Copy:
                    case VsCommands.Paste:
                    case VsCommands.Cut:
                    case VsCommands.Rename:
                        result |= QueryStatusResult.NOTSUPPORTED;
                        return VSConstants.S_OK;

                    case VsCommands.ViewCode:
                    case VsCommands.Open:
                    case VsCommands.OpenWith:
                        result |= QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED;
                        return VSConstants.S_OK;
                }
            }
            else if (cmdGroup == Microsoft.VisualStudio.Project.VsMenus.guidStandardCommandSet2K)
            {
                if ((VsCommands2K)cmd == VsCommands2K.EXCLUDEFROMPROJECT)
                {
                    result |= QueryStatusResult.NOTSUPPORTED;
                    return VSConstants.S_OK;
                }
            }
            else
            {
                return (int)OleConstants.OLECMDERR_E_UNKNOWNGROUP;
            }
            return base.QueryStatusOnNode(cmdGroup, cmd, pCmdText, ref result);
        }

        /// <summary>
        /// DependentFileNodes node cannot be dragged.
        /// </summary>
        /// <returns>null</returns>
        protected override StringBuilder PrepareSelectedNodesForClipBoard()
        {
            return null;
        }

        /// <summary>
        /// Redraws the state icon if the node is not excluded from source control.
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Scc")]
        protected override void UpdateSccStateIcons()
        {
            if (!this.ExcludeNodeFromScc)
            {
                this.Parent.ReDraw(UIHierarchyElement.SccState);
            }
        }

        #endregion



    }
}
