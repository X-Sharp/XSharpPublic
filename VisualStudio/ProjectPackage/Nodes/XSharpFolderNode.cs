//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using System.Runtime.InteropServices;

using VsCommands = Microsoft.VisualStudio.VSConstants.VSStd97CmdID;
using VsCommands2K = Microsoft.VisualStudio.VSConstants.VSStd2KCmdID;
using OleConstants = Microsoft.VisualStudio.OLE.Interop.Constants;
using System.Globalization;
using System.ComponentModel;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Imaging;

namespace XSharp.Project
{

    public class XSharpPropertiesFolderNode: XSharpFolderNode
    {
        public XSharpPropertiesFolderNode(XSharpProjectNode root, string directoryPath, ProjectElement element)
           : base(root, directoryPath, element,false)
        {
               
        }
        

        public override string Caption
        {
            get
            {
                return "Properties";
            }
        }



        // =========================================================================================
        // Methods
        // =========================================================================================

        /// <summary>
        /// Provides the node name for inline editing of caption.
        /// Overridden to disable this functionality for properties node.
        /// </summary>
        /// <returns>Caption of the folder node if the node is a member item, null otherwise.</returns>
        public override string GetEditLabel()
        {
            return null;
        }

        protected override bool SupportsIconMonikers => true;
        protected override ImageMoniker GetIconMoniker(bool open)
        {
#if VS17
            if (open)
                return KnownMonikers.PropertiesFolderOpen;
            else
                return KnownMonikers.PropertiesFolderClosed;
#else
            // VS2019 does not have these image monikers
            return KnownMonikers.Property;
#endif
        }

        /// <summary>
        /// Creates an object derived from <see cref="NodeProperties"/> that will be used to expose
        /// properties specific for this object to the property browser.
        /// </summary>
        /// <returns>A new <see cref="XSharpVirtualFolderNodeProperties"/> object.</returns>
        protected override NodeProperties CreatePropertiesObject()
        {
            return new XSharpVirtualFolderNodeProperties(this);

        }
        public override int SortPriority => DefaultSortOrderNode.ProjectProperties;

        /// <summary>
        /// Handles command status on a node. Should be overridden by descendant nodes. If a command cannot be handled then the base should be called.
        /// </summary>
        /// <param name="cmdGroup">A unique identifier of the command group. The pguidCmdGroup parameter can be NULL to specify the standard group.</param>
        /// <param name="cmd">The command to query status for.</param>
        /// <param name="pCmdText">Pointer to an OLECMDTEXT structure in which to return the name and/or status information of a single command. Can be NULL to indicate that the caller does not require this information.</param>
        /// <param name="result">An out parameter specifying the QueryStatusResult of the command.</param>
        /// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
        [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#", Justification = "Suppressing to avoid conflict with style cop.")]
        protected override int QueryStatusOnNode(Guid cmdGroup, uint cmd, IntPtr pCmdText, ref QueryStatusResult result)
        {
            if (cmdGroup == VsMenus.guidStandardCommandSet97)
            {
                switch ((VsCommands)cmd)
                {
                    case VsCommands.Delete:
                    case VsCommands.Remove:
                    case VsCommands.Cut:
                    case VsCommands.PropSheetOrProperties:
                    case VsCommands.Rename:
                        result = QueryStatusResult.SUPPORTED | QueryStatusResult.INVISIBLE;
                        return VSConstants.S_OK;
                    case VsCommands.Open:
                        result = QueryStatusResult.SUPPORTED | QueryStatusResult.ENABLED ;
                        return VSConstants.S_OK;
                }
            }
            if (cmdGroup == VsMenus.guidStandardCommandSet2K)
            {
                switch ((VsCommands2K)cmd)
                {
                    case VsCommands2K.EXCLUDEFROMPROJECT:
                        result = QueryStatusResult.SUPPORTED | QueryStatusResult.INVISIBLE ;
                        return VSConstants.S_OK;

                }
            }
            return base.QueryStatusOnNode(cmdGroup, cmd, pCmdText, ref result);
        }

        protected override void DoDefaultAction()
        {
            ExecCommandOnNode(VsMenus.guidStandardCommandSet97, (uint) VsCommands.PropSheetOrProperties, 0, IntPtr.Zero, IntPtr.Zero);
        }

        /// <summary>
        /// Handles command execution.
        /// </summary>
        /// <param name="cmdGroup">Unique identifier of the command group</param>
        /// <param name="cmd">The command to be executed.</param>
        /// <param name="cmdexecopt">Values describe how the object should execute the command.</param>
        /// <param name="pvaIn">Pointer to a VARIANTARG structure containing input arguments. Can be NULL</param>
        /// <param name="pvaOut">VARIANTARG structure to receive command output. Can be NULL.</param>
        /// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
        [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#", Justification = "Suppressing to avoid conflict with style cop.")]
        [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "2#", Justification = "Suppressing to avoid conflict with style cop.")]
        protected override int ExecCommandOnNode(Guid cmdGroup, uint cmd, uint cmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
             if (cmdGroup == VsMenus.guidStandardCommandSet97)
            {
                switch ((VsCommands)cmd)
                {
                    case VsCommands.Delete:
                    case VsCommands.Remove:
                        return VSConstants.S_FALSE;
                    case VsCommands.Open:
                    case VsCommands.PropertiesWindow:
                    case VsCommands.Properties:
                    case VsCommands.PropertyPages:
                    case VsCommands.PropSheetOrProperties:
                        DocumentManager docmgr = this.ProjectMgr.GetDocumentManager();
                        Guid editorType = new Guid(VSConstants.GUID_ProjectDesignerEditor.ToString());
                        Guid logicalView = new Guid();
                        IVsWindowFrame frame;
                        docmgr.OpenWithSpecific(0, ref editorType, "", ref logicalView, (IntPtr)(-1), out frame, WindowFrameShowAction.Show);

                        return VSConstants.S_OK;


                }
            }

            return base.ExecCommandOnNode(cmdGroup, cmd, cmdexecopt, pvaIn, pvaOut);
        }
    }

    /// <summary>
    /// Represents a Folder node in a XSharp project.
    /// </summary>
    public class XSharpFolderNode : XFolderNode
    {
        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpFolderNode"/> class.
        /// </summary>
        /// <param name="root">The root <see cref="XSharpProjectNode"/> that contains this node.</param>
        /// <param name="directoryPath">Root of the hierarchy.</param>
        /// <param name="element">The element that contains MSBuild properties.</param>
        public XSharpFolderNode(XSharpProjectNode root, string directoryPath, ProjectElement element)
           : this(root, directoryPath, element, false)
        {
        }


        public override void OnItemAdded(HierarchyNode parent, HierarchyNode child)
        {
            base.OnItemAdded(parent, child);
            if (child is XSharpFileNode xfile)
            {
                xfile.SetSpecialPropertiesEx();
            }
        }
        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpFolderNode"/> class.
        /// </summary>
        /// <param name="root">The root <see cref="XSharpProjectNode"/> that contains this node.</param>
        /// <param name="directoryPath">Root of the hierarchy</param>
        /// <param name="element">The element that contains MSBuild properties.</param>
        /// <param name="isNonMemberItem">Indicates if this node is not a member of the project.</param>
        [SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "NonMember")]
        public XSharpFolderNode(XSharpProjectNode root, string directoryPath, ProjectElement element, bool isNonMemberItem)
           : base(root, directoryPath, element, isNonMemberItem)
        {
            root.AddURL(this.Url, this);
        }


        protected override void DeleteFromStorage(string path)
        {
            if (Directory.Exists(path))
            {
                File.SetAttributes(path, FileAttributes.Normal); // make sure it's not readonly.
                OurNativeMethods.ShellDelete(path, OurNativeMethods.RecycleOption.SendToRecycleBin,
                   OurNativeMethods.UICancelOption.DoNothing, OurNativeMethods.FileOrDirectory.Directory);

            }
        }

        public override void OnItemDeleted()
        {
            if (this.ProjectMgr is XSharpProjectNode)
            {
                var projectNode = this.ProjectMgr as XSharpProjectNode;
                if (projectNode != null)
                    projectNode.RemoveURL(this);

                base.OnItemDeleted();
            }
        }


        public override int SetEditLabel(string label)
        {
            int iResult;
            String sOldUrl = this.Url;
            iResult = base.SetEditLabel(label);
            if (iResult == VSConstants.S_OK && String.Compare(this.Url, sOldUrl, true) != 0)
            {
                XSharpProjectNode project = this.ProjectMgr as XSharpProjectNode;
                if (project != null)
                {
                    project.RemoveURL(sOldUrl);
                    project.AddURL(this.Url, this);
                }
            }
            return iResult;
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

    }
}
