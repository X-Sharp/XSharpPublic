//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;
using Microsoft.VisualStudio.Project;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.Project
{
    public class  XSharpFileDocumentManager: FileDocumentManager
    {
        #region ctors

        public XSharpFileDocumentManager(FileNode node)
            : base(node)
        {
        }
        #endregion


        /// <summary>
        /// Open a file in a document window
        /// </summary>
        /// <param name="newFile">Open the file as a new file</param>
        /// <param name="openWith">Use a dialog box to determine which editor to use</param>
        /// <param name="logicalView">In MultiView case determines view to be activated by IVsMultiViewDocumentView. For a list of logical view GUIDS, see constants starting with LOGVIEWID_ defined in NativeMethods class</param>
        /// <param name="docDataExisting">IntPtr to the IUnknown interface of the existing document data object</param>
        /// <param name="windowFrame">A reference to the window frame that is mapped to the file</param>
        /// <param name="windowFrameAction">Determine the UI action on the document window</param>
        /// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
        public override int Open(bool newFile, bool openWith, ref Guid logicalView, IntPtr docDataExisting, out IVsWindowFrame windowFrame, WindowFrameShowAction windowFrameAction)
        {
            Guid editorType = VSConstants.LOGVIEWID_Primary;
            string fullPath = this.GetFullPathForDocument();
            XFileType type = XFileTypeHelpers.GetFileType(fullPath);
            switch (type)
            {
                case XFileType.SourceCode:
                case XFileType.Header:
                case XFileType.PreprocessorOutput:
                case XFileType.NativeResource:
                case XFileType.License:
                case XFileType.Template:
                    editorType = XSharpConstants.guidSourcecodeEditorFactory;
                    break;
                case XFileType.VOForm:
                case XFileType.VOMenu:
                case XFileType.VODBServer:
                case XFileType.VOFieldSpec:
                    // the primary view is the normal editor which is defined with an attribute on the package
                    // we only have to tell it what to do when we want to see the code.
                    if (logicalView == VSConstants.LOGVIEWID.Code_guid)
                    {
                        editorType = XSharpConstants.guidVSXmlEditor;
                    }
                    break;
            }
            var xNode = this.Node.ProjectMgr as XSharpProjectNode;
            if (xNode.ProjectModel != null)
            {
                xNode.ProjectModel.ResolveReferences();
            }
            return base.Open(newFile, openWith, 0, ref editorType, null, ref logicalView, docDataExisting, out windowFrame, windowFrameAction);
        }
    }
}
