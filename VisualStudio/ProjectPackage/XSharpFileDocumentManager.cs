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
            windowFrame = null;
            Guid editorType = VSConstants.LOGVIEWID_Primary;
            string fullPath = this.GetFullPathForDocument();
            switch (System.IO.Path.GetExtension(fullPath).ToLower())
            {
                case ".prg":
                case ".ppo":
                case ".vh":
                case ".xs":
                case ".xh":
                    // No idea why but testwindow needs this. Other windows not ! In fact specifying this for other windows
                    // has the opposite effect that files will NOT be opened correctly
                    if (Environment.StackTrace.Contains("VisualStudio.TestWindow"))
                    {
                        editorType = GuidStrings.guidSourcecodeEditorFactory;
                    }
                    break;
                case ".xsfrm":
                case ".vnfrm":
                    editorType = GuidStrings.guidVOFormEditorFactory;
                    break;
                case ".xsmnu":
                case ".vnmnu":
                    editorType = GuidStrings.guidVOMenuEditorFactory;
                    break;
                case ".xsdbs":
                case ".vndbs":
                    editorType = GuidStrings.guidVODbServerEditorFactory;
                    break;
                case ".xsfs":
                case ".vnfs":
                    editorType = GuidStrings.guidVOFieldSpecEditorFactory;
                    break;
            }

            return base.Open(newFile, openWith, 0, ref editorType, null, ref logicalView, docDataExisting, out windowFrame, windowFrameAction);
        }
    }
}
