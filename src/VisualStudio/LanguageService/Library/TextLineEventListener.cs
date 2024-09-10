//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/*****************************************************************************
* Based on IronStudio/IronPythonTools/IronPythonTools/Navigation
****************************************************************************/

using System;
using System.Collections.Generic;

using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using VSConstants = Microsoft.VisualStudio.VSConstants;

namespace XSharp.LanguageService
{
#if TEXTCHANGELISTENER
    /// <summary>
    /// Used to attach to IVsTextLines events and been notified when something happens to the Buffer
    /// </summary>
    internal class TextLineEventListener : IVsTextLinesEvents, IDisposable
    {
        private const int defaultDelay = 2000;

        private string fileName;
        private XSharpModuleId fileId;
        private IVsTextLines buffer;
        private bool isDirty;

        private IConnectionPoint connectionPoint;
        private uint connectionCookie;

        public TextLineEventListener(IVsTextLines buffer, string fileName, XSharpModuleId id)
        {
            this.buffer = buffer;
            this.fileId = id;
            this.fileName = fileName;
            IConnectionPointContainer container = buffer as IConnectionPointContainer;
            if (null != container)
            {
                Guid eventsGuid = typeof(IVsTextLinesEvents).GUID;
                container.FindConnectionPoint(ref eventsGuid, out connectionPoint);
                connectionPoint.Advise(this as IVsTextLinesEvents, out connectionCookie);
            }
        }

    #region Properties
        public XSharpModuleId FileID
        {
            get { return fileId; }
        }
        public string FileName
        {
            get { return fileName; }
            set { fileName = value; }
        }
    #endregion

    #region Events
        private EventHandler<HierarchyEventArgs> onFileChanged;
        public event EventHandler<HierarchyEventArgs> OnFileChanged
        {
            add { onFileChanged += value; }
            remove { onFileChanged -= value; }
        }

        public event TextLineChangeEvent OnFileChangedImmediate;

    #endregion

    #region IVsTextLinesEvents Members
        void IVsTextLinesEvents.OnChangeLineAttributes(int iFirstLine, int iLastLine)
        {
            // Do Nothing
        }

        void IVsTextLinesEvents.OnChangeLineText(TextLineChange[] pTextLineChange, int fLast)
        {
            TextLineChangeEvent eh = OnFileChangedImmediate;
            if (null != eh)
            {
                eh(this, pTextLineChange, fLast);
            }
            // As soon as we have a modification on a buffer... Maybe a little bit too much !! :)
            isDirty = true;
        }
    #endregion

    #region IDisposable Members
        public void Dispose()
        {
            if ((null != connectionPoint) && (0 != connectionCookie))
            {
                connectionPoint.Unadvise(connectionCookie);
                System.Diagnostics.Debug.WriteLine("\n\tUnadvised from TextLinesEvents\n");
            }
            connectionCookie = 0;
            connectionPoint = null;

            this.buffer = null;
            this.fileId = null;
        }
    #endregion

    #region Idle time processing
        public void OnIdle()
        {
            if (!isDirty)
            {
                return;
            }
            if (null != onFileChanged)
            {
                HierarchyEventArgs args = new HierarchyEventArgs(fileId.ItemID, fileName);
                args.TextBuffer = buffer;
                // Enqueue a Library Request
                //onFileChanged(fileId.Hierarchy, args);
            }

            isDirty = false;
        }
    #endregion

    }
#endif
}
