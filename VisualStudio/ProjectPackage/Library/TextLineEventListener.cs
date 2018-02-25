/*****************************************************************************
 *
 * Copyright(c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
* copy of the license can be found in the License.html file at the root of this distribution.If
* you cannot locate the Apache License, Version 2.0, please send an email to
* ironpy@microsoft.com.By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Apache License, Version 2.0.
 *
 * You must not remove this notice, or any other, from this software.
*
****************************************************************************/
/*****************************************************************************
* XSharp.BV
* Based on IronStudio/IronPythonTools/IronPythonTools/Navigation
*
****************************************************************************/

using System;
using System.Collections.Generic;

using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;

using VSConstants = Microsoft.VisualStudio.VSConstants;

namespace XSharp.Project
{

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
                onFileChanged(fileId.Hierarchy, args);
            }

            isDirty = false;
        }
        #endregion
    }
}
