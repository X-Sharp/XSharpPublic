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
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using IServiceProvider = System.IServiceProvider;
using ShellConstants = Microsoft.VisualStudio.Shell.Interop.Constants;
using XSharpModel;
using Microsoft.VisualStudio.Shell;

namespace Microsoft.VisualStudio.Project
{
    /// <summary>
    /// helper to make the editor ignore external changes
    /// </summary>
    internal class SuspendFileChanges
    {
        private string documentFileName;

        private bool isSuspending;

        private IServiceProvider site;

        private IVsDocDataFileChangeControl fileChangeControl;

        public SuspendFileChanges(IServiceProvider site, string document)
        {
            this.site = site;
            this.documentFileName = document;
        }


        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily")]
        public void Suspend()
        {
            if(this.isSuspending)
                return;

            IntPtr docData = IntPtr.Zero;
            try
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                IVsRunningDocumentTable rdt = this.site.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable;

                IVsHierarchy hierarchy;
                uint itemId;
                uint docCookie;
                IVsFileChangeEx fileChange;

                if (rdt == null)
                    return;

                ErrorHandler.ThrowOnFailure(rdt.FindAndLockDocument((uint)_VSRDTFLAGS.RDT_NoLock, this.documentFileName, out hierarchy, out itemId, out docData, out docCookie));

                if((docCookie == (uint)ShellConstants.VSDOCCOOKIE_NIL) || docData == IntPtr.Zero)
                    return;

                fileChange = this.site.GetService(typeof(SVsFileChangeEx)) as IVsFileChangeEx;

                if(fileChange != null)
                {
                    this.isSuspending = true;
                    ErrorHandler.ThrowOnFailure(fileChange.IgnoreFile(0, this.documentFileName, 1));
                    if(docData != IntPtr.Zero)
                    {
                        IVsPersistDocData persistDocData = null;

                        // if interface is not supported, return null
                        object unknown = Marshal.GetObjectForIUnknown(docData);
                        if(unknown is IVsPersistDocData)
                        {
                            persistDocData = (IVsPersistDocData)unknown;
                            if(persistDocData is IVsDocDataFileChangeControl)
                            {
                                this.fileChangeControl = (IVsDocDataFileChangeControl)persistDocData;
                                if(this.fileChangeControl != null)
                                {
                                    ErrorHandler.ThrowOnFailure(this.fileChangeControl.IgnoreFileChanges(1));
                                }
                            }
                        }
                    }
                }
            }
            catch(InvalidCastException e)
            {
                XSettings.LogMessage("Exception" + e.Message);
            }
            finally
            {
                if(docData != IntPtr.Zero)
                {
                    Marshal.Release(docData);
                }
            }
            return;
        }

        public void Resume()
        {
            if(!this.isSuspending)
                return;
            IVsFileChangeEx fileChange;
            ThreadHelper.ThrowIfNotOnUIThread();

            fileChange = this.site.GetService(typeof(SVsFileChangeEx)) as IVsFileChangeEx;
            if(fileChange != null)
            {
                this.isSuspending = false;
                ErrorHandler.ThrowOnFailure(fileChange.IgnoreFile(0, this.documentFileName, 0));
                if(this.fileChangeControl != null)
                {
                    ErrorHandler.ThrowOnFailure(this.fileChangeControl.IgnoreFileChanges(0));
                }
            }
        }
    }
}
