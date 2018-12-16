using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Project
{
    class XSharpDocumentWatcher : IVsRunningDocTableEvents, IDisposable
    {
        private IServiceProvider provider;
        private uint runningDocTableCookie = 0;
        private IVsRunningDocumentTable rdt = null;
        private List<uint> knownCookies;

        public XSharpDocumentWatcher(IServiceProvider provider)
        {
            this.provider = provider;
            knownCookies = new List<uint>();
            rdt = provider.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable;
            if (null != rdt)
            {
                // Do not throw here in case of error, simply skip the registration.
                rdt.AdviseRunningDocTableEvents(this, out runningDocTableCookie);
            }

        }
        public void Dispose()
        {
            if (0 == runningDocTableCookie || this.rdt == null)
            {
                return;
            }
            // Do not throw in case of error.
            rdt.UnadviseRunningDocTableEvents(runningDocTableCookie);
            runningDocTableCookie = 0;
        }

        public int OnAfterAttributeChange(uint docCookie, uint grfAttribs)
        {
            return VSConstants.S_OK;
        }

        public int OnAfterDocumentWindowHide(uint docCookie, IVsWindowFrame pFrame)
        {
            if (knownCookies.Contains(docCookie))
            {
                knownCookies.Remove(docCookie);
            }
            return VSConstants.S_OK;
        }

        public int OnAfterFirstDocumentLock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining)
        {
            return VSConstants.S_OK;
        }

        public int OnAfterSave(uint docCookie)
        {
            return VSConstants.S_OK;
        }

        public int OnBeforeDocumentWindowShow(uint docCookie, int fFirstShow, IVsWindowFrame pFrame)
        {
            // We want to make sure that we are not 'stealing' vulcans files
            // so we check for a vulcan file before opening the window
            // when it one of our files then we register the XFile object in the XSolution,
            // when it is an orphaned file then it is registered in the OrphanedFiles Project
            if (fFirstShow != 0)
            {
                if (knownCookies.Contains(docCookie))
                {
                    return VSConstants.S_OK;
                }
                knownCookies.Add(docCookie);
                if (null != rdt)
                {
                    // Note that here we don't want to throw in case of error.
                    uint flags;
                    uint readLocks;
                    uint writeLoks;
                    string documentMoniker;
                    IVsHierarchy hierarchy;
                    uint itemId;
                    IntPtr unkDocData;
                    int hr = rdt.GetDocumentInfo(docCookie, out flags, out readLocks, out writeLoks,
                                                 out documentMoniker, out hierarchy, out itemId, out unkDocData);
                    // check to see if this is one our files.
                    VsTextViewCreationListener.IsOurSourceFile(documentMoniker);
                }
            }
            return VSConstants.S_OK;
        }

        public int OnBeforeLastDocumentUnlock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining)
        {
            return VSConstants.S_OK;
        }
    }
}
