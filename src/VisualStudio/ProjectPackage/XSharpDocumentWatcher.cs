using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
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
        private uint runningDocTableCookie = 0;
        private readonly IVsRunningDocumentTable rdt = null;
        private List<uint> knownCookies;

		// todo: we can probably delete this code.
        public XSharpDocumentWatcher(IServiceProvider provider)
        {
            knownCookies = new List<uint>();
            ThreadHelper.ThrowIfNotOnUIThread();
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
            ThreadHelper.ThrowIfNotOnUIThread();
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
            return VSConstants.S_OK;
        }

        public int OnBeforeLastDocumentUnlock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining)
        {
            return VSConstants.S_OK;
        }
    }
}
