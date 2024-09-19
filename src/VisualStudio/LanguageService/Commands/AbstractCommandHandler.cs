using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.LanguageService
{
    internal class AbstractCommandHandler : IOleCommandTarget
    {
        private readonly IOleCommandTarget m_nextCommandHandler;

        internal AbstractCommandHandler(IVsTextView textViewAdapter)
        {
            textViewAdapter.AddCommandFilter(this, out m_nextCommandHandler);
        }
        public virtual int QueryStatus(ref Guid cmdGroup, uint cCmds, OLECMD[] commands, IntPtr pCmdText)
        {
            return m_nextCommandHandler.QueryStatus(ref cmdGroup, cCmds, commands, pCmdText);
        }

        public virtual int Exec(ref Guid cmdGroup, uint nCmdID, uint nCmdExecOpt, IntPtr pvaIn, IntPtr pvaOut)
        {
            return m_nextCommandHandler.Exec(ref cmdGroup, nCmdID, nCmdExecOpt, pvaIn, pvaOut);
        }
    }
}
