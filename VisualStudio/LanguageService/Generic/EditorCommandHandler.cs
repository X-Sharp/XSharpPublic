//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text.Editor;
using System;
using System.Linq;
using XSharpModel;
namespace XSharp.LanguageService
{
    internal sealed partial class XSharpEditorCommandHandler : IOleCommandTarget
    {
        public ITextView TextView { get; private set; }
        public IOleCommandTarget Next { get; set; }

        readonly XFile _file;


        public XSharpEditorCommandHandler(IWpfTextView textView)
        {
            TextView = textView;
            _file = textView.TextBuffer.GetFile();
        }


        internal void ShowHelp(uint nCmdID)
        {
            var result = this.TextView.GetSymbolUnderCursor(out _, out _, out var tokens); ;
            if (result.Count == 0 && tokens.Count > 0)
            {
                var token = tokens[0].Text;
                HelpViewer.ShowHelp(TextView, nCmdID, token);
            }
            HelpViewer.ShowHelp(TextView, nCmdID, result.First());
        }

        public int Exec(ref Guid pguidCmdGroup, uint nCmdID, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            var cmdGrp = pguidCmdGroup;
            int hresult = VSConstants.S_OK;

            // 1. Pre-process
            if (pguidCmdGroup == VSConstants.VSStd2K)
            {
                switch (nCmdID)
                {
                    case (int)VSConstants.VSStd2KCmdID.HELPKEYWORD:
                    case (int)VSConstants.VSStd2KCmdID.HELP:
                        ShowHelp(nCmdID);
                        return VSConstants.S_OK;
                }
            }
            else if (pguidCmdGroup == VSConstants.GUID_VSStandardCommandSet97)
            {
                switch (nCmdID)
                {
                    case (int)VSConstants.VSStd97CmdID.F1Help:
                    case (int)VSConstants.VSStd97CmdID.WindowHelp:
                        ShowHelp(nCmdID);
                        return VSConstants.S_OK;

                    case (int)VSConstants.VSStd97CmdID.GotoDefn:
                        XSharpGotoDefinition.GotoDefn(TextView);
                        return VSConstants.S_OK;
                }
            }

            // 2. Let others do their thing
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                 await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                 hresult = Next.Exec(ref cmdGrp, nCmdID, nCmdexecopt, pvaIn, pvaOut);
             });

            return hresult;
        }

        public int QueryStatus(ref Guid pguidCmdGroup, uint cCmds, OLECMD[] prgCmds, IntPtr pCmdText)
        {
            bool isSource = _file != null && _file.XFileType == XFileType.SourceCode;
            if (pguidCmdGroup == VSConstants.GUID_VSStandardCommandSet97)
            {
                switch (prgCmds[0].cmdID)
                {

                    case (int)VSConstants.VSStd97CmdID.GotoDefn:
                        if (isSource)
                        {
                            prgCmds[0].cmdf = (uint)OLECMDF.OLECMDF_ENABLED | (uint)OLECMDF.OLECMDF_SUPPORTED;
                            return VSConstants.S_OK;
                        }
                        break;
                }
            }
            int result = 0;
            var cmdGroup = pguidCmdGroup;
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                result = Next.QueryStatus(cmdGroup, cCmds, prgCmds, pCmdText);
            });
            return result;
        }

    }

}
