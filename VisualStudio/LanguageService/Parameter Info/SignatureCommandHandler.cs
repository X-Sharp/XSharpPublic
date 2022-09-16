//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//------------------------------------------------------------------------------

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Runtime.InteropServices;
using XSharpModel;


#pragma warning disable CS0649 // Field is never assigned to, for the imported fields
namespace XSharp.LanguageService
{
    [Export(typeof(IVsTextViewCreationListener))]
    [Name("XSharp Signature Help Command Handler")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [ContentType(XSharpConstants.LanguageName)]
    internal class XSharpSignatureHelpCommandProvider : IVsTextViewCreationListener
    {

        [Import]
        internal IVsEditorAdaptersFactoryService AdapterService;

        [Import]
        internal ISignatureHelpBroker SignatureHelpBroker;

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            if (XSettings.DisableParameterInfo)
                return;

            ITextView textView = AdapterService.GetWpfTextView(textViewAdapter);
            if (textView == null)
                return;

            textView.Properties.GetOrCreateSingletonProperty(
                 () => new XSharpSignatureHelpCommandHandler(textViewAdapter,
                    textView,
                    SignatureHelpBroker));
        }
    }
    internal class XSharpSignatureHelpCommandHandler : IOleCommandTarget
    {
        readonly ITextView _textView;
        readonly ISignatureHelpBroker _signatureBroker;
        readonly IOleCommandTarget m_nextCommandHandler;
        ISignatureHelpSession _signatureSession = null;
#if !ASYNCCOMPLETION
        XSharpCompletionCommandHandler _completionCommandHandler = null;
#endif
        internal XSharpSignatureHelpCommandHandler(IVsTextView textViewAdapter, ITextView textView, ISignatureHelpBroker broker)
        {
            this._textView = textView;
            this._signatureBroker = broker;
            //add this to the filter chain
            textViewAdapter.AddCommandFilter(this, out m_nextCommandHandler);
        }


        public int Exec(ref Guid pguidCmdGroup, uint nCmdID, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            char typedChar = char.MinValue;
            int result = VSConstants.S_OK;
            Guid cmdGroup = pguidCmdGroup;
            ThreadHelper.ThrowIfNotOnUIThread();

            // 1. Pre-process

            if (XSettings.DisableParameterInfo)
            {
                ;
            }
            else if (pguidCmdGroup == VSConstants.VSStd2K)
            {
                switch (nCmdID)
                {
                    case (int)VSConstants.VSStd2KCmdID.PARAMINFO:
                        if (cursorAfterOpenToken())
                        {
                            StartSignatureSession();
                        }
                        break;
                    case (int)VSConstants.VSStd2KCmdID.COMPLETEWORD:
                    case (int)VSConstants.VSStd2KCmdID.AUTOCOMPLETE:
                    case (int)VSConstants.VSStd2KCmdID.SHOWMEMBERLIST:
                        if (HasActiveSignatureSession)
                        {
                            CancelSignatureSession();
                        }
                        break;
                    case (int)VSConstants.VSStd2KCmdID.BACKSPACE:
                        if (HasActiveSignatureSession)
                        {
                            SnapshotPoint ssp = _textView.Caret.Position.BufferPosition;
                            if (ssp.Position > 0)
                            {
                                // get previous char
                                var previous = _textView.TextBuffer.CurrentSnapshot.GetText().Substring(ssp.Position - 1, 1);
                                if (previous == "(" || previous == "{")
                                {
                                    _signatureSession.Dismiss();
                                }
                            }

                        }
                        break;
                }
            }
            else if (pguidCmdGroup == VSConstants.GUID_VSStandardCommandSet97)
            {
                switch (nCmdID)
                {
                    case (int)VSConstants.VSStd97CmdID.Undo:
                    case (int)VSConstants.VSStd97CmdID.Redo:
                        if (HasActiveSignatureSession)
                        {
                            CancelSignatureSession();
                        }
                        break;
                }
            }
            var completionActive = IsCompletionActive();
            // 2. Let others do their thing
            result = m_nextCommandHandler.Exec(ref cmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut);
            // 3. Post process
            if (ErrorHandler.Succeeded(result) && !XSettings.DisableParameterInfo)
            {
                if (pguidCmdGroup == VSConstants.VSStd2K)
                {

                    switch (nCmdID)
                    {

                        case (int)VSConstants.VSStd2KCmdID.TYPECHAR:
                            typedChar = (char)(ushort)Marshal.GetObjectForNativeVariant(pvaIn);
                            switch (typedChar)
                            {
                                case '(':
                                case '{':
                                    CancelSignatureSession();
                                    StartSignatureSession(triggerchar: typedChar);
                                    break;
                                case ')':
                                case '}':
                                    CancelSignatureSession();
                                    if (cursorAfterOpenToken())
                                    {
                                        StartSignatureSession(triggerchar: typedChar);
                                    }
                                    break;
                                case ',':
                                    if (cursorAfterOpenToken())
                                    {
                                        if (HasActiveSignatureSession)
                                        {
                                            MoveSignature();
                                        }
                                        else
                                        {
                                            StartSignatureSession(triggerchar: typedChar);
                                        }
                                    }
                                    break;
                                case ':':
                                case '.':
                                    if (HasActiveSignatureSession)
                                    {
                                        CancelSignatureSession();
                                    }
                                    break;

                            }
                            break;
                        case (int)VSConstants.VSStd2KCmdID.RETURN:
                            if (!completionActive)
                            {
                                CancelSignatureSession();
                            }
                            break;

                        case (int)VSConstants.VSStd2KCmdID.LEFT:
                        case (int)VSConstants.VSStd2KCmdID.RIGHT:
                        case (int)VSConstants.VSStd2KCmdID.BACKSPACE:
                            if (HasActiveSignatureSession)
                            {
                                MoveSignature();
                            }
                            break;
                        case (int)VSConstants.VSStd2KCmdID.UP:
                        case (int)VSConstants.VSStd2KCmdID.DOWN:
                            if (HasActiveSignatureSession && _signatureSession.Signatures.Count == 1)
                            {
                                CancelSignatureSession();
                            }
                            break;


                    }
                }
            }
            return result;
        }

        bool HasActiveSignatureSession => _signatureSession != null;
        bool cursorAfterOpenToken()
        {
            SnapshotPoint ssp = this._textView.Caret.Position.BufferPosition;
            var level = 0;
            bool done = false;
            bool inString = false;
            char closechar = '\0';
            int pos = ssp.Position;
            var snapshot = ssp.Snapshot;
            while (!done && pos > 0)
            {
                pos = pos - 1;
                var ch = snapshot[pos];
                {
                    switch (ch)
                    {
                        case '\'':
                        case '"':
                            if (!inString)
                            {
                                inString = true;
                                closechar = ch;
                            }
                            else if (closechar == ch)
                            {
                                inString = false;
                                closechar = '\0';
                            }
                            break;
                        case '(' when ! inString:
                        case '{' when !inString:
                            level += 1;
                            break;
                        case ')' when !inString:
                        case '}' when !inString:
                            level -= 1;
                            break;
                        case '\r' when !inString:
                        case '\n' when !inString:
                            done = true;
                            break;

                    }
                }
            }
            return level > 0;
        }

        bool IsCompletionActive()
        {
#if !ASYNCCOMPLETION
            if (_completionCommandHandler == null)
                _completionCommandHandler = _textView.Properties.GetProperty<XSharpCompletionCommandHandler>(typeof(XSharpCompletionCommandHandler));
            if (_completionCommandHandler != null)
            {
                return _completionCommandHandler.HasActiveSession;
            }
#endif
            return false;

        }
        public int QueryStatus(ref Guid pguidCmdGroup, uint cCmds, OLECMD[] prgCmds, IntPtr pCmdText)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            return m_nextCommandHandler.QueryStatus(ref pguidCmdGroup, cCmds, prgCmds, pCmdText);
        }
        IXMemberSymbol findElementAt(bool command, char triggerChar, SnapshotPoint ssp, XSharpSignatureProperties props)
        {
            // when coming from the completion list then there is no need to check a lot of stuff
            // we can then simply lookup the method and that is it.
            // Also no need to filter on visibility since that has been done in the completionlist already !
            // First, where are we ?
            var location = props.Location;
            bool comma = triggerChar == ',';
            // When we have a multi line source line this is the line where the open paren or open curly is

            if (location.Member != null && location.Member.Range.StartLine == ssp.GetContainingLine().LineNumber)
            {
                // if we are at the start of an entity then do not start a signature session
                return null;
            }
            // Then, the corresponding Type/Element if possible
            // Check if we can get the member where we are

            var tokenList = XSharpTokenTools.GetTokenListBeforeCaret(location, out var state);
            // tokenlist may look like ID1 ( ID2 ( e1 , e2)
            // after the closing paren we come here and want to completely remove the  ID2 ( e1, e2 ) part
            // when we have this ID1 ( e1 ) then there should be no parameter completion at all
            // The same for ID1 { e1 }
            // so we need to see if LPAREN / RPAREN is closed and if LCURLY / RCURLY is closed and if LBRKT / RBRKT is closed
            int nested = 0;
            var openTokens = new Stack<IToken>();
            for (int i = 0; i < tokenList.Count; i++)
            {
                var token = (XSharpToken)tokenList[i];
                // comma right before closing token should still trigger the parameter tips
                // so skip the closing token
                // the comma itself is usually not in the list yet because the tokens are from the previous
                // snapshot.
                if (token.Position == props.Location.Position && (comma || command))
                {
                    if (token.Type == XSharpLexer.RPAREN ||
                        token.Type == XSharpLexer.RBRKT ||
                        token.Type == XSharpLexer.RCURLY)
                    {
                        break;
                    }
                }
                switch (token.Type)
                {
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LCURLY:
                    case XSharpLexer.LBRKT:
                        openTokens.Push(token);
                        break;
                    case XSharpLexer.RPAREN:
                        if (openTokens.Count > 0 && openTokens.Peek().Type == XSharpLexer.LPAREN)
                            openTokens.Pop();
                        break;
                    case XSharpLexer.RCURLY:
                        if (openTokens.Count > 0 && openTokens.Peek().Type == XSharpLexer.LCURLY)
                            openTokens.Pop();
                        break;
                    case XSharpLexer.RBRKT:
                        if (openTokens.Count > 0 && openTokens.Peek().Type == XSharpLexer.LBRKT)
                            openTokens.Pop();
                        break;
                    case XSharpLexer.STRING_CONST:
                    case XSharpLexer.INTERPOLATED_STRING_CONST:
                    case XSharpLexer.CHAR_CONST:
                        if (token.Position < location.Position && location.Position < token.Position + token.Text.Length)
                        {
                            if (comma)
                                return null;
                        }
                        break;
                }
            }

            if (openTokens.Count == 0)
            {
                tokenList.Clear();
            }
            else
            {
                var pos = tokenList.IndexOf(openTokens.Peek());
                if (pos >= 0)
                {
                    ((List<IToken>)tokenList).RemoveRange(pos + 1, tokenList.Count - pos - 1);
                }
            }
            if (comma || command)
            {
                // check to see if there is a lparen or lcurly before the comma
                bool done = false;
                nested = 0;
                while (tokenList.Count > 0 && !done)
                {
                    var token = tokenList[tokenList.Count - 1];
                    bool delete = false;
                    switch (token.Type)
                    {
                        case XSharpLexer.RPAREN:
                        case XSharpLexer.RBRKT:
                        case XSharpLexer.RCURLY:
                            nested += 1;
                            delete = true;
                            break;
                        case XSharpLexer.LBRKT:
                            nested -= 1;
                            delete = true;
                            break;
                        case XSharpLexer.LPAREN:
                        case XSharpLexer.LCURLY:
                            nested -= 1;
                            if (nested < 0)
                            {
                                done = true;
                            }
                            else
                            {
                                delete = true;
                            }
                            break;
                        default:
                            delete = true;
                            break;
                    }
                    if (delete)
                    {
                        tokenList.RemoveAt(tokenList.Count - 1);
                    }
                }
            }

            IXMemberSymbol currentElement = null;
            // We don't care of the corresponding Type, we are looking for the currentElement
            var element = XSharpLookup.RetrieveElement(location, tokenList, state, out var notProcessed, true).FirstOrDefault();
            if (element is IXMemberSymbol mem)
            {
                currentElement = mem;
                if (currentElement.Kind == Kind.Constructor)
                {
                    bool done = false;
                    for (int i = tokenList.Count - 1; !done && i > 0; i--)
                    {
                        var token = tokenList[i];
                        switch (token.Type)
                        {
                            case XSharpLexer.LPAREN:
                            case XSharpLexer.LCURLY:
                                props.triggerToken = tokenList[i - 1].Text;
                                break;
                        }
                    }
                }
            }
            else if (element is IXTypeSymbol xtype)
            {
                currentElement = xtype.Members.Where(m => m.Kind == Kind.Constructor).FirstOrDefault();
            }
            return currentElement;

        }

        internal bool StartSignatureSession(IXTypeSymbol type = null, string methodName = null, char triggerchar = '\0')
        {
            if (HasActiveSignatureSession)
            {
                return false;
            }
            WriteOutputMessage("StartSignatureSession()");
            bool command = triggerchar == '\0';
            IXMemberSymbol currentElement = null;
            SnapshotPoint ssp = this._textView.Caret.Position.BufferPosition;
            if (triggerchar == '(' && ssp.Position < ssp.Snapshot.Length && ssp.GetChar() == ')')
                ssp -= 1;
            if (triggerchar == '{' && ssp.Position < ssp.Snapshot.Length && ssp.GetChar() == '}')
                ssp -= 1;
            var location = _textView.FindLocation(ssp);
            if (location == null || location.Member == null)
                return false;
            if (location.Member.Range.StartLine == location.LineNumber)
                return false;

            var props = new XSharpSignatureProperties(location);
            props.triggerChar = triggerchar;
            var bufpos = this._textView.Caret.Position.BufferPosition;
            props.triggerPosition = bufpos.Position;
            props.triggerLine = bufpos.GetContainingLine().LineNumber;


            if (type != null && methodName != null)
            {
                var findStatic = triggerchar == '.';
                currentElement = XSharpLookup.SearchMethod(location, type, methodName, Modifiers.Private, findStatic).FirstOrDefault();
            }
            else
            {
                currentElement = findElementAt(command, triggerchar, ssp, props);
            }
            if (currentElement == null)
                return false;

            SnapshotPoint caret = _textView.Caret.Position.BufferPosition;
            ITextSnapshot caretsnapshot = caret.Snapshot;
            //
            if (_signatureBroker.IsSignatureHelpActive(_textView))
            {
                _signatureSession = _signatureBroker.GetSessions(_textView)[0];
            }
            else
            {
                _signatureSession = _signatureBroker.CreateSignatureHelpSession(_textView, caretsnapshot.CreateTrackingPoint(caret, PointTrackingMode.Positive), false);
            }
            _signatureSession.Properties[typeof(XSharpSignatureProperties)] = props;

            if (location.Member.Kind.IsGlobalTypeMember())
            {
                props.Visibility = Modifiers.Public;
            }
            else
            {
                props.Visibility = Modifiers.Protected;
            }
            _signatureSession.Dismissed += OnSignatureSessionDismiss;
            props.Element = currentElement;
            props.Start = ssp.Position;
            var caretPos = caret.Position;
            props.Length = caretPos - ssp.Position;
            var tokenList = XSharpTokenTools.GetTokenListBeforeCaret(location, out var state);
            bool done = false;
            int nested = 0;
            var last = tokenList.Count - 1;
            for (int i = last; i >= 0 && !done; i--)
            {
                var token = (XSharpToken)tokenList[i];
                switch (token.Type)
                {
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LCURLY:
                    case XSharpLexer.LBRKT:
                        done = nested == 0;
                        if (done)
                        {
                            props.Start = token.Position;
                        }
                        nested -= 1;
                        break;
                    case XSharpLexer.RPAREN:
                    case XSharpLexer.RCURLY:
                    case XSharpLexer.RBRKT:
                        if (token.Position < caretPos)
                        {
                            nested += 1;
                        }
                        break;
                }
            }

            var line = ssp.GetContainingLine();
            var start = props.Start - line.Start;
            props.Length = line.Length - start;
            try
            {
                _signatureSession.Start();
                MoveSignature();
            }
            catch (Exception e)
            {
                XSettings.LogException(e, "Start Signature session failed:");
            }
            //

            return true;
        }
        bool CancelSignatureSession()
        {
            if (!HasActiveSignatureSession)
                return false;

            _signatureSession.Dismiss();
            return true;
        }

        private void OnSignatureSessionDismiss(object sender, EventArgs e)
        {
            _signatureSession.Dismissed -= OnSignatureSessionDismiss;
            _signatureSession = null;
        }

        bool MoveSignature()
        {
            if (!HasActiveSignatureSession)
                return false;
            var props = _signatureSession.GetSignatureProperties();
            if (props == null)
                return false;
            SnapshotPoint ssp = this._textView.Caret.Position.BufferPosition;
            if (ssp.GetContainingLine().LineNumber != props.triggerLine)
            {
                CancelSignatureSession();
                return false;
            }
            var start = props.Start;
            int pos = ssp.Position;
            if (pos <= start)
            {
                CancelSignatureSession();
                return false;
            }


            ((XSharpVsSignature)_signatureSession.SelectedSignature).ComputeCurrentParameter(pos - start - 1);


            return true;
        }
        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableParameterLog && XSettings.EnableLogging)
            {
                XSettings.LogMessage("XSharp.ParameterInfo:" + strMessage);
            }
        }

    }
    internal static class SignatureExtensions
    {
        internal static XSharpSignatureProperties GetSignatureProperties(this ISignatureHelpSession session)
        {
            XSharpSignatureProperties props;
            if (!session.Properties.TryGetProperty(typeof(XSharpSignatureProperties), out props))
                return null;
            return props;
        }
    }
}
