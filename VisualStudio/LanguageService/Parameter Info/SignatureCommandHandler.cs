using System;
using System.Linq;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Text;
using XSharpModel;
using Microsoft.VisualStudio.Editor;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Collections.Generic;


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
            else if (pguidCmdGroup == VSConstants.VSStd2K )
            {
                switch (nCmdID)
                {
                    case (int)VSConstants.VSStd2KCmdID.PARAMINFO:
                        StartSignatureSession(true);
                        break;
                    case (int)VSConstants.VSStd2KCmdID.COMPLETEWORD:
                    case (int)VSConstants.VSStd2KCmdID.AUTOCOMPLETE:
                    case (int)VSConstants.VSStd2KCmdID.SHOWMEMBERLIST:
                        CancelSignatureSession();
                        break;
                    case (int)VSConstants.VSStd2KCmdID.BACKSPACE:
                        if (_signatureSession != null)
                        {
                            int pos = _textView.Caret.Position.BufferPosition;
                            if (pos > 0)
                            {
                                // get previous char
                                var previous = _textView.TextBuffer.CurrentSnapshot.GetText().Substring(pos - 1, 1);
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
                        CancelSignatureSession();
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

                        case (int) VSConstants.VSStd2KCmdID.TYPECHAR:
                            typedChar = (char)(ushort)Marshal.GetObjectForNativeVariant(pvaIn);
                            switch (typedChar)
                            {
                                case '(':
                                case '{':
                                    CancelSignatureSession();
                                    StartSignatureSession(false, triggerchar:typedChar);
                                    break;
                                case ')':
                                case '}':
                                    CancelSignatureSession();
                                    if (hasopenSignatureSession())
                                    {
                                        StartSignatureSession(false, triggerchar: typedChar);
                                    }
                                    break;
                                case ',':
                                     StartSignatureSession(true, triggerchar: typedChar);
                                    //MoveSignature();
                                    break;
                                case ':':
                                case '.':
                                    CancelSignatureSession();
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
                            MoveSignature();
                            break;


                    }
                }
            }
            return result;
        }

        bool hasopenSignatureSession()
        {
            SnapshotPoint ssp = this._textView.Caret.Position.BufferPosition;
            var level = 0;
            bool done = false;
            while (ssp.Position > 0 && ! done)
            {
                ssp = ssp - 1;
                var ch = ssp.GetChar();
                {
                    switch (ch)
                    {
                        case '(':
                        case '{':
                            level += 1;
                            break;
                        case ')':
                        case '}':
                            level -= 1;
                            break;
                        case '\r':
                        case '\n':
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
        IXMemberSymbol findElementAt(bool comma, SnapshotPoint ssp, XSharpSignatureProperties props)
        {
            // when coming from the completion list then there is no need to check a lot of stuff
            // we can then simply lookup the method and that is it.
            // Also no need to filter on visibility since that has been done in the completionlist already !
            // First, where are we ?
            var location = props.Location;
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
            var openTokens = new Stack<XSharpToken>();
            for (int i = 0; i < tokenList.Count; i++)
            {
                var token = tokenList[i];
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
                        if (token.Position < location.Position && location.Position < token.Position+token.Text.Length)
                        {
                            // comma inside literal !
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
                    tokenList.RemoveRange(pos + 1, tokenList.Count - pos - 1);
                }
            }
            if (comma)
            {
                // check to see if there is a lparen or lcurly before the comma
                bool done = false;
                nested = 0;
                while (tokenList.Count > 0 && ! done)
                {
                    var token = tokenList[tokenList.Count-1];
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
                    for (int i = tokenList.Count -1; ! done && i > 0; i--)
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

        internal bool StartSignatureSession(bool comma, IXTypeSymbol type = null, string methodName = null, char triggerchar = '\0')
        {
            WriteOutputMessage("StartSignatureSession()");

            if (_signatureSession != null)
                return false;
            IXMemberSymbol currentElement = null;
            SnapshotPoint ssp = this._textView.Caret.Position.BufferPosition;
            if (triggerchar == '(' && ssp.Position < ssp.Snapshot.Length && ssp.GetChar() == ')')
                ssp -= 1;
            var location = _textView.FindLocation(ssp);
            if (location == null)
                return false;
            if (location.Member.Range.StartLine == location.LineNumber)
                return false;

            var props = new XSharpSignatureProperties(location);
            props.triggerChar = triggerchar;
            props.triggerPosition = this._textView.Caret.Position.BufferPosition.Position;

            if (type != null && methodName != null)
            {
                var findStatic = triggerchar == '.';
                currentElement = XSharpLookup.SearchMethod(location, type, methodName, Modifiers.Private, findStatic).FirstOrDefault();
            }
            else
            {
                currentElement = findElementAt(comma, ssp, props);
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
                _signatureSession = _signatureBroker.CreateSignatureHelpSession(_textView, caretsnapshot.CreateTrackingPoint(caret, PointTrackingMode.Positive), true);
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
            if (comma)
            {
                var tokenList = XSharpTokenTools.GetTokenListBeforeCaret(location, out var state);
                bool done = false;
                int nested = 0;
                for (int i = tokenList.Count-1; i >= 0 && ! done; i--)
                {
                    var token = tokenList[i];
                    switch (token.Type)
                    {
                        case XSharpLexer.LPAREN:
                        case XSharpLexer.LCURLY:
                        case XSharpLexer.LBRKT:
                            done = nested == 0;
                            if (done)
                            {
                                props.Start = token.Position;
                                props.Length = _textView.Caret.Position.BufferPosition.Position - token.Position;
                            }
                            nested -= 1;
                            break;
                        case XSharpLexer.RPAREN:
                        case XSharpLexer.RCURLY:
                        case XSharpLexer.RBRKT:
                            nested += 1;
                            break;
                    }
                }

            }
            else
            {
                props.Start = ssp.Position;
                props.Length = _textView.Caret.Position.BufferPosition.Position - ssp.Position;
            }
            

            try
            {
                _signatureSession.Start();
            }
            catch (Exception e)
            {
                WriteOutputMessage("Start Signature session failed:");
                XSettings.DisplayException(e);
            }
            //
            return true;
        }
        internal bool HasActiveSession => _signatureSession != null;
        bool CancelSignatureSession()
        {
            if (_signatureSession == null)
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
            if (_signatureSession == null)
                return false;
            if (! _signatureSession.Properties.TryGetProperty(typeof(XSharpSignatureProperties), out XSharpSignatureProperties props))
                return false;
            var start = props.Start;
            int pos = this._textView.Caret.Position.BufferPosition.Position;

            ((XSharpVsSignature)_signatureSession.SelectedSignature).ComputeCurrentParameter(pos - start - 1);


            return true;
        }
        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableParameterLog && XSettings.EnableLogging)
            {
                XSettings.DisplayOutputMessage("XSharp.ParameterInfo:" + strMessage);
            }
        }

    }
}
