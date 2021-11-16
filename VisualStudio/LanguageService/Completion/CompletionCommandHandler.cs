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
using Microsoft.VisualStudio.Text.Tagging;
using System.Collections.Generic;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp;
using static XSharp.LanguageService.XSharpFormattingCommandHandler;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
#pragma warning disable CS0649 // Field is never assigned to, for the imported fields
#if !ASYNCCOMPLETION
namespace XSharp.LanguageService
{
    [Export(typeof(IVsTextViewCreationListener))]
    [Name("XSharp Completion Provider")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [ContentType(XSharpConstants.LanguageName)]
    internal class XSharpCompletionProvider : IVsTextViewCreationListener
    {

        [Import]
        internal IVsEditorAdaptersFactoryService AdapterService;

        [Import]
        internal ICompletionBroker CompletionBroker { get; set; }

        [Import]
        internal IBufferTagAggregatorFactoryService BufferTagAggregatorFactoryService { get; set; }
        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            if (XSettings.DisableCodeCompletion)
                return;
            ITextView textView = AdapterService.GetWpfTextView(textViewAdapter);
            if (textView == null)
                return;
            textView.Properties.GetOrCreateSingletonProperty(
                 () => new XSharpCompletionCommandHandler(textViewAdapter,
                    textView,
                    CompletionBroker,
                    BufferTagAggregatorFactoryService
                    ));
        }
    }

    internal class XSharpCompletionCommandHandler : IOleCommandTarget
    {
        readonly ITextView _textView;
        readonly ICompletionBroker _completionBroker;
        private ICompletionSession _completionSession;
        readonly IOleCommandTarget m_nextCommandHandler;
        readonly IBufferTagAggregatorFactoryService _aggregator;
        bool completionWasSelected = false;
        XSharpSignatureHelpCommandHandler _signatureCommandHandler = null;
        int slashCounter;

        internal XSharpCompletionCommandHandler(IVsTextView textViewAdapter, ITextView textView,
            ICompletionBroker completionBroker, IBufferTagAggregatorFactoryService aggregator)
        {
            this._textView = textView;
            this._completionBroker = completionBroker;
            this._completionSession = null;
            this._aggregator = aggregator;
            //add this to the filter chain
            textViewAdapter.AddCommandFilter(this, out m_nextCommandHandler);
            slashCounter = 0;
        }


        public int Exec(ref Guid pguidCmdGroup, uint nCmdID, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            int result = VSConstants.S_OK;
            bool handled = false;
            Guid cmdGroup = pguidCmdGroup;
            // 1. Pre-process
            if (XSettings.DisableCodeCompletion)
            {
                ;
            }
            else if (pguidCmdGroup == VSConstants.VSStd2K)
            {
                switch (nCmdID)
                {
                    case (int)VSConstants.VSStd2KCmdID.COMPLETEWORD:
                    case (int)VSConstants.VSStd2KCmdID.AUTOCOMPLETE:
                    case (int)VSConstants.VSStd2KCmdID.SHOWMEMBERLIST:
                        // in this case we WANT to include keywords in the list
                        // when they type LOCA we want to include LOCAL as well
                        handled = StartCompletionSession(nCmdID, '\0', true);
                        break;
                    case (int)VSConstants.VSStd2KCmdID.RETURN:
                        handled = CompleteCompletionSession('\0');
                        break;
                    case (int)VSConstants.VSStd2KCmdID.TAB:
                        handled = CompleteCompletionSession('\t');
                        break;
                    case (int)VSConstants.VSStd2KCmdID.CANCEL:
                        handled = CancelCompletionSession();
                        break;
                    case (int)VSConstants.VSStd2KCmdID.TYPECHAR:
                        char ch = GetTypeChar(pvaIn);
                        if (_completionSession != null)
                        {
                            if (char.IsLetterOrDigit(ch) || ch == '_')
                            {
                                ; // do nothing now. Let the provider filter the list for us.
                            }
                            else
                            {
                                if (ch == '=' && _completionSession.Properties.TryGetProperty(XsCompletionProperties.Char, out char triggerChar) && triggerChar == ':')
                                {
                                    CancelCompletionSession();
                                }
                                else if (ch == '.' || ch == ':')
                                {
                                    handled = CompleteCompletionSession(ch);
                                }
                                else if (XSettings.EditorCommitChars.Contains(ch))
                                {
                                    handled = CompleteCompletionSession(ch);
                                }
                                else
                                {
                                    CancelCompletionSession();
                                }
                            }
                            //
                        }

                        break;
                    default:
                        break;
                }
            }
            else if (pguidCmdGroup == VSConstants.GUID_VSStandardCommandSet97)
            {
                switch (nCmdID)
                {
                    case (int)VSConstants.VSStd97CmdID.Undo:
                    case (int)VSConstants.VSStd97CmdID.Redo:
                        CancelCompletionSession();
                        break;

                    default:
                        break;
                }
            }
            // 2. Let others do their thing
            // Let others do their thing
            if (!handled)
            {
                if (_completionSession != null)
                {
                    if (_completionSession.SelectedCompletionSet != null)
                    {
                        completionWasSelected = _completionSession.SelectedCompletionSet.SelectionStatus.IsSelected;
                    }
                }
                result = m_nextCommandHandler.Exec(ref cmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut);
            }           // 3. Post process
            if (ErrorHandler.Succeeded(result) && !XSettings.DisableCodeCompletion)
            {
                if (pguidCmdGroup == VSConstants.VSStd2K)
                {

                    switch (nCmdID)
                    {
                        case (int)VSConstants.VSStd2KCmdID.BACKSPACE:
                            FilterCompletionSession('\0');
                            break;

                        case (int)VSConstants.VSStd2KCmdID.TYPECHAR:
                            char ch = GetTypeChar(pvaIn);
                            if (_completionSession == null)
                            {
                                switch (ch)
                                {
                                    case ':':
                                    case '.':
                                        StartCompletionSession(nCmdID, ch);
                                        break;
                                    case '/':
                                        InsertXMLDoc();
                                        break;
                                    default:
                                        //completeCurrentToken(nCmdID, ch);
                                        break;
                                }
                            }
                            else
                            {
                                FilterCompletionSession(ch);
                            }
                            break;
                        default:
                            break;

                    }
                }
            }

            return result;
        }

        private void InsertXMLDoc()
        {
            try
            {
                // Retrieve Position
                SnapshotPoint caret = _textView.Caret.Position.BufferPosition;
                var line = caret.GetContainingLine();
                SnapshotSpan lineSpan = new SnapshotSpan(line.Start, caret.Position - line.Start);
                // And the text before the current character 
                string lineText = lineSpan.GetText();
                // Remove all type of spaces
                lineText = lineText.TrimStart(' ', '\t');
                // XMLDoc ?
                if (lineText == "///")
                {
                    bool inAComment = true;
                    // Just to be sure we are not in a comment...
                    // Let's move at the beginning of the next line
                    ITextSnapshotLine lineDown = _textView.TextSnapshot.GetLineFromLineNumber(line.LineNumber + 1);
                    if (lineDown.Length > 0)
                    {
                        SnapshotPoint sspStart = new SnapshotPoint(_textView.TextSnapshot, lineDown.Start.Position);
                        inAComment = cursorIsAfterSLComment(sspStart);
                    }
                    // 
                    if (!inAComment)
                    {
                        // Retrieve the tokens 
                        var lineTokens = getTokensInLine(lineDown);
                        FormattingContext context = new FormattingContext(lineTokens, XSharpDialect.Core);
                        IToken startToken = context.GetFirstToken(true, true);
                        // Search for ID
                        while ((startToken != null) &&
                            ( (startToken.Type != XSharpLexer.ID) &&
                            (startToken.Type != XSharpLexer.CONSTRUCTOR) &&
                            (startToken.Type != XSharpLexer.DESTRUCTOR)
                            ))
                        {
                            context.MoveToNext();
                            startToken = context.GetFirstToken(true, true);
                        }
                        if (startToken == null)
                        {
                            return;
                        }

                        // Try to retrieve the Entity down
                        SnapshotPoint ssp = new SnapshotPoint(_textView.TextSnapshot, lineDown.Start.Position + startToken.StartIndex);
                        var location = lineDown.Snapshot.TextBuffer.FindLocation(ssp);
                        var tokens = lineDown.Snapshot.TextBuffer.GetTokens();
                        CompletionState state;
                        var tokenList = XSharpTokenTools.GetTokensUnderCursor(location, tokens.TokenStream, out state);
                        var lookupresult = new List<IXSymbol>();
                        lookupresult.AddRange(XSharpLookup.RetrieveElement(location, tokenList, state, out var notProcessed, true));
                        //
                        if (lookupresult.Count > 0)
                        {
                            var element = lookupresult[0];
                            // Default information
                            // Retrieve the original line, and keep the prefix
                            lineText = lineSpan.GetText();
                            string prefix = lineText.Remove(lineText.Length - 3, 3);
                            // Insert XMLDoc template
                            string xmlDoc = " <summary>" + Environment.NewLine;
                            xmlDoc += prefix + "/// " + Environment.NewLine;
                            xmlDoc += prefix + "/// </summary>";
                            // Has parameters ?
                            if (element.Kind.HasParameters())
                            {
                                if (element is IXMemberSymbol mem)
                                {
                                    // Now fill with retrieved information
                                    foreach (var param in mem.Parameters)
                                    {
                                        xmlDoc += Environment.NewLine + prefix + "/// <param name=";
                                        xmlDoc += '"';
                                        xmlDoc += param.Name;
                                        xmlDoc += '"';
                                        xmlDoc += "></param>";
                                    }
                                }
                            }
                            if ( element.Kind.HasReturnType() )
                            {
                                if (element is IXSymbolBase elt)
                                {
                                    if ((string.Compare(elt.TypeName, "void", true) != 0))
                                    {
                                        xmlDoc += Environment.NewLine + prefix + "/// <returns>";
                                        xmlDoc += "</returns>";
                                    }
                                }
                            }
                            _textView.TextBuffer.Insert(_textView.Caret.Position.BufferPosition.Position, xmlDoc);
                            // Move the Caret in the Summary area
                            ITextSnapshotLine moveToline = _textView.TextSnapshot.GetLineFromLineNumber(line.LineNumber + 1);
                            SnapshotPoint point = new SnapshotPoint(moveToline.Snapshot, moveToline.End.Position);
                            _textView.Caret.MoveTo(point);
                        }
                    }
                }
            }
            catch (Exception e)
            {
                WriteOutputMessage("InsertXMLDoc: error " + e.Message);
            }
        }

        //private void completeCurrentToken(uint nCmdID, char ch)
        //{
        //    SnapshotPoint caret = _textView.Caret.Position.BufferPosition;
        //    if (cursorIsInStringorComment(caret))
        //    {
        //        return;
        //    }
        //    if (char.IsLetterOrDigit(ch) || ch == '_')
        //    {
        //        var line = caret.GetContainingLine();

        //        var lineText = line.GetText();
        //        var pos = caret.Position - line.Start.Position;
        //        int chars = 0;
        //        // count the number of characters in the current word. When > limit set in the options dialog then trigger completion
        //        for (int i = pos - 1; i >= 0; i--)
        //        {
        //            var c = lineText[i];
        //            if (! char.IsLetterOrDigit(c) && c != '_' )
        //            { 
        //                break;
        //            }
        //            chars++;
        //            if (chars >= XSettings.CompleteNumChars)
        //                break;
        //        }
        //        if (chars >= XSettings.CompleteNumChars)
        //        {
        //            StartCompletionSession(nCmdID, '\0', true, true);
        //        }
        //    }
        //}

        private void FilterCompletionSession(char ch)
        {

            WriteOutputMessage("FilterCompletionSession()");
            if (_completionSession == null)
                return;

            WriteOutputMessage(" --> in Filter");
            if (_completionSession.SelectedCompletionSet != null)
            {
                WriteOutputMessage(" --> Filtering ?");
                _completionSession.SelectedCompletionSet.Filter();
                if (_completionSession.SelectedCompletionSet.Completions.Count == 0)
                {
                    CancelCompletionSession();
                }
                else
                {
                    WriteOutputMessage(" --> Selecting ");
                    _completionSession.SelectedCompletionSet.SelectBestMatch();
                    _completionSession.SelectedCompletionSet.Recalculate();
                }
            }

        }


        bool CancelCompletionSession()
        {
            if (_completionSession == null)
            {
                return false;
            }
            _completionSession.Dismiss();
            return true;
        }

        bool CompleteCompletionSession(char ch)
        {
            if (_completionSession == null)
            {
                return false;
            }
            bool commit = false;
            bool moveBack = false;
            Completion completion = null;
            XSCompletion xscompletion = null;
            ITextCaret caret = null;
            WriteOutputMessage("CompleteCompletionSession()");
            if (_completionSession.SelectedCompletionSet != null)
            {
                bool addDelim = false;

                if (_completionSession.SelectedCompletionSet.SelectionStatus.Completion != null)
                {
                    completion = _completionSession.SelectedCompletionSet.SelectionStatus.Completion;
                }
                xscompletion = completion as XSCompletion;
                Kind kind = Kind.Unknown;
                if (xscompletion != null)
                {
                    kind = xscompletion.Kind;
                }
                bool ctor = false;
                // some tokens need to be added to the insertion text.
                switch (kind)
                {
                    case Kind.Keyword:
                        formatKeyword(completion);
                        break;
                    case Kind.Class:
                    case Kind.Structure:
                    case Kind.Constructor:
                        ctor = true;
                        goto default;
                    default:
                        switch (ch)
                        {
                            case '{' when ctor:
                            case '}' when ctor:
                                if (!completion.InsertionText.EndsWith("{"))
                                    completion.InsertionText += "{";
                                break;
                            case '\t': // Tab
                            case '\r': // CR
                            case '\n': // CR
                            case '.': // DOT
                            case ':': // COLON
                            case '\0':
                                break;
                            default:
                                var s = ch.ToString();
                                if (!completion.InsertionText.EndsWith(s))
                                    completion.InsertionText += s;
                                break;
                        }
                        break;
                }
                if ((_completionSession.SelectedCompletionSet.Completions.Count > 0) && (_completionSession.SelectedCompletionSet.SelectionStatus.IsSelected))
                {

                    if (XSettings.EditorCompletionAutoPairs)
                    {
                        caret = _completionSession.TextView.Caret;
                        addDelim = true;
                        WriteOutputMessage(" --> select " + completion.InsertionText);
                        if (kind == Kind.Constructor)
                        {
                            completion.InsertionText += "{";
                        }
                        if (kind.HasParameters() && !completion.InsertionText.EndsWith("("))
                        {
                            completion.InsertionText += "(";
                        }
                    }
                    commit = true;
                }
                else
                {
                    if (completion != null)
                    {
                        // Push the completion char into the InsertionText if needed
                        if (!completion.InsertionText.EndsWith(ch.ToString()))
                        {
                            completion.InsertionText += ch;
                        }
                        if (XSettings.EditorCompletionAutoPairs)
                        {
                            caret = _completionSession.TextView.Caret;
                            addDelim = true;
                        }
                    }
                    commit = true;
                }
                if (addDelim)
                {
                    if (completion.InsertionText.EndsWith("("))
                    {
                        moveBack = true;
                        completion.InsertionText += ")";
                    }
                    else if (completion.InsertionText.EndsWith("{"))
                    {
                        moveBack = true;
                        completion.InsertionText += "}";
                    }
                    else if (completion.InsertionText.EndsWith("["))
                    {
                        moveBack = true;
                        completion.InsertionText += "]";
                    }

                }
            }
            if (commit)
            {
                WriteOutputMessage(" --> Commit");
                var session = _completionSession;
                session.Properties.TryGetProperty(XsCompletionProperties.Type, out IXTypeSymbol type);
                string insertionText = completion.InsertionText;
                session.Properties.TryGetProperty(XsCompletionProperties.Char, out char triggerChar);
                if (ch == '.' || ch == ':')
                {
                    if (insertionText.IndexOfAny("({".ToCharArray()) == -1)
                        completion.InsertionText += ch;

                }
                _completionSession.Commit();
                if (moveBack && (caret != null))
                {
                    caret.MoveToPreviousCaretPosition();
                }
                // if a method or constructor was chosen, then trigger the signature help
                if (insertionText.Contains('(') || insertionText.Contains('{'))
                {
                    TriggerSignatureHelp(type, insertionText, triggerChar);
                }
                return true;
            }

            WriteOutputMessage(" --> Dismiss");
            _completionSession.Dismiss();


            return false;
        }
        bool StartCompletionSession(uint nCmdId, char typedChar, bool includeKeywords = false, bool autoType = false)
        {
            WriteOutputMessage("StartCompletionSession()");

            if (_completionSession != null)
            {
                if (!_completionSession.IsDismissed)
                    return false;
            }
            SnapshotPoint caret = _textView.Caret.Position.BufferPosition;
            if (cursorIsAfterSLComment(caret))
                return false;

            ITextSnapshot snapshot = caret.Snapshot;

            if (!_completionBroker.IsCompletionActive(_textView))
            {
                _completionSession = _completionBroker.CreateCompletionSession(_textView, snapshot.CreateTrackingPoint(caret, PointTrackingMode.Positive), true);
            }
            else
            {
                _completionSession = _completionBroker.GetSessions(_textView)[0];
            }

            _completionSession.Dismissed += OnCompletionSessionDismiss;
            //_completionSession.Committed += OnCompletionSessionCommitted;
            _completionSession.SelectedCompletionSetChanged += _completionSession_SelectedCompletionSetChanged;

            _completionSession.Properties[XsCompletionProperties.Command] = nCmdId;
            _completionSession.Properties[XsCompletionProperties.Char] = typedChar;
            _completionSession.Properties[XsCompletionProperties.AutoType] = autoType;
            _completionSession.Properties[XsCompletionProperties.Type] = null;
            _completionSession.Properties[XsCompletionProperties.IncludeKeywords] = includeKeywords;
            try
            {
                _completionSession.Start();
            }
            catch (Exception e)
            {
                WriteOutputMessage("Start Completion failed");
                XSettings.DisplayException(e);
            }
            return true;
        }

        internal bool HasActiveSession => _completionSession != null;

        private void _completionSession_SelectedCompletionSetChanged(object sender, ValueChangedEventArgs<CompletionSet> e)
        {
            if (e.NewValue.SelectionStatus.IsSelected == false)
            {
                ;
            }
        }

        private void TriggerSignatureHelp(IXTypeSymbol type, string method, char triggerChar)
        {
            // it MUST be the case....
            WriteOutputMessage("OnCompletionSessionCommitted()");

            if (triggerChar == '{' || triggerChar == '}')
            {
                if (method.EndsWith("{}"))
                    method = method.Substring(0, method.Length - 2);
                else if (method.EndsWith("{"))
                    method = method.Substring(0, method.Length - 1);
                //triggerChar = '{';
            }
            else
            {
                if (method.EndsWith("()"))
                    method = method.Substring(0, method.Length - 2);
                else if (method.EndsWith("("))
                    method = method.Substring(0, method.Length - 1);
                //triggerChar = '(';
            }

            // send command to editor to Signature Helper
            if (_signatureCommandHandler == null)
            {
                _signatureCommandHandler = _textView.Properties.GetProperty<XSharpSignatureHelpCommandHandler>(typeof(XSharpSignatureHelpCommandHandler));
            }
            if (_signatureCommandHandler != null)
            {
                _signatureCommandHandler.StartSignatureSession(true, type, method, triggerChar);
            }

            //
            //
        }

        private void OnCompletionSessionDismiss(object sender, EventArgs e)
        {
            if (_completionSession.SelectedCompletionSet != null)
            {
                _completionSession.SelectedCompletionSet.Filter();
            }
            //
            _completionSession.Dismissed -= OnCompletionSessionDismiss;
            _completionSession.SelectedCompletionSetChanged -= _completionSession_SelectedCompletionSetChanged;
            _completionSession = null;
        }

        public int QueryStatus(ref Guid pguidCmdGroup, uint cCmds, OLECMD[] prgCmds, IntPtr pCmdText)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            return m_nextCommandHandler.QueryStatus(ref pguidCmdGroup, cCmds, prgCmds, pCmdText);
        }
        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableCodeCompletionLog && XSettings.EnableLogging)
            {
                XSettings.DisplayOutputMessage("XSharp.Completion:" + strMessage);
            }
        }
        private char GetTypeChar(IntPtr pvaIn)
        {
            return (char)(ushort)Marshal.GetObjectForNativeVariant(pvaIn);
        }
        private bool cursorIsInStringorComment(SnapshotPoint caret)
        {
            var classification = getClassification(caret);
            return IsCommentOrString(classification);
        }
        private bool cursorIsAfterSLComment(SnapshotPoint caret)
        {

            var classification = getClassification(caret);
            return classification != null && classification.ToLower() == "comment";
        }
        void formatKeyword(Completion completion)
        {
            completion.InsertionText = XSettings.FormatKeyword(completion.InsertionText);
        }

        private string getClassification(SnapshotPoint caret)
        {
            var line = caret.GetContainingLine();
            SnapshotSpan lineSpan = new SnapshotSpan(line.Start, caret.Position - line.Start);
            var tagAggregator = _aggregator.CreateTagAggregator<IClassificationTag>(_textView.TextBuffer);
            var tags = tagAggregator.GetTags(lineSpan);
            var tag = tags.LastOrDefault();
            return tag?.Tag?.ClassificationType?.Classification;
        }
        private bool IsCommentOrString(string classification)
        {
            if (string.IsNullOrEmpty(classification))
                return false;
            switch (classification.ToLower())
            {
                case "comment":
                case "string":
                case "xsharp.text":
                    return true;
            }
            return false;
        }

        #region Token Helpers for XMLDoc generation
        private bool getBufferedTokens(out XSharpTokens xTokens, ITextBuffer textBuffer)
        {
            if (textBuffer.Properties != null && textBuffer.Properties.TryGetProperty(typeof(XSharpTokens), out xTokens))
            {
                return xTokens != null && xTokens.Complete;
            }
            xTokens = null;
            return false;
        }

        private IList<IToken> getTokensInLine(ITextSnapshotLine line)
        {
            IList<IToken> tokens = new List<IToken>();
            ITextBuffer textBuffer = line.Snapshot.TextBuffer;
            // Already been lexed ?
            if (getBufferedTokens(out var xTokens, textBuffer))
            {
                var allTokens = xTokens.TokenStream.GetTokens();
                if (allTokens != null)
                {
                    if (xTokens.SnapShot.Version == textBuffer.CurrentSnapshot.Version)
                    {
                        // Ok, use it
                        int startIndex = -1;
                        // Move to the line position
                        for (int i = 0; i < allTokens.Count; i++)
                        {
                            if (allTokens[i].StartIndex >= line.Start.Position)
                            {
                                startIndex = i;
                                break;
                            }
                        }
                        if (startIndex > -1)
                        {
                            // Move to the end of line
                            int currentLine = allTokens[startIndex].Line;
                            do
                            {
                                tokens.Add(allTokens[startIndex]);
                                startIndex++;

                            } while ((startIndex < allTokens.Count) && (currentLine == allTokens[startIndex].Line));
                            return tokens;
                        }
                    }
                }
            }
            // Ok, do it now
            var text = line.GetText();
            tokens = getTokens(text);
            return tokens;
            //
        }

        private IList<IToken> getTokens(string text)
        {
            IList<IToken> tokens;
            try
            {
                string fileName;
                fileName = "MissingFile.prg";
                var reporter = new ErrorIgnorer();
                bool ok = XSharp.Parser.VsParser.Lex(text, fileName, XSharpParseOptions.Default, reporter, out ITokenStream tokenStream);
                var stream = tokenStream as BufferedTokenStream;
                tokens = stream.GetTokens();
            }
            catch (Exception e)
            {
                XSettings.DisplayException(e);
                tokens = new List<IToken>();
            }
            return tokens;
        }
        #endregion
    }
}
#endif
