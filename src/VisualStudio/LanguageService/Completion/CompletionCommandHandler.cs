﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//------------------------------------------------------------------------------

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.Linq;
using System.Runtime.InteropServices;
using XSharpModel;
using XSharp.Settings;
#pragma warning disable CS0649 // Field is never assigned to, for the imported fields
#if !ASYNCCOMPLETION
namespace XSharp.LanguageService
{
    internal class XSharpCompletionCommandHandler : IOleCommandTarget
    {
        readonly ITextView _textView;
        readonly ICompletionBroker _completionBroker;
        private readonly XDocument _doc;
        private readonly IOleCommandTarget m_nextCommandHandler;
        private readonly ITagAggregator<IClassificationTag> _tagAggregator;
        private readonly IVsTextView _textViewAdapter;
        bool completionWasSelected = false;
        XSharpSignatureHelpCommandHandler _signatureCommandHandler = null;

        internal XSharpCompletionCommandHandler(IVsTextView textViewAdapter, ITextView textView,
            ICompletionBroker completionBroker, IBufferTagAggregatorFactoryService aggregator)
        {
            this._textView = textView;
            this._completionBroker = completionBroker;
            this._doc = textView.TextBuffer.GetDocument();
            this._textViewAdapter = textViewAdapter;
            this._doc.CompletionSession = null;
            this._tagAggregator = aggregator.CreateTagAggregator<IClassificationTag>(_textView.TextBuffer);
            //add this to the filter chain
            textViewAdapter.AddCommandFilter(this, out m_nextCommandHandler);
        }
		
		public int QueryStatus(ref Guid pguidCmdGroup, uint cCmds, OLECMD[] commands, IntPtr pCmdText)
        {
            if (pguidCmdGroup == VSConstants.VSStd2K && cCmds > 0)
            {
                // make the commands appear on the context menu
                for (var i = 0; i < cCmds; i++)
                {
                    switch ((VSConstants.VSStd2KCmdID)commands[i].cmdID)
                    {
                        case VSConstants.VSStd2KCmdID.COMPLETEWORD:
                        case VSConstants.VSStd2KCmdID.AUTOCOMPLETE:
                        case VSConstants.VSStd2KCmdID.SHOWMEMBERLIST:
                            commands[i].cmdf = (int)(OLECMDF.OLECMDF_ENABLED | OLECMDF.OLECMDF_SUPPORTED);
                            return VSConstants.S_OK;
                    }
                }
            }

            return m_nextCommandHandler.QueryStatus(ref pguidCmdGroup, cCmds, commands, pCmdText);
        }


        public int Exec(ref Guid pguidCmdGroup, uint nCmdID, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            int result = VSConstants.S_OK;
            bool handled = false;
            Guid cmdGroup = pguidCmdGroup;
            // 1. Pre-process before anybody else has a chance
            if (XEditorSettings.DisableCodeCompletion)
            {
                ;
            }
            else if (cmdGroup == VSConstants.VSStd2K)
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
                    case (int)VSConstants.VSStd2KCmdID.BACKTAB:
                        if (_doc.ExpansionSession != null)
                        {
                            var res = _doc.ExpansionSession.GoToPreviousExpansionField();
                            handled = true;
                        }
                        break;
                    case (int)VSConstants.VSStd2KCmdID.TAB:
                        if (_doc.ExpansionSession != null)
                        {
                            var res = _doc.ExpansionSession.GoToNextExpansionField(1);
                            if (res != VSConstants.S_OK)
                            {
                                _doc.ExpansionSession.EndCurrentExpansion(fLeaveCaret: 1);
                                _doc.ExpansionSession = null;
                            }
                            handled = true;
                        }
                        else if (_doc.CompletionSession != null)
                        {
                        handled = CompleteCompletionSession('\t');
                        }
                        break;
                    case (int)VSConstants.VSStd2KCmdID.CANCEL:
                        handled = CancelCompletionSession();
                        break;
                    case (int)VSConstants.VSStd2KCmdID.TYPECHAR:
                        char ch = GetTypeChar(pvaIn);
                        if (CompletionActive)
                        {
                            switch (ch)
                            {
                                case '_':
                                    // do nothing now. Let the provider filter the list for us.
                                    break;

                                case '=':
                                    // typed :=
                                    CancelCompletionSession();
                                    break;
                                case '.':
                                    handled = CompleteCompletionSession(ch);
                                    break;
                                case ':':
                                    if (nextChar() == '=')
                                    {
                                        CancelCompletionSession();
                                    }
                                    else
                                    {
                                        handled = CompleteCompletionSession(ch);
                                    }
                                    break;
                                default:
                                    if (char.IsLetterOrDigit(ch))
                                    {
                                        ; // do nothing
                                    }
                                    else if (XEditorSettings.CommitChars.Contains(ch))
                                    {
                                        handled = CompleteCompletionSession(ch);
                                    }
                                    else
                                    {
                                        CancelCompletionSession();
                                    }

                                    break;
                            }
                            //
                        }

                        break;
                    default:
                        break;
                }
            }
            else if (cmdGroup == VSConstants.GUID_VSStandardCommandSet97)
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
                if (CompletionActive)
                {
                    if (CompletionSession.SelectedCompletionSet != null)
                    {
                        completionWasSelected = CompletionSession.SelectedCompletionSet.SelectionStatus.IsSelected;
                    }
                }
                result = m_nextCommandHandler.Exec(ref cmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut);
            }
            // 3. Post process
            if (!handled && ErrorHandler.Succeeded(result) && !XEditorSettings.DisableCodeCompletion)
            {
                if (cmdGroup == VSConstants.VSStd2K)
                {
                    switch (nCmdID)
                    {
                        case (int)VSConstants.VSStd2KCmdID.BACKSPACE:
                            FilterCompletionSession('\0');
                            break;

                        case (int)VSConstants.VSStd2KCmdID.RETURN:
                            // Check if we are inside a XMLDoc comment ///
                            InjectXMLDoc();
                            break;

                        case (int)VSConstants.VSStd2KCmdID.TYPECHAR:
                            char ch = GetTypeChar(pvaIn);
                            if (!CompletionActive)
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

        private void InjectXMLDoc()
        {
            try
            {
                XSharpXMLCompletion.InjectXMLDoc(_textView);
            }
            catch (Exception e)
            {
                WriteOutputMessage("InjectXMLDoc: error " + e.Message);
            }
        }
        private void InsertXMLDoc()
        {
            try
            {
                XSharpXMLCompletion.InsertXMLDoc(_textView);
            }
            catch (Exception e)
            {
                WriteOutputMessage("InsertXMLDoc: error " + e.Message);
            }
        }
#if false
        private void completeCurrentToken(uint nCmdID, char ch)
        {
            if (!CompletionAllowed(ch))
            {
                return;
            }
            SnapshotPoint caret = _textView.Caret.Position.BufferPosition;
            if (char.IsLetterOrDigit(ch) || ch == '_')
            {
                var line = caret.GetContainingLine();

                var lineText = line.GetText();
                var pos = caret.Position - line.Start.Position;
                int chars = 0;
                // count the number of characters in the current word. When > limit set in the options dialog then trigger completion
                for (int i = pos - 1; i >= 0; i--)
                {
                    var c = lineText[i];
                    if (!char.IsLetterOrDigit(c) && c != '_')
                    {
                        break;
                    }
                    chars++;
                    if (chars >= XEditorSettings.CompleteNumChars)
                        break;
                }
                if (chars >= XEditorSettings.CompleteNumChars)
                {
                    StartCompletionSession(nCmdID, '\0', true, true);
                }
            }
        }
#endif
        internal bool CompletionActive => _doc.CompletionSession != null;
        ICompletionSession CompletionSession
        {
            get => _doc.CompletionSession;
            set => _doc.CompletionSession = value;
        }
            
        private void FilterCompletionSession(char ch)
        {

            WriteOutputMessage("FilterCompletionSession()");
            if (!CompletionActive)
                return;

            WriteOutputMessage(" --> in Filter");
            if (CompletionSession.SelectedCompletionSet != null)
            {
                WriteOutputMessage(" --> Filtering ?");
                CompletionSession.SelectedCompletionSet.Filter();
                if (CompletionSession.SelectedCompletionSet.Completions.Count == 0)
                {
                    CancelCompletionSession();
                }
                else
                {
                    var props = CompletionSession.GetCompletionProperties();
                    if (ch != '\0')
                        props.Filter += ch;
                    WriteOutputMessage(" --> Selecting ");
                    CompletionSession.SelectedCompletionSet.SelectBestMatch();
                    CompletionSession.SelectedCompletionSet.Recalculate();
                }
            }

        }
        bool CancelCompletionSession()
        {
            if (!CompletionActive)
            {
                return false;
            }
            CompletionSession.Dismiss();
            return true;
        }
        bool CompleteCompletionSession(char ch)
        {
            if (!CompletionActive || CompletionSession.SelectedCompletionSet == null)
            {
                return false;
            }
            var session = CompletionSession;
            if (!session.SelectedCompletionSet.SelectionStatus.IsSelected)
            {
                CancelCompletionSession();
                return false;
            }
            Completion completion = session.SelectedCompletionSet.SelectionStatus.Completion;
            if (completion == null)
            {
                CancelCompletionSession();
                return false;
            }
            ITextCaret caret = session.TextView.Caret;
            WriteOutputMessage("CompleteCompletionSession()");
            bool addClose = false;
            Kind kind = Kind.Unknown;
            if (completion is XSCompletion xscompletion)
            {
                kind = xscompletion.Kind;
            }
            bool ctor = false;
            bool triggerSignatureHelp = false;
            // some tokens need to be added to the insertion text.
            switch (kind)
            {
                case Kind.Snippet:
                    session.Commit();
                    InsertSnippet((XSCompletion)completion);
                    return true;
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
            if (session.SelectedCompletionSet.Completions.Count > 0)
            {
                if (XEditorSettings.CompletionAutoPairs)
                {
                    addClose = true;
                    WriteOutputMessage(" --> select " + completion.InsertionText);
                    if (kind == Kind.Constructor)
                    {
                        completion.InsertionText += "{";
                    }
                    else if (kind.HasParameters() && !kind.IsProperty() && !completion.InsertionText.EndsWith("("))
                    {
                        if (!XEditorSettings.DisableAutoOpen)
                        {
                            completion.InsertionText += "(";
                        }
                    }
                }
            }
            else
            {
                // Push the completion char into the InsertionText if needed
                if (ch != '\0' && !completion.InsertionText.EndsWith(ch.ToString()))
                {
                    completion.InsertionText += ch;
                }
                if (XEditorSettings.CompletionAutoPairs)
                {
                    caret = CompletionSession.TextView.Caret;
                    addClose = true;
                }
            }
            if (addClose)
            {
                if (completion.InsertionText.EndsWith("("))
                {
                    completion.InsertionText += ")";
                    triggerSignatureHelp = true;
                }
                else if (completion.InsertionText.EndsWith("{"))
                {
                    completion.InsertionText += "}";
                    triggerSignatureHelp = true;
                }
                else if (completion.InsertionText.EndsWith("["))
                {
                    completion.InsertionText += "]";
                    triggerSignatureHelp = false;
                }

            }
            WriteOutputMessage(" --> Commit");
            var props = session.GetCompletionProperties();
            var type = props.Type;
            var triggerChar = props.Char;
            string insertionText = completion.InsertionText;
            // When completing by typing '.' or ':' we want to add that token as well
            if (ch == '.' || ch == ':')
            {
                if (insertionText.IndexOfAny(new char[] { '(', '{' }) == -1)
                {
                    completion.InsertionText += ch;
                }
            }
            session.Commit();
            // if a method or constructor was chosen, then trigger the signature help
            if (triggerSignatureHelp)
            {
                if (addClose)
                    caret.MoveToPreviousCaretPosition();
                TriggerSignatureHelp(type, insertionText, triggerChar);
            }
            return true;
        }
        bool StartCompletionSession(uint nCmdId, char typedChar, bool includeKeywords = false, bool autoType = false)
        {
            WriteOutputMessage("StartCompletionSession()");

            if (CompletionActive)
            {
                if (!CompletionSession.IsDismissed)
                    return false;
            }
            if (!CompletionAllowed(typedChar, !autoType))
                return false;


            SnapshotPoint point = _textView.Caret.Position.BufferPosition;
            ITextSnapshot snapshot = point.Snapshot;

            if (!_completionBroker.IsCompletionActive(_textView))
            {
                _doc.CompletionSession = _completionBroker.CreateCompletionSession(_textView, snapshot.CreateTrackingPoint(point, PointTrackingMode.Positive), true);
            }
            else
            {
                _doc.CompletionSession = _completionBroker.GetSessions(_textView)[0];
            }

            CompletionSession.Dismissed += OnCompletionSessionDismiss;
            //_completionSession.Committed += OnCompletionSessionCommitted;
            CompletionSession.SelectedCompletionSetChanged += SelectedCompletionSetChanged;
            var props = CompletionSession.GetCompletionProperties();
            props.Command = nCmdId;
            props.Position = point;
            props.Char = typedChar;
            props.AutoType = autoType;
            props.Type = null;
            props.IncludeKeywords = includeKeywords;
            props.Filter = "";
            props.NumCharsToDelete = 0;
            try
            {
                CompletionSession.Start();
                if (!CompletionAllowed(typedChar, !autoType))
                    CompletionSession.Dismiss();
            }
            catch (Exception e)
            {
                Logger.Exception(e, "Start Completion failed");
            }
            return true;
        }
        private void SelectedCompletionSetChanged(object sender, ValueChangedEventArgs<CompletionSet> e)
        {
            if (e.NewValue.SelectionStatus.IsSelected == false)
            {
                ;
            }
        }
        private void InsertSnippet(XSCompletion completion)
        {
            if (!completion.Properties.TryGetProperty<VsExpansion>(typeof(VsExpansion), out var snippet))
                return;
            var client = new ExpansionClient(_doc, _textViewAdapter);
            client.InsertAnyExpansion(null, snippet.title, snippet.path);
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
                _signatureCommandHandler.StartSignatureSession(type, method, triggerChar);
            }

            //
            //
        }
        private void OnCompletionSessionDismiss(object sender, EventArgs e)
        {
            if (CompletionSession.SelectedCompletionSet != null)
            {
                CompletionSession.SelectedCompletionSet.Filter();
            }
            //
            CompletionSession.Dismissed -= OnCompletionSessionDismiss;
            CompletionSession.SelectedCompletionSetChanged -= SelectedCompletionSetChanged;
            CompletionSession = null;
        }
        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableCodeCompletionLog && XSettings.EnableLogging)
            {
                Logger.Information("XSharp.Completion:" + strMessage);
            }
        }
        private char GetTypeChar(IntPtr pvaIn)
        {
            return (char)(ushort)Marshal.GetObjectForNativeVariant(pvaIn);
        }
        private char curChar()
        {
            // the caret is AFTER the last character !
            var caret = _textView.Caret.Position.BufferPosition;
            var pos = caret.Position;
            if (pos == 0)
                return '\0';
            return _textView.TextSnapshot.GetText(pos - 1, 1)[0];
        }
        private char prevChar()
        {
            // the caret is AFTER the last character !
            // assume the text is   ABC:|DEF where the caret is represented by the |
            // so pos = the next char 'D'
            // pos -1 is the current char ':'
            // and pos - 2 is the previous char 'C'
            var caret = _textView.Caret.Position.BufferPosition;
            var pos = caret.Position;
            if (pos < 2)
                return '\0';
            return _textView.TextSnapshot.GetText(pos - 2, 1)[0];
        }
        private char nextChar()
        {
            // the caret is AFTER the last character !
            var caret = _textView.Caret.Position.BufferPosition;
            var pos = caret.Position;
            if (pos == _textView.TextSnapshot.Length)
                return '\0';
            return _textView.TextSnapshot.GetText(pos, 1)[0];
        }
        private bool IsWs(char c)
        {
            switch (c)
            {
                case ' ':
                case '\t':
                case '\0':
                case '\r':
                case '\n':
                    return true;
            }
            return char.IsWhiteSpace(c);
        }
        private bool CompletionAllowed(char c, bool onCommand)
        {
            // := should never produce a list
            if (c == ':' && nextChar() == '=')
                return false;
            // single character should not trigger completion
            var p = prevChar();
            var n = nextChar();
            if (c == '\0')
                c = curChar();
            if (IsWs(p) && IsWs(n) && !onCommand)
            {
                // no completion for single character
                return false;
            }
            if (!IsWs(n) && !onCommand)
            {
                if (c != ':' && c != '.')
                {
                    // only allow completion before a non whitespace
                    // after a ':' or .'.'
                    if (p != ':' || p != '.')
                    {
                        return false;
                    }
                }
            }
            var caret = _textView.Caret.Position.BufferPosition;
            var line = caret.GetContainingLine();
            var pos = caret.Position - line.Start;
            SnapshotSpan lineSpan = new SnapshotSpan(line.Start, pos);
            var tags = _tagAggregator.GetTags(lineSpan);
            var tag = tags.LastOrDefault();
            var classification = tag?.Tag?.ClassificationType?.Classification;
            return !classification.IsClassificationCommentOrString();
        }
        void formatKeyword(Completion completion)
        {
            completion.InsertionText = XLiterals.FormatKeyword(completion.InsertionText);
        }

    }
}
#endif
