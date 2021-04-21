//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Diagnostics;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Language.Intellisense;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Collections.Generic;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using XSharpModel;
using System.Linq;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft;
using Microsoft.VisualStudio.Shell;

namespace XSharp.LanguageService
{
    internal sealed partial class CommandFilter : IOleCommandTarget
    {
        public ITextView TextView { get; private set; }
        public IOleCommandTarget Next { get; set; }

        readonly ICompletionBroker _completionBroker;
        private ICompletionSession _completionSession;
        XSharpClassifier _classifier;
        readonly XFile _file;
        readonly ISignatureHelpBroker _signatureBroker;
        ISignatureHelpSession _signatureSession;
        private static int _lastIndentValue;    // in number of characters
        private readonly ITextBuffer _buffer;
        readonly IBufferTagAggregatorFactoryService _aggregator;
        readonly List<int> _linesToSync;
        bool _suspendSync = false;

        private void registerClassifier()
        {
            if (_buffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
            {
                if (_classifier == null)
                {
                    if (_buffer.Properties.TryGetProperty(typeof(XSharpClassifier), out _classifier))
                    {
                        _classifier.ClassificationChanged += Classifier_ClassificationChanged;
                    }
                }
            }
            else
            {
                _classifier = null;
            }

        }

        private int currentLine
        {
            get
            {
                SnapshotPoint caret = this.TextView.Caret.Position.BufferPosition;
                ITextSnapshotLine line = caret.GetContainingLine();
                return line.LineNumber;
            }
        }
        private void Classifier_ClassificationChanged(object sender, ClassificationChangedEventArgs e)
        {
            WriteOutputMessage("CommandFilter.ClassificationChanged()");
            if (_suspendSync)
                return;
            if (_settings.KeywordCase == KeywordCase.None)
            {
                return;
            }
            // do not update buffer from background thread
            if (!_buffer.CheckEditAccess())
            {
                return;
            }

            if (_linesToSync.Count > 0)
            {
                int[] lines;
                lock (_linesToSync)
                {
                    lines = _linesToSync.ToArray();
                    _linesToSync.Clear();
                    _linesToSync.Add(this.currentLine);
                    Array.Sort(lines);
                }

                // wait until we can work
                while (_buffer.EditInProgress)
                {
                    System.Threading.Thread.Sleep(100);
                }

                ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        var editSession = _buffer.CreateEdit();
                        var snapshot = editSession.Snapshot;
                        try
                        {
                            var end = DateTime.Now + new TimeSpan(0, 0, 2);
                            int counter = 0;
                            foreach (int nLine in lines)
                            {
                                ITextSnapshotLine line = snapshot.GetLineFromLineNumber(nLine);
                                formatLineCase(editSession, line);
                                // when it takes longer than 2 seconds, then abort
                                if (++counter > 100 && DateTime.Now > end)
                                    break;
                            }
                        }
                        catch (Exception)
                        {
                            ;
                        }
                        finally
                        {
                            if (editSession.HasEffectiveChanges)
                            {
                                editSession.Apply();
                            }
                            else
                            {
                                editSession.Cancel();
                            }
                        }
                    });


            }
        }
        public CommandFilter(IWpfTextView textView, ICompletionBroker completionBroker, ISignatureHelpBroker signatureBroker, IBufferTagAggregatorFactoryService aggregator, VsTextViewCreationListener provider)
        {

            m_provider = provider;

            _completionSession = null;
            _signatureSession = null;

            TextView = textView;
            _completionBroker = completionBroker;
            _signatureBroker = signatureBroker;
            _aggregator = aggregator;
            _linesToSync = new List<int>();
            var package = XSharpLanguageService.Instance;
            //
            _buffer = TextView.TextBuffer;
            if (_buffer != null)
            {
                _buffer.ChangedLowPriority += Textbuffer_Changed;
                _buffer.Changing += Textbuffer_Changing;
                _file = _buffer.GetFile();

                if (_buffer.CheckEditAccess())
                {
                    //formatCaseForWholeBuffer();
                }
            }
            ReadSettings(_file.FullPath);
        }

        private void Textbuffer_Changing(object sender, TextContentChangingEventArgs e)
        {
            if (XSettings.DebuggerIsRunning)
            {
                XSettings.ShowMessageBox("Cannot edit source code while debugging");
                e.Cancel();
            }
        }



        private void formatToken(ITextEdit editSession, int offSet, IToken token)
        {
            if (token.Channel == XSharpLexer.Hidden ||
                token.Channel == XSharpLexer.PREPROCESSORCHANNEL ||
                token.Type == XSharpLexer.TEXT_STRING_CONST)
                return;
            bool syncKeyword = false;
            // Some exceptions are (pseudo) functions. These should not be formatted
            switch (token.Type)
            {
                case XSharpLexer.NAMEOF:
                case XSharpLexer.SIZEOF:
                case XSharpLexer.TYPEOF:
                    // these are keywords but should be excluded I think
                    syncKeyword = false;
                    break;
                case XSharpLexer.TRUE_CONST:
                case XSharpLexer.FALSE_CONST:
                case XSharpLexer.MACRO:
                case XSharpLexer.LOGIC_AND:
                case XSharpLexer.LOGIC_OR:
                case XSharpLexer.LOGIC_NOT:
                case XSharpLexer.LOGIC_XOR:
                case XSharpLexer.VO_AND:
                case XSharpLexer.VO_OR:
                case XSharpLexer.VO_NOT:
                case XSharpLexer.VO_XOR:
                    syncKeyword = true;
                    break;
                default:
                    if (token.Type >= XSharpLexer.FIRST_NULL && token.Type <= XSharpLexer.LAST_NULL)
                    {
                        syncKeyword = true;
                    }
                    else if (XSharpLexer.IsKeyword(token.Type))
                    {
                        syncKeyword = token.Text[0] != '#';
                    }
                    break;
            }
            if (syncKeyword)
            {
                var keyword = token.Text;
                var transform = XSettings.FormatKeyword(keyword, _settings.KeywordCase);
                if (String.Compare(transform, keyword) != 0)
                {
                    int startpos = offSet + token.StartIndex;
                    editSession.Replace(startpos, transform.Length, transform);
                }
            }
            if (token.Type == XSharpLexer.ID && XSettings.IdentifierCase)
            {
                var identifier = token.Text;
                // Remove the @@ marker
                if (identifier.StartsWith("@@"))
                    identifier = identifier.Substring(2);
                var lineNumber = currentLine;
                var currentMember = XSharpLookup.FindMember(lineNumber, _file);
                //
                if (currentMember == null)
                    return;
                IXVariableSymbol element = null;
                // Search in Parameters
                if (currentMember.Parameters != null)
                {
                    element = currentMember.Parameters.Where(x => XSharpTokenTools.StringEquals(x.Name, identifier)).FirstOrDefault();
                }
                if (element == null)
                {
                    // then Locals
                    var location = new XSharpSearchLocation(currentMember, null, lineNumber);
                    var locals = currentMember.GetLocals(location);
                    if (locals != null)
                    {
                        element = locals.Where(x => XSharpTokenTools.StringEquals(x.Name, identifier)).FirstOrDefault();
                    }
                    if (element == null)
                    {
                        if (currentMember.Parent is IXTypeSymbol type)
                        {
                            var field = XSharpLookup.SearchPropertyOrField(location, type, identifier, Modifiers.Private).FirstOrDefault();
                        }
                    }
                }
            }
        }
        private bool canIndentLine(ITextSnapshotLine line)
        {
            var ss = new SnapshotSpan(line.Snapshot, line.Extent);

            var spans = _classifier.GetClassificationSpans(ss);
            if (spans.Count > 0 && spans[0].Span.Snapshot.Version == line.Snapshot.Version)
            {
                var type = spans[0].ClassificationType;
                if (type.Classification.ToLower() == "comment")
                    return false;
                if (type.Classification.ToLower() == "xsharp.text")
                {
                    if (spans.Count == 1)
                        return false;
                    // endtext line starts with token with type "xsharp.text" when it starts with spaces
                    return spans[1].ClassificationType.Classification == "keyword";
                }
            }
            return true;

        }
        private bool canFormatLine(ITextSnapshotLine line)
        {
            // get first token on line
            // when comment: do not format
            // when xsharp.text and only one token: do not format
            // when xsharp.text and second token = keyword, then endtext line, so format
            if (line.Length == 0)
                return false;
            return canIndentLine(line);
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


        private void formatLineCase(ITextEdit editSession, ITextSnapshotLine line)
        {
            if (XSettings.DebuggerIsRunning)
            {
                return;
            }
            if (!canFormatLine(line))
            {
                return;
            }

            if (_settings.KeywordCase == KeywordCase.None)
            {
                return;
            }
            WriteOutputMessage($"CommandFilter.formatLineCase({line.LineNumber + 1})");
            // get classification of the line.
            // when the line is part of a multi line comment then do nothing
            // to detect that we take the start of the line and check if it is in
            int lineStart = line.Start.Position;
            if (line.Length == 0)
                return;
            var tokens = getTokensInLine(line);
            if ( tokens.Count > 0 )
            {
                if ( tokens[0].StartIndex < lineStart )
                {
                    // The Tokens are coming from a single-line parsing
                    // StartIndex is relative to the beginning of line
                }
                else
                {
                    // The Tokens comes from a full-source parsing
                    // StartIndex is relative to the beginning of file
                    lineStart = 0;
                }
            }
            foreach (var token in tokens)
            {
                if (currentLine == line.LineNumber)
                {
                    // do not update tokens touching or after the caret
                    // after typing String it was already uppercasing even when I wanted to type StringComparer
                    // now we wait until the user has typed an extra character. That will trigger another session.
                    // (in this case the C, but it could also be a ' ' or tab and then it would match the STRING keyword)
                    int caretPos = this.TextView.Caret.Position.BufferPosition.Position;
                    if (lineStart + token.StopIndex < caretPos - 1)
                    {
                        formatToken(editSession, lineStart, token);
                    }
                    else
                    {
                        // Come back later.
                        registerLineForCaseSync(line.LineNumber);
                        break;
                    }
                }
                else
                {
                    formatToken(editSession, lineStart, token);
                }
            }
        }

        private void registerLineForCaseSync(int line)
        {
            if (!_suspendSync && _settings.KeywordCase != KeywordCase.None)
            {
                lock (_linesToSync)
                {
                    if (!_linesToSync.Contains(line))
                        _linesToSync.Add(line);
                }
            }
        }
        private void Textbuffer_Changed(object sender, TextContentChangedEventArgs e)
        {
            var snapshot = e.After;
            var changes = e.Changes;
            if (changes != null)
            {
                foreach (var change in changes)
                {
                    int iStart = change.NewSpan.Start;
                    int iEnd = change.NewSpan.End;
                    iStart = snapshot.GetLineFromPosition(iStart).LineNumber;
                    iEnd = snapshot.GetLineFromPosition(iEnd).LineNumber;
                    for (int iline = iStart; iline <= iEnd; iline++)
                    {
                        registerLineForCaseSync(iline);
                    }
                }
            }

        }
        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableCodeCompletionLog && XSettings.EnableLogging)
            {
                XSettings.DisplayOutputMessage(strMessage);
            }
        }
        private void FormatCaseForWholeBuffer()
        {
            if (XSettings.DebuggerIsRunning)
            {
                return;
            }
            if (_settings.KeywordCase != KeywordCase.None)
            {
                WriteOutputMessage("--> CommandFilter.formatCaseForBuffer()");
                /*
                bool changed = false;
                // wait until we can work
                while (_buffer.EditInProgress)
                {
                    System.Threading.Thread.Sleep(100);
                }
                var edit = _buffer.CreateEdit(EditOptions.DefaultMinimalChange, null, null);
                try
                {
                    string text = _buffer.CurrentSnapshot.GetText();
                    var tokens = getTokens(text);
                    foreach (var token in tokens)
                    {
                        formatToken(edit, 0, token);
                    }
                }
                finally
                {
                    if (edit.HasEffectiveChanges)
                    {
                        edit.Apply();
                        changed = true;
                    }
                    else
                    {
                        edit.Cancel();
                    }
                }
                if (changed && _buffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
                */
                if (_buffer.Properties.TryGetProperty(typeof(XSharpClassifier), out XSharpClassifier classify))
                {
                    classify.Classify();
                }
                WriteOutputMessage("<-- CommandFilter.formatCaseForBuffer()");
            }
            return;
        }

        private char GetTypeChar(IntPtr pvaIn)
        {
            return (char)(ushort)Marshal.GetObjectForNativeVariant(pvaIn);
        }

        bool completionWasSelected = false;
        CompletionSelectionStatus completionWas;
        private readonly VsTextViewCreationListener m_provider;

        public int Exec(ref Guid pguidCmdGroup, uint nCmdID, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            var cmdGrp = pguidCmdGroup;
            bool done = false;
            int result = 0;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                if (Microsoft.VisualStudio.Shell.VsShellUtilities.IsInAutomationFunction(m_provider.ServiceProvider))
                {
                    done = true;
                    result = Next.Exec(ref cmdGrp, nCmdID, nCmdexecopt, pvaIn, pvaOut);
                }
            });
            if (done)
                return result;
            //
            bool handled = false;
            if (_classifier == null)
                registerClassifier();
            int hresult = VSConstants.S_OK;
            bool returnClosedCompletionList = false;

            // 1. Pre-process
            if (pguidCmdGroup == VSConstants.VSStd2K)
            {
                switch ((VSConstants.VSStd2KCmdID)nCmdID)
                {
                    case VSConstants.VSStd2KCmdID.HELPKEYWORD:
                    case VSConstants.VSStd2KCmdID.HELP:
                        break;
                    case VSConstants.VSStd2KCmdID.COMPLETEWORD:
                    case VSConstants.VSStd2KCmdID.AUTOCOMPLETE:
                    case VSConstants.VSStd2KCmdID.SHOWMEMBERLIST:
                        // in this case we WANT to include keywords in the list
                        // when they type LOCA we want to include LOCAL as well
                        CancelSignatureSession();
                        handled = StartCompletionSession(nCmdID, '\0',true);
                        break;
                    case VSConstants.VSStd2KCmdID.RETURN:
                        handled = CompleteCompletionSession();
                        if (handled)
                        {
                            returnClosedCompletionList = true;
                        }
                        break;
                    case VSConstants.VSStd2KCmdID.TAB:
                        handled = CompleteCompletionSession();
                        break;
                    case VSConstants.VSStd2KCmdID.CANCEL:
                        handled = CancelCompletionSession();
                        break;
                    case VSConstants.VSStd2KCmdID.PARAMINFO:
                        StartSignatureSession(false);
                        break;
                    case VSConstants.VSStd2KCmdID.BACKSPACE:
                        if (_signatureSession != null)
                        {
                            int pos = TextView.Caret.Position.BufferPosition;
                            if (pos > 0)
                            {
                                // get previous char
                                var previous = TextView.TextBuffer.CurrentSnapshot.GetText().Substring(pos - 1, 1);
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
                switch ((VSConstants.VSStd97CmdID)nCmdID)
                {
                    case VSConstants.VSStd97CmdID.F1Help:
                    case VSConstants.VSStd97CmdID.WindowHelp:
                        //handled = true;
                        //Todo RvdH Call X# Help
                        break;
                    case VSConstants.VSStd97CmdID.Save:
                    case VSConstants.VSStd97CmdID.SaveAs:
                    case VSConstants.VSStd97CmdID.SaveProjectItem:
                        if (_settings.InsertFinalNewline || _settings.TrimTrailingWhiteSpace)
                        {
                            adjustWhiteSpace();
                        }
                        break;
                    case VSConstants.VSStd97CmdID.GotoDefn:
                        GotoDefn();
                        return VSConstants.S_OK;
                    case VSConstants.VSStd97CmdID.Undo:
                    case VSConstants.VSStd97CmdID.Redo:
                        CancelSignatureSession();
                        CancelCompletionSession();
                        break;
                }
            }

            // Let others do their thing
            if (!handled)
            {
                if (_completionSession != null)
                {
                    if (_completionSession.SelectedCompletionSet != null)
                    {
                        completionWasSelected = _completionSession.SelectedCompletionSet.SelectionStatus.IsSelected;
                        if (completionWasSelected)
                        {
                            completionWas = _completionSession.SelectedCompletionSet.SelectionStatus;
                        }
                        else
                            completionWas = null;
                    }
                }
                ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                    hresult = Next.Exec(ref cmdGrp, nCmdID, nCmdexecopt, pvaIn, pvaOut);
                });
            }

            if (ErrorHandler.Succeeded(hresult))
            {
                // 3. Post process
                if (pguidCmdGroup == Microsoft.VisualStudio.VSConstants.VSStd2K)
                {
                    switch ((VSConstants.VSStd2KCmdID)nCmdID)
                    {
                        case VSConstants.VSStd2KCmdID.TYPECHAR:
                            char ch = GetTypeChar(pvaIn);
                            if (_completionSession != null)
                            {
                                if (char.IsLetterOrDigit(ch) || ch == '_')
                                    FilterCompletionSession(ch);
                                else
                                {
                                    if (completionWasSelected && (XSettings.EditorCommitChars.Contains(ch)))
                                    {
                                        CompleteCompletionSession(true, ch);
                                    }
                                    else
                                    {
                                        CancelCompletionSession();
                                    }
                                    if ((ch == ':') || (ch == '.'))
                                    {
                                        StartCompletionSession(nCmdID, ch);
                                    }
                                }
                                //
                            }
                            else
                            {
                                switch (ch)
                                {
                                    case ':':
                                    case '.':
                                        CancelSignatureSession();
                                        StartCompletionSession(nCmdID, ch);
                                        break;
                                    case '(':
                                    case '{':
                                        CancelSignatureSession();
                                        StartSignatureSession(false);
                                        break;
                                    case ')':
                                    case '}':
                                        CancelSignatureSession();
                                        StartSignatureSession(false);
                                        break;
                                    case ',':
                                        StartSignatureSession(true);
                                        break;
                                    default:
                                        completeCurrentToken(nCmdID, ch);
                                        break;
                                }
                            }
                            break;
                        case VSConstants.VSStd2KCmdID.HELP:
                        case VSConstants.VSStd2KCmdID.HELPKEYWORD:
                            break;
                        case VSConstants.VSStd2KCmdID.BACKSPACE:
                            FilterCompletionSession('\0');
                            break;
                        case VSConstants.VSStd2KCmdID.FORMATDOCUMENT:
                            try
                            {
                                lock (_linesToSync)
                                {
                                    _suspendSync = true;
                                    _linesToSync.Clear();
                                }
                                FormatDocumentV2();
                            }
                            finally
                            {
                                lock (_linesToSync)
                                {
                                    _linesToSync.Clear();
                                    _suspendSync = false;
                                }
                            }
                            break;

                        case VSConstants.VSStd2KCmdID.RETURN:
                            if (!returnClosedCompletionList)
                            { 
                                CancelSignatureSession();
                                FormatLine();
                            }
                            handled = true;
                            break;
                        case VSConstants.VSStd2KCmdID.COMPLETEWORD:
                            break;
                        case VSConstants.VSStd2KCmdID.LEFT:
                        case VSConstants.VSStd2KCmdID.RIGHT:
                            MoveSignature();
                            break;
                    }
                    //
                    if (handled) return VSConstants.S_OK;
                }
            }

            return hresult;
        }


        private void adjustWhiteSpace()
        {
            
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {

                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                var editSession = _buffer.CreateEdit();
                var changed = false;
                try
                {
                    var snapshot = editSession.Snapshot;
                    if (_settings.InsertFinalNewline)
                    {
                        var text = snapshot.GetText();
                        if (!text.EndsWith(Environment.NewLine))
                        {
                            var line = snapshot.GetLineFromLineNumber(snapshot.LineCount - 1);
                            editSession.Insert(line.End.Position, Environment.NewLine);
                            changed = true;
                        }

                    }
                    if (_settings.TrimTrailingWhiteSpace)
                    {
                        foreach (var line in snapshot.Lines)
                        {
                            var text = line.GetText();
                            if (text.Length > 0)
                            {
                                var last = text[text.Length - 1];
                                if (last == ' ' || last == '\t')
                                {
                                    text = text.TrimEnd();
                                    editSession.Replace(line.Start.Position, line.Length, text);
                                    changed = true;
                                }
                            }
                        }
                    }
                }
                catch (Exception)
                {
                    editSession.Cancel();
                }
                finally
                {
                    if (changed)
                        editSession.Apply();
                    else
                        editSession.Cancel();

                }
            });


        }


        #region Goto Definition
        private void GotoDefn()
        {
            try
            {
                if (XSettings.DisableGotoDefinition)
                    return;
                WriteOutputMessage("CommandFilter.GotoDefn()");
                XSharpModel.ModelWalker.Suspend();
                // First, where are we ?

                var ssp = this.TextView.Caret.Position.BufferPosition;
                // find next delimiter, so we will include the '{' or '(' in the search
                
                var snapshot = this.TextView.TextBuffer.CurrentSnapshot;
                int caretPos = ssp.Position;
                int lineNumber = ssp.GetContainingLine().LineNumber;
                XSharpModel.XFile file = this.TextView.TextBuffer.GetFile();
                if (file == null)
                    return;
                // Check if we can get the member where we are
                var member = XSharpLookup.FindMember(lineNumber, file);
                if (member == null)
                    return;
                var currentNamespace = XSharpTokenTools.FindNamespace(caretPos, file);

                // Then, the corresponding Type/Element if possible

                // We don't want to lex the buffer. So get the tokens from the last lex run
                // and when these are too old, then simply bail out
                var tokens = this.TextView.TextBuffer.GetTokens();
                if (tokens != null)
                {
                    if (tokens.SnapShot.Version != snapshot.Version)
                        return;
                }
                String currentNS = "";
                if (currentNamespace != null)
                {
                    currentNS = currentNamespace.Name;
                }
                var location = new XSharpSearchLocation(member, snapshot, lineNumber, caretPos,currentNS);
                var tokenList = XSharpTokenTools.GetTokensUnderCursor(location, tokens.TokenStream);

                // LookUp for the BaseType, reading the TokenList (From left to right)
                var result = new List<IXSymbol>();

                var state = CompletionState.General;
                result.AddRange(XSharpLookup.RetrieveElement(location, tokenList,  state));
                //
                Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
                if (result.Count > 0) 
                {
                    var element = result[0];
                    if (element is XSourceEntity source)
                    {
                        source.OpenEditor();
                    }
                    else
                    {
                        openInObjectBrowser(element.FullName);
                    }
                    return;
                }
                //
                if (tokenList.Count > 1)
                {
                    // try again with just the last element in the list
                    var token = tokenList[tokenList.Count - 1];
                    tokenList.Clear();
                    tokenList.Add(token);
                    location = location.With(currentNS);
                    result.AddRange(XSharpLookup.RetrieveElement(location, tokenList, state));
                }
                if (result.Count > 0 )
                {
                    var element = result[0];
                    if (element is XSourceEntity source)
                        source.OpenEditor();
                    else
                    {
                        openInObjectBrowser(element.FullName);
                    }
                    return;

                }

            }
            catch (Exception ex)
            {
                WriteOutputMessage("Goto failed: ");
                XSettings.DisplayException(ex);
            }
            finally
            {
                XSharpModel.ModelWalker.Resume();
            }
        }

        private void openInObjectBrowser(string name)
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                ObjectBrowserHelper.FindSymbols(name);
            });
            return;
        }

       
        #endregion

        #region Completion Session
        private void completeCurrentToken(uint nCmdID, char ch)
        {
            if (!XSettings.EditorCompletionListAfterEachChar)
                return;
            SnapshotPoint caret = TextView.Caret.Position.BufferPosition;
            if (cursorIsInStringorComment(caret))
            {
                return;
            }
            if (char.IsLetterOrDigit(ch) || ch == '_')
            {
                var line = caret.GetContainingLine();

                var lineText = line.GetText();
                var pos = caret.Position - line.Start.Position;
                int chars = 0;
                bool done = false;
                // count the number of characters in the current word. When > 2 then trigger completion
                for (int i = pos - 1; i >= 0; i--)
                {
                    switch (lineText[i])
                    {
                        case ' ':
                        case '\t':
                            done = true;
                            break;
                    }
                    if (done)
                        break;
                    chars++;
                    if (chars > 2)
                        break;
                }
                if (chars > 2)
                {
                    StartCompletionSession(nCmdID, '\0',true);
                }
            }
        }

        private void FilterCompletionSession(char ch)
        {

            WriteOutputMessage("CommandFilter.FilterCompletionSession()");
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

        void formatKeyword(Completion completion)
        {
            completion.InsertionText = XSettings.FormatKeyword(completion.InsertionText, _settings.KeywordCase);
        }

        bool CompleteCompletionSession(bool force = false, char ch = ' ')
        {
            if (_completionSession == null)
            {
                return false;
            }
            bool commit = false;
            bool moveBack = false;
            ITextCaret caret = null;
            WriteOutputMessage("CommandFilter.CompleteCompletionSession()");
            if (_completionSession.SelectedCompletionSet != null)
            {
                if ((_completionSession.SelectedCompletionSet.Completions.Count > 0) && (_completionSession.SelectedCompletionSet.SelectionStatus.IsSelected))
                {
                    if (XSettings.EditorCompletionAutoPairs)
                    {
                        caret = _completionSession.TextView.Caret;
                        Kind kind = Kind.Unknown;
                        var completion = _completionSession.SelectedCompletionSet.SelectionStatus.Completion;
                        if (completion is XSCompletion completion1)
                        {
                            kind = completion1.Kind;
                        }
                        if (kind == Kind.Keyword)
                        {
                            formatKeyword(completion);
                        }
                        WriteOutputMessage(" --> select " + completion.InsertionText);
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
                        else
                        {
                            if (kind == Kind.Constructor)
                            {
                                moveBack = true;
                                completion.InsertionText += "{}";
                            }
                            if (kind.HasParameters())
                            {
                                moveBack = true;
                                completion.InsertionText += "()";
                            }
                        }
                    }
                    commit = true;
                }
                else if (force)
                {
                    if (completionWas != null)
                    {
                        _completionSession.SelectedCompletionSet.SelectionStatus = completionWas;
                    }
                    //
                    if (_completionSession.SelectedCompletionSet.SelectionStatus.Completion != null)
                    {
                        var completion = _completionSession.SelectedCompletionSet.SelectionStatus.Completion;
                        if (completion is XSCompletion completion1 && completion1.Kind == Kind.Keyword)
                        {
                            formatKeyword(completion);
                        }
                        // Push the completion char into the InsertionText if needed
                        if (!completion.InsertionText.EndsWith(ch.ToString()))
                        {
                            completion.InsertionText += ch;
                        }
                        if (XSettings.EditorCompletionAutoPairs)
                        {
                            caret = _completionSession.TextView.Caret;
                            if (ch == '(')
                            {
                                completion.InsertionText += ')';
                                moveBack = true;
                            }
                            else if (ch == '{')
                            {
                                completion.InsertionText += '}';
                                moveBack = true;
                            }
                            else if (ch == '[')
                            {
                                completion.InsertionText += ']';
                                moveBack = true;
                            }
                        }
                    }
                    commit = true;
                }
            }
            if (commit)
            {
                WriteOutputMessage(" --> Commit");
                _completionSession.Commit();
                if (moveBack && (caret != null))
                {
                    caret.MoveToPreviousCaretPosition();
                    StartSignatureSession(false);
                }
                return true;
            }

            WriteOutputMessage(" --> Dismiss");
            _completionSession.Dismiss();
            return false;
        }

        private string getClassification(SnapshotPoint caret)
        {
            var line = caret.GetContainingLine();
            SnapshotSpan lineSpan = new SnapshotSpan(line.Start, caret.Position - line.Start);
            var tagAggregator = _aggregator.CreateTagAggregator<IClassificationTag>(this.TextView.TextBuffer);
            var tags = tagAggregator.GetTags(lineSpan);
            var tag = tags.LastOrDefault();
            return tag?.Tag?.ClassificationType?.Classification;
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


        bool StartCompletionSession(uint nCmdId, char typedChar, bool includeKeywords = false)
        {
            WriteOutputMessage("CommandFilter.StartCompletionSession()");

            if (_completionSession != null)
            {
                if (!_completionSession.IsDismissed)
                    return false;
            }
            SnapshotPoint caret = TextView.Caret.Position.BufferPosition;
            if (cursorIsAfterSLComment(caret))
                return false;

            ITextSnapshot snapshot = caret.Snapshot;

            if (!_completionBroker.IsCompletionActive(TextView))
            {
                _completionSession = _completionBroker.CreateCompletionSession(TextView, snapshot.CreateTrackingPoint(caret, PointTrackingMode.Positive), true);
            }
            else
            {
                _completionSession = _completionBroker.GetSessions(TextView)[0];
            }

            _completionSession.Dismissed += OnCompletionSessionDismiss;
            _completionSession.Committed += OnCompletionSessionCommitted;
            _completionSession.SelectedCompletionSetChanged += _completionSession_SelectedCompletionSetChanged;

            _completionSession.Properties[XsCompletionProperties.Command] = nCmdId;
            _completionSession.Properties[XsCompletionProperties.Char] = typedChar;
            _completionSession.Properties[XsCompletionProperties.Type] = null;
            _completionSession.Properties[XsCompletionProperties.IncludeKeywords] = includeKeywords;
            try
            {
                _completionSession.Start();
            }
            catch (Exception e)
            {
                WriteOutputMessage("Startcompletion failed");
                XSettings.DisplayException(e);
            }
            return true;
        }

        private void _completionSession_SelectedCompletionSetChanged(object sender, ValueChangedEventArgs<CompletionSet> e)
        {
            if (e.NewValue.SelectionStatus.IsSelected == false)
            {
                ;
            }
        }

        private void OnCompletionSessionCommitted(object sender, EventArgs e)
        {
            // it MUST be the case....
            WriteOutputMessage("CommandFilter.OnCompletionSessionCommitted()");

            if (_completionSession.SelectedCompletionSet.SelectionStatus.Completion != null)
            {
                if (_completionSession.SelectedCompletionSet.SelectionStatus.Completion.InsertionText.EndsWith("("))
                {
                    _completionSession.Properties.TryGetProperty(XsCompletionProperties.Type, out IXTypeSymbol type);
                    string method = _completionSession.SelectedCompletionSet.SelectionStatus.Completion.InsertionText;
                    method = method.Substring(0, method.Length - 1);
                    _completionSession.Dismiss();

                    StartSignatureSession(false, type, method);
                }
            }
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
            _completionSession.Committed -= OnCompletionSessionCommitted;
            _completionSession.SelectedCompletionSetChanged -= _completionSession_SelectedCompletionSetChanged;
            _completionSession = null;
        }
        #endregion


        #region Signature Session

        bool StartSignatureSession(bool comma, IXTypeSymbol type = null, string methodName = null)
        {
            WriteOutputMessage("CommandFilter.StartSignatureSession()");

            if (_signatureSession != null)
                return false;
            int startLineNumber = this.TextView.Caret.Position.BufferPosition.GetContainingLine().LineNumber;
            SnapshotPoint ssp = this.TextView.Caret.Position.BufferPosition;
            // when coming from the completion list then there is no need to check a lot of stuff
            // we can then simply lookup the method and that is it.
            // Also no need to filter on visibility since that has been done in the completionlist already !
            // First, where are we ?
            int caretPos;
            int Level = 0;
            do
            {
                ssp= ssp-1;
                char leftCh = ssp.GetChar();
                bool done = false;
                switch (leftCh)
                {
                    case ')':
                    case '}':
                        Level += 1;
                        break;
                    case '(':
                    case '{':
                        Level -= 1;
                        done = Level < 0;
                        break;
                }
                if (done)
                    break;
            } while (ssp.Position > 0);
            //
            caretPos = ssp.Position;
            // When we have a multi line source line this is the line where the open paren or open curly is
            int lineNumber = ssp.GetContainingLine().LineNumber; 
            var snapshot = this.TextView.TextBuffer.CurrentSnapshot;
            var file = this.TextView.TextBuffer.GetFile();
            if (file == null)
                return false;
            var member = XSharpLookup.FindMember(lineNumber, file);
            var currentNamespace = XSharpTokenTools.FindNamespace(caretPos, file);
            string currentNS = "";
            if (currentNamespace != null)
            {
                currentNS = currentNamespace.Name;
            }
            XSharpSearchLocation location = new XSharpSearchLocation(member, snapshot, lineNumber, caretPos, currentNS );
            IXMemberSymbol currentElement = null;
            if (type != null && methodName != null)
            {
                currentElement = XSharpLookup.SearchMethod(location, type, methodName, XSharpModel.Modifiers.Private, false).FirstOrDefault();
            }
            else
            {

                // Then, the corresponding Type/Element if possible
                // Check if we can get the member where we are

                var tokenList = XSharpTokenTools.GetTokenList(location, out var state);
                // We don't care of the corresponding Type, we are looking for the currentElement
                var element = XSharpLookup.RetrieveElement(location, tokenList, state, true).FirstOrDefault();
                if (element is IXMemberSymbol mem)
                    currentElement = mem;
                
            }
            //
            if ((currentElement != null))
            {

                SnapshotPoint caret = TextView.Caret.Position.BufferPosition;
                ITextSnapshot caretsnapshot = caret.Snapshot;
                //
                if (!_signatureBroker.IsSignatureHelpActive(TextView))
                {
                    _signatureSession = _signatureBroker.CreateSignatureHelpSession(TextView, caretsnapshot.CreateTrackingPoint(caret, PointTrackingMode.Positive), true);
                }
                else
                {
                    _signatureSession = _signatureBroker.GetSessions(TextView)[0];
                }

                _signatureSession.Dismissed += OnSignatureSessionDismiss;
                if (currentElement != null)
                {
                    _signatureSession.Properties[SignatureProperties.Element] = currentElement;
                }
               
                _signatureSession.Properties[SignatureProperties.Line] = startLineNumber;
                _signatureSession.Properties[SignatureProperties.Start] = ssp.Position;
                _signatureSession.Properties[SignatureProperties.Length] = TextView.Caret.Position.BufferPosition.Position - ssp.Position;
                _signatureSession.Properties[SignatureProperties.File] = file;

                try
                {
                    _signatureSession.Start();
                }
                catch (Exception e)
                {
                    WriteOutputMessage("Start Signature session failed:");
                    XSettings.DisplayException(e);
                }
            }
            //
            return true;
        }

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

            _signatureSession.Properties.TryGetProperty(SignatureProperties.Start, out int start);
            int pos = this.TextView.Caret.Position.BufferPosition.Position;

            ((XSharpVsSignature)_signatureSession.SelectedSignature).ComputeCurrentParameter(pos - start - 1);


            return true;
        }


        #endregion

        public int QueryStatus(ref Guid pguidCmdGroup, uint cCmds, OLECMD[] prgCmds, IntPtr pCmdText)
        {
            if (pguidCmdGroup == VSConstants.VSStd2K)
            {
                switch ((VSConstants.VSStd2KCmdID)prgCmds[0].cmdID)
                {
                    case VSConstants.VSStd2KCmdID.AUTOCOMPLETE:
                    case VSConstants.VSStd2KCmdID.COMPLETEWORD:
                        prgCmds[0].cmdf = (uint)OLECMDF.OLECMDF_ENABLED | (uint)OLECMDF.OLECMDF_SUPPORTED;
                        return VSConstants.S_OK;
                }
            }
            else if (pguidCmdGroup == VSConstants.GUID_VSStandardCommandSet97)
            {
                switch ((VSConstants.VSStd97CmdID)prgCmds[0].cmdID)
                {
                    case VSConstants.VSStd97CmdID.GotoDefn:
                        prgCmds[0].cmdf = (uint)OLECMDF.OLECMDF_ENABLED | (uint)OLECMDF.OLECMDF_SUPPORTED;
                        return VSConstants.S_OK;
                }
            }
            int result = 0;
            var cmdGroup = pguidCmdGroup;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                result = Next.QueryStatus(cmdGroup, cCmds, prgCmds, pCmdText);
            });
            return result;
        }

    }


    internal static class ObjectBrowserHelper
    {
        private static Guid GUID_VsSymbolScope_All = new Guid(0xa5a527ea, 0xcf0a, 0x4abf, 0xb5, 0x1, 0xea, 0xfe, 0x6b, 0x3b, 0xa5, 0xc6);
        private static Guid GUID_VsSymbolScope_Solution = new Guid(0xb1ba9461, 0xfc54, 0x45b3, 0xa4, 0x84, 0xcb, 0x6d, 0xd0, 0xb9, 0x5c, 0x94);
        private static Guid GUID_VsSymbolScope_Frameworks = new Guid(0x3168518c, 0xb7c9, 0x4e0c, 0xbd, 0x51, 0xe3, 0x32, 0x1c, 0xa7, 0xb4, 0xd8);

        /*
        DEFINE_GUID(GUID_VsSymbolScope_All, 0xa5a527ea, 0xcf0a, 0x4abf, 0xb5, 0x1, 0xea, 0xfe, 0x6b, 0x3b, 0xa5, 0xc6);
        DEFINE_GUID(GUID_VsSymbolScope_OBSelectedComponents, 0x41fd0b24, 0x8d2b, 0x48c1, 0xb1, 0xda, 0xaa, 0xcf, 0x13, 0xa5, 0x57, 0xf);
        DEFINE_GUID(GUID_VsSymbolScope_FSSelectedComponents, 0xc2146638, 0xc2fe, 0x4c1e, 0xa4, 0x9d, 0x64, 0xae, 0x97, 0x1e, 0xef, 0x39);
        DEFINE_GUID(GUID_VsSymbolScope_Frameworks, 0x3168518c, 0xb7c9, 0x4e0c, 0xbd, 0x51, 0xe3, 0x32, 0x1c, 0xa7, 0xb4, 0xd8);
        DEFINE_GUID(GUID_VsSymbolScope_Solution, 0xb1ba9461, 0xfc54, 0x45b3, 0xa4, 0x84, 0xcb, 0x6d, 0xd0, 0xb9, 0x5c, 0x94);
        */

        /// <summary>
        ///     If Visual Studio's recognizes the given member and knows where its source code is, goes to the source code.
        ///     Otherwise, opens the "Find Symbols" ToolWindow.
        /// </summary>
        public static void GotoMemberDefinition(string memberName, uint searchOptions = (uint)_VSOBSEARCHOPTIONS.VSOBSO_LOOKINREFS)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            gotoDefinition(memberName, _LIB_LISTTYPE.LLT_MEMBERS, searchOptions);
        }

        public static void GotoClassDefinition(string typeName, uint searchOptions = (uint)_VSOBSEARCHOPTIONS.VSOBSO_LOOKINREFS)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            gotoDefinition(typeName, _LIB_LISTTYPE.LLT_CLASSES, searchOptions);
        }

        public static void FindSymbols(string memberName)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            canFindSymbols(memberName, (uint)_VSOBSEARCHOPTIONS.VSOBSO_LOOKINREFS);
        }


        private static void gotoDefinition(string memberName, _LIB_LISTTYPE libListtype, uint searchOptions)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (gotoDefinitionInternal(memberName, libListtype, searchOptions) == false)
            {
                // There was an ambiguity (more than one item found) or no items found at all.
                if (ObjectBrowserHelper.canFindAllSymbols(memberName, searchOptions) == false)
                {
                    Debug.WriteLine("Failed to FindSymbol for symbol " + memberName);
                }
            }
        }

        private static bool gotoDefinitionInternal(string typeOrMemberName, _LIB_LISTTYPE symbolType, uint searchOptions)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (tryFindSymbol(typeOrMemberName, out IVsSimpleObjectList2 list, symbolType, searchOptions))
            {
                const VSOBJGOTOSRCTYPE whereToGo = VSOBJGOTOSRCTYPE.GS_DEFINITION;
                //
                return HResult.Succeeded(list.CanGoToSource(0, whereToGo, out int ok)) &&
                       HResult.Succeeded(ok) &&
                       HResult.Succeeded(list.GoToSource(0, whereToGo));
            }

            return false;
        }

        // Searching in the XSharp Library (Current Solution)

        // Searching in the XSharp Library (Current Solution)
        private static IVsSimpleLibrary2 GetXSharpLibrary()
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            Guid guid = new Guid(XSharpConstants.Library);
            IVsSimpleLibrary2 simpleLibrary = null;
            //
            System.IServiceProvider provider = XSharpLanguageService.Instance;
            // ProjectPackage already switches to UI thread inside GetService
            if (provider.GetService(typeof(SVsObjectManager)) is IVsObjectManager2 mgr)
            {
                ErrorHandler.ThrowOnFailure(mgr.FindLibrary(ref guid, out IVsLibrary2 _library));
                simpleLibrary = _library as IVsSimpleLibrary2;
            }
            return simpleLibrary;
        }

        //private static bool tryGetSourceLocation(string memberName, out string fileName, out uint line, uint searchOptions)
        //{
        //    ThreadHelper.ThrowIfNotOnUIThread();
        //    if (tryFindSymbol(memberName, out IVsSimpleObjectList2 list, _LIB_LISTTYPE.LLT_MEMBERS, searchOptions))
        //    {
        //        return HResult.Succeeded(list.GetSourceContextWithOwnership(0, out fileName, out line));
        //    }

        //    fileName = null;
        //    line = 0;
        //    return false;
        //}

        /// <summary>
        ///     Tries to find a member (field/property/event/methods/etc).
        /// </summary>
        /// <param name="typeOrMemberName">The type or member we are searching for</param>
        /// <param name="resultList">An IVsSimpleObjectList2 which contains a single result.</param>
        /// <param name="symbolType">The type of symbol we are looking for (member/class/etc)</param>
        /// <returns>
        ///     True if a unique match was found. False if the member does not exist or there was an ambiguity
        ///     (more than one member matched the search term).
        /// </returns>
        private static bool tryFindSymbol(string typeOrMemberName,
            out IVsSimpleObjectList2 resultList,
            _LIB_LISTTYPE symbolType,
            uint searchOptions)
        {
            try
            {
                // The Visual Studio API we're using here breaks with superfulous spaces
                typeOrMemberName = typeOrMemberName.Replace(" ", "");
                var library = ObjectBrowserHelper.GetXSharpLibrary();
                ThreadHelper.ThrowIfNotOnUIThread();
                var searchSucceed = HResult.Succeeded(library.GetList2((uint)symbolType,
                    (uint)_LIB_LISTFLAGS.LLF_USESEARCHFILTER,
                    createSearchCriteria(typeOrMemberName, searchOptions),
                    out IVsSimpleObjectList2 list));
                if (searchSucceed && list != null)
                {
                    // Check if there is an ambiguity (where there is more than one symbol that matches)
                    if (getSymbolNames(list).Distinct().Count() == 1)
                    {
                        list.GetItemCount(out uint count);
                        if (count > 1)
                        {
                            list.CanDelete((uint)1, out int ok);
                        }
                        resultList = list;
                        return true;
                    }
                }
            }
            catch (AccessViolationException e)
            {
                /* eat this type of exception (ripped from original implementation) */
                Debug.WriteLine(e.Message);
            }

            resultList = null;
            return false;
        }

        private static bool canFindSymbols(string memberName, uint searchOptions)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            System.IServiceProvider provider = XSharpLanguageService.Instance;
            bool result ;
            // ProjectPackage already switches to UI thread inside GetService
            IVsFindSymbol searcher = provider.GetService(typeof(SVsObjectSearch)) as IVsFindSymbol;
            Assumes.Present(searcher);
            var guidSymbolScope = new Guid(XSharpConstants.Library);
            result = HResult.Succeeded(searcher.DoSearch(ref guidSymbolScope, createSearchCriteria(memberName, searchOptions)));
            return result;
        }

        private static bool canFindAllSymbols(string memberName, uint searchOptions)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            System.IServiceProvider provider = XSharpLanguageService.Instance;
            bool result ;
            // ProjectPackage already switches to UI thread inside GetService
            IVsFindSymbol searcher = provider.GetService(typeof(SVsObjectSearch)) as IVsFindSymbol;
            Assumes.Present(searcher);
            var guidSymbolScope = GUID_VsSymbolScope_All;
            //
            result = HResult.Succeeded(searcher.DoSearch(ref guidSymbolScope, createSearchCriteria(memberName, searchOptions)));
            return result;
        }

        private static VSOBSEARCHCRITERIA2[] createSearchCriteria(string typeOrMemberName, uint searchOptions)
        {
            return new[]
            {
                new VSOBSEARCHCRITERIA2
                {
                    eSrchType = VSOBSEARCHTYPE.SO_ENTIREWORD,
                    //eSrchType = VSOBSEARCHTYPE.SO_PRESTRING,
                    //grfOptions = (uint)_VSOBSEARCHOPTIONS.VSOBSO_LOOKINREFS,
                    grfOptions = searchOptions,
                    szName = typeOrMemberName
                }
            };
        }

        private static IEnumerable<string> getSymbolNames(IVsSimpleObjectList2 list)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            if (HResult.Succeeded(list.GetItemCount(out uint count)))
            {
                for (uint i = 0; i < count; i++)
                {

                    if (HResult.Succeeded(list.GetProperty(i,
                        (int)_VSOBJLISTELEMPROPID.VSOBJLISTELEMPROPID_FULLNAME,
                        out object symbol)))
                    {
                        yield return (string)symbol;
                    }
                }
            }
        }
    }

    internal static class HResult
    {
        internal static bool Succeeded(int v)
        {
            return (v == VSConstants.S_OK);
        }
    }

}
