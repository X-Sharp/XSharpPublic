using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Text;
using XSharpModel;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Text.Classification;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

#pragma warning disable CS0649 // Field is never assigned to, for the imported fields
namespace XSharp.LanguageService
{
    [Export(typeof(IVsTextViewCreationListener))]
    [Name("XSharp Completion Provider")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [ContentType(XSharpConstants.LanguageName)]
    internal class XSharpFormattingProvider : IVsTextViewCreationListener
    {

        [Import]
        internal IVsEditorAdaptersFactoryService AdapterService;

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
                 () => new XSharpFormattingCommandHandler(textViewAdapter,
                    textView,
                    BufferTagAggregatorFactoryService
                    ));
        }
    }
    internal partial class XSharpFormattingCommandHandler : IOleCommandTarget
    {
        readonly ITextView _textView;
        readonly IOleCommandTarget m_nextCommandHandler;
        readonly IBufferTagAggregatorFactoryService _aggregator;
        readonly List<int> _linesToSync;
        readonly XFile _file;
        private static int _lastIndentValue;    // in number of characters
        private readonly ITextBuffer _buffer;
        XSharpClassifier _classifier;

        bool _suspendSync = false;
        int currentLine = -1;

        private void registerClassifier()
        {
            if (_classifier == null)
            {
                if (_buffer.Properties.TryGetProperty(typeof(XSharpClassifier), out _classifier))
                {
                    _classifier.ClassificationChanged += Classifier_ClassificationChanged;
                }
            }

        }
        internal XSharpFormattingCommandHandler(IVsTextView textViewAdapter, ITextView textView,
            IBufferTagAggregatorFactoryService aggregator)
        {
            this._textView = textView;
            this._aggregator = aggregator;
            //add this to the filter chain
            _linesToSync = new List<int>();
            //
            _buffer = _textView.TextBuffer;
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

            textViewAdapter.AddCommandFilter(this, out m_nextCommandHandler);
            registerClassifier();

        }
#if !ASYNCCOMPLETION
        XSharpCompletionCommandHandler _completionCommandHandler = null;
#endif
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

        public int Exec(ref Guid pguidCmdGroup, uint nCmdID, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            int result = VSConstants.S_OK;
            Guid cmdGroup = pguidCmdGroup;
            bool completionActive = false; 
            registerClassifier();
            // 1. Pre-process
            if (pguidCmdGroup == VSConstants.GUID_VSStandardCommandSet97)
            {
                switch (nCmdID)
                {
                    case (int) VSConstants.VSStd97CmdID.Save:
                    case (int)VSConstants.VSStd97CmdID.SaveAs:
                    case (int)VSConstants.VSStd97CmdID.SaveProjectItem:
                        if (_settings.InsertFinalNewline || _settings.TrimTrailingWhiteSpace)
                        {
                            adjustWhiteSpace();
                        }
                        break;
                    default:
                        break;
                }
            }
            else if (pguidCmdGroup == VSConstants.VSStd2K)
            {
                switch (nCmdID)
                {
                    case (int)VSConstants.VSStd2KCmdID.RETURN:
                        completionActive = IsCompletionActive();
                        break;
                    default:
                        break;
                }
            }
            // 2. Let others do their thing
            // Let others do their thing
            result = m_nextCommandHandler.Exec(ref cmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut);
            // 3. Post process
            if (ErrorHandler.Succeeded(result) && !XSettings.DisableCodeCompletion)
            {
                if (pguidCmdGroup == VSConstants.VSStd2K)
                {

                    switch (nCmdID)
                    {
                        case (int) VSConstants.VSStd2KCmdID.FORMATDOCUMENT:
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

                        case (int)VSConstants.VSStd2KCmdID.RETURN:
                            if (!completionActive)
                                FormatLine();
                            break;

                        default:
                            break;

                    }
                }
            }
            var line = getCurrentLine();
            if (line != currentLine)
            {
                currentLine = line;
                ApplyPendingChanges();
            }
            return result;
        }

        private void Textbuffer_Changing(object sender, TextContentChangingEventArgs e)
        {
            if (XSettings.DebuggerIsRunning)
            {
                XSettings.ShowMessageBox("Cannot edit source code while debugging");
                e.Cancel();
            }
        }
        public int QueryStatus(ref Guid pguidCmdGroup, uint cCmds, OLECMD[] prgCmds, IntPtr pCmdText)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            return m_nextCommandHandler.QueryStatus(ref pguidCmdGroup, cCmds, prgCmds, pCmdText);
        }
        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableParameterLog && XSettings.EnableLogging)
            {
                XSettings.DisplayOutputMessage("XSharp.Formatting:" + strMessage);
            }
        }
        private int getCurrentLine()
        {
            SnapshotPoint caret = this._textView.Caret.Position.BufferPosition;
            ITextSnapshotLine line = caret.GetContainingLine();
            return line.LineNumber;
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

        private void Classifier_ClassificationChanged(object sender, ClassificationChangedEventArgs e)
        {
            WriteOutputMessage("ClassificationChanged()");
            if (_suspendSync)
                return;
            ApplyPendingChanges();
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
        private void ApplyPendingChanges()
        {
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
                    _linesToSync.Add(this.getCurrentLine());
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
                            if (nLine < snapshot.LineCount && nLine >= 0)
                            {
                                ITextSnapshotLine line = snapshot.GetLineFromLineNumber(nLine);
                                formatLineCase(editSession, line);
                            }
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
                case XSharpLexer.UDC_KEYWORD:
                    syncKeyword = XSettings.UDCKeywordCase;
                    break;
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
                var identifier = token.CleanText();
                var lineNumber = getCurrentLine();
                var currentMember = _textView.FindMember();
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
                    var location = new XSharpSearchLocation(currentMember.File, currentMember, null, lineNumber);
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
            if (line.LineNumber == getCurrentLine())
            {
                // Come back later.
                registerLineForCaseSync(line.LineNumber);
                return;
            }

            WriteOutputMessage($"formatLineCase({line.LineNumber + 1})");
            // get classification of the line.
            // when the line is part of a multi line comment then do nothing
            // to detect that we take the start of the line and check if it is in
            int lineStart = line.Start.Position;
            if (line.Length == 0)
                return;
            var tokens = getTokensInLine(line);
            if (tokens.Count > 0)
            {
                if (tokens[0].StartIndex < lineStart)
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
                formatToken(editSession, lineStart, token);
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

  
 
   


    }
}
