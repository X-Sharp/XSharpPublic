//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//------------------------------------------------------------------------------

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Threading;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Concurrent;
using System.ComponentModel.Composition;
using System.Linq;
using XSharpModel;

#pragma warning disable CS0649 // Field is never assigned to, for the imported fields
namespace XSharp.LanguageService
{
    [Export(typeof(IVsTextViewCreationListener))]
    [Name("XSharp Formatting Provider")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [ContentType(XSharpConstants.LanguageName)]
    internal class XSharpFormattingProvider : IVsTextViewCreationListener
    {
        [Import] IEditorOptionsFactoryService editorOptionsService;

        private IEditorOptions editorOptions;

        [Import]
        internal IVsEditorAdaptersFactoryService AdapterService;

        [Import]
        internal IBufferTagAggregatorFactoryService BufferTagAggregatorFactoryService { get; set; }
        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            ITextView textView = AdapterService.GetWpfTextView(textViewAdapter);
            if (textView == null)
                return;
            editorOptions = editorOptionsService.GetOptions(textView);
            editorOptions.OptionChanged += EditorOptions_OptionChanged;
            textView.Properties.GetOrCreateSingletonProperty(
                 () => new XSharpFormattingCommandHandler(textViewAdapter,
                    textView,
                    BufferTagAggregatorFactoryService
                    ));
        }

        private void EditorOptions_OptionChanged(object sender, EditorOptionChangedEventArgs e)
        {
            return;
        }
    }
    internal partial class XSharpFormattingCommandHandler : IOleCommandTarget
    {
        readonly ITextView _textView;
        readonly IOleCommandTarget m_nextCommandHandler;
        readonly IBufferTagAggregatorFactoryService _aggregator;
        readonly ConcurrentDictionary<int,int> _linesToSync;
        readonly XFile _file;
        private readonly ITextBuffer _buffer;
        private readonly XDocument _document;
        private readonly LineFormatter _lineFormatter;
        XSharpClassifier _classifier;
        SourceCodeEditorSettings Settings => _buffer.GetSettings();

        bool _suspendSync = false;
        int currentLine = -1;

        private void registerClassifier()
        {
            if (_classifier == null)
            {
                _classifier = _buffer.GetClassifier();
                if (_classifier != null)
                {
                    _classifier.ClassificationChanged += Classifier_ClassificationChanged;
                }

            }
        }

        private void OnClosed(object sender, EventArgs e)
        {
            _textView.Closed -= OnClosed;
            if (_buffer != null)
            {
                _buffer.ChangedLowPriority -= Textbuffer_Changed;
                _buffer.Changing -= Textbuffer_Changing;
            }
            if (_classifier != null)
            {
                _classifier.ClassificationChanged -= Classifier_ClassificationChanged;
            }

        }
        internal XSharpFormattingCommandHandler(IVsTextView textViewAdapter, ITextView textView,
            IBufferTagAggregatorFactoryService aggregator)
        {
            this._textView = textView;
            this._textView.Closed += OnClosed;
            this._aggregator = aggregator;
            //add this to the filter chain
            _linesToSync = new ConcurrentDictionary<int, int>();
            //
            _buffer = _textView.TextBuffer;
            if (_buffer != null)
            {
                _buffer.ChangedLowPriority += Textbuffer_Changed;
                _buffer.Changing += Textbuffer_Changing;
                _file = _buffer.GetFile();

                _document = _buffer.GetDocument();
            }
            if (_file != null)
            {
                EditorConfigReader.ReadSettings(_buffer, _file.FullPath);
            }

            textViewAdapter.AddCommandFilter(this, out m_nextCommandHandler);
            registerClassifier();
            _lineFormatter = new LineFormatter(_buffer);

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
            Guid cmdGroup = pguidCmdGroup;
            bool completionActive = false;
            registerClassifier();
            // 1. Pre-process
            if (pguidCmdGroup == VSConstants.GUID_VSStandardCommandSet97)
            {
                switch (nCmdID)
                {
                    case (int)VSConstants.VSStd97CmdID.Save:
                    case (int)VSConstants.VSStd97CmdID.SaveAs:
                    case (int)VSConstants.VSStd97CmdID.SaveProjectItem:
                        if (Settings.InsertFinalNewline || Settings.TrimTrailingWhiteSpace)
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
            int result = m_nextCommandHandler.Exec(ref cmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut);
            // 3. Post process
            if (ErrorHandler.Succeeded(result) && !XEditorSettings.DisableCodeCompletion)
            {
                if (pguidCmdGroup == VSConstants.VSStd2K)
                {

                    switch (nCmdID)
                    {
                        case (int)VSConstants.VSStd2KCmdID.FORMATDOCUMENT:
                            try
                            {
                                _linesToSync.Clear();
                                _suspendSync = true;
                                FormatDocument();
                            }
                            finally
                            {
                                _linesToSync.Clear();
                                _suspendSync = false;
                            }
                            break;

                        case (int)VSConstants.VSStd2KCmdID.FORMATSELECTION:
                            try
                            {
                                _suspendSync = true;
                                FormatSelection();
                            }
                            finally
                            {
                                _suspendSync = false;
                            }
                            break;

                        case (int)VSConstants.VSStd2KCmdID.RETURN:
                            if (!completionActive)
                            {
                                try
                                {
                                    _suspendSync = true;
                                    FormatLine();
                                }
                                finally
                                {
                                    _suspendSync = false;
                                }
                            }
                            break;

                        default:
                            break;

                    }
                }
            }
            var line = getCurrentLine();
            if (line != currentLine && ! _linesToSync.IsEmpty)
            {
                currentLine = line;
                ApplyPendingChanges();
            }
            return result;
        }

        private void Textbuffer_Changing(object sender, TextContentChangingEventArgs e)
        {
            if (XDebuggerSettings.DebuggerIsRunning && ! XDebuggerSettings.AllowEditing)  
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
                XSettings.LogMessage("XSharp.Formatting:" + strMessage);
            }
        }
        private void adjustWhiteSpace()
        {

            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {

                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                var editSession = _buffer.CreateEdit();
                var settings = Settings;
                try
                {
                    var snapshot = editSession.Snapshot;
                    if (settings.InsertFinalNewline)
                    {
                        var text = snapshot.GetText();
                        if (!text.EndsWith(Environment.NewLine))
                        {
                            var line = snapshot.GetLineFromLineNumber(snapshot.LineCount - 1);
                            editSession.Insert(line.End.Position, Environment.NewLine);
                        }

                    }
                    if (settings.TrimTrailingWhiteSpace)
                    {
                        foreach (var line in snapshot.Lines)
                        {
                            var text = line.GetText();
                            if (text.Length > 0)
                            {
                                var last = text[text.Length - 1];
                                if (last == ' ' || last == '\t')
                                {
                                    editSession.Replace(line.Start.Position, line.Length, text.TrimEnd());
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
                    ApplyChanges(editSession);
                }
            });
        }

        private void ApplyChanges(ITextEdit editSession)
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
        private void Classifier_ClassificationChanged(object sender, ClassificationChangedEventArgs e)
        {
            WriteOutputMessage("ClassificationChanged()");
            if (_suspendSync)
                return;
            ApplyPendingChanges();
        }
        private void registerLineForCaseSync(int line)
        {
            if (!_suspendSync && ChangeCase)
            {
                if (!_linesToSync.ContainsKey(line))
                {
                    _linesToSync.TryAdd(line, line);
                }
            }
        }
        internal int getCurrentLine()
        {
            SnapshotPoint caret = this._textView.Caret.Position.BufferPosition;
            ITextSnapshotLine line = caret.GetContainingLine();
            return line.LineNumber;
        }

        private bool ChangeCase
        {
            get
            {
                if (Settings == null)
                    return false;
                if (Settings.IdentifierCase)
                    return true;
                return Settings.KeywordCase != KeywordCase.None;
            }
        }


        private void Textbuffer_Changed(object sender, TextContentChangedEventArgs e)
        {
            var snapshot = e.After;
            var changes = e.Changes;
            var tag = e.EditTag;
            if (tag is IUndoEditTag)
            {
                ; // do nothing
            }
            else if (changes != null)
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
        private void ProcessLines(int[] lines)
        {
            if (!WaitUntilBufferReady())
            {
                return;
            }

            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                var editSession = _buffer.CreateEdit();
                var snapshot = editSession.Snapshot;
                var curLine = getCurrentLine();
                try
                {
                    var end = DateTime.Now + new TimeSpan(0, 0, 2);
                    int counter = 0;
                    foreach (int nLine in lines)
                    {
                        if (nLine < snapshot.LineCount && nLine >= 0 && nLine != curLine)
                        {
                            ITextSnapshotLine line = snapshot.GetLineFromLineNumber(nLine);
                            _lineFormatter.FormatLineCase(editSession, line);
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
                    ApplyChanges(editSession);
                }
            });
        }

        private void ApplyPendingChanges()
        {
            if (!ChangeCase || !CanEdit)
            {
                return;
            }

            if (_linesToSync.Count > 0)
            {
                int current = this.getCurrentLine();
                var hasCurrent = _linesToSync.ContainsKey(current);
                var lines = _linesToSync.Keys.ToArray();
                Array.Sort(lines);
                ProcessLines(lines);
                _linesToSync.Clear();
                if (hasCurrent)
                {
                    _linesToSync.TryAdd(current, current);
                }
            }
        }
    }
}
