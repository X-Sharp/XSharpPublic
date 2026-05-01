// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;

using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Windows.Controls;

using XSharpModel;

namespace XSharp.LanguageService
{
    /// <summary>
    /// Tool-window pane that hosts the <see cref="DocumentOutlineControl"/>.
    /// It tracks the active text view so the outline always reflects the
    /// document currently open in the editor.
    /// </summary>
    [Guid(XSharpConstants.DocumentOutlinePaneGuidString)]
    public sealed class DocumentOutlineToolWindow : ToolWindowPane, IVsSelectionEvents
    {
        private DocumentOutlineControl _control;
        private IWpfTextView _activeTextView;

        public DocumentOutlineToolWindow() : base(null)
        {
            Caption = "X# Document Outline";
        }

        // -----------------------------------------------------------------------
        // ToolWindowPane overrides
        // -----------------------------------------------------------------------

        protected override void Initialize()
        {
            base.Initialize();

            _control = new DocumentOutlineControl();
            Content = _control;

            // Subscribe to selection-change events to track the active document.
            ThreadHelper.ThrowIfNotOnUIThread();
            // Try to populate the outline for the currently active document.
            UpdateOutlineForActiveDocument();
            VS.Events.WindowEvents.ActiveFrameChanged += OnActiveFrameChanged;

        }
        IVsWindowFrame newFrame = null;
        private void OnActiveFrameChanged(ActiveFrameChangeEventArgs args)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            newFrame = args.NewFrame;
            UpdateOutlineForActiveDocument();
            newFrame = null;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (_activeTextView != null)
                {
                    _activeTextView.Caret.PositionChanged -= OnCaretPositionChanged;
                    _activeTextView = null;
                }

                _control?.Cleanup();
                _control = null;
            }
            base.Dispose(disposing);
        }

        // -----------------------------------------------------------------------
        // Active document tracking
        // -----------------------------------------------------------------------
        static FieldInfo field = null;
        private void UpdateOutlineForActiveDocument()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            // Detach from the previous text view
            if (_activeTextView != null)
            {
                _activeTextView.Caret.PositionChanged -= OnCaretPositionChanged;
                _activeTextView = null;
            }
            if (field == null)
            {
                field = typeof(WindowFrame).GetField("_frame", BindingFlags.Instance | BindingFlags.NonPublic);
            }

            IWpfTextView textView = null;
            string docName = null;
            try
            {
                if (newFrame != null && field != null)
                {

                    dynamic frame = newFrame;
                    var editor = (Guid) frame.Editor;
                    if (editor != new Guid(XSharpConstants.EditorFactoryGuidString))
                    {
                        return;
                    }
                    dynamic _frame = field.GetValue(newFrame);
                    docName = _frame.EffectiveDocumentMoniker;
                    if (!string.IsNullOrEmpty(docName))
                    {
                        ThreadHelper.JoinableTaskFactory.Run(async () =>
                        {
                            var doc = await VS.Documents.GetDocumentViewAsync(docName);
                            textView = doc?.TextView;
                        });
                    }
                }
                else
                {
                    ThreadHelper.JoinableTaskFactory.Run(async () =>
                    {
                        var doc = await VS.Documents.GetActiveDocumentViewAsync();
                        if (doc != null)
                        {
                            docName = doc.TextView.TextBuffer.GetFile().FullPath;
                            textView = doc.TextView;
                        }
                    });
                }
            }
            catch
            {
                ; // ignore exception
            }
            // Find the current active IWpfTextView
            if (textView == null)
            {
                _control?.SetFile(null);
                return;
            }

            // Only process X# documents
            if (!textView.TextBuffer.ContentType.IsOfType(XSharpConstants.LanguageName))
            {
                _control?.SetFile(null);
                return;
            }

            _activeTextView = textView;
            _activeTextView.Caret.PositionChanged += OnCaretPositionChanged;

            var xFile = textView.TextBuffer.GetFile();
            _control?.SetFile(xFile);

            // Highlight the node for the current caret position
            int line = textView.Caret.Position.BufferPosition.GetContainingLine().LineNumber;
            _control?.SelectNodeAtLine(line);
        }


        private void OnCaretPositionChanged(object sender, CaretPositionChangedEventArgs e)
        {
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                int line = e.NewPosition.BufferPosition.GetContainingLine().LineNumber;
                _control?.SelectNodeAtLine(line);
            });
        }

        // -----------------------------------------------------------------------
        // IVsSelectionEvents – fired when the active document changes
        // -----------------------------------------------------------------------

        int IVsSelectionEvents.OnElementValueChanged(uint elementid, object varValueOld, object varValueNew)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            // SEID_DocumentFrame = 2 means the active document frame changed.
            if (elementid == (uint)VSConstants.VSSELELEMID.SEID_DocumentFrame)
                UpdateOutlineForActiveDocument();
            return VSConstants.S_OK;
        }

        int IVsSelectionEvents.OnSelectionChanged(IVsHierarchy pHierOld, uint itemidOld,
            IVsMultiItemSelect pMISOld, ISelectionContainer pSCOld,
            IVsHierarchy pHierNew, uint itemidNew,
            IVsMultiItemSelect pMISNew, ISelectionContainer pSCNew)
            => VSConstants.S_OK;

        int IVsSelectionEvents.OnCmdUIContextChanged(uint dwCmdUICookie, int fActive)
            => VSConstants.S_OK;
    }
}
