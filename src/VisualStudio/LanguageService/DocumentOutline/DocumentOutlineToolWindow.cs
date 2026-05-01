// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.Runtime.InteropServices;
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
        private IVsMonitorSelection _monitorSelection;
        private uint _selectionEventsCookie;
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
            _monitorSelection = GetService(typeof(SVsShellMonitorSelection)) as IVsMonitorSelection;
            _monitorSelection?.AdviseSelectionEvents(this, out _selectionEventsCookie);

            // Try to populate the outline for the currently active document.
            UpdateOutlineForActiveDocument();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (_monitorSelection != null && _selectionEventsCookie != 0)
                {
                    ThreadHelper.JoinableTaskFactory.Run(async () =>
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        _monitorSelection.UnadviseSelectionEvents(_selectionEventsCookie);
                    });
                    _selectionEventsCookie = 0;
                }

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

        private void UpdateOutlineForActiveDocument()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            // Detach from the previous text view
            if (_activeTextView != null)
            {
                _activeTextView.Caret.PositionChanged -= OnCaretPositionChanged;
                _activeTextView = null;
            }

            // Find the current active IWpfTextView
            var textView = GetActiveWpfTextView();
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

        private IWpfTextView GetActiveWpfTextView()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            try
            {
                var textManager = GetService(typeof(SVsTextManager)) as IVsTextManager;
                if (textManager == null)
                    return null;

                textManager.GetActiveView(1, null, out IVsTextView vsView);
                if (vsView == null)
                    return null;

                var adapterFactory = XSharpLanguagePackage.GetComponentModel()
                    ?.GetService<Microsoft.VisualStudio.Editor.IVsEditorAdaptersFactoryService>();

                return adapterFactory?.GetWpfTextView(vsView);
            }
            catch (Exception)
            {
                return null;
            }
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
