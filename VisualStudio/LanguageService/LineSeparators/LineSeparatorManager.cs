//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Immutable;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using System;
using XSharpModel;
using System.Collections.Generic;
using Community.VisualStudio.Toolkit;

namespace XSharp.LanguageService
{
    /// <summary>
    /// This class gets the locations for the separators and sets them
    /// for the lines that need them.
    /// </summary>
    internal class LineSeparatorManager
    {
        private readonly IWpfTextView _textView;
        private readonly ITextBuffer _buffer;
        private readonly IAdornmentLayer _adornmentLayer;
        private readonly object _lineSeperatorTagGate = new object();
        private readonly IEditorFormatMap _editorFormatMap;
        private readonly XSharpClassifier _classifier;
        private bool closed = false;

        private LineSeparatorTag _lineSeparatorTag;


        internal LineSeparatorManager(DocumentView docView, LineSeparatorAdornmentManagerProvider provider, IViewTagAggregatorFactoryService aggregatorService, IEditorFormatMapService editorFormatMapService)
        {
            _textView = docView.TextView;
            _adornmentLayer = _textView.GetAdornmentLayer(Constants.LanguageName + "LineSeparator");
            _textView.LayoutChanged += OnLayoutChanged;
            _textView.Closed += OnClosed;
            _buffer = docView.TextBuffer;
            _editorFormatMap = editorFormatMapService.GetEditorFormatMap("text");
            _editorFormatMap.FormatMappingChanged += FormatMappingChanged;
            _lineSeparatorTag = new LineSeparatorTag(_editorFormatMap);
            if (_buffer.Properties.TryGetProperty(typeof(XSharpClassifier), out _classifier))
            {
                _classifier.LineStateChanged += LineStateChanged;
            }

        }

        private void OnClosed(object sender, EventArgs e)
        {
            closed = true;
            if (_textView != null)
            {
                _textView.Closed -= OnClosed;
                _textView.LayoutChanged -= OnLayoutChanged;
            }
            if (_classifier != null)
            {
                _classifier.LineStateChanged -= LineStateChanged;
            }
            if (_editorFormatMap != null)
            {
                _editorFormatMap.FormatMappingChanged -= FormatMappingChanged;
            }

        }

        private void LineStateChanged(object sender, EventArgs e)
        {
            Repaint();
        }


        private void FormatMappingChanged(object sender, FormatItemsEventArgs e)
        {
            lock (_lineSeperatorTagGate)
            {
                _lineSeparatorTag = new LineSeparatorTag(_editorFormatMap);
            }

        }
        /// <summary>
        /// Repaint the line separators when the layout changes
        /// </summary>
        private void OnLayoutChanged(object sender, TextViewLayoutChangedEventArgs e)
        {
            Repaint();
        }
        internal void Repaint()
        {
            // now we know which lines to draw, now it is time to remove the old lines and paint the new ones
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                if (! closed)
                    _repaint();
            });
        }

        private void _repaint()
        {
            // if they have switched of the dividers then abort
            if (!XEditorSettings.ShowDividers)
                return;
            var snapshot = _textView.TextSnapshot;
            var doc = snapshot.TextBuffer.GetDocument();
            if (doc == null)
                return;
            var lineState = doc.LineState;
            if (doc.SnapShot.Version != snapshot.Version)
                return;
            var viewLines = _textView.TextViewLines;

            var singleLineDividers = XEditorSettings.ShowSingleLineDividers;
            // create new tags
            // first collect the info about the lines to paint and then do the actual painting

            var lineinfo = new List<SnapshotSpan>();

            foreach (var item in lineState.Lines)
            {
                var add = false;
                add = item.Value.HasFlag(LineFlags.EntityStart);
                if (!add)
                    add = singleLineDividers && item.Value.HasFlag(LineFlags.SingleLineEntity);
                if (add)
                {
                    var index = item.Key;
                    // entity on line 0 cannot have a separator because there no line before it
                    if (index > 0 && index < snapshot.LineCount)
                    {
                        // the separator is attached to the line before the entity

                        var line = snapshot.GetLineFromLineNumber(index - 1);
                        // add one to the line length to include the end of line. Otherwise empty lines before an entity
                        // will not get a separator
                        var ssp = new SnapshotSpan(snapshot, line.Start, line.Length + 1);
                        // A text view can hide lines and then IntersectsBufferSpan returns false for invisible lines
                        if (viewLines.IntersectsBufferSpan(ssp))
                        {
                            lineinfo.Add(ssp);
                        }
                    }
                }
            }

           if (lineinfo.Count > 0)
            {
                // remove old tags
                _adornmentLayer.RemoveAllAdornments();
                foreach (var ssp in lineinfo)
                {
                    var tag = new TagSpan<LineSeparatorTag>(ssp, _lineSeparatorTag);
                    var geometry = viewLines.GetMarkerGeometry(ssp);
                    if (geometry != null)
                    {
                        var graphicsResult = _lineSeparatorTag.GetGraphics(_textView, geometry);
                        _adornmentLayer.AddAdornment(
                            behavior: AdornmentPositioningBehavior.TextRelative,
                            visualSpan: ssp,
                            tag: tag,
                            adornment: graphicsResult.VisualElement,
                            removedCallback: delegate { graphicsResult.Dispose(); });
                    }
                }
            }


        }
    }
}
