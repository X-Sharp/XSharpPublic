//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System.Collections.Generic;

namespace XSharp.LanguageService
{
    /// <summary>
    /// This class gets the locations for the separators and sets them
    /// for the lines that need them.
    /// </summary>
    internal class LineSeparatorManager
    {
        readonly IWpfTextView _textView;
        readonly ITextBuffer _buffer;
        readonly IAdornmentLayer adornmentLayer;
        private LineSeparatorTag _lineSeparatorTag;
        private readonly object _lineSeperatorTagGate = new object();
        private readonly IEditorFormatMap _editorFormatMap;

        internal LineSeparatorManager(IWpfTextView textView, IViewTagAggregatorFactoryService aggregatorService, IEditorFormatMapService editorFormatMapService)
        {
            _textView = textView;
            adornmentLayer = textView.GetAdornmentLayer(Constants.LanguageName + "LineSeparator");
            textView.LayoutChanged += OnLayoutChanged;
            _buffer = textView.TextBuffer;
            _editorFormatMap = editorFormatMapService.GetEditorFormatMap("text");
            _editorFormatMap.FormatMappingChanged += FormatMappingChanged;
            _lineSeparatorTag = new LineSeparatorTag(_editorFormatMap);
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
            if (_buffer == null)
                return;
            var lineState = _buffer.GetLineState();
            if (lineState == null) 
                return;
            var snapshot = _textView.TextSnapshot;
            var viewLines = _textView.TextViewLines;

            // remove old tags
            adornmentLayer.RemoveAllAdornments();

            // create new tags
            foreach (var item in lineState.Lines)
            {
                if (item.Value.HasFlag(LineFlags.EntityStart))
                {
                    var index = item.Key;
                    // entity on line 0 cannot have a separator because there no line before it
                    if (index > 0 ) 
                    {
                        // the separator is attached to the lone before the entity
                        var line = snapshot.GetLineFromLineNumber(index-1);
                        // add one to the line length to include the eol. Otherwise empty lines before an entity
                        // will not get a separator
                        var ssp = new SnapshotSpan(snapshot, line.Start, line.Length+1);
                        var tag = new TagSpan<LineSeparatorTag>(ssp, _lineSeparatorTag);
                        // A text view can hide lines and then IntersectsBufferSpan returns false for invisible lines
                        if (viewLines.IntersectsBufferSpan(ssp))
                        {
                            var geometry = viewLines.GetMarkerGeometry(ssp);
                            if (geometry != null)
                            {
                                var graphicsResult = _lineSeparatorTag.GetGraphics(_textView, geometry, format: null);
                                adornmentLayer.AddAdornment(
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

            return;
        }
    }
}
