using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System.Collections.Generic;

namespace XSharp.LanguageService
{
    internal class LineSeparatorManager
    {
        readonly IWpfTextView _textView;
        readonly ITextBuffer _buffer;
        readonly IAdornmentLayer adornmentLayer;
        IViewTagAggregatorFactoryService tagAggregatorFactoryService;
        ITagAggregator<LineSeparatorTag> tagAggregator;
        private LineSeparatorTag _lineSeparatorTag;
        private readonly object _lineSeperatorTagGate = new object();
        private readonly IEditorFormatMap _editorFormatMap;
        //private IList<TagSpan<LineSeparatorTag>> _tags;

        internal LineSeparatorManager(IWpfTextView textView, IViewTagAggregatorFactoryService aggregatorService, IEditorFormatMapService editorFormatMapService)
        {
            _textView = textView;
            adornmentLayer = textView.GetAdornmentLayer(Constants.LanguageName + "LineSeparator");
            textView.LayoutChanged += OnLayoutChanged;
            _buffer = textView.TextBuffer;
            tagAggregatorFactoryService = aggregatorService;
            tagAggregator = tagAggregatorFactoryService.CreateTagAggregator<LineSeparatorTag>(textView);
            tagAggregator.TagsChanged += OnTagsChanged;
            _editorFormatMap = editorFormatMapService.GetEditorFormatMap("text");
            _editorFormatMap.FormatMappingChanged += FormatMappingChanged;
            _lineSeparatorTag = new LineSeparatorTag(_editorFormatMap);
            //_tags = new List<TagSpan<LineSeparatorTag>>();
        }

        private void FormatMappingChanged(object sender, FormatItemsEventArgs e)
        {
            lock (_lineSeperatorTagGate)
            {
                _lineSeparatorTag = new LineSeparatorTag(_editorFormatMap);
            }

        }

        private void OnTagsChanged(object sender, TagsChangedEventArgs e)
        {
            return;
        }

        private void OnLayoutChanged(object sender, TextViewLayoutChangedEventArgs e)
        {
            if (_buffer == null)
                return;
            if (!_buffer.Properties.TryGetProperty<XSharpLineState>(typeof(XSharpLineState), out var lineState)) 
                return;
            var snapshot = _textView.TextSnapshot;
            var viewLines = _textView.TextViewLines;

            // remove old spans
            adornmentLayer.RemoveAllAdornments();
            //lock (_tags)
            //{
                //foreach (var oldtag in _tags)
                //{
                //    var span = oldtag.Span;
                //    span = span.TranslateTo(snapshot, SpanTrackingMode.EdgeInclusive);
                //    // is there any effect on the view?
                //    if (viewLines.IntersectsBufferSpan(span))
                //    {
                //        adornmentLayer.RemoveAdornmentsByVisualSpan(span);
                //    }
                //}
                //_tags.Clear();
            //}

            foreach (var item in lineState.Lines)
            {
                if (item.Value.HasFlag(LineFlags.EntityStart))
                {
                    var index = item.Key;
                    if (index > 0 )
                    {
                        var line = snapshot.GetLineFromLineNumber(index-1);
                        var ssp = new SnapshotSpan(snapshot, line.Start, line.Length+1);
                        var tag = new TagSpan<LineSeparatorTag>(ssp, _lineSeparatorTag);
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
