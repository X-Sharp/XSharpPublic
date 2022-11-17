//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using XSharpModel;
using static XSharp.XSharpConstants;

namespace XSharp.LanguageService
{

    [Export(typeof(IViewTaggerProvider))]
    [ContentType(LanguageName)]
    [TagType(typeof(TextMarkerTag))]

    // we now have 2 separate taggers
    // 1 that matches the operator tokens which is implemented inside the toolkit
    // 2 tagger for keyword open/close pairs. This needs revisiting to use info
    // collected in the classifier
    internal sealed class BraceMatchingProvider : BraceMatchingBase
    {
        public override string TextMarketTagType => ColorizerConstants.BraceFormatDefinition;
        public override Dictionary<char, char> BraceList
        {
            get
            {
                var result = new Dictionary<char, char>()
                {
                    { '{', '}' },
                    { '(', ')' },
                    { '[', ']' },
                };
                if (XEditorSettings.DisableBraceMatching)
                {
                    result.Clear();
                }
                return result;
            }
        }

    }

    // Tagger that matches keyword pairs


    [Export(typeof(IViewTaggerProvider))]
    [ContentType(LanguageName)]
    [TagType(typeof(TextMarkerTag))]
    internal class KeywordMatchingTaggerProvider : IViewTaggerProvider
    {

        [Import]
        internal IBufferTagAggregatorFactoryService BufferTagAggregatorFactoryService { get; set; }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            if (XEditorSettings.DisableKeywordMatching)
                return null;
            if (textView == null || buffer == null)
                return null;



            return new KeywordMatchingTagger(textView, BufferTagAggregatorFactoryService) as ITagger<T>;
        }
    }



    internal class KeyWordTag : TextMarkerTag
    {
        public KeyWordTag() : base(ColorizerConstants.KeyWordFormatDefinition)
        {
        }
    }
    internal class KeywordMatchingTagger : ITagger<TextMarkerTag>
    {
        private readonly ITextView _view;
        private readonly ITextBuffer _buffer;
        private readonly IBufferTagAggregatorFactoryService _aggregator;
        private readonly ITagAggregator<IClassificationTag> _tagAggregator;
        private SnapshotPoint? _currentChar;
        private readonly TextMarkerTag _tag = new KeyWordTag();

        internal KeywordMatchingTagger(ITextView view, IBufferTagAggregatorFactoryService aggregator)
        {
            _view = view;
            _buffer = view.TextBuffer;
            _aggregator = aggregator;
            _tagAggregator = _aggregator.CreateTagAggregator<IClassificationTag>(_buffer);

            _view.Caret.PositionChanged += CaretPositionChanged;
            _view.LayoutChanged += ViewLayoutChanged;
            _view.Closed += ViewClosed;
        }

        private void ViewClosed(object sender, EventArgs e)
        {
            _view.Closed -= ViewClosed;
            _view.Caret.PositionChanged -= CaretPositionChanged;
            _view.LayoutChanged -= ViewLayoutChanged;
        }


        void WriteOutputMessage(string sMessage)
        {
            if (XSettings.EnableBraceMatchLog && XSettings.EnableLogging)
            {
                XSettings.LogMessage("Keyword Matching: " + sMessage);
            }
        }


        private void ViewLayoutChanged(object sender, TextViewLayoutChangedEventArgs e)
        {
            if (e.NewSnapshot != e.OldSnapshot) //make sure that there has really been a change
            {
                UpdateAtCaretPosition(_view.Caret.Position);
            }
        }

        private void CaretPositionChanged(object sender, CaretPositionChangedEventArgs e)
        {
            UpdateAtCaretPosition(e.NewPosition);
        }

        private void UpdateAtCaretPosition(CaretPosition caretPosition)
        {
            _currentChar = caretPosition.Point.GetPoint(_buffer, caretPosition.Affinity);

            if (_currentChar.HasValue)
            {
                SnapshotSpan snapshot = new SnapshotSpan(_buffer.CurrentSnapshot, 0, _buffer.CurrentSnapshot.Length);
                TagsChanged?.Invoke(this, new SnapshotSpanEventArgs(snapshot));
            }
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;
        public IEnumerable<ITagSpan<TextMarkerTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            // Todo: the classifier is now marking open and close keywords with (invisible) classification
            // on another location the classifier is building regions based on Open/Close keywords.
            // During this process of building regions we can also 'remember' the open/close pairs
            // so we do not have to look for these in this code.

            DateTime oStart, oEnd;
            TimeSpan timeSpan;
            if (XEditorSettings.DisableKeywordMatching)
            {
                yield break;
            }
            oStart = DateTime.Now;

            if (spans.Count == 0 || _currentChar == null)   //there is no content in the buffer
                yield break;
            WriteOutputMessage($"GetTags() Spans: {spans.Count}");

            //don't do anything if the current SnapshotPoint is not initialized or at the end of the buffer
            if (!_currentChar.HasValue || _currentChar.Value.Position >= _currentChar.Value.Snapshot.Length)
                yield break;


            //hold on to a snapshot of the current character
            SnapshotPoint currentChar = _currentChar.Value;

            //if the requested snapshot isn't the same as the one the brace is on, translate our spans to the expected snapshot
            if (spans[0].Snapshot != currentChar.Snapshot)
            {
                currentChar = currentChar.TranslateTo(spans[0].Snapshot, PointTrackingMode.Positive);
            }

            //get the current char and the previous char
            SnapshotSpan pairSpan = new SnapshotSpan();


            // check to see if we are on a closing or opening keyword
            if (!cursorOnKwOpenClose(currentChar))
                yield break;

            // Try to Match Keywords
            // Try to retrieve an already parsed list of Tags
            XSharpClassifier xsClassifier = _buffer.GetClassifier();
            if (xsClassifier == null)
            {
                yield break;
            }
            else
            {
                ITagSpan<TextMarkerTag> result1 = null;
                ITagSpan<TextMarkerTag> result2 = null;
                ITagSpan<TextMarkerTag> result3 = null;
                ITagSpan<TextMarkerTag> result4 = null;
                try
                {
                    WriteOutputMessage("Match Open/Close keywords : " + oStart.ToString("hh:mm:ss.fff"));

                    ITextSnapshot snapshot = xsClassifier.Snapshot;
                    if (snapshot.Version != currentChar.Snapshot.Version)
                        yield break;
                    SnapshotSpan Span = new SnapshotSpan(snapshot, 0, snapshot.Length);
                    var classifications = xsClassifier.GetTags();
                    // We cannot use SortedList, because we may have several Classification that start at the same position
                    List<ClassificationSpan> sortedTags = new List<ClassificationSpan>();
                    foreach (var tag in classifications)
                    {
                        // Only keep the Brace matching Tags
                        if ((tag.ClassificationType.IsOfType(ColorizerConstants.XSharpKwOpenFormat)) ||
                                (tag.ClassificationType.IsOfType(ColorizerConstants.XSharpKwCloseFormat)))
                            sortedTags.Add(tag);
                    }
                    sortedTags.Sort((a, b) => a.Span.Start.Position.CompareTo(b.Span.Start.Position) * 1000 + string.Compare(a.ClassificationType.Classification, b.ClassificationType.Classification));
                    //
                    var tags = sortedTags.Where(x => currentChar.Position >= x.Span.Start.Position && currentChar.Position <= x.Span.End.Position);
                    foreach (var currentTag in tags)
                    {
                        var index = sortedTags.IndexOf(currentTag);
                        if (currentTag.ClassificationType.IsOfType(ColorizerConstants.XSharpKwOpenFormat) && result1 == null)
                        {
                            if (FindMatchingCloseTag(sortedTags, index, snapshot, out pairSpan))
                            {
                                var span = currentTag.Span;
                                result1 = new TagSpan<TextMarkerTag>(span, _tag);
                                result2 = new TagSpan<TextMarkerTag>(pairSpan, _tag);
                            }
                        }
                        else if (result3 == null)
                        {
                            if (FindMatchingOpenTag(sortedTags, index, snapshot, out pairSpan))
                            {
                                var span = currentTag.Span;
                                result3 = new TagSpan<TextMarkerTag>(pairSpan, _tag);
                                result4 = new TagSpan<TextMarkerTag>(span, _tag);
                            }
                        }
                    }

                }
                catch (Exception e)
                {
                    XSettings.LogException(e, "KeywordMatchingTagger.GetTags failed");
                }
                finally
                {
                    oEnd = DateTime.Now;
                    timeSpan = oEnd - oStart;
                    WriteOutputMessage("Finished Match Open/Close keywords: " + oEnd.ToString("hh:mm:ss.fff"));
                    WriteOutputMessage("Finished Match Open/Close keywords - total ms: " + timeSpan.TotalMilliseconds.ToString());
                }
                if (result1 != null)
                    yield return result1;
                if (result2 != null)
                    yield return result2;
                if (result3 != null)
                    yield return result3;
                if (result4 != null)
                    yield return result4;
            }
        }

        private bool cursorOnKwOpenClose(SnapshotPoint caret)
        {
            SnapshotSpan span = new SnapshotSpan(_view.TextSnapshot, caret.Position, 0);
            return _tagAggregator.GetTags(span).Any(tag =>
                   tag.Tag.ClassificationType.IsOfType(ColorizerConstants.XSharpKwOpenFormat) ||
                   tag.Tag.ClassificationType.IsOfType(ColorizerConstants.XSharpKwCloseFormat));
        }

        private bool FindMatchingCloseTag(List<ClassificationSpan> sortedTags, int indexTag, ITextSnapshot snapshot, out SnapshotSpan pairSpan)
        {
            pairSpan = new SnapshotSpan(snapshot, 1, 1);
            try
            {
                ClassificationSpan currentTag = sortedTags[indexTag];
                ITextSnapshotLine line = currentTag.Span.Start.GetContainingLine();
                int lineNumber = line.LineNumber;
                int nested = 0;
                for (int i = indexTag + 1; i < sortedTags.Count; i++)
                {
                    var closeTag = sortedTags[i];
                    if (closeTag.ClassificationType.IsOfType(ColorizerConstants.XSharpKwCloseFormat))
                    {
                        nested--;
                        if (nested < 0)
                        {
                            pairSpan = new SnapshotSpan(snapshot, closeTag.Span);
                            return true;
                        }
                    }
                    else
                    {
                        nested++;
                    }
                }
            }
            catch (Exception e)
            {
                XSettings.LogException(e, "FindMatchingCloseTag failed");
            }
            //
            return false;
        }


        private bool FindMatchingOpenTag(List<ClassificationSpan> sortedTags, int indexTag, ITextSnapshot snapshot, out SnapshotSpan pairSpan)
        {
            pairSpan = new SnapshotSpan(snapshot, 1, 1);
            try
            {
                ClassificationSpan currentTag = sortedTags[indexTag];
                ITextSnapshotLine line = currentTag.Span.Start.GetContainingLine();
                int lineNumber = line.LineNumber;
                int nested = 0;
                for (int i = indexTag - 1; i >= 0; i--)
                {
                    var openTag = sortedTags[i];
                    if (openTag.ClassificationType.IsOfType(ColorizerConstants.XSharpKwOpenFormat))
                    {
                        nested--;
                        if (nested < 0)
                        {
                            pairSpan = new SnapshotSpan(snapshot, openTag.Span);
                            return true;
                        }
                    }
                    else
                    {
                        nested++;
                    }
                }
            }
            catch (Exception e)
            {
                XSettings.LogException(e, "FindMatchingOpenTag failed");
            }
            return false;
        }

    }

}
