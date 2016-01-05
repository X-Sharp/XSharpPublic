//------------------------------------------------------------------------------
// <copyright file="XSharpBraceMatching.cs" company="Company">
//     Copyright (c) Company.  All rights reserved.
// </copyright>
//------------------------------------------------------------------------------

using System;
using System.Linq;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Text.Classification;

namespace XSharpBraceMatching
{

    [Export(typeof(IViewTaggerProvider))]
    [ContentType("XSharp")]
    [TagType(typeof(TextMarkerTag))]
    internal class XSharpBraceMatchingTaggerProvider : IViewTaggerProvider
    {

        [Import]
        IClassifierAggregatorService aggregator = null;

        [Import]
        IClassificationTypeRegistryService ClassificationRegistry = null;


        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            if (textView == null)
                return null;

            //provide highlighting only on the top-level buffer
            if (textView.TextBuffer != buffer)
                return null;

            return new XSharpBraceMatchingTagger(textView, buffer, aggregator, ClassificationRegistry) as ITagger<T>;
        }
    }



    internal class XSharpBraceMatchingTagger : ITagger<TextMarkerTag>
    {
        ITextView View { get; set; }
        ITextBuffer SourceBuffer { get; set; }
        SnapshotPoint? CurrentChar { get; set; }
        private readonly IClassifier classifier;
        private IClassificationType xsharpBraceOpenType;
        private IClassificationType xsharpBraceCloseType;


        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        internal XSharpBraceMatchingTagger(ITextView view, ITextBuffer sourceBuffer, IClassifierAggregatorService AggregatorFactory, IClassificationTypeRegistryService registry)
        {
            this.View = view;
            this.SourceBuffer = sourceBuffer;
            this.CurrentChar = null;
            //
            this.classifier = AggregatorFactory.GetClassifier(sourceBuffer);
            this.xsharpBraceOpenType = registry.GetClassificationType(XSharpColorizer.Constants.XSharpBraceOpenFormat);
            this.xsharpBraceCloseType = registry.GetClassificationType(XSharpColorizer.Constants.XSharpBraceCloseFormat);

            this.View.Caret.PositionChanged += CaretPositionChanged;
            this.View.LayoutChanged += ViewLayoutChanged;
        }

        void ViewLayoutChanged(object sender, TextViewLayoutChangedEventArgs e)
        {
            if (e.NewSnapshot != e.OldSnapshot) //make sure that there has really been a change
            {
                UpdateAtCaretPosition(View.Caret.Position);
            }
        }

        void CaretPositionChanged(object sender, CaretPositionChangedEventArgs e)
        {
            UpdateAtCaretPosition(e.NewPosition);
        }

        void UpdateAtCaretPosition(CaretPosition caretPosition)
        {
            CurrentChar = caretPosition.Point.GetPoint(SourceBuffer, caretPosition.Affinity);

            if (!CurrentChar.HasValue)
                return;

            var tempEvent = TagsChanged;
            if (tempEvent != null)
                tempEvent(this, new SnapshotSpanEventArgs(new SnapshotSpan(SourceBuffer.CurrentSnapshot, 0,
                    SourceBuffer.CurrentSnapshot.Length)));
        }

        public IEnumerable<ITagSpan<TextMarkerTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if (spans.Count == 0)   //there is no content in the buffer
                yield break;

            //don't do anything if the current SnapshotPoint is not initialized or at the end of the buffer
            if (!CurrentChar.HasValue || CurrentChar.Value.Position >= CurrentChar.Value.Snapshot.Length)
                yield break;

            //hold on to a snapshot of the current character
            SnapshotPoint currentChar = CurrentChar.Value;

            //if the requested snapshot isn't the same as the one the brace is on, translate our spans to the expected snapshot
            if (spans[0].Snapshot != currentChar.Snapshot)
            {
                currentChar = currentChar.TranslateTo(spans[0].Snapshot, PointTrackingMode.Positive);
            }

            ////get the current char and the previous char
            //char currentText = currentChar.GetChar();
            //SnapshotPoint lastChar = currentChar == 0 ? currentChar : currentChar - 1; //if currentChar is 0 (beginning of buffer), don't move it back
            //char lastText = lastChar.GetChar();
            //SnapshotSpan pairSpan = new SnapshotSpan();

            //
            //
            //
            SnapshotSpan Span = new SnapshotSpan(SourceBuffer.CurrentSnapshot, 0, SourceBuffer.CurrentSnapshot.Length);
            IList<ClassificationSpan> classifications = this.classifier.GetClassificationSpans(Span);
            //
            int cnt = 0;
            int max = classifications.Count;
            if (max <= 0)
            {
                yield break;
            }
            //
            do
            {
                var classification = classifications[cnt];
                //
                if (classification.Span.Contains(currentChar.Position))
                {
                    if ((classification.ClassificationType == this.xsharpBraceOpenType) || (classification.ClassificationType == this.xsharpBraceCloseType))
                    {
                        int direction = 0;
                        int nextPos = cnt;
                        IClassificationType wasHandling;
                        IClassificationType lookingFor;
                        if (classification.ClassificationType == this.xsharpBraceOpenType)
                        {
                            wasHandling = this.xsharpBraceOpenType;
                            lookingFor = this.xsharpBraceCloseType;
                            direction = 1;
                        }
                        else
                        {
                            wasHandling = this.xsharpBraceCloseType;
                            lookingFor = this.xsharpBraceOpenType;
                            direction = -1;
                        }
                        //
                        nextPos = cnt + direction;
                        if ((nextPos < max) && (nextPos >= 0))
                        {
                            int inStack = 1;
                            do
                            {
                                classification = classifications[nextPos];
                                //
                                if (classification.ClassificationType == wasHandling)
                                {
                                    inStack++;
                                }
                                if (classification.ClassificationType == lookingFor)
                                {
                                    inStack--;
                                }
                                if (inStack == 0)
                                {
                                    yield return new TagSpan<TextMarkerTag>(new SnapshotSpan(currentChar, 1), new TextMarkerTag("blue"));
                                    SnapshotSpan pairSpan = new SnapshotSpan(classification.Span.Snapshot, classification.Span.Start, 1);
                                    yield return new TagSpan<TextMarkerTag>(pairSpan, new TextMarkerTag("blue"));
                                    //
                                    yield break;
                                }
                                //
                                nextPos = nextPos + direction;
                            } while ((nextPos < max) && (nextPos >= 0));
                        }
                    }
                }
                cnt++;
            } while (cnt < max);
            //
            //
            //

            //if (m_braceList.ContainsKey(currentText))   //the key is the open brace
            //{
            //    char closeChar;
            //    m_braceList.TryGetValue(currentText, out closeChar);
            //    if (XSharpBraceMatchingTagger.FindMatchingCloseChar(currentChar, currentText, closeChar, View.TextViewLines.Count, out pairSpan) == true)
            //    {
            //        yield return new TagSpan<TextMarkerTag>(new SnapshotSpan(currentChar, 1), new TextMarkerTag("blue"));
            //        yield return new TagSpan<TextMarkerTag>(pairSpan, new TextMarkerTag("blue"));
            //    }
            //}
            //else if (m_braceList.ContainsValue(lastText))    //the value is the close brace, which is the *previous* character 
            //{
            //    var open = from n in m_braceList
            //               where n.Value.Equals(lastText)
            //               select n.Key;
            //    if (XSharpBraceMatchingTagger.FindMatchingOpenChar(lastChar, (char)open.ElementAt<char>(0), lastText, View.TextViewLines.Count, out pairSpan) == true)
            //    {
            //        yield return new TagSpan<TextMarkerTag>(new SnapshotSpan(lastChar, 1), new TextMarkerTag("blue"));
            //        yield return new TagSpan<TextMarkerTag>(pairSpan, new TextMarkerTag("blue"));
            //    }
            //}
        }

    }

}
