using System;
using System.Linq;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using static XSharp.Project.XSharpConstants;
using XSharpColorizer;
using Microsoft.VisualStudio.Text.Classification;
using System.Collections.Immutable;

namespace XSharp.Project.Editors.BraceMatching
{

    [Export(typeof(IViewTaggerProvider))]
    [ContentType(LanguageName)]
    [TagType(typeof(TextMarkerTag))]
    internal class BraceMatchingTaggerProvider : IViewTaggerProvider
    {
        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            var package = XSharp.Project.XSharpProjectPackage.Instance;
            var optionsPage = package.GetIntellisenseOptionsPage();
            if (optionsPage.DisableBraceMatching)
                return null;
            if (textView == null || buffer == null)
                return null;

            //provide highlighting only on the top-level buffer
            if (textView.TextBuffer != buffer)
                return null;

            return new BraceMatchingTagger(textView, buffer) as ITagger<T>;

        }
    }



    internal class BraceMatchingTagger : ITagger<TextMarkerTag>
    {
        ITextView View { get; set; }
        ITextBuffer SourceBuffer { get; set; }
        SnapshotPoint? CurrentChar { get; set; }
        static private Dictionary<char, char> m_braceList;

        internal BraceMatchingTagger(ITextView view, ITextBuffer sourceBuffer)
        {
            //here the keys are the open braces, and the values are the close braces
            if (m_braceList == null)
            {
                m_braceList = new Dictionary<char, char>();
                m_braceList.Add('{', '}');
                m_braceList.Add('[', ']');
                m_braceList.Add('(', ')');
            }
            this.View = view;
            this.SourceBuffer = sourceBuffer;
            this.CurrentChar = null;

            this.View.Caret.PositionChanged += CaretPositionChanged;
            this.View.LayoutChanged += ViewLayoutChanged;
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

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

            //get the current char and the previous char
            char currentText = '\0';
            char lastText = '\0';
            SnapshotSpan pairSpan = new SnapshotSpan();
            SnapshotPoint lastChar = new SnapshotPoint();
            try
            {
                currentText = currentChar.GetChar();
                lastChar = currentChar == 0 ? currentChar : currentChar - 1; //if currentChar is 0 (beginning of buffer), don't move it back
                lastText = lastChar.GetChar();
            }
            catch (Exception)
            {

            }
            // First, try to match Simple chars
            if (m_braceList.ContainsKey(currentText))   //the key is the open brace
            {
                char closeChar;
                m_braceList.TryGetValue(currentText, out closeChar);
                if (BraceMatchingTagger.FindMatchingCloseChar(currentChar, currentText, closeChar, View.TextViewLines.Count, out pairSpan) == true)
                {
                    yield return new TagSpan<TextMarkerTag>(new SnapshotSpan(currentChar, 1), new TextMarkerTag("blue"));
                    yield return new TagSpan<TextMarkerTag>(pairSpan, new TextMarkerTag("blue"));
                }
            }
            else if (m_braceList.ContainsValue(lastText))    //the value is the close brace, which is the *previous* character 
            {
                var open = from n in m_braceList
                            where n.Value.Equals(lastText)
                            select n.Key;
                if (BraceMatchingTagger.FindMatchingOpenChar(lastChar, (char)open.ElementAt<char>(0), lastText, View.TextViewLines.Count, out pairSpan) == true)
                {
                    yield return new TagSpan<TextMarkerTag>(new SnapshotSpan(lastChar, 1), new TextMarkerTag("blue"));
                    yield return new TagSpan<TextMarkerTag>(pairSpan, new TextMarkerTag("blue"));
                }
            }
            else
            {
                // Second, try to Match Keywords
                // Try to retrieve an already parsed list of Tags
                XSharpClassifier xsClassifier = null;
                if (SourceBuffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
                {
                    xsClassifier = SourceBuffer.Properties[typeof(XSharpClassifier)] as XSharpClassifier;
                }

                if (xsClassifier != null)
                {
                    //
                    ITextSnapshot snapshot = xsClassifier.Snapshot;
                    SnapshotSpan Span = new SnapshotSpan(snapshot, 0, snapshot.Length);
                    IImmutableList<ClassificationSpan> classifications = xsClassifier.GetTags();
                    // We cannot use SortedList, because we may have several Classification that start at the same position
                    List<ClassificationSpan> sortedTags = new List<ClassificationSpan>();
                    foreach (var tag in classifications)
                    {
                        // Only keep the Brace matching Tags
                        if ((tag.ClassificationType.IsOfType(ColorizerConstants.XSharpBraceOpenFormat)) ||
                                (tag.ClassificationType.IsOfType(ColorizerConstants.XSharpBraceCloseFormat)))
                            sortedTags.Add(tag);
                    }
                    sortedTags.Sort((a, b) => a.Span.Start.Position.CompareTo(b.Span.Start.Position));
                    //
                    int indexTag = sortedTags.FindIndex(x => currentChar.Position >= x.Span.Start.Position && currentChar.Position <= x.Span.End.Position);
                    if (indexTag != -1)
                    {
                        var currentTag = sortedTags[indexTag];
                        if (currentTag.ClassificationType.IsOfType(ColorizerConstants.XSharpBraceOpenFormat))
                        {
                            if (FindMatchingCloseTag(sortedTags, indexTag, snapshot, out pairSpan))
                            {
                                yield return new TagSpan<TextMarkerTag>(currentTag.Span, new TextMarkerTag("bracehighlight"));
                                yield return new TagSpan<TextMarkerTag>(pairSpan, new TextMarkerTag("bracehighlight"));
                            }
                        }
                        else
                        {
                            if (FindMatchingOpenTag(sortedTags, indexTag, snapshot, out pairSpan))
                            {
                                yield return new TagSpan<TextMarkerTag>(pairSpan, new TextMarkerTag("bracehighlight"));
                                yield return new TagSpan<TextMarkerTag>(currentTag.Span, new TextMarkerTag("bracehighlight"));
                            }
                        }
                    }
                }
            }
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
                    if (closeTag.ClassificationType.IsOfType(ColorizerConstants.XSharpBraceCloseFormat))
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
                System.Diagnostics.Debug.WriteLine(e.Message);
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
                    if (openTag.ClassificationType.IsOfType(ColorizerConstants.XSharpBraceOpenFormat))
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
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
            //
            return false;
        }


        private static bool FindMatchingCloseChar(SnapshotPoint startPoint, char open, char close, int maxLines, out SnapshotSpan pairSpan)
        {
            pairSpan = new SnapshotSpan(startPoint.Snapshot, 1, 1);
            try
            {
	            ITextSnapshotLine line = startPoint.GetContainingLine();
	            string lineText = line.GetText();
	            int lineNumber = line.LineNumber;
	            int offset = startPoint.Position - line.Start.Position + 1;
	
	            int stopLineNumber = startPoint.Snapshot.LineCount - 1;
	            if (maxLines > 0)
	                stopLineNumber = Math.Min(stopLineNumber, lineNumber + maxLines);
	
	            int openCount = 0;
	            while (true)
	            {
	                //walk the entire line
	                while (offset < line.Length)
	                {
	                    char currentChar = lineText[offset];
	                    if (currentChar == close) //found the close character
	                    {
	                        if (openCount > 0)
	                        {
	                            openCount--;
	                        }
	                        else    //found the matching close
	                        {
	                            pairSpan = new SnapshotSpan(startPoint.Snapshot, line.Start + offset, 1);
	                            return true;
	                        }
	                    }
	                    else if (currentChar == open) // this is another open
	                    {
	                        openCount++;
	                    }
	                    offset++;
	                }
	
	                //move on to the next line
	                if (++lineNumber > stopLineNumber)
	                    break;
	
	                line = line.Snapshot.GetLineFromLineNumber(lineNumber);
	                lineText = line.GetText();
	                offset = 0;
	            }
            }
            catch (System.Exception ex)
            {
                System.Diagnostics.Debug.WriteLine(ex.Message);
            }

            return false;
        }

        private static bool FindMatchingOpenChar(SnapshotPoint startPoint, char open, char close, int maxLines, out SnapshotSpan pairSpan)
        {
            pairSpan = new SnapshotSpan(startPoint, startPoint);
            try
            {

                ITextSnapshotLine line = startPoint.GetContainingLine();

                int lineNumber = line.LineNumber;
                int offset = startPoint - line.Start - 1; //move the offset to the character before this one

                //if the offset is negative, move to the previous line
                if (offset < 0)
                {
                    line = line.Snapshot.GetLineFromLineNumber(--lineNumber);
                    offset = line.Length - 1;
                }

                string lineText = line.GetText();

                int stopLineNumber = 0;
                if (maxLines > 0)
                    stopLineNumber = Math.Max(stopLineNumber, lineNumber - maxLines);

                int closeCount = 0;

                while (true)
                {
                    // Walk the entire line
                    while (offset >= 0)
                    {
                        char currentChar = lineText[offset];

                        if (currentChar == open)
                        {
                            if (closeCount > 0)
                            {
                                closeCount--;
                            }
                            else // We've found the open character
                            {
                                pairSpan = new SnapshotSpan(line.Start + offset, 1); //we just want the character itself
                                return true;
                            }
                        }
                        else if (currentChar == close)
                        {
                            closeCount++;
                        }
                        offset--;
                    }

                    // Move to the previous line
                    if (--lineNumber < stopLineNumber)
                        break;

                    line = line.Snapshot.GetLineFromLineNumber(lineNumber);
                    lineText = line.GetText();
                    offset = line.Length - 1;
                }
            }
            catch (System.Exception ex)
            {
                System.Diagnostics.Debug.WriteLine(ex.Message);
            }
            return false;
        }


    }
}
