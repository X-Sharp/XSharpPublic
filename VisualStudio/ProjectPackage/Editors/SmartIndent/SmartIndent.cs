using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Editor.OptionsExtensionMethods;
using Microsoft.VisualStudio.Text.Tagging;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using XSharpColorizer;

namespace XSharp.Project
{
    public class XSharpSmartIndent : ISmartIndent
    {
        private readonly ITextView _textView;
        private readonly IBufferTagAggregatorFactoryService _aggregator;
        private int _lastIndentValue;

        public XSharpSmartIndent(ITextView view, IBufferTagAggregatorFactoryService aggregator)
        {
            _textView = view;
            _aggregator = aggregator;
            _lastIndentValue = 0;
        }

        public int? GetDesiredIndentation(ITextSnapshotLine line)
        {
            //
            bool smartSetting = true;
            if (smartSetting)
            {
                int indentValue = 0;

                int lineNumber = line.LineNumber;
                if (lineNumber > 0)
                {
                    // Get Previous line
                    lineNumber = lineNumber - 1;
                    ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber);
                    var buffer = _textView.TextBuffer;
                    SnapshotSpan lineSpan = new SnapshotSpan(prevLine.Start, prevLine.Length);
                    int tabSize = this._textView.Options.GetTabSize();
                    //
                    var tagAggregator = _aggregator.CreateTagAggregator<IClassificationTag>(buffer);
                    var tags = tagAggregator.GetTags(lineSpan);
                    //
                    List<IMappingTagSpan<IClassificationTag>> tagList = new List<IMappingTagSpan<IClassificationTag>>();
                    foreach (var tag in tags)
                    {
                        if (!tag.Tag.ClassificationType.IsOfType(XSharpColorizer.ColorizerConstants.XSharpRegionStartFormat) &&
                            !tag.Tag.ClassificationType.IsOfType(XSharpColorizer.ColorizerConstants.XSharpRegionStopFormat))
                        {
                            tagList.Add(tag);
                        }
                    }
                    //
                    if (tagList.Count > 0)
                    {
                        tagList.Sort(delegate (IMappingTagSpan<IClassificationTag> x, IMappingTagSpan<IClassificationTag> y)
                        {
                            if (x.Span == null && y.Span == null) return 0;
                            //
                            SnapshotPoint? snapPointX = x.Span.Start.GetPoint(buffer, PositionAffinity.Predecessor);
                            SnapshotPoint? snapPointY = y.Span.Start.GetPoint(buffer, PositionAffinity.Predecessor);
                            return snapPointX.Value.Position.CompareTo(snapPointY.Value.Position);
                        });
                        // Get the first element on line
                        int tagIndex = 0;
                        IClassificationTag currentTag = tagList[tagIndex].Tag;
                        IMappingSpan currentSpan = tagList[tagIndex].Span;
                        SnapshotPoint? snapPointFirst = currentSpan.Start.GetPoint(buffer, PositionAffinity.Predecessor);
                        // Extract the start of line
                        SnapshotSpan toIndent = new SnapshotSpan(prevLine.Start, snapPointFirst.Value.Position - prevLine.Start.Position);
                        String startOfLine = toIndent.GetText();
                        // Convert Tabs to Spaces
                        startOfLine = startOfLine.Replace("\t", new String(' ', tabSize));
                        // So, at least, to align to previous line, we will need...
                        indentValue = startOfLine.Length;
                        //
                        string keyword = this.GetKeywordAt(tagList, tagIndex);
                        //
                        // ok, now check what we have, starting the previous line
                        if (keyword != null)
                        {
                            // it could be modifier...
                            bool nextModifier;
                            do
                            {
                                nextModifier = false;
                                switch (keyword)
                                {
                                    case "PROTECTED":
                                    case "INTERNAL":
                                    case "HIDDEN":
                                    case "PRIVATE":
                                    case "EXPORT":
                                    case "PUBLIC":
                                    case "STATIC":
                                    case "SEALED":
                                    case "ABSTRACT":
                                    case "VIRTUAL":
                                        nextModifier = true;
                                        tagIndex++;
                                        keyword = this.GetKeywordAt(tagList, tagIndex);
                                        if (keyword == null)
                                            nextModifier = false;
                                        break;
                                }
                            } while (nextModifier);
                            //
                            // eat a simple DO...
                            bool nextKeyword;
                            do
                            {
                                nextKeyword = false;
                                switch (keyword)
                                {
                                    case "DO":
                                        nextKeyword = true;
                                        tagIndex++;
                                        keyword = this.GetKeywordAt(tagList, tagIndex);
                                        if (keyword == null)
                                            nextKeyword = false;
                                        break;
                                }
                            } while (nextKeyword);
                            //
                            if (keyword != null)
                            {
                                switch (keyword)
                                {
                                    case "FUNCTION":
                                    case "PROCEDURE":
                                        // Align on Declaration ?
                                        // Currently, add one Tab
                                        indentValue += tabSize;
                                        break;

                                    case "CONSTRUCTOR":
                                    case "DESTRUCTOR":
                                    case "PROPERTY":
                                    case "ACCESS":
                                    case "ASSIGN":
                                    case "METHOD":
                                    case "OPERATOR":
                                    case "GET":
                                    case "SET":
                                        //
                                        indentValue += tabSize;
                                        break;

                                    case "INTERFACE":
                                    case "ENUM":
                                    case "CLASS":
                                    case "STRUCTURE":
                                    case "VOSTRUCT":
                                        //
                                        indentValue += tabSize;
                                        break;

                                    case "IF":
                                    case "WHILE":
                                    case "ELSE":
                                    case "ELSEIF":
                                    case "FOR":
                                        //
                                        indentValue += tabSize;
                                        break;

                                    case "END":
                                    case "ENDDO":
                                    case "ENDIF":
                                    case "NEXT":
                                        //
                                        indentValue = AlignToOpenToken(lineNumber);
                                        if (indentValue < 0)
                                            indentValue = 0;
                                        // De-Indent previous line !!!
                                        var edit = buffer.CreateEdit(); // EditOptions.DefaultMinimalChange, 0, null);
                                        try
                                        {
                                            String spaces = new String('\t', indentValue / tabSize) + new String(' ', indentValue % tabSize);
                                            edit.Replace(toIndent, spaces);
                                        }
                                        finally
                                        {
                                            edit.Apply();
                                        }
                                        break;

                                }
                            }
                        }
                        if (indentValue < 0)
                            indentValue = 0;
                        _lastIndentValue = indentValue;
                    }
                    return _lastIndentValue;
                }
            }
            return null;
        }

        private String GetKeywordAt(List<IMappingTagSpan<IClassificationTag>> tagList, int tagIndex)
        {
            string keyword = null;
            if (tagIndex < tagList.Count)
            {
                var buffer = _textView.TextBuffer;
                IClassificationTag currentTag = tagList[tagIndex].Tag;
                IMappingSpan currentSpan = tagList[tagIndex].Span;
                //
                if (currentTag.ClassificationType.IsOfType("keyword"))
                {
                    var spans = currentSpan.GetSpans(buffer);
                    if (spans.Count > 0)
                    {
                        SnapshotSpan kwSpan = spans[0];
                        keyword = kwSpan.GetText();
                        keyword = keyword.ToUpper();
                    }
                }
            }
            return keyword;
        }

        private int AlignToOpenToken(int lineNumber)
        {
            int indentValue = 0;
            //
            var buffer = _textView.TextBuffer;
            // Try to retrieve an already parsed list of Tags
            XSharpClassifier xsClassifier = null;
            if (buffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
            {
                xsClassifier = buffer.Properties[typeof(XSharpClassifier)] as XSharpClassifier;
            }

            if (xsClassifier != null)
            {
                //
                ITextSnapshot snapshot = xsClassifier.Snapshot;
                SnapshotSpan Span = new SnapshotSpan(snapshot, 0, snapshot.Length);
                System.Collections.Immutable.IImmutableList<Microsoft.VisualStudio.Text.Classification.ClassificationSpan> classifications = xsClassifier.GetRegionTags();
                //
                SortedList<int, Microsoft.VisualStudio.Text.Classification.ClassificationSpan> sortedTags = new SortedList<int, Microsoft.VisualStudio.Text.Classification.ClassificationSpan>();
                //
                foreach (var tag in classifications)
                {
                    sortedTags.Add(tag.Span.Start.Position, tag);
                }
                Stack<Microsoft.VisualStudio.Text.Classification.ClassificationSpan> startStack = new Stack<Microsoft.VisualStudio.Text.Classification.ClassificationSpan>();
                foreach (var tag in sortedTags)
                {
                    // Is it a Region ?
                    if (tag.Value.ClassificationType.IsOfType(ColorizerConstants.XSharpRegionStartFormat))
                    {
                        startStack.Push(tag.Value);
                    }
                    else if (tag.Value.ClassificationType.IsOfType(ColorizerConstants.XSharpRegionStopFormat))
                    {
                        //
                        var startTag = startStack.Pop();
                        var startLine = startTag.Span.Start.GetContainingLine();
                        // Looking for an End

                        var endLine = tag.Value.Span.End.GetContainingLine();
                        if (endLine.LineNumber == lineNumber)
                        {
                            // Where is the start ?
                            SnapshotSpan sSpan = new SnapshotSpan(startLine.Start, startLine.End);
                            String lineText = sSpan.GetText();
                            int tabSize = this._textView.Options.GetTabSize();
                            lineText = lineText.Replace("\t", new String(' ', tabSize));
                            // 
                            indentValue = (lineText.Length - lineText.TrimStart().Length);
                            break;
                        }
                    }
                }
            }
            //
            return indentValue;
        }

        public void Dispose() { }


    }
}
