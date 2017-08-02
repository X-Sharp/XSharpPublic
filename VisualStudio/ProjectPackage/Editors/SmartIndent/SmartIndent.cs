using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Editor.OptionsExtensionMethods;
using Microsoft.VisualStudio.Text.Tagging;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Project
{
    public class XSharpSmartIndent : ISmartIndent
    {
        private readonly ITextView _textView;
        private readonly IBufferTagAggregatorFactoryService _aggregator;

        public XSharpSmartIndent(ITextView view, IBufferTagAggregatorFactoryService aggregator)
        {
            _textView = view;
            _aggregator = aggregator;
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
                        int tabSize = this._textView.Options.GetTabSize();
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
                            if ( keyword != null )
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

                                }
                            }
                        }
                    }
                    return indentValue;
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

        public void Dispose() { }
    }
}
