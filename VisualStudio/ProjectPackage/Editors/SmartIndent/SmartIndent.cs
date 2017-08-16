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
                // How many spaces do we need ?
                int indentValue = 0;
                int tabSize = this._textView.Options.GetTabSize();
                var package = XSharp.Project.XSharpProjectPackage.Instance;
                var optionsPage = package.GetIntellisenseOptionsPage();
                bool alignDoCase = optionsPage.AlignDoCase;
                bool alignMethod = optionsPage.AlignMethod;
                // On what line are we ?
                int lineNumber = line.LineNumber;
                if (lineNumber > 0)
                {
                    // We need to analyze the Previous line
                    lineNumber = lineNumber - 1;
                    ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber);
                    bool doSkipped;
                    string keyword = GetFirstKeywordInLine(prevLine, out doSkipped, out indentValue);
                    //
                    if (indentValue > -1)
                        _lastIndentValue = indentValue;
                    // ok, now check what we have, starting the previous line
                    if (!String.IsNullOrEmpty(keyword))
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
                            case "FOREACH":
                            case "FOR":
                            case "REPEAT":
                            case "BEGIN":
                            case "TRY":
                                //
                                indentValue += tabSize;
                                break;

                            case "ELSE":
                            case "ELSEIF":
                                // Retrieve the Indentation for the previous line
                                int elseIndentValue = AlignToIFToken(line);
                                // And apply
                                // De-Indent previous line !!!
                                var buffer = this._textView.TextBuffer;
                                var editSession = buffer.CreateEdit(); 
                                try
                                {
                                    XSharp.Project.CommandFilterHelper.FormatLine(this._aggregator, this._textView, editSession, prevLine, elseIndentValue);
                                }
                                finally
                                {
                                    editSession.Apply();
                                }
                                break;

                            case "END":
                            case "ENDDO":
                            case "ENDIF":
                            case "NEXT":
                                //
                                indentValue = AlignToOpenToken(prevLine);
                                if (indentValue < 0)
                                    indentValue = 0;
                                // De-Indent previous line !!!
                                buffer = this._textView.TextBuffer;
                                editSession = buffer.CreateEdit(); // EditOptions.DefaultMinimalChange, 0, null);
                                try
                                {
                                    XSharp.Project.CommandFilterHelper.FormatLine(this._aggregator, this._textView, editSession, prevLine, indentValue);
                                }
                                finally
                                {
                                    editSession.Apply();
                                }
                                break;
                        }
                        if (indentValue < 0)
                            indentValue = 0;
                        //
                        _lastIndentValue = indentValue;
                    }
                    return _lastIndentValue;
                }
            }
            return null;
        }

        private int AlignToIFToken(ITextSnapshotLine currentLine)
        {
            int indentValue = 0;
            // On what line are we ?
            int lineNumber = currentLine.LineNumber;
            // We need to analyze the Previous line
            lineNumber = lineNumber - 1;
            while (lineNumber > 0)
            {
                // We need to analyze the Previous line
                lineNumber = lineNumber - 1;
                ITextSnapshotLine line = currentLine.Snapshot.GetLineFromLineNumber(lineNumber);
                List<IMappingTagSpan<IClassificationTag>> tagList = GetTagsInLine(line);
                String keyword = "";
                //
                if (tagList.Count > 0)
                {
                    var buffer = this._textView.TextBuffer;
                    int tabSize = this._textView.Options.GetTabSize();
                    IMappingSpan currentSpan = tagList[0].Span;
                    SnapshotPoint? snapPointFirst = currentSpan.Start.GetPoint(buffer, PositionAffinity.Predecessor);
                    // Extract the start of line
                    SnapshotSpan toIndent = new SnapshotSpan(line.Start, snapPointFirst.Value.Position - line.Start.Position);
                    String startOfLine = toIndent.GetText();
                    // Convert Tabs to Spaces
                    startOfLine = startOfLine.Replace("\t", new String(' ', tabSize));
                    // So, at least, to align to previous line, we will need...
                    indentValue = startOfLine.Length;
                    //
                    IClassificationTag currentTag = tagList[0].Tag;
                    currentSpan = tagList[0].Span;
                    //
                    if (currentTag.ClassificationType.IsOfType("keyword"))
                    {
                        var spans = currentSpan.GetSpans(buffer);
                        if (spans.Count > 0)
                        {
                            SnapshotSpan kwSpan = spans[0];
                            keyword = kwSpan.GetText();
                            keyword = keyword.ToUpper();
                            if (keyword == "IF")
                                break;
                        }
                    }
                    // 
                    indentValue = 0;
                }
            }
            //
            return indentValue;
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

        private int AlignToOpenToken(ITextSnapshotLine currentLine)
        {
            int lineNumber = currentLine.LineNumber;
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
                // We cannot use SortedList, because we may have several Classification that start at the same position
                List<Microsoft.VisualStudio.Text.Classification.ClassificationSpan> sortedTags = new List<Microsoft.VisualStudio.Text.Classification.ClassificationSpan>();
                foreach (var tag in classifications)
                {
                    sortedTags.Add(tag);
                }
                sortedTags.Sort((a, b) => a.Span.Start.Position.CompareTo(b.Span.Start.Position));
                //
                //
                Stack<Microsoft.VisualStudio.Text.Classification.ClassificationSpan> startStack = new Stack<Microsoft.VisualStudio.Text.Classification.ClassificationSpan>();
                foreach (var tag in sortedTags)
                {
                    // Is it a Region ?
                    if (tag.ClassificationType.IsOfType(ColorizerConstants.XSharpRegionStartFormat))
                    {
                        startStack.Push(tag);
                    }
                    else if (tag.ClassificationType.IsOfType(ColorizerConstants.XSharpRegionStopFormat))
                    {
                        //
                        var startTag = startStack.Pop();
                        var startLine = startTag.Span.Start.GetContainingLine();
                        // Looking for an End

                        var endLine = tag.Span.End.GetContainingLine();
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

        /// <summary>
        /// Retrieve all Tags in the Line
        /// </summary>
        /// <param name="line"></param>
        /// <returns></returns>
        private List<IMappingTagSpan<IClassificationTag>> GetTagsInLine(ITextSnapshotLine line)
        {
            //
            SnapshotSpan lineSpan = new SnapshotSpan(line.Start, line.Length);
            //
            var buffer = this._textView.TextBuffer;
            var tagAggregator = this._aggregator.CreateTagAggregator<IClassificationTag>(buffer);
            var tags = tagAggregator.GetTags(lineSpan);
            List<IMappingTagSpan<IClassificationTag>> tagList = new List<IMappingTagSpan<IClassificationTag>>();
            foreach (var tag in tags)
            {
                tagList.Add(tag);
            }
            return tagList;
        }

        /// <summary>
        /// Get the first keyword in Line. The modifiers (Private, Protected, ... ) are ignored
        /// If the first Keyword is a Comment, "//" is returned
        /// </summary>
        /// <param name="line">The line to analyze</param>
        /// <param name="doSkipped">Bool value indicating if a "DO" keyword has been skipped</param>
        /// <param name="minIndent"></param>
        /// <returns></returns>
        private String GetFirstKeywordInLine(ITextSnapshotLine line, out bool doSkipped, out int minIndent)
        {
            minIndent = -1;
            doSkipped = false;
            List<IMappingTagSpan<IClassificationTag>> tagList = GetTagsInLine(line);
            String keyword = "";
            //
            if (tagList.Count > 0)
            {
                var buffer = this._textView.TextBuffer;
                int tabSize = this._textView.Options.GetTabSize();
                IMappingSpan currentSpan = tagList[0].Span;
                SnapshotPoint? snapPointFirst = currentSpan.Start.GetPoint(buffer, PositionAffinity.Predecessor);
                // Extract the start of line
                SnapshotSpan toIndent = new SnapshotSpan(line.Start, snapPointFirst.Value.Position - line.Start.Position);
                String startOfLine = toIndent.GetText();
                // Convert Tabs to Spaces
                startOfLine = startOfLine.Replace("\t", new String(' ', tabSize));
                // So, at least, to align to previous line, we will need...
                minIndent = startOfLine.Length;
                //
                int tagIndex = 0;
                while (tagIndex < tagList.Count)
                {
                    IClassificationTag currentTag = tagList[tagIndex].Tag;
                    currentSpan = tagList[tagIndex].Span;
                    //
                    if (currentTag.ClassificationType.IsOfType("keyword"))
                    {
                        var spans = currentSpan.GetSpans(buffer);
                        if (spans.Count > 0)
                        {
                            SnapshotSpan kwSpan = spans[0];
                            keyword = kwSpan.GetText();
                            keyword = keyword.ToUpper();
                            // it could be modifier...
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
                                case "PARTIAL":
                                    tagIndex++;
                                    keyword = "";
                                    continue;
                                case "DO":
                                    tagIndex++;
                                    keyword = "";
                                    doSkipped = true;
                                    continue;
                                default:
                                    break;
                            }
                        }
                    }
                    else if (currentTag.ClassificationType.IsOfType("comment"))
                    {
                        //
                        keyword = "//";
                    }
                    // out please
                    break;
                };
            }
            return keyword;
        }


        public void Dispose() { }


    }
}
