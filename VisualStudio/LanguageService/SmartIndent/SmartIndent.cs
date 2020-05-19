#if SMARTINDENTMEF
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Editor.OptionsExtensionMethods;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.Collections.Generic;
using System.Diagnostics;
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
        private int _tabSize;
        private bool _alignDoCase;
        private bool _alignMethod;
        private vsIndentStyle _indentStyle;
        private IEditorOptions _options;
        private OptionsPages.IntellisenseOptionsPage _optionsPage;
        IVsTextManager4 _textManager;
        private String[] _indentKeywords;
        private String[] _codeBlockKeywords;
        private String[][] _middleKeywords;
        private String[][] _specialKeywords;
        private String[][] _specialOutdentKeywords;

        public XSharpSmartIndent(ITextView view, IBufferTagAggregatorFactoryService aggregator)
        {
            _textView = view;
            _aggregator = aggregator;
            _lastIndentValue = 0;
            _options = _textView.Options;
            // this does not fire when Smartindent option is changed 
            _options.OptionChanged += Options_OptionChanged;
            _options.GlobalOptions.OptionChanged += Options_OptionChanged;
            var package = XSharp.Project.XSharpProjectPackage.Instance;
            _textManager = package.GetTextManager();
            _optionsPage = package.GetIntellisenseOptionsPage();
            getOptions();
            getKeywords();
        }


        private void getOptions()
        {
            _alignDoCase = _optionsPage.AlignDoCase;
            _alignMethod = _optionsPage.AlignMethod;
            var languagePreferences = new LANGPREFERENCES3[1];
            languagePreferences[0].guidLang = GuidStrings.guidLanguageService;
            var result = _textManager.GetUserPreferences4(pViewPrefs: null, pLangPrefs: languagePreferences, pColorPrefs: null);
            if (result == VSConstants.S_OK)
            {
                _indentStyle = languagePreferences[0].IndentStyle;
                _tabSize = (int)languagePreferences[0].uTabSize;
            }
        }

#region Keywords Definitions
        private void getKeywords()
        {
            // Build list for Indent tokens
            _indentKeywords = this.getIndentKeywords();
            // Start of Method, Function, ...
            _codeBlockKeywords = this.getStartOfCodeKeywords();
            // Middle Keywords : ELSE, ELSEIF, ...
            _middleKeywords = this.getMiddleKeywords();
            // Name is Self-explanatory
            _specialKeywords = this.getSpecialMiddleKeywords();
            // Build list for Outdent tokens
            _specialOutdentKeywords = this.getSpecialOutdentKeywords();
        }


        private String[] getIndentKeywords()
        {
            // "DO" is removed by getFirstKeywordInLine(), so it is useless here...
            return new String[]{
                "DO","FOR","FOREACH","WHILE","IF",
                "BEGIN","TRY","REPEAT",
                "INTERFACE","ENUM","CLASS","STRUCTURE","VOSTRUCT"};
        }

        private String[] getOutdentKeywords()
        {
            // 
            return new String[]{
                "END","ENDIF","ENDCASE","ENDDO","NEXT","UNTIL"};
        }

        private String[][] getSpecialOutdentKeywords()
        {
            // These are keywords that we have between other keywords
            // "CASE" is the keyword that will trigger the process
            // "DO,SWITCH,BEGIN" is the list of possible start keyword
            // ...
            return new String[][]
            {
                new String[]{ "ENDIF","IF" },
                new String[]{ "ENDCASE", "DO" },
                new String[]{ "NEXT", "FOR,FOREACH" },
                new String[]{ "UNTIL", "REPEAT" },
                new String[]{ "END", "BEGIN,DO,IF,TRY,WHILE" },
                new String[]{ "ENDDO", "DO,WHILE" }
            };
        }

        private String[] getStartOfCodeKeywords()
        {
            // 
            return new String[]{
                "FUNCTION","PROCEDURE","CONSTRUCTOR","DESTRUCTOR","PROPERTY",
                "ACCESS","ASSIGN","METHOD","OPERATOR","GET","SET" };
        }

        private String[][] getMiddleKeywords()
        {
            // These are keywords that we have between other keywords
            //
            // "ELSE" is the keyword that will trigger the process
            // "IF" is the keyword to align to
            // ...
            return new String[][]
            {
                new String[]{ "ELSE","IF" },
                new String[]{ "ELSEIF", "IF" },
                new String[]{ "FINALLY", "TRY" },
                new String[]{ "CATCH", "TRY" },
                new String[]{ "RECOVER", "BEGIN" }
            };
        }

        private String[][] getSpecialMiddleKeywords()
        {
            // These are keywords that we have between other keywords
            // "CASE" is the keyword that will trigger the process
            // "DO,SWITCH,BEGIN" is the list of possible start keyword
            // ...
            return new String[][]
            {
                new String[]{ "CASE","DO,SWITCH,BEGIN" },
                new String[]{ "OTHERWISE", "DO,SWITCH,BEGIN" }
            };
        }
#endregion

        private void Options_OptionChanged(object sender, EditorOptionChangedEventArgs e)
        {
            getOptions();

        }

        public int? GetDesiredIndentation(ITextSnapshotLine line)
        {
            try
            {
                // Update Please... as the event doesn't seems to work
                getOptions();
                //
                if (_indentStyle != vsIndentStyle.vsIndentStyleSmart)
                    return null;
                // How many spaces do we need ?
                int indentValue = 0;
                string outdentToken;
                // On what line are we ?
                int lineNumber = line.LineNumber;
                if (lineNumber > 0)
                {
                    // We need to analyze the Previous line
                    lineNumber = lineNumber - 1;
                    ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber);
                    bool doSkipped;
                    string keyword = getFirstKeywordInLine(prevLine, out doSkipped, out indentValue);
                    //
                    if (indentValue > -1)
                        _lastIndentValue = indentValue;
                    // ok, now check what we have, starting the previous line
                    if (!String.IsNullOrEmpty(keyword) && !doSkipped)
                    {
                        // Start of a block of code ?
                        if (_codeBlockKeywords.Contains<String>(keyword))
                        {
                            if (!_alignMethod)
                            {
                                indentValue += _tabSize;
                            }
                        }
                        else if (_indentKeywords.Contains<String>(keyword))
                        {
                            //
                            indentValue += _tabSize;
                        }
                        else if ((outdentToken = this.searchSpecialOutdentKeyword(keyword)) != null)
                        {
                            if (this.hasRegions())
                            {
                                // We are aligning on the Open Token
                                indentValue = alignToOpenToken(prevLine);
                                if (indentValue < 0)
                                    indentValue = 0;
                            }
                            else
                            {
                                // Ok, let's try to make it smooth...
                                int? specialOutdentValue = null;
                                // The startToken is a list of possible tokens
                                specialOutdentValue = alignToSpecificTokens(line, outdentToken);
                                if (specialOutdentValue != null)
                                {
                                    indentValue = (int)specialOutdentValue;
                                }
                            }
                            // De-Indent previous line !!!
                            var buffer = this._textView.TextBuffer;
                            var editSession = buffer.CreateEdit();
                            try
                            {
                                XSharp.Project.CommandFilterHelper.FormatLine(this._aggregator, this._textView, editSession, prevLine, indentValue);
                            }
                            finally
                            {
                                editSession.Apply();
                            }
                        }
                        else
                        {
                            string startToken = this.searchMiddleKeyword(keyword);
                            int? specialIndentValue = null;
                            if (startToken != null)
                            {
                                // Retrieve the Indentation for the previous line
                                specialIndentValue = alignToSpecificTokens(line, startToken);
                            }
                            else
                            {
                                // We could have "special" middle keyword : CASE or OTHERWISE
                                startToken = this.searchSpecialMiddleKeyword(keyword);
                                if (startToken != null)
                                {
                                    // The startToken is a list of possible tokens
                                    specialIndentValue = alignToSpecificTokens(line, startToken);
                                    // The can be aligned to SWITCH/DO CASE or indented
                                    if (!_alignDoCase)
                                    {
                                        specialIndentValue += _tabSize;
                                    }
                                }
                            }
                            if (specialIndentValue != null)
                            {
                                // and Indent the new line
                                indentValue = (int)specialIndentValue + _tabSize;
                                // And apply
                                // De-Indent previous line !!!
                                var buffer = this._textView.TextBuffer;
                                var editSession = buffer.CreateEdit();
                                try
                                {
                                    XSharp.Project.CommandFilterHelper.FormatLine(this._aggregator, this._textView, editSession, prevLine, specialIndentValue);
                                }
                                finally
                                {
                                    editSession.Apply();
                                }
                            }
                        }
                        if (indentValue < 0)
                            indentValue = 0;
                        //
                        _lastIndentValue = indentValue;
                    }
                    //
                    return _lastIndentValue;
                }
            }
            catch (Exception ex)
            {
                XSharpProjectPackage.Instance.DisplayException(ex);
            }
            return _lastIndentValue;
        }

        private string searchMiddleKeyword(string keyword)
        {
            string startToken = null;
            for (int i = 0; i < this._middleKeywords.Length; i++)
            {
                var pair = this._middleKeywords[i];
                if (String.Compare(keyword, pair[0], true) == 0)
                {
                    startToken = pair[1];
                    break;
                }
            }
            return startToken;
        }

        private string searchSpecialMiddleKeyword(string keyword)
        {
            string startToken = null;
            for (int i = 0; i < this._specialKeywords.Length; i++)
            {
                var pair = this._specialKeywords[i];
                if (String.Compare(keyword, pair[0], true) == 0)
                {
                    startToken = pair[1];
                    break;
                }
            }
            return startToken;
        }

        private string searchSpecialOutdentKeyword(string keyword)
        {
            string startToken = null;
            for (int i = 0; i < this._specialOutdentKeywords.Length; i++)
            {
                var pair = this._specialOutdentKeywords[i];
                if (String.Compare(keyword, pair[0], true) == 0)
                {
                    startToken = pair[1];
                    break;
                }
            }
            return startToken;
        }

        private int? alignToSpecificToken_(ITextSnapshotLine currentLine, String token)
        {
            int indentValue = 0;
            bool found = false;
            try
            {
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
                        IMappingSpan currentSpan = tagList[0].Span;
                        /*
                        SnapshotPoint? snapPointFirst = currentSpan.Start.GetPoint(buffer, PositionAffinity.Predecessor);
                        // Extract the start of line
                        SnapshotSpan toIndent = new SnapshotSpan(line.Start, snapPointFirst.Value.Position - line.Start.Position);
                        String startOfLine = toIndent.GetText();
                        // Convert Tabs to Spaces
                        startOfLine = startOfLine.Replace("\t", new String(' ', _tabSize));
                        // So, at least, to align to previous line, we will need...
                        indentValue = startOfLine.Length;
                        */
                        String startOfLine = line.GetText();
                        startOfLine = startOfLine.Replace("\t", new String(' ', _tabSize));
                        // So, at least, to align to previous line, we will need...
                        indentValue = (startOfLine.Length - startOfLine.TrimStart(' ').Length);
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
                                if (keyword == token)
                                {
                                    found = true;
                                    break;
                                }
                            }
                        }
                        // 
                        indentValue = 0;
                    }
                }

            }
            finally
            {
                //
            }
            //
            if (found)
                return indentValue;
            else
                return null;

        }

        private int? alignToSpecificTokens(ITextSnapshotLine currentLine, String tokenList)
        {
            int indentValue = 0;
            bool found = false;
            Stack<String> context = new Stack<String>();
            //
            try
            {
                String[] possibleTokens = tokenList.Split(',');
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
                    String currentKeyword = "";
                    //
                    if (tagList.Count > 0)
                    {
                        var buffer = this._textView.TextBuffer;
                        IMappingSpan currentSpan = tagList[0].Span;
                        /*
                        SnapshotPoint? snapPointFirst = currentSpan.Start.GetPoint(buffer, PositionAffinity.Predecessor);
                        // Extract the start of line
                        SnapshotSpan toIndent = new SnapshotSpan(line.Start, snapPointFirst.Value.Position - line.Start.Position);
                        String startOfLine = toIndent.GetText();
                        // Convert Tabs to Spaces
                        startOfLine = startOfLine.Replace("\t", new String(' ', _tabSize));
                        // So, at least, to align to previous line, we will need...
                        indentValue = startOfLine.Length;
                        */
                        String startOfLine = line.GetText();
                        startOfLine = startOfLine.Replace("\t", new String(' ', _tabSize));
                        // So, at least, to align to previous line, we will need...
                        indentValue = (startOfLine.Length - startOfLine.TrimStart(' ').Length);
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
                                currentKeyword = kwSpan.GetText();
                                currentKeyword = currentKeyword.ToUpper();
                                if (possibleTokens.Contains<String>(currentKeyword))
                                {
                                    if (context.Count == 0)
                                    {
                                        found = true;
                                        break;
                                    }
                                    else
                                    {
                                        tokenList = context.Pop();
                                        possibleTokens = tokenList.Split(',');
                                    }
                                }
                                // Here we should also check for nested construct or we might get false positive...
                                string outdentToken;
                                if ((outdentToken = this.searchSpecialOutdentKeyword(currentKeyword)) != null)
                                {
                                    context.Push(tokenList);
                                    tokenList = outdentToken;
                                    possibleTokens = tokenList.Split(',');
                                }
                            }
                        }
                        // 
                        indentValue = 0;
                    }
                }
            }
            finally
            {
                //
            }
            //
            if (found)
                return indentValue;
            else
                return null;

        }

        private String getKeywordAt(List<IMappingTagSpan<IClassificationTag>> tagList, int tagIndex)
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


        private bool hasRegions()
        {
            bool result = false;
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
                System.Collections.Immutable.IImmutableList<Microsoft.VisualStudio.Text.Classification.ClassificationSpan> classifications = xsClassifier.GetRegionTags();
                result = (classifications.Count > 0);
            }
            return result;
        }

        private int alignToOpenToken(ITextSnapshotLine currentLine)
        {
            int indentValue = 0;
            try
            {
                int lineNumber = currentLine.LineNumber;
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
                                lineText = lineText.Replace("\t", new String(' ', _tabSize));
                                // 
                                indentValue = (lineText.Length - lineText.TrimStart().Length);
                                break;
                            }
                        }
                    }
                }
            }
            finally
            {

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
        private String getFirstKeywordInLine(ITextSnapshotLine line, out bool doSkipped, out int minIndent)
        {
            minIndent = -1;
            doSkipped = false;
            List<IMappingTagSpan<IClassificationTag>> tagList = GetTagsInLine(line);
            String keyword = "";
            //
            if (tagList.Count > 0)
            {
                var buffer = this._textView.TextBuffer;
                IMappingSpan currentSpan = tagList[0].Span;
                /////////////////////////////////////////// 
                //SnapshotPoint? snapPointFirst = currentSpan.Start.GetPoint(buffer, PositionAffinity.Predecessor);
                //// Extract the start of line
                //SnapshotSpan toIndent = new SnapshotSpan(line.Start, snapPointFirst.Value.Position - line.Start.Position);
                //String startOfLine = toIndent.GetText();
                //// Convert Tabs to Spaces
                //startOfLine = startOfLine.Replace("\t", new String(' ', _tabSize));
                //// So, at least, to align to previous line, we will need...
                //minIndent = startOfLine.Length;
                ////////////////////////////////////////////
                String startOfLine = line.GetText();
                startOfLine = startOfLine.Replace("\t", new String(' ', _tabSize));
                // So, at least, to align to previous line, we will need...
                minIndent = (startOfLine.Length - startOfLine.TrimStart(' ').Length);
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
#endif