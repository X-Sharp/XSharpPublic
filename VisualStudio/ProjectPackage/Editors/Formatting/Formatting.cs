using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Language.Intellisense;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Collections.Generic;
using System.Reflection;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Text.Editor.OptionsExtensionMethods;
using XSharpColorizer;
using XSharpModel;
using XSharpLanguage;
using System.Linq;
using System.Diagnostics;
using System;

namespace XSharp.Project
{
    partial class CommandFilter
    {

        #region Keywords Definitions
        private static String[] _indentKeywords;
        private static String[] _codeBlockKeywords;
        private static String[] _specialCodeBlockKeywords;
        private static String[][] _middleKeywords;
        private static String[][] _specialKeywords;
        private static String[][] _specialOutdentKeywords;

        private static void getKeywords()
        {
            if (_indentKeywords == null)
            {
                // Build list for Indent tokens
                _indentKeywords = getIndentKeywords();
                // Start of Method, Function, ...
                _codeBlockKeywords = getStartOfCodeKeywords();
                _specialCodeBlockKeywords = getSpecialStartOfCodeKeywords();
                // Middle Keywords : ELSE, ELSEIF, ...
                _middleKeywords = getMiddleKeywords();
                // Name is Self-explanatory
                _specialKeywords = getSpecialMiddleKeywords();
                // Build list for Outdent tokens
                _specialOutdentKeywords = getSpecialOutdentKeywords();
            }
        }


        private static String[] getIndentKeywords()
        {
            // "DO" is removed by getFirstKeywordInLine(), so it is useless here...
            return new String[]{
                "DO","FOR","FOREACH","WHILE","IF",
                "BEGIN","TRY","REPEAT",
                "INTERFACE","ENUM","CLASS","STRUCTURE","VOSTRUCT"};
        }

        private static String[][] getSpecialOutdentKeywords()
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
                new String[]{ "END", "BEGIN,DO,IF,TRY,WHILE,GET,SET,PROPERTY" },
                new String[]{ "ENDDO", "DO,WHILE" }
            };
        }

        private static String[] getStartOfCodeKeywords()
        {
            // 
            return new String[]{
                "FUNCTION","PROCEDURE",
                "CONSTRUCTOR","DESTRUCTOR",
                "ACCESS","ASSIGN",
                "METHOD","OPERATOR"
            };
        }

        // These are special Start of Code, because they have an END
        private static String[] getSpecialStartOfCodeKeywords()
        {
            // 
            return new String[]{
                "GET", "SET", "PROPERTY"
            };
        }

        private static String[][] getMiddleKeywords()
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

        private static String[][] getSpecialMiddleKeywords()
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
        private static string searchMiddleKeyword(string keyword)
        {
            string startToken = null;
            for (int i = 0; i < _middleKeywords.Length; i++)
            {
                var pair = _middleKeywords[i];
                if (String.Compare(keyword, pair[0], true) == 0)
                {
                    startToken = pair[1];
                    break;
                }
            }
            return startToken;
        }

        private static string searchSpecialMiddleKeyword(string keyword)
        {
            string startToken = null;
            for (int i = 0; i < _specialKeywords.Length; i++)
            {
                var pair = _specialKeywords[i];
                if (String.Compare(keyword, pair[0], true) == 0)
                {
                    startToken = pair[1];
                    break;
                }
            }
            return startToken;
        }

        private static string searchSpecialOutdentKeyword(string keyword)
        {
            string startToken = null;
            for (int i = 0; i < _specialOutdentKeywords.Length; i++)
            {
                var pair = _specialOutdentKeywords[i];
                if (String.Compare(keyword, pair[0], true) == 0)
                {
                    startToken = pair[1];
                    break;
                }
            }
            return startToken;
        }

        static CommandFilter()
        {
            getKeywords();
            getOptions();
        }

        #endregion



#if SMARTINDENT

        private void FormatLine(bool previous)
        {
            //
            getOptions();
            _buffer = this.TextView.TextBuffer;
            _tagAggregator = _aggregator.CreateTagAggregator<IClassificationTag>(_buffer);
            //
            SnapshotPoint caret = this.TextView.Caret.Position.BufferPosition;
            ITextSnapshotLine line = caret.GetContainingLine();
            // On what line are we ?
            bool alignOnPrev = false;
            int lineNumber = line.LineNumber;
            int? indentation = null;
            if ((lineNumber > 0) && previous)
            {
                //
                if (caret.Position < line.End.Position)
                {
                    alignOnPrev = true;
                }
                //
                var editSession = _buffer.CreateEdit();
                // This will calculate the desired indentation of the current line, based on the previous one
                // and may de-Indent the previous line if needed
                indentation = getDesiredIndentation(line, editSession, alignOnPrev);
                //
                try
                {
                    // but we may need to re-Format the previous line for Casing and Indentifiers
                    // so, do it before indenting the current line.
                    lineNumber = lineNumber - 1;
                    ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber);
                    CommandFilterHelper.FormatLineCase(this._aggregator, this.TextView, editSession, prevLine);
                    //
                    CommandFilterHelper.FormatLineIndent(this._aggregator, this.TextView, editSession, line, indentation);
                }
                finally
                {
                    editSession.Apply();
                }
            }
        }


        private void FormatDocument()
        {
            // Read Settings
            getOptions();
            _buffer = this.TextView.TextBuffer;
            _tagAggregator = _aggregator.CreateTagAggregator<IClassificationTag>(_buffer);

            // Try to retrieve an already parsed list of Tags
            XSharpClassifier xsClassifier = null;
            if (_buffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
            {
                xsClassifier = _buffer.Properties[typeof(XSharpClassifier)] as XSharpClassifier;
            }
            //
            if (xsClassifier != null)
            {
#if TRACE
                //
                Stopwatch stopWatch = new Stopwatch();
                stopWatch.Start();
#endif
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
                // Now that Tags are sorted, we can use a stack to arrange them by pairs
                Stack<Span> regionStarts = new Stack<Microsoft.VisualStudio.Text.Span>();
                List<Tuple<Span, Span>> regions = new List<Tuple<Microsoft.VisualStudio.Text.Span, Microsoft.VisualStudio.Text.Span>>();
                //
                foreach (var tag in sortedTags)
                {
                    //
                    if (tag.ClassificationType.IsOfType(XSharpColorizer.ColorizerConstants.XSharpRegionStartFormat))
                    {
                        //
                        regionStarts.Push(tag.Span.Span);
                    }
                    else if (tag.ClassificationType.IsOfType(XSharpColorizer.ColorizerConstants.XSharpRegionStopFormat))
                    {
                        if (regionStarts.Count > 0)
                        {
                            var start = regionStarts.Pop();
                            //
                            regions.Add(new Tuple<Span, Span>(start, tag.Span.Span));
                        }
                    }
                }
                // In order to try to speed up the formatting process, it would be good to have the regions sorted by their Start 
                regions.Sort((a, b) => a.Item1.Start.CompareTo(b.Item1.Start));
                //Now, we have a list of Regions Start/Stop
                var editSession = _buffer.CreateEdit();
                try
                {
                    var lines = _buffer.CurrentSnapshot.Lines;
                    int indentSize = 0;
                    foreach (var snapLine in lines)
                    {
                        //
                        indentSize = getDesiredIndentationInDocument(snapLine, regions, indentSize);
                        //
                        CommandFilterHelper.FormatLine(this._aggregator, this.TextView, editSession, snapLine, indentSize);
                    }
                    //
                }
                finally
                {
                    editSession.Apply();
                }
                //
#if TRACE
                stopWatch.Stop();
                // Get the elapsed time as a TimeSpan value.
                TimeSpan ts = stopWatch.Elapsed;

                // Format and display the TimeSpan value.
                string elapsedTime = String.Format("{0:00}h {1:00}m {2:00}.{3:00}s",
                    ts.Hours, ts.Minutes, ts.Seconds,
                    ts.Milliseconds / 10);
                //
                Trace.WriteLine("FormatDocument : Done in " + elapsedTime);
#endif
            }
        }


        private int getDesiredIndentationInDocument(ITextSnapshotLine snapLine, List<Tuple<Span, Span>> regions, int previousIndentSize)
        {
            int indentValue = 0;
            int mlCmtSpaces = 0;
            String openKeyword = "";
            //
            List<IMappingTagSpan<IClassificationTag>> tags = getTagsInLine(snapLine);
            // In Tuple Regions, the items are :
            // Item1 is Start
            // Item2 is End
            foreach (var region in regions)
            {
                // The line is before the current region, so skip
                if (snapLine.End.Position < region.Item1.Start)
                {
                    continue;
                }
                // The line is after the current region, so skip
                if (snapLine.Start.Position > region.Item2.Start)
                {
                    continue;
                }
                // 
                int length;
                length = getLineLength(snapLine.Snapshot, region.Item1.Start);
                if (length <= 0)
                    length = 1;
                // Get the opening keyword, at the beginning of the currently processed region
                openKeyword = getFirstKeywordInLine(snapLine.Snapshot, region.Item1.Start, length);
                //
                if ((snapLine.Start.Position <= region.Item1.Start) && (snapLine.End.Position >= region.Item1.Start))
                {
                    // We are on the line opening a Region
                    // What kind of region ? 
                    // Skip comment and using regions
                    switch (openKeyword)
                    {
                        case "//":
                        case "USING":
                        case "#USING":
                        case "DEFINE":
                        case "#DEFINE":
                        case "#INCLUDE":
                            continue;
                        default:
                            break;
                    }
                    if (openKeyword == "/*")
                    {
                        // Get the current indentation
                        SnapshotSpan sSpan = new SnapshotSpan(snapLine.Start, snapLine.End);
                        String lineText = sSpan.GetText();
                        lineText = lineText.Replace("\t", new String(' ', _tabSize));
                        mlCmtSpaces = (lineText.Length - lineText.TrimStart().Length);
                        // What is the difference with the start
                        length = region.Item1.End - region.Item1.Start + 1;
                        if (length <= 0)
                            length = 1;
                        sSpan = new SnapshotSpan(snapLine.Snapshot, region.Item1.Start, length);
                        lineText = sSpan.GetText();
                        lineText = lineText.Replace("\t", new String(' ', _tabSize));
                        mlCmtSpaces = mlCmtSpaces - (lineText.Length - lineText.TrimStart().Length);
                        //
                        continue;
                    }
                    // Move back keywords ( ELSE, ELSEIF, FINALLY, CATCH, RECOVER )
                    string startToken = searchMiddleKeyword(openKeyword);
                    if (startToken != null)
                    {
                        indentValue--;
                    }
                    // Some Users wants CASE/OTHERWISE to be aligned to the opening DO CASE
                    // Check for a setting
                    if (_alignDoCase)
                    {
                        // Move back keywords ( CASE, OTHERWISE )
                        startToken = searchSpecialMiddleKeyword(openKeyword);
                        if (startToken != null)
                        {
                            indentValue--;
                        }
                    }
                }
                else if ((snapLine.Start.Position > region.Item1.Start) && (snapLine.End.Position < region.Item2.Start))
                {
                    // We are inside a Region
                    // Comment or Using region ?
                    switch (openKeyword)
                    {
                        case "//":
                        case "USING":
                        case "#USING":
                        case "DEFINE":
                        case "#DEFINE":
                        case "#INCLUDE":
                            continue;
                        default:
                            break;
                    }
                    // We are between the opening Keyword and the closing Keyword
                    if (!_alignMethod)
                    {
                        indentValue++;
                    }
                    else
                    {
                        // no closing keyword
                        if (! _codeBlockKeywords.Contains<String>(openKeyword))
                        {
                            indentValue++;
                        }
                    }
                    
                    // Move back keywords ( ELSE, ELSEIF, FINALLY, CATCH, RECOVER )
                    string startToken = searchMiddleKeyword(openKeyword);
                    if (startToken != null)
                    {
                        indentValue--;
                    }
                }
                else //if ((region.Item2.Start >= snapLine.Start.Position) && (region.Item2.End <= snapLine.End.Position))
                {
                    // We are on the closing Keyword
                    if (!_alignMethod)
                    {
                        // no closing keyword
                        if (_codeBlockKeywords.Contains<String>(openKeyword))
                        {
                            indentValue++;
                        }
                    }
                    //
                    if (!_alignDoCase)
                    {
                        // Don't indent
                        // Move back keywords ( CASE, OTHERWISE )
                        string startToken = searchSpecialMiddleKeyword(openKeyword);
                        if (startToken != null)
                        {
                            indentValue++;
                        }
                    }
                }
                //}
                //}
            }
            //}
            // This should NOT happen
            if (indentValue < 0)
            {
                indentValue = 0;
            }
            //
            return (indentValue * _indentSize) + mlCmtSpaces;
        }

        private int getLineLength(ITextSnapshot snapshot, int start)
        {
            int length = 0;
            bool found = false;
            char car;
            int currrentPos = start;
            int pos = 0;
            char[] newLine = Environment.NewLine.ToCharArray();
            do
            {
                car = snapshot[currrentPos];
                if (car == newLine[pos])
                {
                    if (pos == newLine.Length - 1)
                    {
                        found = true;
                        break;
                    }
                    pos++;
                }
                else
                {
                    pos = 0;
                }
                currrentPos++;
                if (currrentPos >= snapshot.Length)
                {
                    break;
                }
            } while (!found);
            //
            if (found)
            {
                length = (currrentPos - start + 1) - newLine.Length;
            }
            return length;
        }


        /// <summary>
        /// Retrieve all Tags in the Line
        /// </summary>
        /// <param name="line"></param>
        /// <returns></returns>
        private List<IMappingTagSpan<IClassificationTag>> getTagsInLine(ITextSnapshotLine line)
        {
            //
            SnapshotSpan lineSpan = new SnapshotSpan(line.Start, line.Length);
            var tags = _tagAggregator.GetTags(lineSpan);
            List<IMappingTagSpan<IClassificationTag>> tagList = new List<IMappingTagSpan<IClassificationTag>>();
            foreach (var tag in tags)
            {
                tagList.Add(tag);
            }
            return tagList;
        }

        private List<IMappingTagSpan<IClassificationTag>> getTagsInLine(ITextSnapshot snapshot, int start, int length)
        {
            //
            SnapshotSpan lineSpan = new SnapshotSpan(snapshot, start, length);
            var tags = _tagAggregator.GetTags(lineSpan);
            List<IMappingTagSpan<IClassificationTag>> tagList = new List<IMappingTagSpan<IClassificationTag>>();
            foreach (var tag in tags)
            {
                tagList.Add(tag);
            }
            return tagList;
        }


        private String getFirstKeywordInLine(ITextSnapshot snapshot, int start, int length)
        {
            String keyword = "";
            List<IMappingTagSpan<IClassificationTag>> tagList = getTagsInLine(snapshot, start, length);
            //
            if (tagList.Count > 0)
            {
                int tagIndex = 0;
                while (tagIndex < tagList.Count)
                {
                    IClassificationTag currentTag = tagList[tagIndex].Tag;
                    IMappingSpan currentSpan = tagList[tagIndex].Span;
                    //
                    if (currentTag.ClassificationType.IsOfType("keyword") ||
                        currentTag.ClassificationType.IsOfType("preprocessor keyword"))
                    {
                        var spans = currentSpan.GetSpans(_buffer);
                        if (spans.Count > 0)
                        {
                            SnapshotSpan kwSpan = spans[0];
                            keyword = kwSpan.GetText();
                            keyword = keyword.ToUpper();
                            // it could be modifier...
                            if (keywordIsModifier(keyword))
                            {
                                tagIndex++;
                                keyword = "";
                                continue;
                            }
                        }
                    }
                    else if (currentTag.ClassificationType.IsOfType("comment"))
                    {
                        //
                        keyword = "//";
                        var spans = currentSpan.GetSpans(_buffer);
                        if (spans.Count > 0)
                        {
                            SnapshotSpan kwSpan = spans[0];
                            keyword = kwSpan.GetText();
                            if (keyword.Length >= 2)
                            {
                                keyword = keyword.Substring(0, 2);
                            }
                        }
                    }
                    // out please
                    break;
                };
            }
            return keyword;
        }
#endif


        #region SmartIndent
        // SmartIndent
        private static int _lastIndentValue;
        private static int _tabSize;
        private static int _indentSize;
        private static bool _alignDoCase;
        private static bool _alignMethod;
        private static vsIndentStyle _indentStyle;
        private static bool _optionsValid = false;
        //private IEditorOptions _options;
        //
        private ITextBuffer _buffer;
        private ITagAggregator<IClassificationTag> _tagAggregator;

        internal static void InvalidateOptions()
        {
            _optionsValid = false;
        }

        private static void getOptions()
        {
            if (!_optionsValid)
            {
                var package = XSharp.Project.XSharpProjectPackage.Instance;
                var optionsPage = package.GetIntellisenseOptionsPage();
                var textManager = package.GetTextManager();
                //
                _alignDoCase = optionsPage.AlignDoCase;
                _alignMethod = optionsPage.AlignMethod;
                var languagePreferences = new LANGPREFERENCES3[1];
                languagePreferences[0].guidLang = GuidStrings.guidLanguageService;
                var result = textManager.GetUserPreferences4(pViewPrefs: null, pLangPrefs: languagePreferences, pColorPrefs: null);
                if (result == VSConstants.S_OK)
                {
                    _indentStyle = languagePreferences[0].IndentStyle;
                    _tabSize = (int)languagePreferences[0].uTabSize;
                    _indentSize = (int)languagePreferences[0].uIndentSize;
                }
                _optionsValid = true;
            }
            //
        }


        private void Options_OptionChanged(object sender, EditorOptionChangedEventArgs e)
        {
            getOptions();

        }

        private int? getDesiredIndentation(ITextSnapshotLine line, ITextEdit editSession, bool alignOnPrev)
        {
            try
            {
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
                    //if (indentValue > -1)
                    _lastIndentValue = indentValue;
                    if (alignOnPrev)
                        return _lastIndentValue;
                    // ok, now check what we have, starting the previous line
                    if (!String.IsNullOrEmpty(keyword))// && !doSkipped)
                    {
                        // Start of a block of code ?
                        if (_codeBlockKeywords.Contains<String>(keyword))
                        {
                            if (!_alignMethod)
                            {
                                indentValue += _tabSize;
                            }
                        }
                        else if (_specialCodeBlockKeywords.Contains<String>(keyword))
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
                        else if ((outdentToken = searchSpecialOutdentKeyword(keyword)) != null)
                        {
                            //if (this.hasRegions())
                            //{
                            //    // We are aligning on the Open Token
                            //    indentValue = alignToOpenToken(prevLine);
                            //    if (indentValue < 0)
                            //        indentValue = 0;
                            //}
                            //else
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
                            try
                            {
                                XSharp.Project.CommandFilterHelper.FormatLineIndent(this._aggregator, this.TextView, editSession, prevLine, indentValue);
                            }
                            catch (Exception ex)
                            {
                                Trace.WriteLine("Indentation of line : " + ex.Message);
                            }
                        }
                        else
                        {
                            string startToken = searchMiddleKeyword(keyword);
                            int? specialIndentValue = null;
                            if (startToken != null)
                            {
                                // Retrieve the Indentation for the previous line
                                specialIndentValue = alignToSpecificTokens(line, startToken);
                            }
                            else
                            {
                                if (doSkipped && String.Equals(keyword, "CASE", StringComparison.InvariantCulture))
                                {
                                    if (!_alignDoCase)
                                    {
                                        indentValue += _tabSize;
                                    }
                                }
                                else
                                {
                                    // We could have "special" middle keyword : CASE or OTHERWISE
                                    startToken = searchSpecialMiddleKeyword(keyword);
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

                            }
                            if (specialIndentValue != null)
                            {
                                // and Indent the new line
                                indentValue = (int)specialIndentValue + _tabSize;
                                // And apply
                                // De-Indent previous line !!!
                                try
                                {
                                    XSharp.Project.CommandFilterHelper.FormatLineIndent(this._aggregator, this.TextView, editSession, prevLine, specialIndentValue);
                                }
                                catch (Exception ex)
                                {
                                    Trace.WriteLine("Error indenting of line : " + ex.Message);
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
                Trace.WriteLine("SmartIndent.GetDesiredIndentation Exception : " + ex.Message);
            }
            return _lastIndentValue;
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
                    List<IMappingTagSpan<IClassificationTag>> tagList = getTagsInLine(line);
                    String currentKeyword = "";
                    //
                    if (tagList.Count > 0)
                    {
                        IMappingSpan currentSpan = tagList[0].Span;
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
                            var spans = currentSpan.GetSpans(_buffer);
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
                                if ((outdentToken = searchSpecialOutdentKeyword(currentKeyword)) != null)
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
                IClassificationTag currentTag = tagList[tagIndex].Tag;
                IMappingSpan currentSpan = tagList[tagIndex].Span;
                //
                if (currentTag.ClassificationType.IsOfType("keyword"))
                {
                    var spans = currentSpan.GetSpans(_buffer);
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
            // Try to retrieve an already parsed list of Tags
            XSharpClassifier xsClassifier = null;
            if (_buffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
            {
                xsClassifier = _buffer.Properties[typeof(XSharpClassifier)] as XSharpClassifier;
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
                // Try to retrieve an already parsed list of Tags
                XSharpClassifier xsClassifier = null;
                if (_buffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
                {
                    xsClassifier = _buffer.Properties[typeof(XSharpClassifier)] as XSharpClassifier;
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
            List<IMappingTagSpan<IClassificationTag>> tagList = getTagsInLine(line);
            String keyword = "";
            //
            String startOfLine = line.GetText();
            startOfLine = startOfLine.Replace("\t", new String(' ', _tabSize));
            // So, at least, to align to previous line, we will need...
            minIndent = (startOfLine.Length - startOfLine.TrimStart(' ').Length);
            //
            if (tagList.Count > 0)
            {
                IMappingSpan currentSpan = tagList[0].Span;
                //
                int tagIndex = 0;
                while (tagIndex < tagList.Count)
                {
                    IClassificationTag currentTag = tagList[tagIndex].Tag;
                    currentSpan = tagList[tagIndex].Span;
                    //
                    if (currentTag.ClassificationType.IsOfType("keyword"))
                    {
                        var spans = currentSpan.GetSpans(_buffer);
                        if (spans.Count > 0)
                        {
                            SnapshotSpan kwSpan = spans[0];
                            keyword = kwSpan.GetText();
                            keyword = keyword.ToUpper();
                            // it could be modifier...
                            if (keywordIsModifier(keyword))
                            {
                                tagIndex++;
                                keyword = "";
                                continue;
                            }
                            if (keyword == "DO")
                            {
                                tagIndex++;
                                keyword = "";
                                doSkipped = true;
                                continue;
                            }
                        }
                    }
                    else if (currentTag.ClassificationType.IsOfType("comment"))
                    {
                        //
                        keyword = "//";
                        var spans = currentSpan.GetSpans(_buffer);
                        if (spans.Count > 0)
                        {
                            SnapshotSpan kwSpan = spans[0];
                            keyword = kwSpan.GetText();
                            if (keyword.Length >= 2)
                            {
                                keyword = keyword.Substring(0, 2);
                            }
                        }
                    }
                    // out please
                    break;
                };
            }
            return keyword;
        }
        static bool keywordIsModifier(string keyword)
        {
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
                    return true;
                default:
                    return false;
            }
        }


        #endregion

    }
}
