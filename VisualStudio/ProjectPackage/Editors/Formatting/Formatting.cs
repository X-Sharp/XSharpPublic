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
using LanguageService.CodeAnalysis.XSharp;

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

        private XSharpParseOptions _parseoptions = null;
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
                "BEGIN","TRY","REPEAT","SWITCH",
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
                new String[]{ "END", "BEGIN,DO,IF,TRY,WHILE,GET,SET,PROPERTY,EVENT,ADD,REMOVE,SWITCH,CLASS,STRUCTURE,INTERFACE,ENUM" },
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
                "GET", "SET", "PROPERTY", "ADD", "REMOVE", "EVENT"
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
        }

        #endregion



#if SMARTINDENT

        private void FormatLine()
        {
            //
            getEditorPreferences(TextView);
            //
            SnapshotPoint caret = this.TextView.Caret.Position.BufferPosition;
            ITextSnapshotLine line = caret.GetContainingLine();
            // On what line are we ?
            bool alignOnPrev = false;
            int lineNumber = line.LineNumber;
            int indentation = -1;
            // we calculate the indent based on the previous line so we must be on the second line
            if (lineNumber > 0)
            {
                if (caret.Position < line.End.Position)
                {
                    alignOnPrev = true;
                }

                // wait until we can work
                while (_buffer.EditInProgress)
                {
                    System.Threading.Thread.Sleep(100);
                }
                var editSession = _buffer.CreateEdit();
                // This will calculate the desired indentation of the current line, based on the previous one
                // and may de-Indent the previous line if needed
                indentation = getDesiredIndentation(line, editSession, alignOnPrev);
                //
                try
                {
                    // but we may need to re-Format the previous line for Casing and Identifiers
                    // so, do it before indenting the current line.
                    lineNumber = lineNumber - 1;
                    ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber);
                    this.formatLineCase(editSession, prevLine);
                    CommandFilterHelper.FormatLineIndent(this.TextView, editSession, line, indentation);
                }
                finally
                {
                    if (editSession.HasEffectiveChanges)
                    {
                        editSession.Apply();
                    }
                    else
                    {
                        editSession.Cancel();
                    }
                }
            }
        }

        private void FormatDocument()
        {
            XSharpProjectPackage.Instance.DisplayOutPutMessage("CommandFilter.FormatDocument()");
            if (!_buffer.CheckEditAccess())
            {
                // can't edit !
                return;
            }
            // Read Settings
            getEditorPreferences(TextView);
            formatCaseForWholeBuffer();
            // Try to retrieve an already parsed list of Tags
            if (_classifier != null)
            {
#if TRACE
                //
                Stopwatch stopWatch = new Stopwatch();
                stopWatch.Start();
#endif
                //
                ITextSnapshot snapshot = _classifier.Snapshot;
                SnapshotSpan Span = new SnapshotSpan(snapshot, 0, snapshot.Length);
                var classifications = _classifier.GetRegionTags();
                // We cannot use SortedList, because we may have several Classification that start at the same position
                List<ClassificationSpan> sortedTags = new List<ClassificationSpan>();
                foreach (var tag in classifications)
                {
                    sortedTags.Add(tag);
                }
                sortedTags.Sort((a, b) => a.Span.Start.Position.CompareTo(b.Span.Start.Position));
                // Now that Tags are sorted, we can use a stack to arrange them by pairs
                Stack<Span> regionStarts = new Stack<Span>();
                List<Tuple<Span, Span>> regions = new List<Tuple<Span, Span>>();
                //
                foreach (var tag in sortedTags)
                {
                    if (tag.ClassificationType.IsOfType(ColorizerConstants.XSharpRegionStartFormat))
                    {
                        regionStarts.Push(tag.Span.Span);
                    }
                    else if (tag.ClassificationType.IsOfType(ColorizerConstants.XSharpRegionStopFormat))
                    {
                        if (regionStarts.Count > 0)
                        {
                            var start = regionStarts.Pop();
                            regions.Add(new Tuple<Span, Span>(start, tag.Span.Span));
                        }
                    }
                }
                // In order to try to speed up the formatting process, it would be good to have the regions sorted by their Start
                regions.Sort((a, b) => a.Item1.Start.CompareTo(b.Item1.Start));
                //Now, we have a list of Regions Start/Stop
                // wait until we can work
                while (_buffer.EditInProgress)
                {
                    System.Threading.Thread.Sleep(100);
                }
                var editSession = _buffer.CreateEdit();
                try
                {
                    var lines = _buffer.CurrentSnapshot.Lines;
                    int indentSize = 0;
                    bool inComment = false;
                    bool prevContinue = false;
                    int prevIndentSize = 0;
                    foreach (var snapLine in lines)
                    {
                        // Ignore Empty lines
                        if (snapLine.Length > 0)
                        {
                            SnapshotSpan sSpan = new SnapshotSpan(snapLine.Start, snapLine.End);
                            string lineText = sSpan.GetText();
                            lineText = lineText.Trim();
                            if (lineText.Length > 0)
                            {
                                char start = lineText.Substring(0, 1)[0];
                                char end = lineText.Substring(lineText.Length - 1, 1)[0];
                                //
                                if (prevContinue)
                                {
                                    indentSize = prevIndentSize;
                                }
                                else
                                {
                                    indentSize = getDesiredIndentationInDocument(snapLine, regions, out inComment);
                                }
                                if (!inComment && (end == ';'))
                                {
                                    // Keep the previous Indentation
                                    prevContinue = true;
                                    prevIndentSize = indentSize;
                                }
                                else
                                {
                                    prevContinue = false;
                                }
                            }
                        }
                        formatLineCase(editSession, snapLine);
                        CommandFilterHelper.FormatLineIndent(this.TextView, editSession, snapLine, indentSize);
                    }
                }
                finally
                {
                    if (editSession.HasEffectiveChanges)
                    {
                        editSession.Apply();
                    }
                    else
                    {
                        editSession.Cancel();
                    }
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
                XSharpProjectPackage.Instance.DisplayOutPutMessage("FormatDocument : Done in " + elapsedTime);
#endif
            }
        }

        /// <summary>
        /// Calculate the indentation in characters
        /// </summary>
        /// <param name="snapLine"></param>
        /// <param name="regions"></param>
        /// <param name="inComment"></param>
        /// <returns></returns>
        private int getDesiredIndentationInDocument(ITextSnapshotLine snapLine, List<Tuple<Span, Span>> regions, out bool inComment)
        {
            int indentValue = 0;
            int mlCmtSpaces = 0;
            String openKeyword = "";
            inComment = false;
            //
            //List<IMappingTagSpan<IClassificationTag>> tags = getTagsInLine(snapLine);
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
                openKeyword = getFirstKeywordInLine(snapLine, region.Item1.Start, length);
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
                        case "#REGION":
                            continue;
                        default:
                            break;
                    }
                    if (openKeyword == "//")
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
                        inComment = true;
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
                        case "#REGION":
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
                        if (!_codeBlockKeywords.Contains<String>(openKeyword))
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
            bool mayContinue = false;
            char[] newLine = Environment.NewLine.ToCharArray();
            do
            {
                car = snapshot[currrentPos];
                if (car == newLine[pos])
                {
                    if (pos == newLine.Length - 1)
                    {
                        if (!mayContinue)
                        {
                            found = true;
                            break;
                        }
                        pos = 0;
                    }
                    else
                        pos++;
                }
                else
                {
                    if (car == ';')
                        mayContinue = true;
                    else
                        mayContinue = false;
                    pos = 0;
                }
                //
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
        private IList<IToken> getTokensInLine(ITextSnapshotLine line)
        {
            var text = line.GetText();
            return getTokens(text);
        }

        private IList<IToken> getTokensInLine(ITextSnapshot snapshot, int start, int length)
        {
            SnapshotSpan lineSpan = new SnapshotSpan(snapshot, start, length);
            var text = lineSpan.GetText();
            return getTokens(text);
        }


        private String getFirstKeywordInLine(ITextSnapshotLine line, int start, int length)
        {
            String keyword = "";
            var tokens = getTokensInLine(line.Snapshot, start, length);
            bool inAttribute = false;
            //
            if (tokens.Count > 0)
            {
                int index = 0;
                while (index < tokens.Count)
                {
                    var token = tokens[index];
                    // skip whitespace tokens
                    if (token.Type == XSharpLexer.WS)
                    {
                        index++;
                        continue;
                    }
                    keyword = "";
                    if (XSharpLexer.IsKeyword(token.Type) || (token.Type >= XSharpLexer.PP_FIRST && token.Type <= XSharpLexer.PP_LAST ))
                    {
                        keyword = token.Text.ToUpper();
                        // it could be modifier...
                        if (keywordIsModifier(token.Type))
                        {
                            index++;
                            continue;
                        }
                        else
                        {
                            // keyword found
                            break;
                        }
                    }
                    else if (XSharpLexer.IsComment(token.Type))
                    {
                        keyword = token.Text;
                        if (keyword.Length >= 2)
                        {
                            keyword = keyword.Substring(0, 2);
                        }
                        break;
                    }
                    else if (XSharpLexer.IsOperator(token.Type))
                    {
                        keyword = token.Text;
                        if (token.Type == XSharpLexer.LBRKT)
                        {
                            inAttribute = true;
                            index++;
                            continue;
                        }
                        else if (token.Type == XSharpLexer.RBRKT)
                        {
                            inAttribute = false;
                            index++;
                            continue;
                        }
                    }
                    else
                    {
                        if (inAttribute)
                        {
                            // Skip All Content in
                            index++;
                            continue;
                        }

                    }
                    break;
                }
            }
            return keyword;
        }
#endif


        #region SmartIndent
        // This get reset when a modal dialog is opened in VS
        private static bool _optionsValid = false;

        // SmartIndent
        private static int _lastIndentValue;    // in number of characters
        private static int _tabSize;
        private static int _indentSize;
        private static bool _alignDoCase;
        private static bool _alignMethod;
        private static int _keywordCase;
        private static bool _identifierCase;
        private static bool _noGotoDefinition;
        private static vsIndentStyle _indentStyle;
        internal static int KeywordCase => _keywordCase;
        internal static bool IdentifierCase => _identifierCase;
        //private IEditorOptions _options;
        //
        private ITextBuffer _buffer;
        private ITagAggregator<IClassificationTag> _tagAggregator = null;

        internal static void InvalidateOptions()
        {
            _optionsValid = false;
        }

        private static void getEditorPreferences(ITextView textView)
        {
            if (!_optionsValid)
            {
                var package = XSharp.Project.XSharpProjectPackage.Instance;
                var optionsPage = package.GetIntellisenseOptionsPage();
                var textManager = package.GetTextManager();
                //
                _alignDoCase = optionsPage.AlignDoCase;
                _alignMethod = optionsPage.AlignMethod;
                _keywordCase = optionsPage.KeywordCase;
                _identifierCase = optionsPage.IdentifierCase;
                _noGotoDefinition = optionsPage.DisableGotoDefinition;
                var languagePreferences = new LANGPREFERENCES3[1];
                languagePreferences[0].guidLang = GuidStrings.guidLanguageService;
                var result = textManager.GetUserPreferences4(pViewPrefs: null, pLangPrefs: languagePreferences, pColorPrefs: null);
                if (result == VSConstants.S_OK)
                {
                    _indentStyle = languagePreferences[0].IndentStyle;
                }
                _tabSize = textView.Options.GetTabSize();
                _indentSize = textView.Options.GetIndentSize();
                _optionsValid = true;
            }
        }
        /// <summary>
        /// the indentation is measured in # of characters
        /// </summary>
        /// <param name="line"></param>
        /// <param name="editSession"></param>
        /// <param name="alignOnPrev"></param>
        /// <returns></returns>
        private int getDesiredIndentation(ITextSnapshotLine line, ITextEdit editSession, bool alignOnPrev)
        {
            try
            {
                //
                if (_indentStyle != vsIndentStyle.vsIndentStyleSmart)
                    return -1;
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
                    if (indentValue < 0)
                        indentValue = 0;
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
                                indentValue += _indentSize;
                            }
                        }
                        else if (_specialCodeBlockKeywords.Contains<String>(keyword))
                        {
                            if (!_alignMethod)
                            {
                                indentValue += _indentSize;
                            }
                        }
                        else if (_indentKeywords.Contains<String>(keyword))
                        {
                            indentValue += _indentSize;
                        }
                        else if ((outdentToken = searchSpecialOutdentKeyword(keyword)) != null)
                        {
                            // Ok, let's try to make it smooth...
                            int specialOutdentValue = -1;
                            // The startToken is a list of possible tokens
                            specialOutdentValue = alignToSpecificTokens(line, outdentToken);
                            if (specialOutdentValue >= 0)
                            {
                                indentValue = (int)specialOutdentValue;
                            }
                            // De-Indent previous line !!!
                            try
                            {
                                CommandFilterHelper.FormatLineIndent(this.TextView, editSession, prevLine, indentValue);
                            }
                            catch (Exception ex)
                            {
                                XSharpProjectPackage.Instance.DisplayOutPutMessage("Indentation of previous line failed" );
                                XSharpProjectPackage.Instance.DisplayException(ex);
                            }
                        }
                        else
                        {
                            string startToken = searchMiddleKeyword(keyword);
                            int specialIndentValue = -1;
                            if (startToken != null)
                            {
                                // Retrieve the Indentation for the previous line
                                specialIndentValue = alignToSpecificTokens(line, startToken);
                            }
                            else
                            {
                                if (doSkipped && keyword == "CASE" )
                                {
                                    if (!_alignDoCase)
                                    {
                                        indentValue += _indentSize;
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
                                            specialIndentValue += _indentSize;
                                        }
                                    }
                                }

                            }
                            if (specialIndentValue != -1)
                            {
                                try
                                {
                                    // De-Indent previous line !!!
                                    CommandFilterHelper.FormatLineIndent(this.TextView, editSession, prevLine, specialIndentValue);
                                    indentValue = specialIndentValue + _indentSize;
                                }
                                catch (Exception ex)
                                {
                                    XSharpProjectPackage.Instance.DisplayOutPutMessage("Error indenting of current line ") ;
                                    XSharpProjectPackage.Instance.DisplayException(ex);
                                }
                            }
                        }
                        if (indentValue < 0)
                        {
                            indentValue = 0;
                        }
                        _lastIndentValue = indentValue;
                    }
                    return _lastIndentValue;
                }
            }
            catch (Exception ex)
            {
                XSharpProjectPackage.Instance.DisplayOutPutMessage("SmartIndent.GetDesiredIndentation failed: " );
                XSharpProjectPackage.Instance.DisplayException(ex);
            }
            return _lastIndentValue;
        }

        private int alignToSpecificTokens(ITextSnapshotLine currentLine, String tokenList)
        {
            int indentValue = 0;
            bool found = false;
            Stack<String> context = new Stack<String>();
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
                    var tokens = getTokensInLine(line);
                    String currentKeyword = "";
                    //
                    if (tokens.Count > 0)
                    {
                        var token = tokens[0];
                        indentValue = 0;
                        int index = 0;
                        if (token.Type == XSharpLexer.WS)
                        {
                            indentValue = getIndentTokenLength(token);
                            index++;
                            token = tokens[index];
                        }
                        //
                        currentKeyword = token.Text.ToUpper();
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
                return -1;

        }

        private IList<IToken> getTokens(string text)
        {
            IList<IToken> tokens;
            try
            {
                var reporter = new ErrorIgnorer();
                ITokenStream tokenStream;
                bool ok = XSharp.Parser.VsParser.Lex(text, _file.SourcePath, _parseoptions, reporter, out tokenStream);
                var stream = tokenStream as BufferedTokenStream;
                tokens = stream.GetTokens();
            }
            catch (Exception e)
            {
                XSharpProjectPackage.Instance.DisplayException(e);
                tokens = new List<IToken>();
            }
            return tokens;
        }
        /// <summary>
        /// Returns the indent width in characters
        /// </summary>
        /// <param name="token"></param>
        /// <returns></returns>
        private int getIndentTokenLength(IToken token)
        {
            int len = 0;
            if (token.Type == XSharpLexer.WS)
            {
                var text = token.Text;
                bool space = false; // was last token a space
                foreach (var ch in text)
                {
                    switch (ch)
                    {
                        case ' ':
                            len += 1;
                            space = true;
                            break;
                        case '\t':
                            if (space)
                            {
                                // if already at tab position then increment with whole tab
                                // otherwise round up to next tab
                                var mod = len % _tabSize;
                                len = len - mod + _tabSize;
                                space = false;
                            }
                            else
                            {
                                len += _tabSize;
                            }
                            break;
                        default:
                            // the only other token that is allowed inside a WS is an old style pragma like ~"ONLYEARLY+"
                            // these do not influence the tab position.
                            break;
                    }
                }
                int rest = len % _indentSize;
                len = len / _indentSize;
                if (rest != 0)
                {
                    len+= 1;
                }
            }
            return len* _indentSize;
        }
        /// <summary>
        /// Get the first keyword in Line. The keyword is in UPPERCASE The modifiers (Private, Protected, ... ) are ignored
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
            string startOfLine = line.GetText();
            string keyword = "";
            int index = 0;
            var tokens = getTokens(startOfLine);
            if (tokens.Count > 0)
            {
                if (tokens[0].Type == XSharpLexer.WS)
                {
                    index = 1;
                    minIndent = getIndentTokenLength(tokens[0]);
                }
                else
                {
                    minIndent = 0;
                }
                while (tokens.Count > index)
                {
                    var token = tokens[index];
                    if (token.Type == XSharpLexer.WS)
                    {
                        index++;
                        continue;
                    }

                    if (XSharpLexer.IsKeyword(token.Type))
                    {
                        keyword = token.Text.ToUpper();
                        // it could be modifier...
                        if (keywordIsModifier(token.Type))
                        {
                            index++ ;
                            keyword = "";
                            continue;
                        }
                        if (token.Type == XSharpLexer.DO)
                        {
                            index++;
                            keyword = "";
                            doSkipped = true;
                            continue;
                        }
                    }
                    else if (XSharpLexer.IsComment(token.Type))
                    {
                        keyword = token.Text.Substring(0,2);
                    }
                    break;
                }
            }
            return keyword;
        }
        static bool keywordIsModifier(int type)
        {
            switch (type)
            {
                case XSharpLexer.PROTECTED:
                case XSharpLexer.INTERNAL:
                case XSharpLexer.HIDDEN:
                case XSharpLexer.PRIVATE:
                case XSharpLexer.EXPORT:
                case XSharpLexer.PUBLIC:
                case XSharpLexer.STATIC:
                case XSharpLexer.SEALED:
                case XSharpLexer.ABSTRACT:
                case XSharpLexer.VIRTUAL:
                case XSharpLexer.PARTIAL:
                case XSharpLexer.UNSAFE:
                case XSharpLexer.NEW:
                    return true;
                default:
                    return false;
            }
        }
        #endregion

    }
}
