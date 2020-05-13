using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Collections.Generic;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Text.Editor.OptionsExtensionMethods;
using XSharpColorizer;
using XSharpLanguage;
using System.Linq;
using System;
using LanguageService.CodeAnalysis.XSharp;
using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{
    partial class CommandFilter
    {



        #region Keywords Definitions
        private static string[] _indentKeywords;
        private static string[] _codeBlockKeywords;
        private static string[] _specialCodeBlockKeywords;
        private static string[][] _middleKeywords;
        private static string[][] _specialKeywords;
        private static Dictionary<string, List<string>> _specialOutdentKeywords;
        //private static string[] _xtraKeywords;

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
                //
                //_xtraKeywords = getXtraKeywords();
            }
        }


        private static string[] getIndentKeywords()
        {
            // "DO" is removed by getFirstKeywordInLine(), so it is useless here...
            return new string[]{
                "DO","FOR","FOREACH","WHILE","IF",
                "BEGIN","TRY","REPEAT","SWITCH",
                "INTERFACE","ENUM","CLASS","STRUCTURE","VOSTRUCT","UNION",
                "#IFDEF" };
        }

        private static Dictionary<string, List<string>> getSpecialOutdentKeywords()
        {
            // These are keywords that trigger out-denting. Some keywords have multiple begin keywords
            // ...
            var result = new Dictionary<string, List<string>>(StringComparer.OrdinalIgnoreCase);
            result.Add("ENDIF", new List<string>() { "IF" });
            result.Add("ENDCASE", new List<string>() { "DO" });
            result.Add("UNTIL", new List<string>() { "REPEAT" });
            result.Add("NEXT", new List<string>() { "FOR", "FOREACH" });
            result.Add("END", new List<string>() { "BEGIN", "DO", "IF", "TRY", "WHILE", "GET", "SET", "PROPERTY", "EVENT", "ADD", "REMOVE", "SWITCH", "CLASS", "STRUCTURE", "INTERFACE", "ENUM", "FUNCTION", "PROCEDURE", "CONSTUCTOR", "DESTRUCTOR", "ACCESS", "ASSIGN", "METHOD", "OPERATOR" });
            result.Add("ENDDO", new List<string>() { "DO", "WHILE" });
            result.Add("#ENDIF", new List<string>() { "#IFDEF" });
            return result;
        }

        private static string[] getStartOfCodeKeywords()
        {
            // Entities where a closing keyword is optional
            return new string[]{
                "FUNCTION","PROCEDURE",
                "CONSTRUCTOR","DESTRUCTOR",
                "ACCESS","ASSIGN",
                "METHOD","OPERATOR"
            };
        }

        // These are special Start of Code, because they have an END
        private static string[] getSpecialStartOfCodeKeywords()
        {
            // Entities where a closing keyword is mandatory
            return new string[]{
                "GET", "SET", "PROPERTY", "ADD", "REMOVE", "EVENT"
            };
        }

        private static string[][] getMiddleKeywords()
        {
            // These are keywords that we have between other keywords
            //
            // "ELSE" is the keyword that will trigger the process
            // "IF" is the keyword to align to
            // ...
            return new string[][]
            {
                new string[]{ "ELSE","IF" },
                new string[]{ "ELSEIF", "IF" },
                new string[]{ "FINALLY", "TRY" },
                new string[]{ "CATCH", "TRY" },
                new string[]{ "RECOVER", "BEGIN" },
                new string[]{ "#ELSE","#IFDEF" }
            };
        }

        private static string[][] getSpecialMiddleKeywords()
        {
            // These are keywords that we have between other keywords
            // "CASE" is the keyword that will trigger the process
            // "DO,SWITCH,BEGIN" is the list of possible start keyword
            // ...
            return new string[][]
            {
                new string[]{ "CASE","DO,SWITCH,BEGIN" },
                new string[]{ "OTHERWISE", "DO,SWITCH,BEGIN" }
            };
        }

        //private static string[] getXtraKeywords()
        //{
        //    //
        //    return new string[]{
        //        "ENDFUNC", "ENDPROC", "ENDFOR", "ENDDEFINE"
        //    };
        //}

        private static string searchMiddleKeyword(string keyword)
        {
            string startToken = null;
            for (int i = 0; i < _middleKeywords.Length; i++)
            {
                var pair = _middleKeywords[i];
                if (string.Compare(keyword, pair[0], true) == 0)
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
                if (string.Compare(keyword, pair[0], true) == 0)
                {
                    startToken = pair[1];
                    break;
                }
            }
            return startToken;
        }

        private static List<string> searchSpecialOutdentKeyword(string keyword)
        {
            if (_specialOutdentKeywords.ContainsKey(keyword))
                return _specialOutdentKeywords[keyword];
            return null;
        }

        static CommandFilter()
        {
            getKeywords();
        }

        #endregion



#if SMARTINDENT

        private void copyWhiteSpaceFromPreviousLine(ITextEdit editSession, ITextSnapshotLine line)
        {
            // only copy the indentation from the previous line
            var text = line.GetText();
            if (text?.Length == 0)
            {
                if (line.LineNumber > 0)
                {
                    var prev = line.Snapshot.GetLineFromLineNumber(line.LineNumber - 1);
                    var prevText = prev.GetText();
                    int nWs = 0;
                    while (nWs < prevText.Length && Char.IsWhiteSpace(prevText[nWs]))
                    {
                        nWs++;
                    }
                    if (nWs <= prevText.Length)
                    {
                        prevText = prevText.Substring(0, nWs);
                        editSession.Replace(new Span(line.Start.Position, 0), prevText);
                    }
                }
            }
        }

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
                //
                try
                {
                    if (!canIndentLine(line))
                    {
                        copyWhiteSpaceFromPreviousLine(editSession, line);
                    }
                    else
                    {
                        switch (_indentStyle)
                        {
                            case vsIndentStyle.vsIndentStyleSmart:
                                indentation = getDesiredIndentation(line, editSession, alignOnPrev);
                                if (indentation == -1)
                                {
                                    copyWhiteSpaceFromPreviousLine(editSession, line);
                                }
                                else
                                {
                                    // but we may need to re-Format the previous line for Casing and Identifiers
                                    // so, do it before indenting the current line.
                                    lineNumber = lineNumber - 1;
                                    ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber);
                                    if (canFormatLine(prevLine))
                                    {
                                        this.formatLineCase(editSession, prevLine);
                                    }
                                    CommandFilterHelper.FormatLineIndent(this.TextView, editSession, line, indentation);
                                }
                                break;
                            case vsIndentStyle.vsIndentStyleDefault:
                            case vsIndentStyle.vsIndentStyleNone:
                                break;
                        }
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
            }
        }

        private void FormatDocument()
        {
            XSharpProjectPackage.Instance.DisplayOutPutMessage("CommandFilter.FormatDocument() -->>");
            if (!_buffer.CheckEditAccess())
            {
                // can't edit !
                return;
            }
            // Read Settings
            getEditorPreferences(TextView);

            // Try to retrieve an already parsed list of Tags
            if (_classifier != null)
            {
#if TRACE
                //
                System.Diagnostics.Stopwatch stopWatch = new System.Diagnostics.Stopwatch();
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
                Stack<Tuple<Span, int>> regionStarts = new Stack<Tuple<Span, int>>();
                List<Tuple<Span, Span, int, int>> regions = new List<Tuple<Span, Span, int, int>>();
                //
                foreach (var tag in sortedTags)
                {
                    if (tag.ClassificationType.IsOfType(ColorizerConstants.XSharpRegionStartFormat))
                    {
                        int startTokenType = -1;
                        if (tag is XsClassificationSpan)
                            startTokenType = (tag as XsClassificationSpan).startTokenType;
                        regionStarts.Push(new Tuple<Span, int>(tag.Span.Span, startTokenType));
                    }
                    else if (tag.ClassificationType.IsOfType(ColorizerConstants.XSharpRegionStopFormat))
                    {
                        if (regionStarts.Count > 0)
                        {
                            var start = regionStarts.Pop();
                            int endTokenType = -1;
                            if (tag is XsClassificationSpan)
                                endTokenType = (tag as XsClassificationSpan).endTokenType;
                            regions.Add(new Tuple<Span, Span, int, int>(start.Item1, tag.Span.Span, start.Item2, endTokenType));
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
                    int lineContinue = 0;
                    int prevIndentSize = 0;
                    int continueOffset = _indentSize * _indentFactor;
                    char prevstart = '\0';
                    foreach (var snapLine in lines)
                    {
                        bool lineAfterAttributes = false;
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
                                if (lineContinue == 1)
                                {
                                    if (prevstart != '[')
                                    {
                                        indentSize = prevIndentSize + continueOffset;
                                    }
                                    else
                                    {
                                        lineAfterAttributes = true;
                                    }

                                }
                                else if (lineContinue > 1)
                                {
                                    indentSize = prevIndentSize;
                                }
                                else
                                {
                                    indentSize = getDesiredIndentationInDocument(snapLine, regions, out inComment);
                                }
                                prevstart = start;
                                // Not in comment, Multiple line but not Attribute
                                if (!inComment && (end == ';'))
                                {
                                    if (lineContinue == 0)
                                    {
                                        // Keep the previous Indentation
                                        lineContinue = 1;
                                        prevIndentSize = indentSize;
                                    }
                                    else if (lineContinue == 1)
                                    {
                                        if (!lineAfterAttributes)
                                        {
                                            lineContinue = 2;
                                            prevIndentSize = indentSize;
                                        }
                                    }
                                }
                                else
                                {
                                    lineContinue = 0;
                                }
                            }
                        }
                        if (canFormatLine(snapLine))
                        {
                            formatLineCase(editSession, snapLine);
                            CommandFilterHelper.FormatLineIndent(this.TextView, editSession, snapLine, indentSize);
                        }
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
                string elapsedTime = string.Format("{0:00}h {1:00}m {2:00}.{3:00}s",
                    ts.Hours, ts.Minutes, ts.Seconds,
                    ts.Milliseconds / 10);
                //
                XSharpProjectPackage.Instance.DisplayOutPutMessage("FormatDocument : Done in " + elapsedTime);
#endif
            }
            else
                formatCaseForWholeBuffer();
            //
            XSharpProjectPackage.Instance.DisplayOutPutMessage("CommandFilter.FormatDocument() <<--");
        }

        /// <summary>
        /// Calculate the indentation in characters
        /// </summary>
        /// <param name="snapLine"></param>
        /// <param name="regions"></param>
        /// <param name="inComment"></param>
        /// <returns></returns>
        private int getDesiredIndentationInDocument(ITextSnapshotLine snapLine, List<Tuple<Span, Span, int, int>> regions, out bool inComment)
        {
            int indentValue = 0;
            int mlCmtSpaces = 0;
            string openKeyword = "";
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
                int startTokenType = region.Item3;
                int endTokenType = region.Item4;
                if (startTokenType == -1)
                    openKeyword = getFirstKeywordInLine(snapLine, region.Item1.Start, length);
                //
                if ((snapLine.Start.Position <= region.Item1.Start) && (snapLine.End.Position >= region.Item1.Start))
                {
                    // We are on the line opening a Region
                    // What kind of region ?
                    // Skip comment and using regions
                    if (startTokenType == -1)
                    {
                        switch (openKeyword)
                        {
                            case "//":
                            case "USING":
                            case "#USING":
                            case "#DEFINE":
                            case "#INCLUDE":
                            case "#REGION":
                                continue;
                            case "DEFINE":
                                // Warning !! It could DEFINE CLASS in FOXPRO
                                openKeyword = getKeywordInLine(snapLine, region.Item1.Start, length, 2);
                                if (string.Compare(openKeyword, "class", true) != 0)
                                    continue;
                                break;
                            default:
                                break;
                        }
                        /*
                        if (openKeyword == "//")
                        {
                            // Get the current indentation
                            SnapshotSpan sSpan = new SnapshotSpan(snapLine.Start, snapLine.End);
                            string lineText = sSpan.GetText();
                            lineText = lineText.Replace("\t", new string(' ', _tabSize));
                            mlCmtSpaces = (lineText.Length - lineText.TrimStart().Length);
                            // What is the difference with the start
                            length = region.Item1.End - region.Item1.Start + 1;
                            if (length <= 0)
                                length = 1;
                            sSpan = new SnapshotSpan(snapLine.Snapshot, region.Item1.Start, length);
                            lineText = sSpan.GetText();
                            lineText = lineText.Replace("\t", new string(' ', _tabSize));
                            mlCmtSpaces = mlCmtSpaces - (lineText.Length - lineText.TrimStart().Length);
                            //
                            inComment = true;
                            continue;
                        }
                        */
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
                    else
                    {
                        switch (startTokenType)
                        {
                            case XSharpLexer.SL_COMMENT:
                            case XSharpLexer.USING:
                            case XSharpLexer.PP_INCLUDE:
                            case XSharpLexer.PP_DEFINE:
                            case XSharpLexer.PP_REGION:
                                continue;
                            case XSharpLexer.DEFINE:
                                // Warning !! It could DEFINE CLASS in FOXPRO
                                openKeyword = getKeywordInLine(snapLine, region.Item1.Start, length, 2);
                                if (string.Compare(openKeyword, "class", true) != 0)
                                    continue;
                                break;
                            default:
                                break;
                        }
                        // Move back keywords ( ELSE, ELSEIF, FINALLY, CATCH, RECOVER )
                        switch (startTokenType)
                        {
                            case XSharpLexer.ELSE:
                            case XSharpLexer.ELSEIF:
                            case XSharpLexer.FINALLY:
                            case XSharpLexer.CATCH:
                            case XSharpLexer.RECOVER:
                            case XSharpLexer.PP_ELSE:
                                indentValue--;
                                break;
                        }
                        // Some Users wants CASE/OTHERWISE to be aligned to the opening DO CASE
                        // Check for a setting
                        if (_alignDoCase)
                        {
                            // Move back keywords ( CASE, OTHERWISE )
                            switch (startTokenType)
                            {
                                case XSharpLexer.CASE:
                                case XSharpLexer.OTHERWISE:
                                    indentValue--;
                                    break;
                            }
                        }
                    }
                }
                else if ((snapLine.Start.Position > region.Item1.Start) && (snapLine.End.Position < region.Item2.Start))
                {
                    // We are inside a Region
                    // Comment or Using region ?
                    if (startTokenType == -1)
                    {
                        switch (openKeyword)
                        {
                            case "//":
                            case "USING":
                            case "#USING":
                            case "#DEFINE":
                            case "#INCLUDE":
                            case "#REGION":
                                continue;
                            case "DEFINE":
                                // Warning !! It could DEFINE CLASS in FOXPRO
                                openKeyword = getKeywordInLine(snapLine, region.Item1.Start, length, 2);
                                if (string.Compare(openKeyword, "class", true) != 0)
                                    continue;
                                break;
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
                            if (!_codeBlockKeywords.Contains(openKeyword))
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
                        //
                    }
                    else
                    {
                        switch (startTokenType)
                        {
                            case XSharpLexer.SL_COMMENT:
                            case XSharpLexer.USING:
                            case XSharpLexer.PP_INCLUDE:
                            case XSharpLexer.PP_DEFINE:
                            case XSharpLexer.PP_REGION:
                                continue;
                            case XSharpLexer.DEFINE:
                                // Warning !! It could DEFINE CLASS in FOXPRO
                                openKeyword = getKeywordInLine(snapLine, region.Item1.Start, length, 2);
                                if (string.Compare(openKeyword, "class", true) != 0)
                                    continue;
                                break;
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
                            switch (startTokenType)
                            {
                                case XSharpLexer.FUNCTION:
                                case XSharpLexer.PROCEDURE:
                                case XSharpLexer.CONSTRUCTOR:
                                case XSharpLexer.DESTRUCTOR:
                                case XSharpLexer.ASSIGN:
                                case XSharpLexer.ACCESS:
                                case XSharpLexer.METHOD:
                                case XSharpLexer.OPERATOR:
                                    break;
                                default:
                                    indentValue++;
                                    break;
                            }
                        }
                        // Move back keywords ( ELSE, ELSEIF, FINALLY, CATCH, RECOVER )
                        switch (startTokenType)
                        {
                            case XSharpLexer.ELSE:
                            case XSharpLexer.ELSEIF:
                            case XSharpLexer.FINALLY:
                            case XSharpLexer.CATCH:
                            case XSharpLexer.RECOVER:
                            case XSharpLexer.PP_ELSE:
                                indentValue--;
                                break;
                        }
                    }
                }
                else //if ((region.Item2.Start >= snapLine.Start.Position) && (region.Item2.End <= snapLine.End.Position))
                {
                    // We are on the closing Keyword
                    if (!_alignMethod)
                    {
                        if (startTokenType == -1)
                        {
                            // normally, no closing keyword
                            if (_codeBlockKeywords.Contains(openKeyword))
                            {
                                // per Default
                                indentValue++;
                                // Ok, CodeBlock, we can have an optionnal END as the last statement
                                int currentLength;
                                currentLength = getLineLength(snapLine.Snapshot, snapLine.Start.Position);
                                if (currentLength <= 0)
                                    currentLength = 1;
                                // Get the opening keyword, at the beginning of the currently processed region
                                string insideKeyword = getFirstKeywordInLine(snapLine, snapLine.Start.Position, currentLength);
                                //if (Array.Find(_xtraKeywords, kw => string.Compare(kw, insideKeyword, true) == 0) != null)
                                //{
                                //    indentValue--;
                                //}
                                //else
                                if (string.Compare(insideKeyword, "end", true) == 0)
                                {
                                    // We may have an optionnal closing keyword indication
                                    insideKeyword = getKeywordInLine(snapLine, snapLine.Start.Position, currentLength, 2);
                                    if ((string.Compare(openKeyword, insideKeyword, true) == 0)) //|| (string.Compare(openKeyword, "class", true) == 0) )
                                    {
                                        indentValue--;
                                    }
                                }
                            }
                        }
                        else
                        {
                            switch (startTokenType)
                            {
                                case XSharpLexer.FUNCTION:
                                case XSharpLexer.PROCEDURE:
                                case XSharpLexer.CONSTRUCTOR:
                                case XSharpLexer.DESTRUCTOR:
                                case XSharpLexer.ASSIGN:
                                case XSharpLexer.ACCESS:
                                case XSharpLexer.METHOD:
                                case XSharpLexer.OPERATOR:
                                    // per Default
                                    indentValue++;
                                    //
                                    switch (endTokenType)
                                    {
                                        case XSharpLexer.END:
                                            // We may have an optionnal closing keyword indication
                                            indentValue--;
                                            break;
                                    }
                                    break;
                            }
                        }
                    }
                    //
                    if (!_alignDoCase)
                    {
                        // Don't indent
                        // Move back keywords ( CASE, OTHERWISE )
                        if (startTokenType == -1)
                        {
                            string startToken = searchSpecialMiddleKeyword(openKeyword);
                            if (startToken != null)
                            {
                                indentValue++;
                            }
                        }
                        else
                        {
                            switch (startTokenType)
                            {
                                case XSharpLexer.CASE:
                                case XSharpLexer.OTHERWISE:
                                    indentValue++;
                                    break;
                            }
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
            IList<IToken> tokens = new List<IToken>();
            // Already been lexed ?
            if (_buffer.Properties != null && _buffer.Properties.ContainsProperty(typeof(XSharpTokens)))
            {
                XSharpTokens xTokens = null;
                xTokens = _buffer.Properties.GetProperty<XSharpTokens>(typeof(XSharpTokens));
                if (!(xTokens == null || xTokens.TokenStream == null || xTokens.SnapShot == null))
                {
                    var allTokens = xTokens.TokenStream.GetTokens();
                    if (allTokens != null)
                    {
                        if (xTokens.SnapShot.Version == _buffer.CurrentSnapshot.Version)
                        {
                            // Ok, use it
                            int startIndex = -1;
                            // Move to the line position
                            for (int i = 0; i < allTokens.Count; i++)
                            {
                                if (allTokens[i].StartIndex >= line.Start.Position)
                                {
                                    startIndex = i;
                                    break;
                                }
                            }
                            if ( startIndex > -1 )
                            {
                                // Move to the end of line
                                int currentLine = allTokens[startIndex].Line;
                                do
                                {
                                    tokens.Add(allTokens[startIndex]);
                                    startIndex++;

                                } while ((startIndex < allTokens.Count) && (currentLine == allTokens[startIndex].Line));
                                return tokens;
                            }
                        }
                    }
                }
            }
            // Ok, do it now
            var text = line.GetText();
            tokens = getTokens(text);
            return tokens;
            //
        }

        private IList<IToken> getTokensInLine(ITextSnapshot snapshot, int start, int length)
        {
            IList<IToken> tokens = new List<IToken>();
            // Already been lexed ?
            if (_buffer.Properties != null && _buffer.Properties.ContainsProperty(typeof(XSharpTokens)))
            {
                XSharpTokens xTokens = null;
                xTokens = _buffer.Properties.GetProperty<XSharpTokens>(typeof(XSharpTokens));
                if (!(xTokens == null || xTokens.TokenStream == null || xTokens.SnapShot == null))
                {
                    var allTokens = xTokens.TokenStream.GetTokens();
                    if (allTokens != null)
                    {
                        if (xTokens.SnapShot.Version == _buffer.CurrentSnapshot.Version)
                        {
                            // Ok, use it
                            int startIndex = -1;
                            // Move to the line position
                            for (int i = 0; i < allTokens.Count; i++)
                            {
                                if (allTokens[i].StartIndex >= start)
                                {
                                    startIndex = i;
                                    break;
                                }
                            }
                            if (startIndex > -1)
                            {
                                // Move to the end of span
                                int lastPosition = start + length;
                                do
                                {
                                    tokens.Add(allTokens[startIndex]);
                                    startIndex++;

                                } while ((startIndex < allTokens.Count) && (allTokens[startIndex].StopIndex < lastPosition ));
                                return tokens;
                            }
                        }
                    }
                }
            }
            //
            SnapshotSpan lineSpan = new SnapshotSpan(snapshot, start, length);
            var text = lineSpan.GetText();
            tokens = getTokens(text);
            return tokens;
        }


        private string getFirstKeywordInLine(ITextSnapshotLine line, int start, int length)
        {
            string keyword = "";
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
                    if (XSharpLexer.IsKeyword(token.Type) || (token.Type >= XSharpLexer.PP_FIRST && token.Type <= XSharpLexer.PP_LAST))
                    //|| (Array.Find(_xtraKeywords, kw => string.Compare(kw, token.Text, true) == 0) != null) )
                    {
                        keyword = token.Text.ToUpper();
                        // it could be modifier...
                        if (XSharpLexer.IsModifier(token.Type))
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

        private string getKeywordInLine(ITextSnapshotLine line, int start, int length, int keywordPosition)
        {
            int keywordPos = 0;
            string keyword = "";
            var tokens = getTokensInLine(line.Snapshot, start, length);
            bool inAttribute = false;
            //
            if (tokens.Count > 0)
            {
                int index = 0;
                do
                {
                    keywordPos++;
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
                        if (XSharpLexer.IsKeyword(token.Type) || (token.Type >= XSharpLexer.PP_FIRST && token.Type <= XSharpLexer.PP_LAST))
                        //|| (Array.Find(_xtraKeywords, kw => string.Compare(kw, token.Text, true) == 0) != null))
                        {
                            keyword = token.Text.ToUpper();
                            // it could be modifier...
                            if (XSharpLexer.IsModifier(token.Type))
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
                    //
                    if (keywordPos < keywordPosition)
                    {
                        keyword = "";
                        index++;
                    }
                } while (keywordPos < keywordPosition);
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
        private static int _indentFactor;
        private static bool _alignDoCase;
        private static bool _alignMethod;
        private static KeywordCase _keywordCase;
        private static bool _identifierCase;
        private static bool _noGotoDefinition;
        private static vsIndentStyle _indentStyle;
        internal static KeywordCase KeywordCase => _keywordCase;
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
                int result = 0;
                UIThread.DoOnUIThread( () => result = textManager.GetUserPreferences4(pViewPrefs: null, pLangPrefs: languagePreferences, pColorPrefs: null));
                if (result == VSConstants.S_OK)
                {
                    _indentStyle = languagePreferences[0].IndentStyle;
                    optionsPage.HideAdvancemembers = languagePreferences[0].fHideAdvancedAutoListMembers != 0;
                }
                _tabSize = textView.Options.GetTabSize();
                _indentSize = textView.Options.GetIndentSize();
                _indentFactor = optionsPage.MultiFactor;
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
            XSharpProjectPackage.Instance.DisplayOutPutMessage($"CommandFilter.getDesiredIndentation({line.LineNumber + 1})");
            try
            {
                //
                //if (_indentStyle != vsIndentStyle.vsIndentStyleSmart)
                //    return -1;
                // How many spaces do we need ?
                int indentValue = 0;
                List<string> outdentTokens;
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
                    if (!string.IsNullOrEmpty(keyword))// && !doSkipped)
                    {
                        // Start of a block of code ?
                        if (_codeBlockKeywords.Contains(keyword))
                        {
                            if (!_alignMethod)
                            {
                                indentValue += _indentSize;
                            }
                        }
                        else if (_specialCodeBlockKeywords.Contains(keyword))
                        {
                            if (!_alignMethod)
                            {
                                indentValue += _indentSize;
                            }
                        }
                        else if (_indentKeywords.Contains(keyword))
                        {
                            indentValue += _indentSize;
                        }
                        else if ((outdentTokens = searchSpecialOutdentKeyword(keyword)) != null)
                        {
                            // Ok, let's try to make it smooth...
                            int specialOutdentValue = -1;
                            // The startToken is a list of possible tokens
                            specialOutdentValue = alignToSpecificTokens(line, outdentTokens);
                            if (specialOutdentValue >= 0)
                            {
                                indentValue = (int)specialOutdentValue;
                            }
                            // De-Indent previous line !!!
                            if (canFormatLine(prevLine))
                            {
                                try
                                {
                                    CommandFilterHelper.FormatLineIndent(this.TextView, editSession, prevLine, indentValue);
                                }
                                catch (Exception ex)
                                {
                                    XSharpProjectPackage.Instance.DisplayOutPutMessage("Indentation of previous line failed");
                                    XSharpProjectPackage.Instance.DisplayException(ex);
                                }
                            }
                        }
                        else
                        {
                            string startToken = searchMiddleKeyword(keyword);
                            int specialIndentValue = -1;
                            if (startToken != null)
                            {
                                // Retrieve the Indentation for the previous line
                                specialIndentValue = alignToSpecificTokens(line, new List<string> { startToken });
                            }
                            else
                            {
                                if (doSkipped && keyword == "CASE")
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
                                        specialIndentValue = alignToSpecificTokens(line, new List<string>( startToken.Split( new char[]{',' } ) ) );
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
                                    if (canIndentLine(prevLine))
                                    {
                                        CommandFilterHelper.FormatLineIndent(this.TextView, editSession, prevLine, specialIndentValue);
                                    }
                                    indentValue = specialIndentValue + _indentSize;
                                }
                                catch (Exception ex)
                                {
                                    XSharpProjectPackage.Instance.DisplayOutPutMessage("Error indenting of current line ");
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
                XSharpProjectPackage.Instance.DisplayOutPutMessage("SmartIndent.GetDesiredIndentation failed: ");
                XSharpProjectPackage.Instance.DisplayException(ex);
            }
            return _lastIndentValue;
        }

        private int alignToSpecificTokens(ITextSnapshotLine currentLine, List<string> tokenList)
        {
            int indentValue = 0;
            bool found = false;
            var context = new Stack<List<string>>();
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
                    var tokens = getTokensInLine(line);
                    string currentKeyword = "";
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
                        if (tokenList.Contains(currentKeyword))
                        {
                            if (context.Count == 0)
                            {
                                found = true;
                                break;
                            }
                            else
                            {
                                tokenList = context.Pop();
                            }
                        }
                        // Here we should also check for nested construct or we might get false positive...
                        List<string> outdentTokens;
                        if ((outdentTokens = searchSpecialOutdentKeyword(currentKeyword)) != null)
                        {
                            context.Push(tokenList);
                            tokenList = outdentTokens;
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

        public XSharpParseOptions ParseOptions
        {
            get
            {
                XSharpParseOptions parseoptions;
                if (_file != null)
                {
                    parseoptions = _file.Project.ParseOptions;
                }
                else
                {
                    parseoptions = XSharpParseOptions.Default;
                }
                return parseoptions;
            }
        }

        private IList<IToken> getTokens(string text)
        {
            IList<IToken> tokens;
            try
            {
                string fileName;
                XSharpParseOptions parseoptions = ParseOptions;
                if (_file != null)
                {
                    fileName = _file.FullPath;
                }
                else
                {
                    fileName = "MissingFile.prg";
                }
                ITokenStream tokenStream;
                var reporter = new ErrorIgnorer();
                bool ok = XSharp.Parser.VsParser.Lex(text, fileName, parseoptions, reporter, out tokenStream);
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
        private static int getIndentTokenLength(IToken token)
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
                    len += 1;
                }
            }
            return len * _indentSize;
        }
        /// <summary>
        /// Get the first keyword in Line. The keyword is in UPPERCASE The modifiers (Private, Protected, ... ) are ignored
        /// If the first Keyword is a Comment, "//" is returned
        /// </summary>
        /// <param name="line">The line to analyze</param>
        /// <param name="doSkipped">Bool value indicating if a "DO" keyword has been skipped</param>
        /// <param name="minIndent"></param>
        /// <returns></returns>
        private string getFirstKeywordInLine(ITextSnapshotLine line, out bool doSkipped, out int minIndent)
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
                        // it could be modifier...
                        if (XSharpLexer.IsModifier(token.Type))
                        {
                            index++;
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
                        keyword = token.Text.ToUpper();
                    }
                    else if (XSharpLexer.IsComment(token.Type))
                    {
                        keyword = token.Text.Substring(0, 2);
                    }
                    break;
                }
            }
            return keyword;
        }
        #endregion

        #region New Formatting process
        class FormattingContext
        {
            IList<IToken> allTokens;
            int currentIndex;
            int prevIndex;
            int currentPosition;
            public XSharpDialect Dialect { get; private set; }

            public int CurrentIndex
            {
                get
                {
                    return currentIndex;
                }

                set
                {
                    currentIndex = value;
                    if (currentIndex < allTokens.Count && currentIndex >= 0)
                        currentPosition = allTokens[currentIndex].StartIndex;
                    else
                        currentPosition = -1;
                }
            }

            public int CurrentPosition
            {
                get
                {
                    return currentPosition;
                }

            }

            internal FormattingContext(CommandFilter cf, ITextSnapshot snapshot)
            {
                allTokens = cf.getTokensInLine(snapshot, 0, snapshot.Length);
                if (allTokens.Count > 0)
                {
                    currentIndex = 0;
                    prevIndex = 0;
                    currentPosition = allTokens[0].StartIndex;
                }
                else
                {
                    currentIndex = -1;
                    prevIndex = -1;
                    currentPosition = -1;
                }
                //
                Dialect = cf.ParseOptions.Dialect;
            }

            internal FormattingContext(IList<IToken> tokens, XSharpDialect dialect)
            {
                allTokens = tokens;
                if (allTokens.Count > 0)
                {
                    currentIndex = 0;
                    prevIndex = 0;
                    currentPosition = allTokens[0].StartIndex;
                }
                else
                {
                    currentIndex = -1;
                    prevIndex = -1;
                    currentPosition = -1;
                }
                //
                Dialect = dialect;
            }

            public void MoveTo(int positionToReach)
            {
                this.prevIndex = this.currentIndex;
                if (positionToReach == currentPosition)
                    return;
                //
                int newPos = binarySearch(0, allTokens.Count - 1, positionToReach);
                if ( newPos > -1 )
                {
                    currentIndex = newPos;
                    currentPosition = allTokens[newPos].StartIndex;
                            
                }
                else
                {
                    currentIndex = -1;
                    currentPosition = -1;    
                }
            }

            public void MoveToNext()
            {
                this.prevIndex = this.currentIndex;
                this.CurrentIndex++;
            }

            public void MoveBack()
            {
                this.currentIndex = this.prevIndex;
            }


            public IToken GetFirstToken(bool ignoreSpaces)
            {
                IToken first = null;
                if (currentPosition > -1)
                {
                    int currentLine = allTokens[currentIndex].Line;
                    int start = currentIndex;
                    while (start < allTokens.Count)
                    {
                        IToken token = allTokens[start];
                        if (token.Line != currentLine)
                        {
                            break;
                        }
                        // skip whitespace tokens
                        if (((token.Type == XSharpLexer.WS) && ignoreSpaces) ||
                            (token.Type == XSharpLexer.EOS))
                        {
                            start++;
                            continue;
                        }
                        else
                        {
                            first = token;
                            break;
                        }
                    }
                }
                return first;
            }

            public IToken GetLastToken(bool ignoreSpaces)
            {
                IToken last = null;
                if (currentPosition > -1)
                {
                    int currentLine = allTokens[currentIndex].Line;
                    int start = currentIndex;
                    IToken token = null;
                    // Move to the end, and pass
                    while (start < allTokens.Count)
                    {
                        token = allTokens[start];
                        if (token.Line != currentLine)
                        {
                            break;
                        }
                        // skip whitespace tokens
                        if (((token.Type == XSharpLexer.WS) && ignoreSpaces) ||
                            (token.Type == XSharpLexer.EOS))
                        {
                            start++;
                            continue;
                        }
                        last = token;
                        start++;
                    }
                }
                return last;
            }

            public IToken GetToken(bool ignoreSpaces)
            {
                IToken first = null;
                if (currentPosition > -1)
                {
                    int currentLine = allTokens[currentIndex].Line;
                    int start = currentIndex;
                    while (start < allTokens.Count)
                    {
                        IToken token = allTokens[start];
                        // skip whitespace tokens
                        if (((token.Type == XSharpLexer.WS) && ignoreSpaces) ||
                            (token.Type == XSharpLexer.EOS))
                        {
                            start++;
                            continue;
                        }
                        else
                        {
                            first = token;
                            break;
                        }
                    }
                    //
                    CurrentIndex = start;
                }
                return first;
            }

            private int binarySearch( int left, int right, int toReach)
            {
                if (right >= left)
                {
                    int middle = left + (right - left) / 2;

                    // If the element is present at the 
                    // middle itself 
                    if (allTokens[middle].StartIndex == toReach)
                        return middle;

                    // If element is smaller than middle, then 
                    // it can only be present in left subarray 
                    if (allTokens[middle].StartIndex > toReach)
                        return binarySearch( left, middle - 1, toReach);

                    // Else the element can only be present 
                    // in right subarray 
                    return binarySearch( middle + 1, right, toReach);
                }

                // We reach here when element is not present 
                // in array 
                return -1;
            }
        }

        /// <summary>
        /// A RegionTag has a TagSpan ans a TagType
        /// </summary>
        class RegionTag
        {
            public Span TagSpan { get; }
            public int TagType { get; set; }

            public RegionTag(Span s, int t)
            {
                TagSpan = s;
                TagType = t;
            }
        }

        /// <summary>
        /// A Region contains two RegionTag : Start and End
        /// </summary>
        class Region
        {
            public RegionTag Start { get; }
            public RegionTag End { get; }

            public Region(Span s, int st, Span e, int et)
            {
                Start = new RegionTag(s, st);
                End = new RegionTag(e, et);
            }

            public Region(Span s, Span e, int st, int et) : this(s, st, e, et)
            { }

        }

        private void FormatDocumentV2()
        {
            XSharpProjectPackage.Instance.DisplayOutPutMessage("CommandFilter.FormatDocumentV2() -->>");
            if (!_buffer.CheckEditAccess())
            {
                // can't edit !
                return;
            }
            // Read Settings
            getEditorPreferences(TextView);

            // Try to retrieve an already parsed list of Tags
            if (_classifier != null)
            {
#if TRACE
                //
                System.Diagnostics.Stopwatch stopWatch = new System.Diagnostics.Stopwatch();
                stopWatch.Start();
#endif
                //
                FormattingContext context = null;
                // Already been lexed ?
                if (_buffer.Properties != null && _buffer.Properties.ContainsProperty(typeof(XSharpTokens)))
                {
                    XSharpTokens xTokens = null;
                    xTokens = _buffer.Properties.GetProperty<XSharpTokens>(typeof(XSharpTokens));
                    if (!(xTokens == null || xTokens.TokenStream == null || xTokens.SnapShot == null))
                    {
                        var tokens = xTokens.TokenStream.GetTokens();
                        // Ok, we have some tokens
                        if (tokens != null)
                        {
                            // And they are the right ones
                            if (xTokens.SnapShot.Version == _buffer.CurrentSnapshot.Version)
                            {
                                // Ok, use it
                                context = new FormattingContext(tokens, this.ParseOptions.Dialect);
                            }
                        }
                    }
                }
                // No Tokens....Ok, do the lexing now
                if (context == null)
                    context = new FormattingContext(this, _buffer.CurrentSnapshot);
                //
                #region Get and Sort Regions
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
                Stack<RegionTag> regionStarts = new Stack<RegionTag>();
                List<Region> regions = new List<Region>();
                //
                foreach (var tag in sortedTags)
                {
                    if (tag.ClassificationType.IsOfType(ColorizerConstants.XSharpRegionStartFormat))
                    {
                        int startTokenType = -1;
                        if (tag is XsClassificationSpan)
                            startTokenType = (tag as XsClassificationSpan).startTokenType;
                        if ( startTokenType == -1 )
                        {
                            // Ok, we miss the info during the parsing, but, we will try to get it here
                            context.MoveTo(tag.Span.Start);
                            IToken openKeyword = context.GetFirstToken(true);
                            if (openKeyword != null)
                            {
                                startTokenType = openKeyword.Type;
                            }
                        }
                        regionStarts.Push(new RegionTag(tag.Span.Span, startTokenType));
                    }
                    else if (tag.ClassificationType.IsOfType(ColorizerConstants.XSharpRegionStopFormat))
                    {
                        if (regionStarts.Count > 0)
                        {
                            var start2 = regionStarts.Pop();
                            int endTokenType = -1;
                            if (tag is XsClassificationSpan)
                                endTokenType = (tag as XsClassificationSpan).endTokenType;
                            regions.Add(new Region(start2.TagSpan, tag.Span.Span, start2.TagType, endTokenType));
                        }
                    }
                }
                // In order to try to speed up the formatting process, it would be good to have the regions sorted by their Start
                regions.Sort((a, b) => a.Start.TagSpan.Start.CompareTo(b.Start.TagSpan.Start));

                //Now, we have a list of Regions Start/Stop
                #endregion
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
                    int lineContinue = 0;
                    int prevIndentSize = 0;
                    int continueOffset = _indentSize * _indentFactor;
                    IToken prevstart = null;
                    // We are more forward, line per line
                    foreach (var snapLine in lines)
                    {
                        bool lineAfterAttributes = false;
                        // Ignore Empty lines
                        if (snapLine.Length > 0)
                        {
                            context.MoveTo(snapLine.Start);
                            IToken start = context.GetFirstToken(true);
                            if (start != null)
                            {
                                IToken end = context.GetLastToken(true);
                                //
                                if (lineContinue == 1)
                                {
                                    if (prevstart.Type != XSharpLexer.LBRKT)
                                    {
                                        indentSize = prevIndentSize + continueOffset;
                                    }
                                    else
                                    {
                                        lineAfterAttributes = true;
                                    }

                                }
                                else if (lineContinue > 1)
                                {
                                    indentSize = prevIndentSize;
                                }
                                else
                                {
                                    indentSize = getDesiredIndentationInDocumentV2(context, snapLine, regions, out inComment);
                                }
                                prevstart = start;
                                // Not in comment, Multiple line but not Attribute
                                if (!inComment && (end.Type == XSharpLexer.SEMI))
                                {
                                    if (lineContinue == 0)
                                    {
                                        // Keep the previous Indentation
                                        lineContinue = 1;
                                        prevIndentSize = indentSize;
                                    }
                                    else if (lineContinue == 1)
                                    {
                                        if (!lineAfterAttributes)
                                        {
                                            lineContinue = 2;
                                            prevIndentSize = indentSize;
                                        }
                                    }
                                }
                                else
                                {
                                    lineContinue = 0;
                                }
                            }
                        }
                        if (canFormatLine(snapLine))
                        {
                            formatLineCaseV2(context, editSession, snapLine);
                        }
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
                string elapsedTime = string.Format("{0:00}h {1:00}m {2:00}.{3:00}s",
                    ts.Hours, ts.Minutes, ts.Seconds,
                    ts.Milliseconds / 10);
                //
                XSharpProjectPackage.Instance.DisplayOutPutMessage("FormatDocument : Done in " + elapsedTime);
#endif
            }
            else
                formatCaseForWholeBuffer();
            //
            XSharpProjectPackage.Instance.DisplayOutPutMessage("CommandFilter.FormatDocument() <<--");
        }

        private int getLineLengthV2(ITextSnapshot snapshot, int start)
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

        private int getDesiredIndentationInDocumentV2(FormattingContext context, ITextSnapshotLine snapLine, List<Region> regions, out bool inComment)
        {
            int indentValue = 0;
            int mlCmtSpaces = 0;
            //string openKeyword = "";
            inComment = false;
            //
            //List<IMappingTagSpan<IClassificationTag>> tags = getTagsInLine(snapLine);
            // In Tuple Regions, the items are :
            // Item1 is Start
            // Item2 is End
            foreach (var region in regions)
            {
                // The line is before the current region, so skip
                if (snapLine.End.Position < region.Start.TagSpan.Start)
                {
                    continue;
                }
                // The line is after the current region, so skip
                if (snapLine.Start.Position > region.End.TagSpan.Start)
                {
                    continue;
                }
                // Get the opening keyword, at the beginning of the currently processed region
                int startTokenType = region.Start.TagType;
                int endTokenType = region.End.TagType; // Do we have a Closing Keyword ?
                if (startTokenType == -1) // The parsing has not initialized it, do it now.
                {
                    int prevIndex = context.CurrentIndex;
                    context.MoveTo(region.Start.TagSpan.Start);
                    IToken openKeyword = context.GetFirstToken(true);
                    if (openKeyword == null)
                    {
                        XSharpProjectPackage.Instance.DisplayOutPutMessage("FormatDocument : Error when moving in Tokens");
                        continue; // This should never happen
                    }
                    startTokenType = openKeyword.Type;
                    // save it for the next time...
                    region.Start.TagType = startTokenType;
                    context.CurrentIndex = prevIndex;
                }
                //
                if ((snapLine.Start.Position <= region.Start.TagSpan.Start) && (snapLine.End.Position >= region.Start.TagSpan.Start))
                {
                    // We are on the line opening a Region
                    // What kind of region ?
                    // Skip comment and using regions
                    switch (startTokenType)
                    {
                        case XSharpLexer.ML_COMMENT:
                        case XSharpLexer.SL_COMMENT:
                        case XSharpLexer.USING:
                        case XSharpLexer.PP_INCLUDE:
                        case XSharpLexer.PP_DEFINE:
                        case XSharpLexer.PP_REGION:
                            continue;
                        case XSharpLexer.DEFINE:
                            // Warning !! It could DEFINE CLASS in FOXPRO
                            if (context.Dialect == XSharpDialect.FoxPro)
                            {
                                context.MoveToNext();
                                IToken openKeyword = context.GetFirstToken(true);
                                if (openKeyword == null)
                                {
                                    XSharpProjectPackage.Instance.DisplayOutPutMessage("FormatDocument : Error when moving in Tokens");
                                    continue; // This should never happen
                                }
                                context.MoveBack();
                                if (openKeyword.Type != XSharpLexer.CLASS)
                                    continue;
                            }
                            else
                                continue;
                            break;
                        default:
                            break;
                    }
                    // Move back keywords ( ELSE, ELSEIF, FINALLY, CATCH, RECOVER )
                    switch (startTokenType)
                    {
                        case XSharpLexer.ELSE:
                        case XSharpLexer.ELSEIF:
                        case XSharpLexer.FINALLY:
                        case XSharpLexer.CATCH:
                        case XSharpLexer.RECOVER:
                        case XSharpLexer.PP_ELSE:
                            indentValue--;
                            break;
                    }
                    // Some Users wants CASE/OTHERWISE to be aligned to the opening DO CASE
                    // Check for a setting
                    if (_alignDoCase)
                    {
                        // Move back keywords ( CASE, OTHERWISE )
                        switch (startTokenType)
                        {
                            case XSharpLexer.CASE:
                            case XSharpLexer.OTHERWISE:
                                indentValue--;
                                break;
                        }
                    }
                }
                else if ((snapLine.Start.Position > region.Start.TagSpan.Start) && (snapLine.End.Position < region.End.TagSpan.Start))
                {
                    // We are inside a Region
                    // Comment or Using region ?
                    switch (startTokenType)
                    {
                        case XSharpLexer.ML_COMMENT:
                        case XSharpLexer.SL_COMMENT:
                        case XSharpLexer.USING:
                        case XSharpLexer.PP_INCLUDE:
                        case XSharpLexer.PP_DEFINE:
                        case XSharpLexer.PP_REGION:
                            continue;
                        case XSharpLexer.DEFINE:
                            // Warning !! It could DEFINE CLASS in FOXPRO
                            if (context.Dialect == XSharpDialect.FoxPro)
                            {
                                context.MoveToNext();
                                IToken openKeyword = context.GetFirstToken(true);
                                if (openKeyword == null)
                                {
                                    XSharpProjectPackage.Instance.DisplayOutPutMessage("FormatDocument : Error when moving in Tokens");
                                    continue; // This should never happen
                                }
                                context.MoveBack();
                                if (openKeyword.Type != XSharpLexer.CLASS)
                                    continue;
                            }
                            else
                                continue;
                            break;
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
                        switch (startTokenType)
                        {
                            case XSharpLexer.FUNCTION:
                            case XSharpLexer.PROCEDURE:
                            case XSharpLexer.CONSTRUCTOR:
                            case XSharpLexer.DESTRUCTOR:
                            case XSharpLexer.ASSIGN:
                            case XSharpLexer.ACCESS:
                            case XSharpLexer.METHOD:
                            case XSharpLexer.OPERATOR:
                                break;
                            default:
                                indentValue++;
                                break;
                        }
                    }
                    // Move back keywords ( ELSE, ELSEIF, FINALLY, CATCH, RECOVER )
                    switch (startTokenType)
                    {
                        case XSharpLexer.ELSE:
                        case XSharpLexer.ELSEIF:
                        case XSharpLexer.FINALLY:
                        case XSharpLexer.CATCH:
                        case XSharpLexer.RECOVER:
                        case XSharpLexer.PP_ELSE:
                            indentValue--;
                            break;
                    }
                }
                else //if ((region.Item2.Start >= snapLine.Start.Position) && (region.Item2.End <= snapLine.End.Position))
                {
                    // We are on the closing Keyword
                    if (!_alignMethod)
                    {
                        switch (startTokenType)
                        {
                            case XSharpLexer.FUNCTION:
                            case XSharpLexer.PROCEDURE:
                            case XSharpLexer.CONSTRUCTOR:
                            case XSharpLexer.DESTRUCTOR:
                            case XSharpLexer.ASSIGN:
                            case XSharpLexer.ACCESS:
                            case XSharpLexer.METHOD:
                            case XSharpLexer.OPERATOR:
                                // per Default
                                indentValue++;
                                //
                                switch (endTokenType)
                                {
                                    case XSharpLexer.END:
                                        // We may have an optionnal closing keyword indication
                                        indentValue--;
                                        break;
                                }
                                break;
                        }
                    }
                    //
                    if (!_alignDoCase)
                    {
                        // Don't indent
                        // Move back keywords ( CASE, OTHERWISE )
                        switch (startTokenType)
                        {
                            case XSharpLexer.CASE:
                            case XSharpLexer.OTHERWISE:
                                indentValue++;
                                break;
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

        private void formatLineCaseV2(FormattingContext context, ITextEdit editSession, ITextSnapshotLine line)
        {
            if (XSharpProjectPackage.Instance.DebuggerIsRunning)
            {
                return;
            }
            if (!canFormatLine(line))
            {
                return;
            }

            getEditorPreferences(TextView);
            if (_keywordCase == KeywordCase.None)
            {
                return;
            }
            XSharpProjectPackage.Instance.DisplayOutPutMessage($"CommandFilter.formatLineCaseV2({line.LineNumber + 1})");
            //
            context.MoveTo(line.Start);
            IToken token = context.GetToken(true);
            int workOnLine = -1;
            if ( token != null )
                workOnLine = token.Line;
            while ( token != null )
            {
                if (currentLine == line.LineNumber)
                {
                    // do not update tokens touching or after the caret
                    // after typing String it was already uppercasing even when I wanted to type StringComparer
                    // now we wait until the user has typed an extra character. That will trigger another session.
                    // (in this case the C, but it could also be a ' ' or tab and then it would match the STRING keyword)
                    int caretPos = this.TextView.Caret.Position.BufferPosition.Position;
                    if ( token.StopIndex < caretPos - 1)
                    {
                        formatToken(editSession, 0, token);
                    }
                    else
                    {
                        // Come back later.
                        registerLineForCaseSync(line.LineNumber);
                        break;
                    }
                }
                else
                {
                    formatToken(editSession, 0, token);
                }
                //
                context.MoveToNext();
                token = context.GetToken(true);
                if (token != null)
                {
                    if (token.Line != workOnLine)
                    {
                        context.MoveBack();
                        token = null;
                    }
                }
            }
            
        }
        #endregion

    }
}
