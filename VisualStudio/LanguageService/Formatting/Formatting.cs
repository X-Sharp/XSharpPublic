using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using System;
using System.Collections.Generic;
using System.Linq;
using XSharpModel;
using static XSharp.Parser.VsParser;

namespace XSharp.LanguageService
{
    partial class XSharpFormattingCommandHandler
    {

        SourceCodeEditorSettings _settings;

        private void CopyWhiteSpaceFromPreviousLine(ITextEdit editSession, ITextSnapshotLine line, XDocument document)
        {
            // only copy the indentation from the previous line
            var text = line.GetText();
            var wsLength = text.Length - text.TrimStart().Length;
            if (line.LineNumber > 0)
            {
                var prev = line.Snapshot.GetLineFromLineNumber(line.LineNumber - 1);
                var prevText = prev.GetText();
                int nWs = 0;
                while (nWs < prevText.Length && Char.IsWhiteSpace(prevText[nWs]))
                {
                    nWs++;
                }
                if (nWs <= prevText.Length && nWs != wsLength)
                {
                    prevText = prevText.Substring(0, nWs);
                    editSession.Replace(new Span(line.Start.Position, wsLength), prevText);
                }
            }
        }

        private void FormatLine()
        {
            // When we get here we are at the start of the line.
            // We want to make sure that the previous line is properly indented
            // for example if the previous line closes a block such as FOR .. NEXT
            // We also want to adjust the starting whitespace based on the previous line.
            // When the previous line indicates the start of a block / entity
            // then we add one "tab stop"
            // otherwise we copy the whitespace from the previous line


            SnapshotPoint caret = this._textView.Caret.Position.BufferPosition;
            ITextSnapshotLine line = caret.GetContainingLine();
            // On what line are we ?

            int lineNumber = line.LineNumber;

            // we calculate the indent based on the previous line so we must be on the second line
            if (lineNumber > 0)
            {
                int indentation;
                bool alignOnPrev = false;
                var document = _buffer.GetDocument();
                if (caret.Position < line.End.Position)
                {
                    alignOnPrev = true;
                }

                // wait until we can work
                while (_buffer.EditInProgress)
                {
                    System.Threading.Thread.Sleep(100);
                }
                var _ = _classifier.ClassifyWhenNeededAsync();
                var editSession = _buffer.CreateEdit();
                // This will calculate the desired indentation of the current line, based on the previous one
                // and may de-Indent the previous line if needed
                //
                try
                {
                    switch ((EnvDTE.vsIndentStyle)_settings.IndentStyle)
                    {
                        case EnvDTE.vsIndentStyle.vsIndentStyleSmart:
                            int prevIndent = -1;
                            if (lineNumber > 0)
                            {

                                ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber - 1);
                                if (canChangeLine(prevLine, document))
                                {
                                    this.FormatLineCase(editSession, prevLine);

                                    prevIndent = CalculatePreviousLineIndent(document, prevLine);
                                    if (prevIndent == -1)
                                        CopyWhiteSpaceFromPreviousLine(editSession, prevLine, document);
                                    else
                                        FormatLineIndent(editSession, prevLine, prevIndent);
                                }

                                indentation = GetDesiredIndentation(line, editSession, alignOnPrev, document, prevIndent);
                                if (indentation == -1)
                                {
                                    CopyWhiteSpaceFromPreviousLine(editSession, line, document);
                                }
                                else
                                {
                                    // but we may need to re-Format the previous line for Casing and Identifiers
                                    // so, do it before indenting the current line.
                                    FormatLineIndent(editSession, line, indentation);
                                }
                            }
                            break;
                        case EnvDTE.vsIndentStyle.vsIndentStyleDefault:
                        case EnvDTE.vsIndentStyle.vsIndentStyleNone:
                            break;
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
        private int IndentLine(XDocument document, XKeyword keyword, int lineNo)
        {
            var rule = XFormattingRule.GetFirstRuleByStart(keyword);
            lineNo = lineNo - 1;
            int prevIndentation ;
            if (document.GetTokens(lineNo, out var tokens) && tokens[0].Type == XSharpLexer.WS)
            {
                prevIndentation = GetIndentTokenLength(tokens[0]);
            }
            else
            {
                prevIndentation = 0;
            }
            if (_settings.IndentCaseLabel && rule.Flags.HasFlag(XFormattingFlags.Case))
            {
                if (rule.Flags.HasFlag(XFormattingFlags.Middle))
                {
                    prevIndentation += _settings.IndentSize;
                }
            }
            return prevIndentation;
        }
        private int OutdentLine(XDocument document, XKeyword keyword, int lineNo)
        {
            // find starting rules that we should match with
            var rules = XFormattingRule.GetEndRules(keyword);
            lineNo = lineNo - 1;
            int prevIndentation = -1;
            bool done = false;
            int nested = 0;
            while (lineNo >= 0 && !done)
            {
                if (document.GetKeyword(lineNo, out var prevkw))
                {
                    if (prevkw.Equals(keyword))
                    {
                        nested++;
                        lineNo -= 1;
                        continue;
                    }
                    foreach (var rule in rules)
                    {
                        // does this rule start with what we expect
                        if (!prevkw.Equals(rule.Start))
                            continue;
                        if (nested > 0)
                        {
                            if (!rule.Flags.HasFlag(XFormattingFlags.Middle))
                                nested--;
                        }
                        else
                        {
                            done = true;
                            if (document.GetTokens(lineNo, out var tokens) && tokens[0].Type == XSharpLexer.WS)
                            {
                                prevIndentation = GetIndentTokenLength(tokens[0]);
                            }
                            else
                            {
                                prevIndentation = 0;
                            }
                            if (_settings.IndentCaseLabel && rule.Flags.HasFlag(XFormattingFlags.Case))
                            {
                                if (rule.Flags.HasFlag(XFormattingFlags.Middle))
                                {
                                    prevIndentation += _settings.IndentSize;
                                }
                            }
                            break;
                        }
                    }
                }
                lineNo -= 1;
            }
            return prevIndentation;
        }

        private int MiddleLine(XDocument document, XKeyword keyword, int lineNo)
        {
            int prevIndentation = -1;
            var rules = XFormattingRule.GetMiddleRules(keyword);
            bool done = false;
            while (lineNo >= 0 && !done)
            {
                if (document.GetKeyword(lineNo, out var prevkw))
                {
                    foreach (var rule in rules)
                    {
                        // does this rule end with what we expect
                        if (!prevkw.Equals(rule.Start))
                        {
                            continue;
                        }
                        done = true;
                        if (document.GetTokens(lineNo, out var tokens) && tokens[0].Type == XSharpLexer.WS)
                        {
                            prevIndentation = GetIndentTokenLength(tokens[0]);
                        }
                        else
                        {
                            prevIndentation = 0;
                        }
                        if (_settings.IndentCaseLabel && rule.Flags.HasFlag(XFormattingFlags.Case))
                        {
                            prevIndentation += _settings.IndentSize;
                        }
                        break;
                    }
                }
                lineNo -= 1;
            }
            return prevIndentation;
        }

        private int MemberToken(XDocument document, XKeyword keyword, int lineNo)
        {
            int prevIndentation = -1;
            bool done = false;
            lineNo = lineNo - 1;
            var rule = XFormattingRule.GetFirstRuleByStart(keyword);
            while (lineNo >= 0 && !done)
            {
                if (document.GetKeyword(lineNo, out var prevKw))
                {
                    if (XFormattingRule.IsMember(prevKw))
                    {
                        prevIndentation = 0;
                        done = true;
                        if (document.GetTokens(lineNo, out var tokens) && tokens[0].Type == XSharpLexer.WS)
                        {
                            prevIndentation = GetIndentTokenLength(tokens[0]);
                        }
                    }
                    else if (XFormattingRule.IsType(prevKw))
                    {
                        prevIndentation = 0;
                        done = true;
                        if (document.GetTokens(lineNo, out var tokens) && tokens[0].Type == XSharpLexer.WS)
                        {
                            prevIndentation = GetIndentTokenLength(tokens[0]);
                            if (_settings.IndentTypeMembers)
                            {
                                prevIndentation += _settings.IndentSize;
                            }
                        }
                    }
                }
                lineNo -= 1;
            }
            return prevIndentation;
        }

        private int CalculatePreviousLineIndent(XDocument document, ITextSnapshotLine line)
        {

            var lineNo = line.LineNumber-1;
            XKeyword keyword = GetFirstKeywordInLine(line, out _, out _);
            int prevIndentation = -1;

            if (XFormattingRule.IsStartKeyword(keyword))
            {
                prevIndentation = IndentLine(document, keyword, lineNo);
            }
            else if (XFormattingRule.IsEndKeyword(keyword))
            {
                prevIndentation = OutdentLine(document, keyword, lineNo);
            }
            else if (XFormattingRule.IsMiddleKeyword(keyword))
            {
                prevIndentation = MiddleLine(document, keyword, lineNo);
            }
            else
            {
                var templine = lineNo - 1;
                while (!document.GetKeyword(templine, out keyword))
                {
                    if (!keyword.IsEmpty )
                        break;
                    templine -= 1;
                    if (templine < 0)
                        break;
                }

                if (XFormattingRule.IsMiddleKeyword(keyword) ||
                    XFormattingRule.IsStartKeyword(keyword))
                {
                    if (document.GetTokens(templine, out var tokens) && tokens[0].Type == XSharpLexer.WS)
                    {
                        prevIndentation = GetIndentTokenLength(tokens[0]);
                    }
                    prevIndentation += _settings.IndentSize;
                }
            }
            if (XFormattingRule.IsEntity(keyword))
            {
                prevIndentation = MemberToken(document, keyword, lineNo);
            }
            return prevIndentation;
        }
        private void FormatLineIndent(ITextEdit editSession, ITextSnapshotLine line, int desiredIndentation)
        {
            int tabSize = _settings.TabSize;
            bool useSpaces = _settings.TabsAsSpaces;
            int lineLength = line.Length;
            var originalLine = line.GetText();

            int originalIndentLength = lineLength - originalLine.TrimStart().Length;
            if (desiredIndentation < 0)
            {
                ; //do nothing
            }
            else if (desiredIndentation == 0)
            {
                // remove indentation
                if (originalIndentLength != 0)
                {
                    Span indentSpan = new Span(line.Start.Position, originalIndentLength);
                    editSession.Replace(indentSpan, "");
                }
            }
            else
            {
                string newIndent;
                if (useSpaces)
                {
                    newIndent = new string(' ', desiredIndentation);
                }
                else
                {
                    // fill indent room with tabs and optionally also with one or more spaces
                    // if the indentsize is not the same as the tabsize
                    int numTabs = desiredIndentation / tabSize;
                    int numSpaces = desiredIndentation % tabSize;
                    newIndent = new string('\t', numTabs);
                    if (numSpaces != 0)
                    {
                        newIndent += new string(' ', numSpaces);
                    }
                }
                if (originalIndentLength == 0)
                {
                    editSession.Insert(line.Start.Position, newIndent);
                }
                else
                {
                    var originalIndent = originalLine.Substring(0, originalIndentLength);
                    if (originalIndent != newIndent)
                    {
                        Span indentSpan = new Span(line.Start.Position, originalIndentLength);
                        editSession.Replace(indentSpan, newIndent);
                    }
                }
            }
        }


        /// <summary>
        /// Format document, evaluating line after line
        /// </summary>
        private void FormatDocument()
        {
            WriteOutputMessage("FormatDocument() -->>");
            if (!_buffer.CheckEditAccess())
            {
                // can't edit !
                return;
            }
            if (_buffer.CurrentSnapshot.Length == 0)
            {
                // Nothing to do
                return;
            }
            // Try to retrieve an already parsed list of Tags
            if (_classifier != null)
            {
#if TRACE
                //
                System.Diagnostics.Stopwatch stopWatch = new System.Diagnostics.Stopwatch();
                stopWatch.Start();
#endif
                //
                var _ = _classifier.ClassifyWhenNeededAsync().ConfigureAwait(true);

                //
                // wait until we can work
                while (_buffer.EditInProgress)
                {
                    System.Threading.Thread.Sleep(100);
                }
                // Get all lines
                var lines = _buffer.CurrentSnapshot.Lines;
                var endLine = _buffer.CurrentSnapshot.LineCount - 1;
                if (endLine < 1)
                {
                    // Nothing to do
                    return;
                }
                // Format the full text, with an first Indentation set to 0
                FormatSpan(lines, 0, endLine, 0);
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
                WriteOutputMessage("FormatDocument : Done in " + elapsedTime);
#endif
            }
            else
            {
                FormatCaseForWholeBuffer();
            }
            //
            WriteOutputMessage("FormatDocument() <<--");
        }

        private void FormatSpanWorker(ITextEdit editSession, IEnumerable<ITextSnapshotLine> lines, int startLine, int endLine, int startIndent)
        {
            var document = _buffer.GetDocument();
            var formatter = new DocFormatter(document, _settings);
            var expectedIndent = formatter.GetIndentSizes(lines, startLine, endLine, startIndent);

            // now process the lines
            foreach (var line in lines)
            {
                var number = line.LineNumber;
                if (number >= startLine && number <= endLine)
                {
                    var indent = expectedIndent[number];
                    FormatLineIndent(editSession, line, indent * _settings.IndentSize);
                    FormatLineCase(editSession, line);
                }
            }
        }

        private void FormatSpan(IEnumerable<ITextSnapshotLine> lines, int startLine, int endLine, int startIndent)
        {
            ITextEdit editSession = null;
            try
            {
                editSession = _buffer.CreateEdit();
                FormatSpanWorker(editSession, lines, startLine, endLine, startIndent);
                if (editSession.HasEffectiveChanges)
                {
                    editSession.Apply();
                }
                else
                {
                    editSession.Cancel();
                }
            }
            catch
            {
                if (editSession != null && editSession.HasEffectiveChanges)
                {
                    editSession.Cancel();
                }
            }
        }

        /// <summary>
        /// Format the current selection
        /// </summary>
        private void FormatSelection()
        {
            int startPosition = _textView.Selection.Start.Position.Position;
            int endPosition = _textView.Selection.End.Position.Position;
            //
            int startLine = _buffer.CurrentSnapshot.GetLineNumberFromPosition(startPosition);
            int endLine = _buffer.CurrentSnapshot.GetLineNumberFromPosition(endPosition);
            //
            var lines = _buffer.CurrentSnapshot.Lines;
            FormatSpan(lines, startLine, endLine, 0);
        }

        #region Tokens manipulations
        /// <summary>
        /// Retrieve all Tags in the Line
        /// </summary>
        /// <param name="line"></param>
        /// <returns></returns>
        private IList<XSharpToken> GetTokensInLine(ITextSnapshotLine line)
        {
            IList<XSharpToken> tokens = new List<XSharpToken>();
            // Already been lexed ?
            var document = _buffer.GetDocument();
            if (document != null)
            {
                var allTokens = document.TokenStream.GetTokens();

                if (allTokens != null)
                {
                    if (document.SnapShot.Version == _buffer.CurrentSnapshot.Version)
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
                        if (startIndex > -1)
                        {
                            // Move to the end of line
                            int currentLine = allTokens[startIndex].Line;
                            do
                            {
                                tokens.Add((XSharpToken)allTokens[startIndex]);
                                startIndex++;

                            } while ((startIndex < allTokens.Count) && (currentLine == allTokens[startIndex].Line));
                            return tokens;
                        }
                    }
                }
            }
            // Ok, do it now
            var text = line.GetText();
            tokens = GetTokens(text);
            return tokens;
            //
        }


        #endregion


        #region SmartIndent



        /// <summary>
        /// the indentation is measured in # of characters
        /// </summary>
        /// <param name="line"></param>
        /// <param name="editSession"></param>
        /// <param name="alignOnPrev"></param>
        /// <returns></returns>
        private int GetDesiredIndentation(ITextSnapshotLine line, ITextEdit editSession, bool alignOnPrev, XDocument document, int prevIndent)
        {
            WriteOutputMessage($"getDesiredIndentation({line.LineNumber + 1})");
            try
            {
                // How many spaces do we need ?
                int indentValue = 0;
                int lineNumber = line.LineNumber;
                if (lineNumber > 1)
                {
                    // We need to analyze the Previous line
                    ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber - 1);
                    XKeyword prevLineKeyword = GetFirstKeywordInLine(prevLine, out indentValue, out var tokens);
                    if (_settings.IndentContinuedLines)
                    {
                        var lastToken = tokens.Where((t) => t.Type != XSharpLexer.EOS && t.Type != XSharpLexer.Eof).LastOrDefault();
                        if (lastToken != null && lastToken.Type == XSharpLexer.LINE_CONT)
                        {
                            indentValue += _settings.IndentSize;
                        }
                    }
                    _lastIndentValue = indentValue;
                    if (alignOnPrev)
                        return _lastIndentValue;

                    if (prevLineKeyword.IsEmpty)
                    {
                        var temp = prevLine.LineNumber - 1;
                        while (temp >= 0 && prevLineKeyword.IsEmpty)
                        {
                            var previous = line.Snapshot.GetLineFromLineNumber(temp);
                            prevLineKeyword = GetFirstKeywordInLine(previous, out indentValue, out _);
                            temp -= 1;
                        }
                    }
                    if (!prevLineKeyword.IsEmpty)
                    {
                        // Start of a block of code ?
                        if (XFormattingRule.IsEntity(prevLineKeyword))
                        {
                            if (_settings.IndentTypeMembers)
                            {
                                indentValue += _settings.IndentSize;
                            }
                        }
                        else if (XFormattingRule.IsStartKeyword(prevLineKeyword) ||
                            XFormattingRule.IsMiddleKeyword(prevLineKeyword))
                        {
                            indentValue += _settings.IndentSize;
                            if (prevLineKeyword.Kw2 == XTokenType.Namespace && !_settings.IndentNamespace)
                            {
                                indentValue -= _settings.IndentSize;
                            }
                            // Check to see if we need to adjust CASE / OTHERWISE
                            if (prevLineKeyword.Kw1 == XTokenType.Case ||
                                prevLineKeyword.Kw1 == XTokenType.Otherwise)
                            {

                                if (!_settings.IndentCaseContent)
                                {
                                    indentValue -= _settings.IndentSize;
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
                XSettings.LogException(ex, "SmartIndent.GetDesiredIndentation failed");
            }
            return _lastIndentValue;
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

        private IList<XSharpToken> GetTokens(string text)
        {
            var tokens = new List<XSharpToken>();
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
                var reporter = new ErrorIgnorer();
                bool ok = XSharp.Parser.VsParser.Lex(text, fileName, parseoptions, reporter, out ITokenStream tokenStream);
                var stream = tokenStream as BufferedTokenStream;
                foreach (var token in stream.GetTokens())
                {
                    tokens.Add((XSharpToken)token);
                }
                return tokens;
            }
            catch (Exception e)
            {
                XSettings.LogException(e, "GetTokens");
            }
            return tokens;
        }
        /// <summary>
        /// Returns the indent width in characters
        /// </summary>
        /// <param name="token"></param>
        /// <returns></returns>
        private int GetIndentTokenLength(IToken token)
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
                                var mod = len % _settings.TabSize;
                                len = len - mod + _settings.IndentSize;
                                space = false;
                            }
                            else
                            {
                                len += _settings.IndentSize;
                            }
                            break;
                        default:
                            // the only other token that is allowed inside a WS is an old style pragma like ~"ONLYEARLY+"
                            // these do not influence the tab position.
                            break;
                    }
                }
                int rest = len % _settings.IndentSize;
                len /= _settings.IndentSize;
                if (rest != 0)
                {
                    len += 1;
                }
            }
            return len * _settings.IndentSize;
        }
        /// <summary>
        /// Get the first keyword in Line as a token of the XKeyword type.
        /// </summary>
        /// <param name="line">The line to analyze</param>
        /// <param name="doSkipped">Bool value indicating if a "DO" keyword has been skipped</param>
        /// <param name="minIndent"></param>
        /// <returns></returns>
        private XKeyword GetFirstKeywordInLine(ITextSnapshotLine line, out int minIndent, out IList<XSharpToken> tokens)
        {
            minIndent = -1;
            string startOfLine = line.GetText();
            XKeyword keyword = default;
            int index = 0;
            tokens = GetTokens(startOfLine);
            if (tokens.Count > 0)
            {
                if (tokens[0].Type == XSharpLexer.WS)
                {
                    index = 1;
                    minIndent = GetIndentTokenLength(tokens[0]);
                }
                else
                {
                    minIndent = 0;
                }
                while (index < tokens.Count)
                {
                    var token = tokens[index];
                    if (token.Type == XSharpLexer.WS)
                    {
                        index++;
                        continue;
                    }
                    if (XSharpLexer.IsKeyword(token.Type) || this.IsPPKeyword(token.Type))
                    {
                        // Skip modifiers at start of line
                        if (XSharpLexer.IsModifier(token.Type) && token.Type != XSharpLexer.CLASS)
                        {
                            index++;
                            keyword = default;
                            continue;
                        }
                        keyword = new XKeyword(token.Type);
                        // check for 2 keyword tokens
                        if (index < tokens.Count - 2 && !XFormattingRule.IsSingleKeyword(token.Type))
                        {
                            var token2 = tokens[index + 2];
                            keyword = new XKeyword(token.Type, token2.Type);
                            if (token.Type == XSharpLexer.LOCAL
                                && (token2.Type == XSharpLexer.PROCEDURE || token2.Type == XSharpLexer.FUNCTION))
                            {
                                keyword = new XKeyword(token2.Type);
                            }
                            else if (token.Type == XSharpLexer.DEFINE && ParseOptions.Dialect == XSharpDialect.FoxPro)
                            {
                                index += 2;
                                // skip modifiers between DEFINE and CLASS
                                while ((XSharpLexer.IsModifier(token2.Type) || token2.Type == XSharpLexer.WS) && index < tokens.Count - 1)
                                {
                                    index++;
                                    token2 = tokens[index];
                                }
                                if (token2.Type == XSharpLexer.CLASS)
                                {
                                    keyword = new XKeyword(token.Type, token2.Type);
                                }
                            }
                        }
                    }
                    else if (XSharpLexer.IsComment(token.Type))
                    {
                        keyword = default;
                    }
                    break;
                }
            }
            return keyword;
        }
        private bool IsPPKeyword(int kw)
        {
            return kw >= XSharpLexer.PP_FIRST && kw <= XSharpLexer.PP_LAST;
        }
        #endregion

    }


    public class ErrorIgnorer : IErrorListener
    {
        #region IErrorListener
        public void ReportError(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }

        public void ReportWarning(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }
        #endregion
    }

    #region List extension used in New Formatting process
    static class ListExtension
    {
        public static T Pop<T>(this List<T> list)
        {
            int listEnd = list.Count;
            if (listEnd > 0)
            {
                T r = list[listEnd - 1];
                list.RemoveAt(listEnd - 1);
                return r;
            }
            else
            {
                throw new InvalidOperationException("List cannot be empty");
            }
        }

        public static void Push<T>(this List<T> list, T v)
        {
            list.Add(v);
        }

        public static T Peek<T>(this List<T> list)
        {
            int listEnd = list.Count;
            if (listEnd > 0)
            {
                T r = list[listEnd - 1];
                return r;
            }
            else
            {
                throw new InvalidOperationException("List cannot be empty");
            }
        }
    }
    #endregion
}
