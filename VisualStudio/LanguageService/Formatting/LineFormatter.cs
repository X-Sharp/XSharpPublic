//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//------------------------------------------------------------------------------

using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using System;
using System.Collections.Generic;
using System.Linq;
using XSharpModel;

namespace XSharp.LanguageService
{
    internal class LineFormatter
    {
        readonly XDocument _document;
        readonly ITextBuffer _buffer;
        readonly XFile _file;
        readonly SourceCodeEditorSettings _settings;
        private static int _lastIndentValue;    // in number of characters



        internal LineFormatter(ITextBuffer buffer, SourceCodeEditorSettings settings)
        {
            _buffer = buffer;
            _document = buffer.GetDocument();
            _file = buffer.GetFile();
            _settings = settings;
        }
        private XKeyword GetFirstKeywordInLine(ITextSnapshotLine line, out int minIndent, out IList<IToken> tokens)
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
                        if (index < tokens.Count - 2
                            && !XFormattingRule.IsSingleKeyword(token.Type))
                        {
                            var token2 = tokens[index + 2];
                            if (token2.Type != XSharpLexer.EOS
                                && token2.Type != XSharpLexer.Eof)
                            {
                                keyword = new XKeyword(token.Type, token2.Type);
                            }
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


        internal bool CanChangeLine(ITextSnapshotLine line)
        {
            if (line.Length == 0)
                return true;
            if (_document.GetTokens(line.LineNumber, out var tokens))
            {
                var token = tokens.Where((t) => t.Type != XSharpLexer.WS).FirstOrDefault();
                if (token == null)
                    return true;  // should not happen 
                if (XSharpLexer.IsComment(token.Type))
                    return false;
                if (token.Type == XSharpLexer.TEXT_STRING_CONST)
                    return false;
                return true;
            }
            return false;
        }
        #region Formatting
        internal void FormatLine(ITextEdit editSession, ITextSnapshotLine line)
        {
            // When we get here we are at the start of the line.
            // We want to make sure that the previous line is properly indented
            // for example if the previous line closes a block such as FOR .. NEXT
            // We also want to adjust the starting whitespace based on the previous line.

            int lineNumber = line.LineNumber;
            bool alignOnPrev = true;
            int prevIndent = -1;
            var previousLine = line.Snapshot.GetLineFromLineNumber(lineNumber - 1);
            if (CanChangeLine(previousLine))
            {
                this.FormatLineCase(editSession, previousLine);
                prevIndent = CalculatePreviousLineIndent(previousLine);
                SetIndentation(editSession, previousLine, prevIndent);
            }
            // calculate the indentation from the new current line
            if (line.Length == 0 || CanChangeLine(line))
            {
                var indentation = GetDesiredIndentation(line, editSession, alignOnPrev, prevIndent);
                SetIndentation(editSession, line, indentation);
            }

        }

        private void SetIndentation(ITextEdit editSession, ITextSnapshotLine line, int indentation)
        {
            if (indentation == -1)
            {
                CopyWhiteSpaceFromPreviousLine(editSession, line);
            }
            else
            {
                FormatLineIndent(editSession, line, indentation);
            }
        }

        internal void FormatLineCase(ITextEdit editSession, ITextSnapshotLine line)
        {
            // get classification of the line.
            // when the line is part of a multi line comment then do nothing
            // to detect that we take the start of the line and check if it is in
            int lineStart = line.Start.Position;
            if (line.Length == 0)
                return;
            var tokens = GetTokensInLine(line);
            if (tokens.Count > 0)
            {
                if (tokens[0].StartIndex < lineStart)
                {
                    // The Tokens are coming from a single-line parsing
                    // StartIndex is relative to the beginning of line
                }
                else
                {
                    // The Tokens comes from a full-source parsing
                    // StartIndex is relative to the beginning of file
                    lineStart = 0;
                }
            }
            foreach (var token in tokens)
            {
                FormatToken(editSession, lineStart, token);
            }

        }

        internal void FormatLineIndent(ITextEdit editSession, ITextSnapshotLine line, int desiredIndentation)
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

        private bool SyncKeywordCase => _settings.KeywordCase != KeywordCase.None;
        private void FormatToken(ITextEdit editSession, int offSet, IToken token)
        {
            if (token.Channel == XSharpLexer.Hidden ||
                token.Channel == XSharpLexer.PREPROCESSORCHANNEL ||
                token.Type == XSharpLexer.TEXT_STRING_CONST)
                return;
            bool syncKeyword = false;
            bool syncID = false;
            // Some exceptions are (pseudo) functions. These should not be formatted
            switch (token.Type)
            {
                case XSharpLexer.ID:
                    syncID = XSettings.IdentifierCase;
                    break;
                case XSharpLexer.UDC_KEYWORD:
                    syncKeyword = XSettings.UDCKeywordCase;
                    break;
                case XSharpLexer.NAMEOF:
                case XSharpLexer.SIZEOF:
                case XSharpLexer.TYPEOF:
                    // these are keywords but should be excluded I think
                    syncKeyword = false;
                    break;
                case XSharpLexer.TRUE_CONST:
                case XSharpLexer.FALSE_CONST:
                case XSharpLexer.MACRO:
                case XSharpLexer.LOGIC_AND:
                case XSharpLexer.LOGIC_OR:
                case XSharpLexer.LOGIC_NOT:
                case XSharpLexer.LOGIC_XOR:
                case XSharpLexer.VO_AND:
                case XSharpLexer.VO_OR:
                case XSharpLexer.VO_NOT:
                case XSharpLexer.VO_XOR:
                    syncKeyword = SyncKeywordCase;
                    break;
                default:
                    if (token.Type >= XSharpLexer.FIRST_NULL && token.Type <= XSharpLexer.LAST_NULL)
                    {
                        syncKeyword = SyncKeywordCase;
                    }
                    else if (XSharpLexer.IsKeyword(token.Type))
                    {
                        syncKeyword = token.Text[0] != '#';
                    }
                    break;
            }
            if (syncKeyword)
            {
                var keyword = token.Text;
                var transform = XSettings.FormatKeyword(keyword, _settings.KeywordCase);
                if (string.Compare(transform, keyword) != 0)
                {
                    int startpos = offSet + token.StartIndex;
                    editSession.Replace(startpos, transform.Length, transform);
                }
            }
            if (syncID)
            {
                var id = token.Text;
                var transform = id;
                if (_document.Identifiers.ContainsKey(id))
                {
                    transform = _document.Identifiers[id];
                }
                if (string.Compare(transform, id) != 0)
                {
                    int startpos = offSet + token.StartIndex;
                    editSession.Replace(startpos, transform.Length, transform);
                }
            }
        }

        #endregion
        #region Calculate Indention
        private int CalculatePreviousLineIndent(ITextSnapshotLine line)
        {

            var lineNo = line.LineNumber;
            var keyword = GetFirstKeywordInLine(line, out _, out _);
            int prevIndentation = -1;

            if (XFormattingRule.IsStartKeyword(keyword))
            {
                prevIndentation = IndentLine(keyword, lineNo);
            }
            else if (XFormattingRule.IsEndKeyword(keyword))
            {
                prevIndentation = OutdentLine(keyword, lineNo);
            }
            else if (XFormattingRule.IsMiddleKeyword(keyword))
            {
                prevIndentation = MiddleLine(keyword, lineNo);
            }
            else
            {
                var templine = lineNo - 1;
                if (_document.GetTokens(templine, out var tokens) )
                {
                    if (tokens[0].Type == XSharpLexer.WS)
                        prevIndentation = GetIndentTokenLength(tokens[0]);
                    else
                        prevIndentation = 0;
                }
                _document.GetKeyword(templine, out keyword);

                if (XFormattingRule.IsMiddleKeyword(keyword) ||
                    XFormattingRule.IsStartKeyword(keyword))
                {
                    prevIndentation += _settings.IndentSize;
                }
            }
            return prevIndentation;
        }
     
        private int GetDesiredIndentation(ITextSnapshotLine line, ITextEdit editSession, bool alignOnPrev, int prevIndent)
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
                    if (XFormattingRule.IsStartKeyword(prevLineKeyword) ||
                        XFormattingRule.IsMiddleKeyword(prevLineKeyword))
                    {
                        alignOnPrev = false;
                    }
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
                        return prevIndent;

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
                            indentValue = prevIndent + _settings.IndentSize;

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

        private void CopyWhiteSpaceFromPreviousLine(ITextEdit editSession, ITextSnapshotLine line)
        {
            // only copy the indentation from the previous line
            if (line.LineNumber > 0)
            {
                var text = line.GetText();
                var wsLength = text.Length - text.TrimStart().Length;
                var prev = line.Snapshot.GetLineFromLineNumber(line.LineNumber - 1);
                var prevText = prev.GetText();
                int nWs = 0;
                while (nWs < prevText.Length && char.IsWhiteSpace(prevText[nWs]))
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
        #endregion
        #region Indent/Outdent Line
        private int IndentLine(XKeyword keyword, int lineNo)
        {
            var rule = XFormattingRule.GetFirstRuleByStart(keyword);
            int prevIndentation;
            if (_document.GetTokens(lineNo, out var tokens) && tokens[0].Type == XSharpLexer.WS)
            {
                prevIndentation = GetIndentTokenLength(tokens[0]);
            }
            else
            {
                prevIndentation = 0;
            }
            return prevIndentation;
        }
        private int OutdentLine(XKeyword keyword, int lineNo)
        {
            // find starting rules that we should match with
            var rules = XFormattingRule.GetEndRules(keyword);
            int prevIndentation = -1;
            bool done = false;
            lineNo = lineNo - 1;
            int nested = 0;
            while (lineNo >= 0 && !done)
            {
                if (_document.GetKeyword(lineNo, out var prevkw))
                {
                    if (prevkw.Equals(keyword) || XFormattingRule.IsSynonym(keyword, prevkw))
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
                            if (_document.GetTokens(lineNo, out var tokens) && tokens[0].Type == XSharpLexer.WS)
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

        private int MiddleLine(XKeyword keyword, int lineNo)
        {
            int prevIndentation = -1;
            var rules = XFormattingRule.GetMiddleRules(keyword);
            bool done = false;
            while (lineNo >= 0 && !done)
            {
                if (_document.GetKeyword(lineNo, out var prevkw))
                {
                    foreach (var rule in rules)
                    {
                        // does this rule end with what we expect
                        if (!prevkw.Equals(rule.Start))
                        {
                            continue;
                        }
                        done = true;
                        if (_document.GetTokens(lineNo, out var tokens) && tokens[0].Type == XSharpLexer.WS)
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
        private int MemberToken(XKeyword keyword, int lineNo)
        {
            int prevIndentation = -1;
            bool done = false;
            lineNo = lineNo - 1;
            var rule = XFormattingRule.GetFirstRuleByStart(keyword);
            while (lineNo >= 0 && !done)
            {
                if (_document.GetKeyword(lineNo, out var prevKw))
                {
                    if (XFormattingRule.IsMember(prevKw))
                    {
                        prevIndentation = 0;
                        done = true;
                        if (_document.GetTokens(lineNo, out var tokens) && tokens[0].Type == XSharpLexer.WS)
                        {
                            prevIndentation = GetIndentTokenLength(tokens[0]);
                        }
                    }
                    else if (XFormattingRule.IsType(prevKw))
                    {
                        prevIndentation = 0;
                        done = true;
                        if (_document.GetTokens(lineNo, out var tokens) && tokens[0].Type == XSharpLexer.WS)
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
        #endregion
#region Token Handling

        private IList<IToken> GetTokensInLine(ITextSnapshotLine line)
        {
            IList<IToken> tokens ;
            if (_document.SnapShot.Version == _buffer.CurrentSnapshot.Version)
            {
                // Ok, use it
                if (_document.GetTokens(line.LineNumber, out tokens))
                {
                    return tokens;
                }
            }
            // Ok, do it now
            var text = line.GetText();
            tokens = GetTokens(text);
            return tokens;
            //
        }
        private IList<IToken> GetTokens(string text)
        {
            var tokens = new List<IToken>();
            try
            {
                string fileName;
                if (_file != null)
                {
                    fileName = _file.FullPath;
                }
                else
                {
                    fileName = "MissingFile.prg";
                }
                var reporter = new ErrorIgnorer();
                bool ok = XSharp.Parser.VsParser.Lex(text, fileName, this.ParseOptions, reporter, out ITokenStream tokenStream);
                var stream = tokenStream as BufferedTokenStream;
                foreach (var token in stream.GetTokens())
                {
                    tokens.Add(token);
                }
                return tokens;
            }
            catch (Exception e)
            {
                XSettings.LogException(e, "GetTokens");
            }
            return tokens;
        }
        private bool IsPPKeyword(int kw)
        {
            return kw >= XSharpLexer.PP_FIRST && kw <= XSharpLexer.PP_LAST;
        }
#endregion

        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableParameterLog && XSettings.EnableLogging)
            {
                XSettings.LogMessage("XSharp.Formatting:" + strMessage);
            }
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


    }
}
