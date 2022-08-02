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
        // session buffer for tokens per line 
        readonly Dictionary<int, IList<IToken>> _tokenBuffer; 
        internal LineFormatter(ITextBuffer buffer, SourceCodeEditorSettings settings)
        {
            _buffer = buffer;
            _document = buffer.GetDocument();
            _file = buffer.GetFile();
            _settings = settings;
            _tokenBuffer = new Dictionary<int, IList<IToken>>();
        }

        private XKeyword GetFirstKeywordInLine(int lineNo)
        {
            var line = _buffer.CurrentSnapshot.GetLineFromLineNumber(lineNo);
            return GetFirstKeywordInLine(line, out _, out _);
        }
        private XKeyword GetFirstKeywordInLine(ITextSnapshotLine line, out int minIndent, out IList<IToken> tokens)
        {
            minIndent = 0;
            XKeyword keyword = default;
            tokens = _document.GetTokensInLine(line.LineNumber);
            if (tokens.Count > 0)
            {
                keyword = XSharpLineKeywords.Tokens2Keyword(tokens);
                if (tokens[0].Type == XSharpLexer.WS)
                {
                    minIndent = GetIndentTokenLength(tokens[0]);
                }
            }
            
            return keyword;
        }

        internal bool CanChangeLine(ITextSnapshotLine line)
        {
            if (line.Length == 0)
                return true;
            var tokens = _document.GetTokensInLine(line.LineNumber);
            if (tokens.Count > 0)
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
            _tokenBuffer.Clear();
            int lineNumber = line.LineNumber;
            int prevIndent = -1;
            var previousLine = line.Snapshot.GetLineFromLineNumber(lineNumber - 1);
            if (CanChangeLine(previousLine))
            {
                this.FormatLineCase(editSession, previousLine);
                prevIndent = CalculateIndentForPreviousLine(previousLine);
                SetIndentation(editSession, previousLine, prevIndent);
            }
            // calculate the indentation from the new current line
            if (line.Length == 0 || CanChangeLine(line))
            {
                var indentation = GetDesiredIndentation(line, editSession, prevIndent);
                SetIndentation(editSession, line, indentation);
            }
            _tokenBuffer.Clear();
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
            if (line.Length == 0)
                return;
            int lineStart = line.Start.Position;
            var tokens = _document.GetTokensInLine(line.LineNumber);
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
        private int CalculateIndentForPreviousLine(ITextSnapshotLine line)
        {
            var lineNo = line.LineNumber;
            int prevIndentation = 0;
            XKeyword kw = default;
            while (lineNo >= 0)
            {
                kw = GetFirstKeywordInLine(lineNo);
                    
                if (kw.IsStart())
                {
                    prevIndentation = IndentLine(kw, lineNo);
                    break;
                }
                else if (kw.IsStop())
                {
                    prevIndentation = OutdentLine(kw, lineNo);
                    break;
                }
                else if (kw.IsMiddle())
                {
                    prevIndentation = MiddleLine(kw, lineNo);
                    break;
                }
                else
                {
                    var tokens = _document.GetTokensInLine(lineNo);
                    if (tokens.Count > 0 && tokens[0].Type == XSharpLexer.WS)
                    {
                        prevIndentation = GetIndentTokenLength(tokens[0]);
                        break;
                    }

                }
                lineNo -= 1;
            }

            return prevIndentation;
        }
     
        private int GetDesiredIndentation(ITextSnapshotLine line, ITextEdit editSession,  int prevIndent)
        {
            WriteOutputMessage($"getDesiredIndentation({line.LineNumber + 1})");
            try
            {
                // How many spaces do we need ?
                int indentValue = 0;
                bool mustIndentAfterPreviousLine = false;
                int lineNumber = line.LineNumber;
                if (lineNumber > 0)
                {
                    // Derive the indentation from the previous line
                    ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber - 1);
                    XKeyword prevLineKeyword = GetFirstKeywordInLine(prevLine, out _, out var tokens);
                    indentValue = prevIndent;
                    if (prevLineKeyword.IsStart() ||prevLineKeyword.IsMiddle())
                    {
                        mustIndentAfterPreviousLine = true;
                    }
                    if (_settings.IndentContinuedLines)
                    {
                        var lastToken = tokens.Where((t) => t.Type != XSharpLexer.EOS && t.Type != XSharpLexer.Eof).LastOrDefault();
                        if (lastToken != null && lastToken.Type == XSharpLexer.LINE_CONT)
                        {
                            indentValue += _settings.IndentSize;
                            mustIndentAfterPreviousLine = false;
                        }
                    }
                    _lastIndentValue = indentValue;
                    if (!mustIndentAfterPreviousLine)
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
                        if (prevLineKeyword.IsEntity())
                        {
                            if (prevLineKeyword.IsType() && _settings.IndentTypeMembers)
                            {
                                indentValue += _settings.IndentSize;
                            }
                            else if (prevLineKeyword.IsMember() && _settings.IndentStatements)
                            {
                                indentValue += _settings.IndentSize;
                            }
                        }
                        else if (prevLineKeyword.IsStart() || prevLineKeyword.IsMiddle())
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
            int prevIndentation = 0;
            IList<IToken> tokens;
            if (keyword.IsEntity())
            {
                if (keyword.IsType())
                {
                    // walk back to the last type or namespace
                    // when we find a type, then copy its indentation
                    // when we find a namespace then indent the type when needed
                    var tempLine = lineNo - 1;
                    bool done = false;
                    prevIndentation = 0;
                    while (! done && tempLine >= 0)
                    {
                        var kw = GetFirstKeywordInLine(tempLine);
                        if (kw.IsType())
                        {
                            tokens = _document.GetTokensInLine(tempLine);
                            if (tokens.Count > 0)
                            {
                                prevIndentation = GetIndentTokenLength(tokens[0]);
                            }
                            done = true;
                        }
                        else if (kw.Kw2 == XTokenType.Namespace && kw.Kw1 == XTokenType.Begin)
                        {
                            tokens = _document.GetTokensInLine(tempLine);
                            if (tokens.Count > 0)
                            {
                                prevIndentation = GetIndentTokenLength(tokens[0]);
                                if (_settings.IndentNamespace)
                                {
                                    prevIndentation += _settings.IndentSize;
                                }
                            }
                            done = true;
                        }
                        tempLine -= 1;
                    }
                }
                else if (keyword.IsMember())
                {
                    // walk back to last line that is also a member
                    // and copy its indentation
                    // if we find a type first then copy the indentation from the type +
                    // indent the member when needed
                    var tempLine = lineNo - 1;
                    bool done = false;
                    prevIndentation = 0;
                    while (!done && tempLine >= 0)
                    {
                        var kw = GetFirstKeywordInLine(tempLine);
                        if (kw.IsMember())
                        {
                            tokens = _document.GetTokensInLine(tempLine);
                            if (tokens.Count > 0)
                            {
                                prevIndentation = GetIndentTokenLength(tokens[0]);
                            }
                            done = true;
                        }
                        else if (kw.IsType())
                        {
                            tokens = _document.GetTokensInLine(tempLine);
                            if (tokens.Count > 0)
                            {
                                prevIndentation = GetIndentTokenLength(tokens[0]);
                                if (_settings.IndentTypeMembers)
                                {
                                    prevIndentation += _settings.IndentSize;
                                }
                            }
                            done = true;
                        }//
                        tempLine -= 1;
                    }
                }
             
            }
            else
            {
                tokens = _document.GetTokensInLine(lineNo - 1);
                if (tokens.Count > 0)
                {
                    prevIndentation = GetIndentTokenLength(tokens[0]);
                }
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
                var prevkw = GetFirstKeywordInLine(lineNo);
                if (!prevkw.IsEmpty)
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
                            var tokens = _document.GetTokensInLine(lineNo);
                            if (tokens.Count > 0  && tokens[0].Type == XSharpLexer.WS)
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
                var prevkw = GetFirstKeywordInLine(lineNo);
                if (! prevkw.IsEmpty)
                {
                    foreach (var rule in rules)
                    {
                        // does this rule end with what we expect
                        if (!prevkw.Equals(rule.Start))
                        {
                            continue;
                        }
                        done = true;
                        var tokens = _document.GetTokensInLine(lineNo);
                        if (tokens.Count > 0 && tokens[0].Type == XSharpLexer.WS)
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
        //private int MemberToken(XKeyword keyword, int lineNo)
        //{
        //    int prevIndentation = -1;
        //    bool done = false;
        //    lineNo = lineNo - 1;
        //    var rule = XFormattingRule.GetFirstRuleByStart(keyword);
        //    while (lineNo >= 0 && !done)
        //    {
        //        if (_document.GetKeyword(lineNo, out var prevKw))
        //        {
        //            if (XFormattingRule.IsMember(prevKw))
        //            {
        //                prevIndentation = 0;
        //                done = true;
        //                if (_document.GetTokens(lineNo, out var tokens) && tokens[0].Type == XSharpLexer.WS)
        //                {
        //                    prevIndentation = GetIndentTokenLength(tokens[0]);
        //                }
        //            }
        //            else if (XFormattingRule.IsType(prevKw))
        //            {
        //                prevIndentation = 0;
        //                done = true;
        //                if (_document.GetTokens(lineNo, out var tokens) && tokens[0].Type == XSharpLexer.WS)
        //                {
        //                    prevIndentation = GetIndentTokenLength(tokens[0]);
        //                    if (_settings.IndentTypeMembers)
        //                    {
        //                        prevIndentation += _settings.IndentSize;
        //                    }
        //                }
        //            }
        //        }
        //        lineNo -= 1;
        //    }
        //    return prevIndentation;
        //}
        #endregion

       
        

        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableParameterLog && XSettings.EnableLogging)
            {
                XSettings.LogMessage("XSharp.Formatting:" + strMessage);
            }
        }
       


    }
}
