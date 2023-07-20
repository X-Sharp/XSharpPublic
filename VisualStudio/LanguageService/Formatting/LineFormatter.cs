//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//------------------------------------------------------------------------------

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using System;
using System.Collections.Generic;
using System.Linq;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.LanguageService
{
    internal class LineFormatter
    {
        readonly XDocument _document;
        readonly ITextBuffer _buffer;
        readonly XFile _file;
        SourceCodeEditorSettings Settings => _buffer.GetSettings();
        private static int _lastIndentValue;    // in number of characters
        // session buffer for tokens per line
        readonly Dictionary<int, IList<IToken>> _tokenBuffer;
        readonly Dictionary<int, int> _lineIndent;
        internal LineFormatter(ITextBuffer buffer)
        {
            _buffer = buffer;
            _document = buffer.GetDocument();
            _file = buffer.GetFile();
            _tokenBuffer = new Dictionary<int, IList<IToken>>();
            _lineIndent = new Dictionary<int, int>();
        }

        private XKeyword GetFirstKeywordInLine(int lineNo)
        {
            var line = _buffer.CurrentSnapshot.GetLineFromLineNumber(lineNo);
            return GetFirstKeywordInLine(line, out _, out _);
        }
        private XKeyword GetFirstKeywordInLine(ITextSnapshotLine line, out int minIndent, out IList<IToken> tokens)
        {
            // When line is a continued line, then walk back until the first line that is not a continued line
            minIndent = 0;
            XKeyword keyword = default;
            int lineNo = line.LineNumber;
            while (lineNo > 0 && LineIsContinuation(lineNo))
            {
                lineNo--;
            }
            if (lineNo != line.LineNumber)
            {
                line = line.Snapshot.GetLineFromLineNumber(lineNo);
            }
            tokens = _document.GetTokensInSingleLine(line, true);
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
            var tokens = _document.GetTokensInSingleLine(line, true);
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
            _lineIndent.Clear();
            int lineNumber = line.LineNumber;
            var previousLine = line.Snapshot.GetLineFromLineNumber(lineNumber - 1);
            if (CanChangeLine(previousLine))
            {
                var prevIndent = CalculateIndentForPreviousLine(previousLine);
                SetIndentation(editSession, previousLine, prevIndent);
                _lineIndent[lineNumber - 1] = prevIndent;
                this.FormatLineCase(editSession, previousLine);
            }
            // calculate the indentation from the new current line
            if (line.Length == 0 || CanChangeLine(line))
            {
                var indentation = GetDesiredIndentation(line);
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
            // to detect that we check the linestate in the document
            if (line.Length == 0)
                return;
            if (_document.HasLineState(line.LineNumber, LineFlags.MultiLineComments))
                return;
            var tokens = _document.GetTokensInSingleLine(line, false);
            IToken lastToken = null;
            foreach (var token in tokens)
            {
                FormatToken(editSession, token, lastToken);
                lastToken = token;
            }
        }

        internal void FormatLineIndent(ITextEdit editSession, ITextSnapshotLine line, int desiredIndentation)
        {
            var settings = Settings;
            int tabSize = settings.TabSize;
            bool useSpaces = settings.TabsAsSpaces;
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

        private bool SyncKeywordCase => Settings.KeywordCase != KeywordCase.None;
        private void FormatToken(ITextEdit editSession,  IToken token, IToken lastToken)
        {
            if (token.Channel == XSharpLexer.Hidden ||
                token.Channel == XSharpLexer.PREPROCESSORCHANNEL ||
                token.Type == XSharpLexer.TEXT_STRING_CONST)
                return;
            bool syncKeyword = false;
            bool syncID = false;
            switch (token.Type)
            {
                case XSharpLexer.ID:
                    if (lastToken != null)
                    {
                        switch (lastToken.Type)
                        {
                            // do not sync case in Fields, Methods, Namespaces
                            // These require a lookup in the Database.
                            // We can do that later if we want.
                            case XSharpLexer.COLON:
                            case XSharpLexer.DOT:
                                syncID = false;
                                break;
                            case XSharpLexer.WS:
                            default:
                                syncID = XEditorSettings.IdentifierCase;
                                break;
                        }
                    }
                    else
                    {
                        syncID = XEditorSettings.IdentifierCase;
                    }
                    break;
                case XSharpLexer.UDC_KEYWORD:                   // This is a keyword but we handle
                    syncKeyword = XEditorSettings.UDCKeywordCase;
                    break;
                //case XSharpLexer.NAMEOF:
                //case XSharpLexer.SIZEOF:
                //case XSharpLexer.TYPEOF:
                //    // these are keywords but should be excluded I think
                //    syncKeyword = false;
                //    break;

                case XSharpLexer.TRUE_CONST:        // Constant
                case XSharpLexer.FALSE_CONST:       // Constant
                case XSharpLexer.MACRO:             // __TERM__
                case XSharpLexer.LOGIC_AND:         // Operator
                case XSharpLexer.LOGIC_OR:          // Operator
                case XSharpLexer.LOGIC_NOT:         // Operator
                case XSharpLexer.LOGIC_XOR:         // Operator
                case XSharpLexer.VO_AND:            // Operator
                case XSharpLexer.VO_OR:             // Operator
                case XSharpLexer.VO_NOT:            // Operator
                case XSharpLexer.VO_XOR:            // Operator
                    syncKeyword = SyncKeywordCase;
                    break;
                default:
                    if (token.Type >= XSharpLexer.FIRST_NULL && token.Type <= XSharpLexer.LAST_NULL)
                    {
                        syncKeyword = SyncKeywordCase;
                    }
                    else if (XSharpLexer.IsPPKeyword(token.Type))
                    {
                        syncKeyword = false;
                    }
                    else if (XSharpLexer.IsKeyword(token.Type))
                    {
                        syncKeyword = SyncKeywordCase;
                    }
                    break;
            }
            if (syncKeyword)
            {
                var keyword = token.Text;
                var transform = XLiterals.FormatKeyword(keyword, Settings.KeywordCase);
                if (string.Compare(transform, keyword) != 0)
                {
                    editSession.Replace(token.StartIndex, transform.Length, transform);
                }
            }
            if (syncID)
            {
                var id = token.Text;
                var transform = id;
                if (_document.Identifiers.ContainsKey(id))
                {
                    var list = _document.Identifiers[id];
                    transform = list.First().Text;
                }
                if (string.Compare(transform, id) != 0)
                {
                    editSession.Replace(token.StartIndex, transform.Length, transform);
                }
            }
        }

        #endregion
        #region Calculate Indention
        private int CalculateIndentForPreviousLine(ITextSnapshotLine line)
        {
            // to calculate the indent of the previous line
            // we may have to walk a few lines back in the buffer

            var lineNo = line.LineNumber;
            int prevIndentation = 0;

            while (lineNo >= 0)
            {
                XKeyword kw = GetFirstKeywordInLine(lineNo);

                if (kw.IsEntity() || kw.IsAttribute())
                {
                    prevIndentation = CalculateIndentForLine(kw, lineNo);
                    break;
                }
                if (kw.IsStart())
                {
                    prevIndentation = CalculateIndentForLine(kw, lineNo);
                    break;
                }
                else if (kw.IsStop())
                {
                    prevIndentation = CalculateOutdentForLine(kw, lineNo);
                    break;
                }
                else if (kw.IsMiddle())
                {
                    prevIndentation = CalculateMiddleIndentForLine(kw, lineNo);
                    break;
                }
                else
                {
                    // Derive indent from previous line
                    if (lineNo > 0)
                    {
                        prevIndentation = GetDesiredIndentationAfterLine(lineNo - 1);
                        if (prevIndentation != 0)
                            break;
                    }
                    else
                    {
                        prevIndentation = 0;
                    }
                }
                lineNo -= 1;
            }

            return prevIndentation;
        }
        bool AllEmptyTokens(IList<IToken> tokens)
        {
            foreach (var token in tokens)
            {
                switch (token.Type)
                {
                    case XSharpLexer.WS:
                    case XSharpLexer.EOS:
                    case XSharpLexer.Eof:
                    case XSharpLexer.SL_COMMENT:
                    case XSharpLexer.ML_COMMENT:
                    case XSharpLexer.DOC_COMMENT:
                        continue;
                    default:
                        return false;
                }
            }
            return true;
        }
        bool LineIsContinuation(int lineNo)
        {
            if (_document.HasLineState(lineNo, LineFlags.IsContinued))
            {
                return !LineIsContinuationFromAttribute(lineNo);
            }
            return false;
        }
        bool LineIsContinuationFromAttribute(int lineNo)
        {
            while (lineNo > 0)
            {
                lineNo -= 1;
                if (_document.HasLineState(lineNo, LineFlags.StartsWithAttribute))
                    return true;
                if (!_document.HasLineState(lineNo, LineFlags.IsContinued))
                    return false;
            }
            return false;
        }

        private int GetDesiredIndentationAfterLine(int prevLineNo)
        {
            if (prevLineNo < 0)
                return 0;
            int curLine = prevLineNo + 1;
            var line = _buffer.CurrentSnapshot.GetLineFromLineNumber(prevLineNo);
            var indentValue = GetLineIndent(prevLineNo);
            if (_document.HasLineState(prevLineNo, LineFlags.StartsWithAttribute))
                return indentValue;
            var prevLineKeyword = GetFirstKeywordInLine(line, out _, out var prevTokens);
            var settings = Settings;
            var rule = XFormattingRule.GetFirstRuleByStart(prevLineKeyword);
            if (_document.HasLineState(curLine, LineFlags.IsContinued))
            {
                // when this was already a continued line then we do not need to add another tab
                if (LineIsContinuation(prevLineNo))
                {
                    return indentValue;
                }
                // when we are the first continued line and the option is enabled
                // then indent one tab
                if (settings.IndentContinuedLines)
                    return indentValue + settings.IndentSize;
                // otherwise keep same indent as previous line
                return indentValue;
            }
            // if the previous line was an indentation then we have to unindent
            // but not if that was a line after a attribute

            if (LineIsContinuation(prevLineNo) &&
                settings.IndentContinuedLines &&
                !LineIsContinuationFromAttribute(prevLineNo))
            {
                indentValue -= settings.IndentSize;
                return indentValue;
            }
            if (rule != null && rule.Flags.IsSingleLine())
            {
                ; // do nothing
            }
            else if (prevLineKeyword.IsEntity())
            {
                // inside a type we may want to indent members
                if (prevLineKeyword.IsType() && settings.IndentTypeMembers)
                {
                    indentValue += settings.IndentSize;
                }
                // inside a member we may want to indent statements
                else if (prevLineKeyword.IsMember() && settings.IndentStatements)
                {
                    indentValue += settings.IndentSize;
                }
                return indentValue;
            }
            else if (prevLineKeyword.IsStart() || prevLineKeyword.IsMiddle())
            {
                // after a start or middle keyword we increment the indentation
                indentValue += settings.IndentSize;
                // Namespace is an exception. This depends on a setting
                if (prevLineKeyword.Kw2 == XTokenType.Namespace && !settings.IndentNamespace)
                {
                    indentValue -= settings.IndentSize;
                }
                // Check to see if we need to adjust CASE / OTHERWISE
                if (prevLineKeyword.Kw1 == XTokenType.Case ||
                    prevLineKeyword.Kw1 == XTokenType.Otherwise)
                {
                    if (!settings.IndentCaseContent)
                    {
                        indentValue -= settings.IndentSize;
                    }
                }
                return indentValue;
            }
            else if (prevLineKeyword.IsStop())
            {
                // Align to the same column as the end keyword
                return indentValue;
            }
            else if (AllEmptyTokens(prevTokens))
            {
                // Read the previous line until we find something that we can use
                return GetDesiredIndentationAfterLine(prevLineNo - 1);
            }
            // if we are right after the last line of a group of continued lines then we want
            // to copy the indent value of the first of this group of lines
            // this last line is not continued, but its predecessors are
            while (prevLineNo >= 0 && LineIsContinuation(prevLineNo))
            {
                prevLineNo -= 1;
            }
            // get the indent of the right line
            indentValue = GetLineIndent(prevLineNo);
            return indentValue;
        }

        private int GetDesiredIndentation(ITextSnapshotLine line)
        {
            WriteOutputMessage($"getDesiredIndentation({line.LineNumber + 1})");
            try
            {
                // How many spaces do we need ?
                //int indentValue = 0;
                //bool mustIndentAfterPreviousLine = false;
                int lineNumber = line.LineNumber;
                if (lineNumber > 0)
                {
                    int indentValue = GetDesiredIndentationAfterLine(lineNumber - 1);
                    _lastIndentValue = indentValue;
                    return indentValue;
                }
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "SmartIndent.GetDesiredIndentation failed");
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
        private int GetLineIndent(int line)
        {
            int indent = 0;
            if (_lineIndent.ContainsKey(line))
                return _lineIndent[line];
            var tokens = _document.GetTokensInLineAndFollowing(line);
            if (tokens.Count > 0 && tokens[0].Type == XSharpLexer.WS)
            {
                indent = GetIndentTokenLength(tokens[0]);
            }
            _lineIndent[line] = indent;
            return indent;
        }
        private int GetIndentTokenLength(IToken token)
        {
            int len = 0;
            var settings = Settings;
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
                                var mod = len % settings.TabSize;
                                len = len - mod + settings.IndentSize;
                                space = false;
                            }
                            else
                            {
                                len += settings.IndentSize;
                            }
                            break;
                        default:
                            // the only other token that is allowed inside a WS is an old style pragma like ~"ONLYEARLY+"
                            // these do not influence the tab position.
                            break;
                    }
                }
                int rest = len % settings.IndentSize;
                len /= settings.IndentSize;
                if (rest != 0)
                {
                    len += 1;
                }
            }
            return len * settings.IndentSize;
        }
        #endregion
        #region Indent/Outdent Line
        private int CalculateIndentForLine(XKeyword keyword, int lineNo)
        {
            int prevIndentation = 0;
            IList<IToken> tokens;
            var settings = Settings;
            if (keyword.IsEntity() || _document.HasLineState(lineNo, LineFlags.EntityStart))
            {
                if (keyword.IsType()) // Type or namespace
                {
                    // Start of a type or of a namespace
                    // walk back to the last type or namespace
                    // when we find a type, then copy its indentation
                    // when we find a namespace then indent the type when needed
                    var tempLine = lineNo - 1;
                    bool done = false;
                    prevIndentation = 0;
                    while (! done && tempLine >= 0)
                    {
                        var kw = GetFirstKeywordInLine(tempLine);
                        if (kw.IsType())    // note namespaces are also types
                        {
                            prevIndentation = GetLineIndent(tempLine);
                            var rule = XFormattingRule.GetFirstRuleByStart(kw);
                            if (rule.Flags.IsNested())
                            {
                                prevIndentation += settings.IndentSize;
                            }
                            done = true;
                            if (settings.IndentNamespace && kw.IsNamespace() )
                            {
                                prevIndentation += settings.IndentSize;
                            }

                        }
                        if (kw.IsEndOfType())
                        {
                            prevIndentation = GetLineIndent(tempLine);
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
                    var tempLineNo = lineNo - 1;
                    bool accessor = keyword.IsAccessor();
                    bool done = false;
                    prevIndentation = 0;
                    while (!done && tempLineNo >= 0)
                    {
                        var kw = GetFirstKeywordInLine(tempLineNo);
                        if (kw.IsMember())
                        {
                            prevIndentation = GetLineIndent(tempLineNo);
                            if (accessor)
                            {
                                prevIndentation += settings.IndentSize;
                            }
                            done = true;
                        }
                        else if (kw.IsType())
                        {
                            tokens = _document.GetTokensInLineAndFollowing(tempLineNo);
                            if (tokens.Count > 0)
                            {
                                prevIndentation = GetLineIndent(tempLineNo);
                                if (settings.IndentTypeMembers)
                                {
                                    prevIndentation += settings.IndentSize;
                                }
                            }
                            done = true;
                        }//
                        else if (kw.IsEnd)
                        {
                            prevIndentation = GetLineIndent(tempLineNo);
                            done = true;
                        }
                        tempLineNo -= 1;
                    }
                }
                else if (keyword.IsEndOfType())
                {
                    var tempLine = lineNo - 1;
                    prevIndentation = GetLineIndent(tempLine);
                    // align with the END <type> keyword
                }
            }
            else
            {
                prevIndentation = GetDesiredIndentationAfterLine(lineNo - 1);

            }
            return prevIndentation;
        }
        private int CalculateOutdentForLine(XKeyword keyword, int lineNo)
        {
            // find starting rules that we should match with
            var rules = XFormattingRule.GetEndRules(keyword);
            int prevIndentation = -1;
            bool done = false;
            lineNo = lineNo - 1;
            int nested = 0;
            var settings = Settings;
            while (lineNo >= 0 && !done)
            {
                if (!LineIsContinuation(lineNo))
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
                                if (!rule.Flags.IsMiddle())
                                    nested--;
                            }
                            else
                            {
                                done = true;
                                prevIndentation = GetLineIndent(lineNo);
                                if (settings.IndentCaseLabel && rule.Flags.IsMiddle() && rule.Flags.IsCase())
                                {
                                    prevIndentation += settings.IndentSize;
                                    break;
                                }
                            }
                        }
                    }
                }
                lineNo -= 1;
            }
            return prevIndentation;
        }

        private int CalculateMiddleIndentForLine(XKeyword keyword, int lineNo)
        {
            int prevIndentation = -1;
            var rules = XFormattingRule.GetMiddleRules(keyword);
            bool done = false;
            var settings = Settings;
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
                        prevIndentation = GetLineIndent(lineNo);
                        if (settings.IndentCaseLabel && rule.Flags.IsCase())
                        {
                            prevIndentation += settings.IndentSize;
                        }
                        break;
                    }
                }
                lineNo -= 1;
            }
            return prevIndentation;
        }

        #endregion




        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableParameterLog && XSettings.EnableLogging)
            {
                Logger.Information("XSharp.Formatting:" + strMessage);
            }
        }



    }
}
