using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using XSharpModel;
using static XSharp.Parser.VsParser;

namespace XSharp.LanguageService
{
    partial class XSharpFormattingCommandHandler
    {

        SourceCodeEditorSettings _settings;
        #region Keywords Definitions

        private static IList<XKeyword> _indentKeywords;
        private static IList<XKeyword> _memberKeywords;
        private static IReadOnlyDictionary<XKeyword, XKeyword> _singleMiddleKeywords;
        private static IList<XKeyword> _allowEndToken;
        private static IReadOnlyDictionary<XKeyword, XKeyword> _endKeywords;
        private static IReadOnlyDictionary<XKeyword, IList<XKeyword>> _multiMiddleKeywords;
        //private static string[] _xtraKeywords;
        #endregion


        private static void GetKeywords()
        {
            _memberKeywords = XFormattingRule.MemberKeywords();
            _singleMiddleKeywords = XFormattingRule.SingleMiddleKeywords();
            _multiMiddleKeywords = XFormattingRule.MultiMiddleKeywords();
            _endKeywords = XFormattingRule.EndKeywords();
            _allowEndToken = XFormattingRule.AllowEndKeywords();
            _indentKeywords = XFormattingRule.IndentKeywords();
        }



        private static XKeyword SearchMiddleOrEndKeyword(XKeyword keyword, out bool isMiddle)
        {
            keyword = XFormattingRule.TranslateToken(keyword);
            isMiddle = false;
            if (_singleMiddleKeywords.ContainsKey(keyword))
            {
                isMiddle = true;
                return _singleMiddleKeywords[keyword];
            }
            if (_endKeywords.ContainsKey(keyword))
            {
                isMiddle = false;
                return _endKeywords[keyword];
            }

            isMiddle = false;
            return default;
        }

        private IList<XKeyword> SearchSpecialMiddleKeyword(XKeyword keyword)
        {
            keyword = XFormattingRule.TranslateToken(keyword);
            if (_multiMiddleKeywords.ContainsKey(keyword))
            {
                return _multiMiddleKeywords[keyword];
            }
            if (keyword.IsEnd)
            {
                return _allowEndToken;
            }
            return null;
        }


        static XSharpFormattingCommandHandler()
        {
            GetKeywords();
        }



        private void CopyWhiteSpaceFromPreviousLine(ITextEdit editSession, ITextSnapshotLine line)
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
            bool alignOnPrev = false;
            int lineNumber = line.LineNumber;
            int indentation;
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
                var _ = _classifier.ClassifyWhenNeededAsync();
                var editSession = _buffer.CreateEdit();
                // This will calculate the desired indentation of the current line, based on the previous one
                // and may de-Indent the previous line if needed
                //
                try
                {
                    if (!canIndentLine(line))
                    {
                        CopyWhiteSpaceFromPreviousLine(editSession, line);
                    }
                    else
                    {
                        switch ((EnvDTE.vsIndentStyle)_settings.IndentStyle)
                        {
                            case EnvDTE.vsIndentStyle.vsIndentStyleSmart:
                                indentation = GetDesiredIndentation(line, editSession, alignOnPrev);
                                if (indentation == -1)
                                {
                                    CopyWhiteSpaceFromPreviousLine(editSession, line);
                                }
                                else
                                {
                                    // but we may need to re-Format the previous line for Casing and Identifiers
                                    // so, do it before indenting the current line.
                                    lineNumber--;
                                    ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber);
                                    if (canFormatLine(prevLine))
                                    {
                                        this.FormatLineCase(editSession, prevLine);
                                    }
                                    FormatLineIndent(editSession, line, indentation);
                                }
                                break;
                            case EnvDTE.vsIndentStyle.vsIndentStyleDefault:
                            case EnvDTE.vsIndentStyle.vsIndentStyleNone:
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

        private void FormatLineIndent(ITextEdit editSession, ITextSnapshotLine line, int desiredIndentation)
        {
            //CommandFilter.WriteOutputMessage($"FormatLineIndent({line.LineNumber + 1})");
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


        [DebuggerDisplay("{kw} {indent}")]
        internal struct blockindent
        {
            internal XKeyword kw;
            internal int indent;
            internal blockindent(XKeyword parkw, int parindent)
            {
                kw = parkw;
                indent = parindent;
            }
        }

        

        class DocFormatter
        {
            private readonly Stack<blockindent> blocks;
            private int firstDoccomment ;
            private readonly XDocument document;
            private readonly XSharpLineKeywords lineKeywords;
            private SourceCodeEditorSettings _settings;
            private int[] expectedIndent;
            private int indentSize;
            internal DocFormatter(XDocument document, SourceCodeEditorSettings settings)
            {
                blocks = new Stack<blockindent>();
                firstDoccomment = -1;
                this.document = document;
                lineKeywords = document.LineKeywords;
                _settings = settings;
            }

            private int IndentEntityStart(XKeyword kw, int number, int startIndent)
            {
                while (blocks.Count > 0)
                {
                    var top = blocks.Peek();
                    if (!XFormattingRule.IsEntityKeyword(top.kw))
                    {
                        blocks.Pop();
                    }
                    else
                    {
                        break;
                    }
                }
                if (blocks.Count > 0)
                {
                    indentSize = blocks.Peek().indent;
                    if (_settings.IndentTypeMembers)
                    {
                        if (XFormattingRule.IsMemberKeyword(kw) && XFormattingRule.IsTypeKeyword(blocks.Peek().kw))
                        {
                            indentSize += 1;
                        }

                    }

                    // pop entities from the stack that are "Global"
                    while (XFormattingRule.IsGlobalEntity(blocks.Peek().kw))
                    {
                        blocks.Pop();
                        if (blocks.Count == 0)
                        {
                            break;
                        }
                    }
                }
                else
                {
                    indentSize = startIndent;
                }
                expectedIndent[number] = indentSize;
                if (firstDoccomment != -1)
                {
                    for (int i = firstDoccomment; i < number; i++)
                    {
                        if (!document.HasLineState(i, LineFlags.Preprocessor))
                        {
                            expectedIndent[i] = indentSize;
                        }
                    }
                }
                while (number > 0 && document.HasLineState(number, LineFlags.Continued))
                {
                    number -= 1;
                    expectedIndent[number] = indentSize;
                }
                return indentSize;
            }

            void HandleStart(XKeyword kw, bool isEntity, int lineNumber)
            {
                blocks.Push(new blockindent(kw, indentSize));
                if (isEntity)
                {
                    // Only indent statements when the option is enabled
                    if (_settings.IndentStatements)
                        indentSize += 1;
                }
                else
                {
                    indentSize += 1;
                }
            }
            void HandleEnd(XKeyword kw, int lineNumber)
            {
                var isType = false;
                indentSize -= 1;
                expectedIndent[lineNumber] = indentSize;
                if (!kw.IsSingle)
                {
                    var endKw = new XKeyword(kw.Kw2);
                    isType = XFormattingRule.IsTypeKeyword(endKw);
                }
                if (isType)
                {
                    while (blocks.Count > 0 && !XFormattingRule.IsTypeKeyword(blocks.Peek().kw))
                    {
                        blocks.Pop();
                    }
                    if (blocks.Count > 0)
                    {
                        var top = blocks.Pop();
                        indentSize = top.indent;
                        expectedIndent[lineNumber] = indentSize;
                    }
                }
                else
                {
                    if (blocks.Count > 0)
                    {
                        var top = blocks.Pop();
                        indentSize = top.indent;
                        expectedIndent[lineNumber] = indentSize;
                    }
                }

            }
            void HandleMiddle(XKeyword kw, int lineNumber)
            {
                if (blocks.Count > 0)
                {
                    var top = blocks.Peek();
                    indentSize = top.indent;
                    expectedIndent[lineNumber] = indentSize;
                }
                if (kw.IsSingle)
                {
                    if (kw.Kw1 == XTokenType.Case ||
                        kw.Kw1 == XTokenType.Otherwise)
                    {
                        if (_settings.IndentCaseLabel)
                        {
                            // Indent the case label when they want that
                            indentSize += 1;
                            expectedIndent[lineNumber] = indentSize;
                        }
                        if (_settings.IndentCaseContent)
                        {
                            // Indent the case content when they want that
                            indentSize += 1;
                        }
                    }
                    else
                    {
                        // Other middle blocks should always indent
                        // EG ELSE, ELSEIF, CATCH, RECOVER, FINALLY
                        indentSize += 1;
                    }
                }
            }
            internal int[] GetIndentSizes(IEnumerable<ITextSnapshotLine> lines, int startLine, int endLine, int startIndent)
            {
                expectedIndent = new int[endLine + 1];
                indentSize = startIndent;
                foreach (var line in lines)
                {
                    var lineNumber = line.LineNumber;
                    if (lineNumber >= startLine && lineNumber <= endLine)
                    {
                        if (document.HasLineState(lineNumber, LineFlags.Preprocessor))
                        {
                            if (! _settings.IndentPreprocessorLines)
                            {
                                expectedIndent[lineNumber] = 0;
                                continue;
                            }
                        }
                        if (document.HasLineState(lineNumber, LineFlags.DocComments))
                        {
                            if (firstDoccomment == -1)
                                firstDoccomment = lineNumber;
                            continue;

                        }
                        expectedIndent[lineNumber] = indentSize;
                        if (!lineKeywords.Lines.ContainsKey(lineNumber))
                        {
                            // check for continuation
                            if (_settings.IndentContinuedLines && document.HasLineState(lineNumber, LineFlags.Continued))
                            {
                                expectedIndent[lineNumber] = indentSize + 1;
                            }
                            continue;
                        }
                        var kw = lineKeywords.Lines[lineNumber];
                        var isEntity = XFormattingRule.IsEntityKeyword(kw);
                        var isNamespace = false;
                        if (kw.Kw1 == XTokenType.Begin && kw.Kw2 == XTokenType.Namespace)
                        {
                            isNamespace = true;
                            if (_settings.IndentNamespace)
                                indentSize += 1;
                        }
                        if (isEntity || isNamespace)
                        {
                            indentSize = IndentEntityStart(kw, lineNumber, startIndent);
                            firstDoccomment = -1;
                        }
                        if (XFormattingRule.IsStartKeyword(kw))
                        {
                            HandleStart(kw, isEntity, lineNumber);
                        }
                        if (XFormattingRule.IsEndKeyword(kw) || kw.IsEnd)
                        {
                            HandleEnd(kw, lineNumber);
                        }
                        if (XFormattingRule.IsMiddleKeyword(kw))
                        {
                            HandleMiddle(kw, lineNumber);

                        }
                    }
                }
                return expectedIndent;
            }
        }

        private void FormatSpan(IEnumerable<ITextSnapshotLine> lines, int startLine, int endLine, int startIndent)
        {
            // Already been lexed ?
            ITextEdit editSession = null;
            try
            {
                var document = _buffer.GetDocument();
                var formatter = new DocFormatter(document, _settings);
                var expectedIndent = formatter.GetIndentSizes(lines, startLine, endLine, startIndent);

                // now process the lines
                editSession = _buffer.CreateEdit();
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

        #region Check Token types

        /// <summary>
        /// ML_COMMENT, SL_COMMENT, USING, PP_INCLUDE, PP_DEFINE, PP_REGION
        /// </summary>
        /// <param name="keywordType"></param>
        /// <returns></returns>

        private bool IsIgnored(int keywordType)
        {
            switch (keywordType)
            {
                // Ignoring
                case XSharpLexer.ML_COMMENT:
                case XSharpLexer.SL_COMMENT:
                case XSharpLexer.USING:
                case XSharpLexer.PP_INCLUDE:
                case XSharpLexer.PP_DEFINE:
                case XSharpLexer.PP_REGION:
                    return true;
            }
            return false;
        }

        /// <summary>
        /// CLASS, INTERFACE, STRUCTURE, VOSTRUCT, ENUM, UNION
        /// </summary>
        /// <param name="keywordType"></param>
        /// <returns></returns>
        private bool IsTypeStart(int keywordType)
        {
            // todo: use FormattingRules table
            switch (keywordType)
            {
                case XSharpLexer.CLASS:
                case XSharpLexer.INTERFACE:
                case XSharpLexer.STRUCTURE:
                case XSharpLexer.VOSTRUCT:
                case XSharpLexer.ENUM:
                case XSharpLexer.UNION:
                    return true;
            }
            return false;
        }

        /// <summary>
        /// CLASS, INTERFACE, STRUCTURE
        /// </summary>
        /// <param name="keywordType"></param>
        /// <returns></returns>
        private bool IsOpenEntityWithEndMarker(int keywordType)
        {
            switch (keywordType)
            {
                case XSharpLexer.CLASS:
                case XSharpLexer.INTERFACE:
                case XSharpLexer.STRUCTURE:
                    return true;
            }
            return false;
        }

        /// <summary>
        /// VOSTRUCT, ENUM, UNION
        /// </summary>
        /// <param name="keywordType"></param>
        /// <returns></returns>
        private bool IsOpenEntityWithOptionalEndMarker(int keywordType)
        {
            switch (keywordType)
            {
                case XSharpLexer.VOSTRUCT:
                case XSharpLexer.ENUM:
                case XSharpLexer.UNION:
                    return true;
            }
            return false;
        }

        /// <summary>
        /// DELEGATE, FUNCTION, PROCEDURE, CONSTRUCTOR, DESTRUCTOR, ASSIGN, ACCESS, METHOD, PROPERTY, OPERATOR, EVENT
        /// </summary>
        /// <param name="keywordType"></param>
        /// <returns></returns>
        private bool IsMemberStart(int keywordType)
        {
            switch (keywordType)
            {
                case XSharpLexer.DELEGATE:
                case XSharpLexer.FUNCTION:
                case XSharpLexer.PROCEDURE:
                case XSharpLexer.CONSTRUCTOR:
                case XSharpLexer.DESTRUCTOR:
                case XSharpLexer.ASSIGN:
                case XSharpLexer.ACCESS:
                case XSharpLexer.METHOD:
                case XSharpLexer.PROPERTY:
                case XSharpLexer.OPERATOR:
                case XSharpLexer.EVENT:
                    return true;
            }
            return false;
        }

        /// <summary>
        /// ADD, REMOVE
        /// </summary>
        /// <param name="keywordType"></param>
        /// <returns></returns>
        private bool IsAddOrRemove(int keywordType)
        {
            switch (keywordType)
            {
                case XSharpLexer.ADD:
                case XSharpLexer.REMOVE:
                    return true;
            }
            return false;
        }

        /// <summary>
        /// SET, GET
        /// </summary>
        /// <param name="keywordType"></param>
        /// <returns></returns>
        private bool IsSetOrGet(int keywordType)
        {
            switch (keywordType)
            {
                case XSharpLexer.SET:
                case XSharpLexer.GET:
                case XSharpLexer.INIT:
                    return true;
            }
            return false;
        }

        /// <summary>
        /// IF,  WHILE, SWITCH, REPEAT, PP_IFDEF, PP_IFNDEF, WITH, TRY
        ///</summary>
        /// <param name="keywordType"></param>
        /// <returns></returns>
        private bool IsStartOfBlock(int keywordType)
        {
            switch (keywordType)
            {
                case XSharpLexer.IF:
                case XSharpLexer.WHILE:
                case XSharpLexer.SWITCH:
                case XSharpLexer.REPEAT:
                case XSharpLexer.PP_IF:
                case XSharpLexer.PP_IFDEF:
                case XSharpLexer.PP_IFNDEF:
                case XSharpLexer.WITH:
                case XSharpLexer.TRY:
                    return true;
            }
            return false;
        }

        /// <summary>
        /// FOR, FOREACH
        ///</summary>
        /// <param name="keywordType"></param>
        /// <returns></returns>
        private bool IsForOrForeach(int keywordType)
        {
            // todo: use FormattingRules table
            switch (keywordType)
            {
                case XSharpLexer.FOR:
                case XSharpLexer.FOREACH:
                    return true;
            }
            return false;
        }

        /// <summary>
        /// ELSE, ELSEIF, FINALLY, CATCH, RECOVER, PP_ELSE
        /// </summary>
        /// <param name="keywordType"></param>
        /// <returns></returns>
        private bool IsMiddleOfBlock(int keywordType)
        {
            // todo: use FormattingRules table
            switch (keywordType)
            {
                case XSharpLexer.ELSE:
                case XSharpLexer.ELSEIF:
                case XSharpLexer.FINALLY:
                case XSharpLexer.CATCH:
                case XSharpLexer.RECOVER:
                case XSharpLexer.PP_ELSE:
                    return true;
            }
            return false;
        }

        /// <summary>
        /// ENDIF, ENDDO, ENDCASE, UNTIL, PP_ENDIF
        /// </summary>
        /// <param name="keywordType"></param>
        /// <returns></returns>
        private bool IsEndOfBlock(int keywordType)
        {
            // todo: use FormattingRules table
            switch (keywordType)
            {
                case XSharpLexer.ENDIF:
                case XSharpLexer.ENDDO:
                case XSharpLexer.ENDCASE:
                case XSharpLexer.UNTIL:
                case XSharpLexer.PP_ENDIF:
                    return true;
            }
            return false;
        }

        /// <summary>
        /// NEXT
        /// </summary>
        /// <param name="keywordType"></param>
        /// <returns></returns>
        private bool IsNext(int keywordType)
        {
            // todo: use FormattingRules table
            switch (keywordType)
            {
                case XSharpLexer.NEXT:
                    return true;
            }
            return false;
        }

        /// <summary>
        /// Case or Otherwise
        /// </summary>
        /// <param name="keywordType"></param>
        /// <returns></returns>
        private bool IsCaseOrOtherwise(int keywordType)
        {
            // todo: use FormattingRules table
            switch (keywordType)
            {
                case XSharpLexer.CASE:
                case XSharpLexer.OTHERWISE:
                    return true;
            }
            return false;
        }
        #endregion

        #region Tokens manipulations
        /// <summary>
        /// Retrieve all Tags in the Line
        /// </summary>
        /// <param name="line"></param>
        /// <returns></returns>
        private IList<IToken> GetTokensInLine(ITextSnapshotLine line)
        {
            IList<IToken> tokens = new List<IToken>();
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
                                tokens.Add(allTokens[startIndex]);
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

        private IList<IToken> GetTokensInLine(ITextSnapshot snapshot, int start, int length)
        {
            IList<IToken> tokens = new List<IToken>();
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

                            } while ((startIndex < allTokens.Count) && (allTokens[startIndex].StopIndex < lastPosition));
                            return tokens;
                        }
                    }
                }
            }
            //
            SnapshotSpan lineSpan = new SnapshotSpan(snapshot, start, length);
            var text = lineSpan.GetText();
            tokens = GetTokens(text);
            return tokens;
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
        private int GetDesiredIndentation(ITextSnapshotLine line, ITextEdit editSession, bool alignOnPrev)
        {
            WriteOutputMessage($"getDesiredIndentation({line.LineNumber + 1})");
            try
            {
                // How many spaces do we need ?
                int indentValue = 0;
                int lineNumber = line.LineNumber;
                if (lineNumber > 0)
                {
                    // We need to analyze the Previous line
                    lineNumber--;
                    ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber);
                    XKeyword keyword = GetFirstKeywordInLine(prevLine, out indentValue);
                    if (indentValue < 0)
                        indentValue = 0;
                    _lastIndentValue = indentValue;
                    if (alignOnPrev)
                        return _lastIndentValue;
                    // ok, now check what we have, starting the previous line
                    bool indentNextLine = false;
                    if (!keyword.IsEmpty)// && !doSkipped)
                    {
                        // Start of a block of code ?
                        if (_memberKeywords.Contains(keyword))
                        {
                            if (_settings.IndentTypeMembers)
                            {
                                indentValue += _settings.IndentSize;
                            }
                        }
                        else if (_indentKeywords.Contains(keyword))
                        {
                            indentValue += _settings.IndentSize;
                        }
                        else
                        {
                            // this matches ELSE with IF but also ENDIF with IF and END IF with IF
                            // isMiddle indicates if the next line needs to be indented or not
                            XKeyword startToken = SearchMiddleOrEndKeyword(keyword, out var isMiddle);
                            int outdentValue = -1;
                            if (!startToken.IsEmpty)
                            {
                                // Retrieve the Indentation for the previous line
                                outdentValue = AlignToSpecificTokens(line, new List<XKeyword> { startToken }, out var _);
                                indentNextLine = isMiddle;
                            }
                            else
                            {
                                // This is a keyword that has multiple possible first keywords
                                // such as CASE, OTHERWISE, CATCH, FINALLY
                                var startTokens = SearchSpecialMiddleKeyword(keyword);
                                if (startTokens != null)
                                {
                                    // The startToken is a list of possible tokens

                                    outdentValue = AlignToSpecificTokens(line, startTokens, out var firstKeyword);

                                    // Special handling for CASE and OTHERWISE
                                    // The SWITCH and DOCASE rules have the FormattingClags.Case set

                                    var rule = XFormattingRule.GetStartRule(firstKeyword);
                                    if (!keyword.IsEnd && !firstKeyword.IsEmpty)
                                    {
                                        if (rule.Flags.HasFlag(XFormattingFlags.Case))
                                        {
                                            if (_settings.IndentCaseLabel)
                                            {
                                                outdentValue += _settings.IndentSize;
                                            }
                                            indentNextLine = _settings.IndentCaseContent;
                                        }
                                    }

                                }

                            }
                            if (outdentValue != -1)
                            {
                                try
                                {
                                    // De-Indent previous line !!!
                                    if (canIndentLine(prevLine))
                                    {
                                        FormatLineIndent(editSession, prevLine, outdentValue);
                                    }
                                    indentValue = outdentValue;
                                    if (indentNextLine)
                                    {
                                        indentValue += _settings.IndentSize;
                                    }

                                }
                                catch (Exception ex)
                                {
                                    XSettings.LogException(ex, "Error indenting of current line ");

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

        private int AlignToSpecificTokens(ITextSnapshotLine currentLine, IList<XKeyword> tokenList, out XKeyword firstKeyword)
        {
            int indentValue = 0;
            bool found = false;
            firstKeyword = default;
            var context = new Stack<IList<XKeyword>>();
            try
            {
                // On what line are we ?
                int lineNumber = currentLine.LineNumber;
                // We need to analyze the Previous line
                lineNumber--;
                while (lineNumber > 0)
                {
                    // We need to analyze the Previous line
                    lineNumber--;
                    ITextSnapshotLine line = currentLine.Snapshot.GetLineFromLineNumber(lineNumber);
                    var tokens = GetTokensInLine(line);
                    XKeyword currentKeyword = default;
                    //
                    if (tokens.Count > 0)
                    {
                        var token = tokens[0];
                        indentValue = 0;
                        int index = 0;
                        if (token.Type == XSharpLexer.WS)
                        {
                            indentValue = GetIndentTokenLength(token);
                            index++;
                            if (index < tokens.Count)
                            {
                                token = tokens[index];
                            }
                            else
                            {
                                return -1;
                            }

                        }
                        while (XSharpLexer.IsModifier(token.Type) && index < tokens.Count - 2)
                        {
                            index += 2;
                            token = tokens[index];
                        }
                        //
                        if (XSharpLexer.IsKeyword(token.Type) || IsPPKeyword(token.Type))
                        {
                            currentKeyword = new XKeyword(token.Type);
                        }
                        if (index < tokens.Count - 2)
                        {
                            var token2 = tokens[index + 2];
                            // we are looking for start tokens
                            // so END is not included here
                            if (token.Type == XSharpLexer.DO || token.Type == XSharpLexer.BEGIN)
                            {
                                // must be followed by whitespace and another token
                                if (tokens.Count > index && XSharpLexer.IsKeyword(token2.Type))
                                {
                                    currentKeyword = new XKeyword(token.Type, token2.Type);
                                }
                            }
                            else if (token.Type == XSharpLexer.LOCAL)
                            {
                                // must be followed by whitespace and another token
                                if (tokens.Count > index && (token2.Type == XSharpLexer.FUNCTION || token2.Type == XSharpLexer.PROCEDURE))
                                {
                                    currentKeyword = new XKeyword(token.Type, token2.Type);
                                }
                            }
                        }
                        var translatedKeyword = XFormattingRule.TranslateToken(currentKeyword);
                        if (tokenList.Contains(translatedKeyword))
                        {
                            if (context.Count == 0)
                            {
                                found = true;
                                firstKeyword = currentKeyword;
                                break;
                            }
                            else
                            {
                                tokenList = context.Pop();
                            }
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

        private IList<IToken> GetTokens(string text)
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
                var reporter = new ErrorIgnorer();
                bool ok = XSharp.Parser.VsParser.Lex(text, fileName, parseoptions, reporter, out ITokenStream tokenStream);
                var stream = tokenStream as BufferedTokenStream;
                tokens = stream.GetTokens();
            }
            catch (Exception e)
            {
                XSettings.LogException(e, "GetTokens");
                tokens = new List<IToken>();
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
        /// Get the first keyword in Line. The keyword is in UPPERCASE The modifiers (Private, Protected, ... ) are ignored
        /// If the first Keyword is a Comment, "//" is returned
        /// </summary>
        /// <param name="line">The line to analyze</param>
        /// <param name="doSkipped">Bool value indicating if a "DO" keyword has been skipped</param>
        /// <param name="minIndent"></param>
        /// <returns></returns>
        private XKeyword GetFirstKeywordInLine(ITextSnapshotLine line, out int minIndent)
        {
            minIndent = -1;
            string startOfLine = line.GetText();
            XKeyword keyword = default;
            int index = 0;
            var tokens = GetTokens(startOfLine);
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
                        if (XSharpLexer.IsModifier(token.Type))
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
            return XFormattingRule.TranslateToken(keyword);
        }
        private bool IsPPKeyword(int kw)
        {
            return kw >= XSharpLexer.PP_FIRST && kw <= XSharpLexer.PP_LAST;
        }
        #endregion

        #region New Formatting process
        internal class FormattingContext
        {
            readonly IList<IToken> allTokens;
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

            internal FormattingContext(XSharpFormattingCommandHandler cf, ITextSnapshot snapshot)
            {
                allTokens = cf.GetTokensInLine(snapshot, 0, snapshot.Length);
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

            /// <summary>
            /// Move to a specific position in the Snapshot,
            /// and set to the specific Token
            /// </summary>
            /// <param name="positionToReach"></param>
            public void MoveTo(int positionToReach)
            {
                this.prevIndex = this.currentIndex;
                if (positionToReach == currentPosition)
                    return;
                //
                int newPos = binarySearch(0, allTokens.Count - 1, positionToReach);
                if (newPos > -1)
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

            /// <summary>
            /// Move to the next Token
            /// </summary>
            public void MoveToNext()
            {
                this.prevIndex = this.currentIndex;
                this.CurrentIndex++;
            }

            /// <summary>
            /// Move back to the previous position.
            /// This can be done after a MoveToNext or a GetFirstToken with andMove==true
            /// </summary>
            public void MoveBack()
            {
                this.CurrentIndex = this.prevIndex;
            }

            /// <summary>
            /// Get the first token in the current line, starting from the current position.
            /// The operation won't skip line, so it can return NULL if there are no token in the line.
            /// </summary>
            /// <param name="ignoreSpaces">Ignore Spaces</param>
            /// <param name="andMove">move the position to the found Token. Default to False</param>
            /// <returns></returns>
            public IToken GetFirstToken(bool ignoreSpaces, bool andMove = false)
            {
                IToken first = null;
                int start = 0;
                if (currentPosition > -1)
                {
                    int currentLine = allTokens[currentIndex].Line;
                    start = currentIndex;
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
                if ((first != null) && andMove)
                {
                    this.prevIndex = currentIndex;
                    this.CurrentIndex = start;
                }
                return first;
            }

            /// <summary>
            /// Get the last token in the current line
            /// </summary>
            /// <param name="ignoreSpaces">Ignore Spaces</param>
            /// <returns></returns>
            public IToken GetLastToken(bool ignoreSpaces)
            {
                IToken last = null;
                if (currentPosition > -1)
                {
                    int currentLine = allTokens[currentIndex].Line;
                    int start = currentIndex;
                    // Move to the end, and pass
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
                        last = token;
                        start++;
                    }
                }
                return last;
            }

            /// <summary>
            /// Get the first token, starting from the current position.
            /// This can move to another line.
            /// </summary>
            /// <param name="ignoreSpaces"></param>
            /// <returns></returns>
            public IToken GetToken(bool ignoreSpaces)
            {
                IToken first = null;
                if (currentPosition > -1)
                {
                    //int currentLine = allTokens[currentIndex].Line;
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

            private int binarySearch(int left, int right, int toReach)
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
                        return binarySearch(left, middle - 1, toReach);

                    // Else the element can only be present 
                    // in right subarray 
                    return binarySearch(middle + 1, right, toReach);
                }

                // We reach here when element is not present 
                // in array 
                return -1;
            }
        }

        internal class FormattingLineContext
        {
            readonly IList<XSharpToken> allLineTokens;
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
                    if (currentIndex < allLineTokens.Count && currentIndex >= 0)
                        currentPosition = allLineTokens[currentIndex].StartIndex;
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


            internal FormattingLineContext(IList<XSharpToken> tokens, XSharpDialect dialect)
            {
                allLineTokens = tokens;
                if (allLineTokens.Count > 0)
                {
                    currentIndex = 0;
                    prevIndex = 0;
                    currentPosition = allLineTokens[0].StartIndex;
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

            /// <summary>
            /// Move to the next Token
            /// </summary>
            public void MoveToNext()
            {
                this.prevIndex = this.currentIndex;
                this.CurrentIndex++;
            }

            /// <summary>
            /// Move back to the previous position.
            /// This can be done after a MoveToNext or a GetFirstToken with andMove==true
            /// </summary>
            public void MoveBack()
            {
                this.CurrentIndex = this.prevIndex;
            }

            /// <summary>
            /// Get the first token in the current line, starting from the current position.
            /// The operation won't skip line, so it can return NULL if there are no token in the line.
            /// </summary>
            /// <param name="ignoreSpaces">Ignore Spaces</param>
            /// <param name="andMove">move the position to the found Token. Default to False</param>
            /// <returns></returns>
            public IToken GetFirstToken(bool ignoreSpaces, bool andMove = false)
            {
                IToken first = null;
                int start = 0;
                if (currentPosition > -1)
                {
                    int currentLine = allLineTokens[currentIndex].Line;
                    start = currentIndex;
                    while (start < allLineTokens.Count)
                    {
                        IToken token = allLineTokens[start];
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
                if ((first != null) && andMove)
                {
                    this.prevIndex = currentIndex;
                    this.CurrentIndex = start;
                }
                return first;
            }

            /// <summary>
            /// Get the last token in the current line
            /// </summary>
            /// <param name="ignoreSpaces">Ignore Spaces</param>
            /// <returns></returns>
            public IToken GetLastToken(bool ignoreSpaces)
            {
                IToken last = null;
                if (currentPosition > -1)
                {
                    int currentLine = allLineTokens[currentIndex].Line;
                    int start = currentIndex;
                    // Move to the end, and pass
                    while (start < allLineTokens.Count)
                    {
                        IToken token = allLineTokens[start];
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

            /// <summary>
            /// Get the first token, starting from the current position.
            /// This can move to another line.
            /// </summary>
            /// <param name="ignoreSpaces"></param>
            /// <returns></returns>
            public IToken GetToken(bool ignoreSpaces)
            {
                IToken first = null;
                if (currentPosition > -1)
                {
                    //int currentLine = allTokens[currentIndex].Line;
                    int start = currentIndex;
                    while (start < allLineTokens.Count)
                    {
                        IToken token = allLineTokens[start];
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
