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
using XSharp.LanguageService.Formatting;

namespace XSharp.LanguageService
{
    partial class XSharpFormattingCommandHandler
    {

        SourceCodeEditorSettings _settings;
        #region Keywords Definitions

        private static IList<XToken> _indentKeywords;
        private static IList<XToken> _memberKeywords;
        private static IReadOnlyDictionary<XToken, XToken> _middleKeywords;
        private static IList<XToken> _allowEndToken;
        private static IReadOnlyDictionary<XToken, XToken> _endKeywords;
        private static IReadOnlyDictionary<XToken, IList<XToken>> _specialKeywords;
        //private static string[] _xtraKeywords;
        #endregion
        

        private static void GetKeywords()
        {
                _memberKeywords = FormattingRule.MemberKeywords();
            _middleKeywords = FormattingRule.MiddleKeywords();
            _specialKeywords = FormattingRule.SpecialMiddleKeywords();
            _endKeywords = FormattingRule.EndKeywords();
            _allowEndToken = FormattingRule.AllowEndKeywords();
            _indentKeywords = FormattingRule.IndentKeywords();
        }


        private static XToken SearchMiddleKeyword(XToken keyword, out bool isMiddle)
        {
            keyword = FormattingRule.TranslateToken(keyword);
            isMiddle = false;
            if (_middleKeywords.ContainsKey(keyword))
            {
                isMiddle = true;
                return _middleKeywords[keyword];
            }
            if (_endKeywords.ContainsKey(keyword))
            {
                isMiddle = false;
                return _endKeywords[keyword];
            }
            
            isMiddle = false;
            return default ;
        }

        private IList<XToken> SearchSpecialMiddleKeyword(XToken keyword)
        {
            keyword = FormattingRule.TranslateToken(keyword);
            if (_specialKeywords.ContainsKey(keyword))
            {
                return _specialKeywords[keyword];
            }
            if (keyword.isEnd)
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

        private bool GetBufferedTokens(out XSharpTokens xTokens)
        {
            if (_buffer.Properties != null && _buffer.Properties.TryGetProperty(typeof(XSharpTokens), out xTokens))
            {
                return xTokens != null && xTokens.Complete;
            }
            xTokens = null;
            return false;
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

            int originalIndentLength = lineLength - line.GetText().TrimStart().Length;
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
                    Span indentSpan = new Span(line.Start.Position, originalIndentLength);
                    editSession.Replace(indentSpan, newIndent);
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
                _classifier.ClassifyWhenNeeded();

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

        private void FormatSpan_Next(IEnumerable<ITextSnapshotLine> lines, int startLine, int endLine, int startIndent)
        {

            XSharpTokens xTokens = null;
            // Already been lexed ?
            if (_buffer.Properties != null && _buffer.Properties.TryGetProperty(typeof(XSharpTokens), out xTokens))
            {
                if ((xTokens == null) || !xTokens.Complete)
                {
                    WriteOutputMessage("FormatSpan : no Tokens in the current buffer.");
                    return;
                }
            }
            var xLines = xTokens.Lines;
            // Retrieve the current settings
            SourceCodeEditorSettings settings = null;
            if (!_buffer.Properties.TryGetProperty<SourceCodeEditorSettings>(typeof(SourceCodeEditorSettings), out settings))
            {
                WriteOutputMessage("FormatSpan : no Settings in the current buffer.");
                return;
            }
            XSharpLineState linesState = null;
            if (!_buffer.Properties.TryGetProperty<XSharpLineState>(typeof(XSharpLineState), out linesState))
            {
                WriteOutputMessage("FormatSpan : no LineState in the current buffer.");
                return;
            }
            // Create an Edit Session
            var editSession = _buffer.CreateEdit();
            try
            {
                // Init to -1, so first lineNumber is 0
                int lineNumber = -1;
                int indentSize = startIndent;
                int lineContinue = 0;
                int nextIndentSize = 0;
                int multiIndentSize = 0;
                int moveAfterFormatting = 0;
                int moveContinuingLine = 0;
                List<Tuple<int, int>> nestedEntity = new List<Tuple<int, int>>();
                List<ITextSnapshotLine> listDoc = new List<ITextSnapshotLine>();
                List<ITextSnapshotLine> listAttributes = new List<ITextSnapshotLine>();
                List<ITextSnapshotLine> listMulti = new List<ITextSnapshotLine>();
                IToken endToken = null;
                IList<XSharpToken> lineTokens = null;
                // We are more forward, line per line
                foreach (var snapLine in lines)
                {
                    // The current Line Number (from 0 to LineCount-1)
                    lineNumber++;
                    ///// used by FormatSelection
                    //if (lineNumber < startLine)
                    //    continue;
                    //if (lineNumber > endLine)
                    //    break;
                    /////
                    // Ignore Empty lines
                    if (snapLine.Length > 0)
                    {
                        var lineState = linesState.GetFlags(lineNumber);
                        // XML Doc will be re-indented when we find the corresponding entity
                        if (lineState.HasFlag(LineFlags.DocComments))
                        {
                            listDoc.Add(snapLine);
                            continue;
                        }
                        if (!lineState.HasFlag(LineFlags.SingleLineComments))
                        {
                            if (!xLines.TryGetValue(lineNumber, out lineTokens))
                                continue;
                            FormattingLineContext context = new FormattingLineContext(lineTokens, this.ParseOptions.Dialect);
                            // Get the first Token on line
                            IToken startToken = context.GetFirstToken(true);
                            if (startToken != null)
                            {
                                // Token is NewLine ? Skip
                                if (startToken.Type == XSharpLexer.NL)
                                    continue;
                                // How does the line end ?
                                endToken = context.GetLastToken(true);

                                // Certainly an Attribut, save for later indentation
                                if ((startToken.Type == XSharpLexer.LBRKT) && (endToken.Type == XSharpLexer.LINE_CONT))
                                {
                                    listAttributes.Add(snapLine);
                                    continue;
                                }
                                // Line continuation, save for later indentation
                                if (lineContinue == 1)
                                {
                                    listMulti.Add(snapLine);
                                    if (endToken.Type != XSharpLexer.LINE_CONT)
                                        lineContinue = 0;
                                    continue;
                                }
                                // Not a continuing line
                                if (lineContinue == 0)
                                {
                                    indentSize = GetLineIndentation_Next(context, nextIndentSize, settings, out moveAfterFormatting, out moveContinuingLine, nestedEntity);
                                    if (lineState.HasFlag(LineFlags.SingleLineEntity))
                                        moveAfterFormatting = 0;
                                }
                            }
                            //
                            if ((snapLine.LineNumber >= startLine) && (snapLine.LineNumber <= endLine))
                                if (canFormatLine(snapLine))
                                {
                                    FormatLineCase_Next(context, editSession, snapLine);
                                }
                            // Do we have XMLDoc waiting ?
                            if (listDoc.Count > 0)
                            {
                                foreach (var docLine in listDoc)
                                {
                                    if ((docLine.LineNumber >= startLine) && (docLine.LineNumber <= endLine))
                                        FormatLineIndent(editSession, docLine, indentSize * settings.IndentSize);
                                }
                                listDoc.Clear();
                            }
                            // Do we have Attributes waiting ?
                            if (listAttributes.Count > 0)
                            {
                                foreach (var attrLine in listAttributes)
                                {
                                    if ((attrLine.LineNumber >= startLine) && (attrLine.LineNumber <= endLine))
                                        FormatLineIndent(editSession, attrLine, indentSize * settings.IndentSize);
                                }
                                listAttributes.Clear();
                            }
                            // Do we have some line Continuation waiting ?
                            if (listMulti.Count > 0)
                            {
                                foreach (var multiLine in listMulti)
                                {
                                    if ((multiLine.LineNumber >= startLine) && (multiLine.LineNumber <= endLine))
                                        FormatLineIndent(editSession, multiLine, multiIndentSize * settings.IndentSize);
                                }
                                listMulti.Clear();
                            }
                        }
                        // Ok, now format....
                        if ((snapLine.LineNumber >= startLine) && (snapLine.LineNumber <= endLine))
                            FormatLineIndent(editSession, snapLine, indentSize * settings.IndentSize);
                        //
                        nextIndentSize = indentSize;
                        nextIndentSize += moveAfterFormatting;
                        // The current line will continue
                        if (endToken?.Type == XSharpLexer.LINE_CONT)
                        {
                            lineContinue = 1;
                            if (settings.IndentMultiLines)
                                multiIndentSize = indentSize + moveContinuingLine;
                            else
                                multiIndentSize = indentSize;
                        }
                        else
                        {
                            lineContinue = 0;
                        }
                    }
                }
            }
            catch (Exception e)
            {
                WriteOutputMessage("FormatSpan : error " + e.Message);
            }
            finally
            {
                // Validate the Edit Session ?
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

        private void FormatSpan(IEnumerable<ITextSnapshotLine> lines, int startLine, int endLine, int startIndent)
        {
            FormattingContext context = null;
            // Already been lexed ?
            if (GetBufferedTokens(out var xTokens))
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
            // No Tokens....Ok, do the lexing now
            if (context == null)
                context = new FormattingContext(this, _buffer.CurrentSnapshot);
            // Retrieve the current settings
            var settings = _buffer.Properties.GetProperty<SourceCodeEditorSettings>(typeof(SourceCodeEditorSettings));
            XSharpLineState linesState = null;
            if (!_buffer.Properties.TryGetProperty<XSharpLineState>(typeof(XSharpLineState), out linesState))
            {
                WriteOutputMessage("FormatSpan : no LineState in the current buffer.");
                return;
            }
            // Create an Edit Session
            var editSession = _buffer.CreateEdit();
            try
            {
                // Init to -1, so first lineNumber is 0
                int lineNumber = -1;
                int indentSize = startIndent;
                int lineContinue = 0;
                int nextIndentSize = 0;
                int multiIndentSize = 0;
                int moveAfterFormatting = 0;
                int moveContinuingLine = 0;
                List<Tuple<int, int>> nestedEntity = new List<Tuple<int, int>>();
                List<ITextSnapshotLine> listDoc = new List<ITextSnapshotLine>();
                List<ITextSnapshotLine> listAttributes = new List<ITextSnapshotLine>();
                List<ITextSnapshotLine> listMulti = new List<ITextSnapshotLine>();
                IToken endToken = null;
                // We are more forward, line per line
                foreach (var snapLine in lines)
                {
                    // The current Line Number (from 0 to LineCount-1)
                    lineNumber++;
                    ///// used by FormatSelection
                    //if (lineNumber < startLine)
                    //    continue;
                    //if (lineNumber > endLine)
                    //    break;
                    /////
                    // Ignore Empty lines
                    if (snapLine.Length > 0)
                    {
                        var lineState = linesState.GetFlags(lineNumber);
                        context.MoveTo(snapLine.Start);
                        // Get the first Token on line
                        IToken startToken = context.GetFirstToken(true);
                        if (startToken != null)
                        {
                            // Token is NewLine ? Skip
                            if (startToken.Type == XSharpLexer.NL)
                                continue;
                            // How does the line end ?
                            endToken = context.GetLastToken(true);
                            // XML Doc will be re-indented when we find the corresponding entity
                            if (startToken.Type == XSharpLexer.DOC_COMMENT)
                            {
                                listDoc.Add(snapLine);
                                continue;
                            }
                            // Certainly an Attribute, save for later indentation
                            if ((startToken.Type == XSharpLexer.LBRKT) && (endToken.Type == XSharpLexer.LINE_CONT))
                            {
                                listAttributes.Add(snapLine);
                                continue;
                            }
                            // Line continuation, save for later indentation
                            if (lineContinue == 1)
                            {
                                listMulti.Add(snapLine);
                                if (endToken.Type != XSharpLexer.LINE_CONT)
                                    lineContinue = 0;
                                continue;
                            }
                            // Not a continuing line
                            if (lineContinue == 0)
                            {
                                indentSize = GetLineIndentation(snapLine, context, nextIndentSize, settings, out moveAfterFormatting, out moveContinuingLine, nestedEntity);
                                if (lineState.HasFlag(LineFlags.SingleLineEntity))
                                    moveAfterFormatting = 0;
                            }
                        }
                        //
                        if ((snapLine.LineNumber >= startLine) && (snapLine.LineNumber <= endLine))
                            if (canFormatLine(snapLine))
                            {
                                FormatLineCase(context, editSession, snapLine);
                            }
                        // Do we have XMLDoc waiting ?
                        if (listDoc.Count > 0)
                        {
                            foreach (var docLine in listDoc)
                            {
                                if ((docLine.LineNumber >= startLine) && (docLine.LineNumber <= endLine))
                                    FormatLineIndent(editSession, docLine, indentSize * settings.IndentSize);
                            }
                            listDoc.Clear();
                        }
                        // Do we have Attributes waiting ?
                        if (listAttributes.Count > 0)
                        {
                            foreach (var attrLine in listAttributes)
                            {
                                if ((attrLine.LineNumber >= startLine) && (attrLine.LineNumber <= endLine))
                                    FormatLineIndent(editSession, attrLine, indentSize * settings.IndentSize);
                            }
                            listAttributes.Clear();
                        }
                        // Do we have some line Continuation waiting ?
                        if (listMulti.Count > 0)
                        {
                            foreach (var multiLine in listMulti)
                            {
                                if ((multiLine.LineNumber >= startLine) && (multiLine.LineNumber <= endLine))
                                    FormatLineIndent(editSession, multiLine, multiIndentSize * settings.IndentSize);
                            }
                            listMulti.Clear();
                        }
                        // Ok, now format....
                        if ((snapLine.LineNumber >= startLine) && (snapLine.LineNumber <= endLine))
                            FormatLineIndent(editSession, snapLine, indentSize * settings.IndentSize);
                        //
                        nextIndentSize = indentSize;
                        nextIndentSize += moveAfterFormatting;
                        // The current line will continue
                        if (endToken?.Type == XSharpLexer.LINE_CONT)
                        {
                            lineContinue = 1;
                            if (settings.IndentMultiLines)
                                multiIndentSize = indentSize + moveContinuingLine;
                            else
                                multiIndentSize = indentSize;
                        }
                        else
                        {
                            lineContinue = 0;
                        }
                    }
                }
            }
            catch (Exception e)
            {
                WriteOutputMessage("FormatSpan : error " + e.Message);
            }
            finally
            {
                // Validate the Edit Session ?
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

        /// <summary>
        /// Calculate the indentation in characters
        /// 
        /// </summary>
        /// <param name="snapLine">The current line</param>
        /// <param name="context">The Formatting context with all Tokens</param>
        /// <param name="currentIndent">The currentIndex</param>
        /// <param name="settings">The VS Settings</param>
        /// <param name="moveAfterFormatting">The number of Indentation to apply AFTER the returned value is applied</param>
        /// <param name="nestedEntity">a List of opened Entities</param>
        /// <returns>The number of Indentation to apply</returns>
        private int GetLineIndentation(ITextSnapshotLine snapLine, FormattingContext context, int currentIndent, SourceCodeEditorSettings settings, out int moveAfterFormatting, out int moveContinuingLine, List<Tuple<int, int>> nestedEntity)
        {
            //
            // todo: use FormattingRules table
            moveAfterFormatting = 0;
            moveContinuingLine = 0;
            try
            {

                // Go to the beginning of the line
                context.MoveTo(snapLine.Start);
                IToken openKeyword = context.GetFirstToken(true, true);
                IToken nextKeyword = null;
                Tuple<int, int> current;
                if (openKeyword == null)
                {
                    WriteOutputMessage("FormatDocument : Error when moving in Tokens");
                    return 0; // This should never happen
                }
                while (XSharpLexer.IsModifier(openKeyword.Type))
                {
                    // Check the next one
                    context.MoveToNext();
                    openKeyword = context.GetFirstToken(true, true);
                    if (openKeyword == null)
                    {
                        WriteOutputMessage("FormatDocument : Error when moving in Tokens");
                        return currentIndent; // This should never happen
                    }
                }
                //int startTokenType = openKeyword.Type;
                // DEFINE CLASS in VFP
                if (openKeyword.Type == XSharpLexer.DEFINE)
                {
                    if (context.Dialect == XSharpDialect.FoxPro)
                    {
                        // Check the next one
                        context.MoveToNext();
                        openKeyword = context.GetFirstToken(true);
                        if (openKeyword == null)
                        {
                            WriteOutputMessage("FormatDocument : Error when moving in Tokens");
                            return currentIndent; // This should never happen
                        }
                        if (openKeyword.Type != XSharpLexer.CLASS)
                            context.MoveBack();
                    }
                }
                if (!IsIgnored(openKeyword.Type))
                {
                    var xToken = new XToken(openKeyword.Type);
                    FormattingRule rule = FormattingRule.GetStartRule(xToken);
                    if (IsTypeStart(openKeyword.Type))
                    {
                        // Open Entity
                        // We are inside something ?
                        if (nestedEntity.Count() > 0)
                        {
                            current = nestedEntity.Peek();
                            if (IsMemberStart(current.Item1) || IsOpenEntityWithOptionalEndMarker(current.Item1))
                            {
                                // Move back this opening Keyword
                                currentIndent = current.Item2;
                                nestedEntity.Pop();
                            }
                        }
                        // Move inside this opening Keyword for the next line
                        // and indicate that as the minimum indenting size
                        if (settings.IndentEntityContent)
                            moveAfterFormatting++;
                        nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    }
                    else if (IsMemberStart(openKeyword.Type))
                    {
                        if (nestedEntity.Count() > 0)
                        {
                            current = nestedEntity.Peek();
                            if (IsMemberStart(current.Item1) || !IsOpenEntityWithEndMarker(current.Item1))
                            {
                                // Move back this opening Keyword
                                current = nestedEntity.Pop();
                                currentIndent = current.Item2;
                            }
                        }
                        // Does this line continue ?
                        if (settings.IndentMultiLines)
                        {
                            var endToken = context.GetLastToken(true);
                            if (endToken.Type == XSharpLexer.LINE_CONT)
                            {
                                moveContinuingLine++;
                            }
                        }
                        // Move inside this opening Keyword for the next line
                        if (settings.IndentBlockContent)
                            moveAfterFormatting++;
                        nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    }
                    else if (IsAddOrRemove(openKeyword.Type))
                    {
                        if (nestedEntity.Count() > 0)
                        {
                            current = nestedEntity.Peek();
                            if (current.Item1 == XSharpLexer.EVENT)
                            {
                                // Move back this opening Keyword
                                //currentIndent--;
                            }
                        }
                        // Move inside this opening Keyword for the next line
                        // and indicate that as the minimum indenting size
                        moveAfterFormatting++;
                        nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    }
                    else if (IsSetOrGet(openKeyword.Type))
                    {
                        if (nestedEntity.Count() > 0)
                        {
                            current = nestedEntity.Peek();
                            if (current.Item1 == XSharpLexer.PROPERTY)
                            {
                                // Move back this opening Keyword
                                //currentIndent--;
                            }
                        }
                        // Move inside this opening Keyword for the next line
                        // and indicate that as the minimum indenting size
                        moveAfterFormatting++;
                        nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    }
                    else if (openKeyword.Type == XSharpLexer.BEGIN)
                    {
                        // NAMESPACE ?
                        // Check the next one
                        context.MoveToNext();
                        nextKeyword = context.GetFirstToken(true);
                        if (nextKeyword != null)
                        {
                            if (nextKeyword.Type == XSharpLexer.NAMESPACE)
                            {
                                // A NAMESPACE always start in 0
                                currentIndent = 0;
                            }
                            context.MoveBack();
                            nestedEntity.Push(new Tuple<int, int>(nextKeyword.Type, currentIndent));
                        }
                        moveAfterFormatting++;
                    }
                    else if (openKeyword.Type == XSharpLexer.DO)
                    {
                        // DO CASE, DO WHILE, ...
                        // Check the next one
                        context.MoveToNext();
                        nextKeyword = context.GetFirstToken(true);
                        if (nextKeyword != null)
                        {
                            context.MoveBack();
                            nestedEntity.Push(new Tuple<int, int>(nextKeyword.Type, currentIndent));
                        }
                        if ((nextKeyword.Type == XSharpLexer.CASE) && (settings.IndentCaseLabel))
                            moveAfterFormatting++;
                    }
                    else if (IsStartOfBlock(openKeyword.Type) || IsForOrForeach(openKeyword.Type))
                    {
                        // SWITCH is here as a start of block, DO CASE as it's own start of block
                        if (openKeyword.Type == XSharpLexer.SWITCH)
                        {
                            if (settings.IndentCaseLabel)
                                moveAfterFormatting++;
                        }
                        else
                        {
                            moveAfterFormatting++;
                            // Does this line continue ?
                            if (settings.IndentMultiLines)
                            {
                                var endToken = context.GetLastToken(true);
                                if (endToken.Type == XSharpLexer.LINE_CONT)
                                {
                                    moveContinuingLine++;
                                }
                            }
                        }
                        nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    }
                    else if (IsMiddleOfBlock(openKeyword.Type))
                    {
                        // Move back this opening Keyword
                        currentIndent--;
                        // Move inside this opening Keyword for the next line
                        moveAfterFormatting++;
                    }
                    else if (IsCaseOrOtherwise(openKeyword.Type))
                    {
                        // Move back keywords (or not) ( CASE, OTHERWISE )
                        // Some Users wants CASE/OTHERWISE to be aligned to the opening DO CASE
                        current = null;
                        // we CANNOT have a CASE/OTHERWISE alone....
                        if (nestedEntity.Count() > 0)
                        {
                            // This one should be a CASE or SWITCH
                            current = nestedEntity.Peek();
                            // This is the indentation of the "container"
                            currentIndent = current.Item2;
                            // Check for a setting
                            if (settings.IndentCaseLabel)
                                currentIndent++;
                            if (settings.IndentCaseContent)
                                moveAfterFormatting++;
                        }
                    }
                    else if (openKeyword.Type == XSharpLexer.END)
                    {
                        // Closing Keywords
                        // What about END CLASS, END NAMESPACE, END VOSTRUCT,
                        current = null;
                        if (nestedEntity.Count() > 0)
                        {
                            current = nestedEntity.Peek();
                        }
                        // Check the next one
                        context.MoveToNext();
                        nextKeyword = context.GetFirstToken(true);
                        if ((nextKeyword != null) && (current != null))
                        {
                            context.MoveBack();
                            if ((current.Item1 == nextKeyword.Type) ||
                               ((current.Item1 == XSharpLexer.WHILE) && (nextKeyword.Type == XSharpLexer.DO)))
                            {
                                // Move back this opening Keyword
                                // Close the Entity
                                current = nestedEntity.Pop();
                                currentIndent = current.Item2;
                            }
                            else
                            {
                                if ((nextKeyword.Type == XSharpLexer.NAMESPACE) || IsTypeStart(nextKeyword.Type))
                                {
                                    // Do we have such block Type before in the list ?
                                    int found = nestedEntity.FindLastIndex((pair) => pair.Item1 == nextKeyword.Type);
                                    if (found > -1)
                                    {
                                        while (nestedEntity.Count - 1 >= found)
                                        {
                                            // Move back this opening Keyword
                                            // Close the Entity
                                            current = nestedEntity.Pop();
                                            currentIndent = current.Item2;
                                        }
                                    }
                                }
                            }
                        }
                        else if (current != null)
                        {
                            // todo: use FormattingRules table
                            if (IsStartOfBlock(current.Item1) || IsMemberStart(current.Item1) ||
                                IsTypeStart(current.Item1) || IsAddOrRemove(current.Item1) ||
                                IsSetOrGet(current.Item1) || IsCaseOrOtherwise(current.Item1))

                            {
                                // Move back this opening Keyword
                                // Close the Entity
                                current = nestedEntity.Pop();
                                currentIndent = current.Item2;
                            }
                        }
                    }
                    else if (IsNext(openKeyword.Type) || IsEndOfBlock(openKeyword.Type))
                    {
                        // Move the Keyword back
                        if (nestedEntity.Count() > 0)
                        {
                            current = nestedEntity.Peek();
                            // todo: use FormattingRules table
                            if (((current.Item1 == XSharpLexer.FOR) && (openKeyword.Type == XSharpLexer.NEXT)) ||
                                 ((current.Item1 == XSharpLexer.FOREACH) && (openKeyword.Type == XSharpLexer.NEXT)) ||
                                 ((current.Item1 == XSharpLexer.IF) && (openKeyword.Type == XSharpLexer.ENDIF)) ||
                                 ((current.Item1 == XSharpLexer.WHILE) && (openKeyword.Type == XSharpLexer.ENDDO)) ||
                                 ((current.Item1 == XSharpLexer.CASE) && (openKeyword.Type == XSharpLexer.ENDCASE)) ||
                                 ((current.Item1 == XSharpLexer.REPEAT) && (openKeyword.Type == XSharpLexer.UNTIL)) ||
                                 ((current.Item1 == XSharpLexer.PP_IFDEF) && (openKeyword.Type == XSharpLexer.PP_ENDIF)) ||
                                 ((current.Item1 == XSharpLexer.PP_IFNDEF) && (openKeyword.Type == XSharpLexer.PP_ENDIF))
                                 )
                            {
                                // Move back this opening Keyword
                                // Close the Entity
                                current = nestedEntity.Pop();
                                currentIndent = current.Item2;
                            }
                        }
                    }
                }
            }
            catch (Exception e)
            {
                WriteOutputMessage("GetLineIndentation : error " + e.Message);
            }
            // This should NOT happen
            if (currentIndent < 0)
            {
                currentIndent = 0;
            }
            // The number of needed indentation
            return currentIndent;
        }


        private int GetLineIndentation_Next(FormattingLineContext context, int currentIndent, SourceCodeEditorSettings settings, out int moveAfterFormatting, out int moveContinuingLine, List<Tuple<int, int>> nestedEntity)
        {
            // todo: use FormattingRules table
            moveAfterFormatting = 0;
            moveContinuingLine = 0;
            try
            {
                // Go to the beginning of the line
                IToken openKeyword = context.GetFirstToken(true, true);
                IToken nextKeyword = null;
                Tuple<int, int> current;
                if (openKeyword == null)
                {
                    WriteOutputMessage("FormatDocument : Error when moving in Tokens");
                    return 0; // This should never happen
                }
                // These must NOT change the indentation, so eat them
                int[] typeToIgnore = { XSharpLexer.PRIVATE, XSharpLexer.HIDDEN,
                                    XSharpLexer.PROTECTED, XSharpLexer.INTERNAL,
                                    XSharpLexer.PUBLIC, XSharpLexer.EXPORT,
                                    XSharpLexer.CONST, XSharpLexer.VIRTUAL, XSharpLexer.STATIC };
                while (typeToIgnore.Contains<int>(openKeyword.Type))
                {
                    // Check the next one
                    context.MoveToNext();
                    openKeyword = context.GetFirstToken(true, true);
                    if (openKeyword == null)
                    {
                        WriteOutputMessage("FormatDocument : Error when moving in Tokens");
                        return currentIndent; // This should never happen
                    }
                }
                //int startTokenType = openKeyword.Type;
                // DEFINE CLASS in VFP
                if (openKeyword.Type == XSharpLexer.DEFINE)
                {
                    if (context.Dialect == XSharpDialect.FoxPro)
                    {
                        // Check the next one
                        context.MoveToNext();
                        openKeyword = context.GetFirstToken(true);
                        if (openKeyword == null)
                        {
                            WriteOutputMessage("FormatDocument : Error when moving in Tokens");
                            return currentIndent; // This should never happen
                        }
                        if (openKeyword.Type != XSharpLexer.CLASS)
                            context.MoveBack();
                    }
                }
                //
                if (!IsIgnored(openKeyword.Type))
                {
                    if (IsTypeStart(openKeyword.Type))
                    {
                        // Open Entity
                        // We are inside something ?
                        if (nestedEntity.Count() > 0)
                        {
                            current = nestedEntity.Peek();
                            if (IsMemberStart(current.Item1) || IsOpenEntityWithOptionalEndMarker(current.Item1))
                            {
                                // Move back this opening Keyword
                                currentIndent = current.Item2;
                                nestedEntity.Pop();
                            }
                        }
                        // Move inside this opening Keyword for the next line
                        // and indicate that as the minimum indenting size
                        if (settings.IndentEntityContent)
                            moveAfterFormatting++;
                        nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    }
                    else if (IsMemberStart(openKeyword.Type))
                    {
                        if (nestedEntity.Count() > 0)
                        {
                            current = nestedEntity.Peek();
                            if (IsMemberStart(current.Item1) || !IsOpenEntityWithEndMarker(current.Item1))
                            {
                                // Move back this opening Keyword
                                current = nestedEntity.Pop();
                                currentIndent = current.Item2;
                            }
                        }
                        // Does this line continue ?
                        if (settings.IndentMultiLines)
                        {
                            var endToken = context.GetLastToken(true);
                            if (endToken.Type == XSharpLexer.LINE_CONT)
                            {
                                moveContinuingLine++;
                            }
                        }
                        // Move inside this opening Keyword for the next line
                        if (settings.IndentBlockContent)
                            moveAfterFormatting++;
                        nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    }
                    else if (IsAddOrRemove(openKeyword.Type))
                    {
                        if (nestedEntity.Count() > 0)
                        {
                            current = nestedEntity.Peek();
                            if (current.Item1 == XSharpLexer.EVENT)
                            {
                                // Move back this opening Keyword
                                //currentIndent--;
                            }
                        }
                        // Move inside this opening Keyword for the next line
                        // and indicate that as the minimum indenting size
                        moveAfterFormatting++;
                        nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    }
                    else if (IsSetOrGet(openKeyword.Type))
                    {
                        if (nestedEntity.Count() > 0)
                        {
                            current = nestedEntity.Peek();
                            if (current.Item1 == XSharpLexer.PROPERTY)
                            {
                                // Move back this opening Keyword
                                //currentIndent--;
                            }
                        }
                        // Move inside this opening Keyword for the next line
                        // and indicate that as the minimum indenting size
                        moveAfterFormatting++;
                        nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    }
                    else if (openKeyword.Type == XSharpLexer.BEGIN)
                    {
                        // NAMESPACE ?
                        // Check the next one
                        context.MoveToNext();
                        nextKeyword = context.GetFirstToken(true);
                        if (nextKeyword != null)
                        {
                            if (nextKeyword.Type == XSharpLexer.NAMESPACE)
                            {
                                // A NAMESPACE alwasy start in 0
                                currentIndent = 0;
                            }
                            context.MoveBack();
                            nestedEntity.Push(new Tuple<int, int>(nextKeyword.Type, currentIndent));
                        }
                        moveAfterFormatting++;
                    }
                    else if (openKeyword.Type == XSharpLexer.DO)
                    {
                        // DO CASE, DO WHILE, ...
                        // Check the next one
                        context.MoveToNext();
                        nextKeyword = context.GetFirstToken(true);
                        if (nextKeyword != null)
                        {
                            context.MoveBack();
                            nestedEntity.Push(new Tuple<int, int>(nextKeyword.Type, currentIndent));
                        }
                        if ((nextKeyword.Type == XSharpLexer.CASE) && (settings.IndentCaseLabel))
                            moveAfterFormatting++;
                    }
                    else if (IsStartOfBlock(openKeyword.Type) || IsForOrForeach(openKeyword.Type))
                    {
                        // SWITCH is here as a start of block, DO CASE as it's own start of block
                        if (openKeyword.Type == XSharpLexer.SWITCH)
                        {
                            if (settings.IndentCaseLabel)
                                moveAfterFormatting++;
                        }
                        else
                        {
                            moveAfterFormatting++;
                            // Does this line continue ?
                            if (settings.IndentMultiLines)
                            {
                                var endToken = context.GetLastToken(true);
                                if (endToken.Type == XSharpLexer.LINE_CONT)
                                {
                                    moveContinuingLine++;
                                }
                            }
                        }
                        nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    }
                    else if (IsMiddleOfBlock(openKeyword.Type))
                    {
                        // Move back this opening Keyword
                        currentIndent--;
                        // Move inside this opening Keyword for the next line
                        moveAfterFormatting++;
                    }
                    else if (IsCaseOrOtherwise(openKeyword.Type))
                    {
                        // Move back keywords (or not) ( CASE, OTHERWISE )
                        // Some Users wants CASE/OTHERWISE to be aligned to the opening DO CASE
                        current = null;
                        // we CANNOT have a CASE/OTHERWISE alone....
                        if (nestedEntity.Count() > 0)
                        {
                            // This one should be a CASE or SWITCH
                            current = nestedEntity.Peek();
                            // This is the indentation of the "container"
                            currentIndent = current.Item2;
                            // Check for a setting
                            if (settings.IndentCaseLabel)
                                currentIndent++;
                            if (settings.IndentCaseContent)
                                moveAfterFormatting++;
                        }
                    }
                    else if (openKeyword.Type == XSharpLexer.END)
                    {
                        // Closing Keywords
                        // What about END CLASS, END NAMESPACE, END VOSTRUCT,
                        current = null;
                        if (nestedEntity.Count() > 0)
                        {
                            current = nestedEntity.Peek();
                        }
                        // Check the next one
                        context.MoveToNext();
                        nextKeyword = context.GetFirstToken(true);
                        if ((nextKeyword != null) && (current != null))
                        {
                            context.MoveBack();
                            if ((current.Item1 == nextKeyword.Type) ||
                               ((current.Item1 == XSharpLexer.WHILE) && (nextKeyword.Type == XSharpLexer.DO)))
                            {
                                // Move back this opening Keyword
                                // Close the Entity
                                current = nestedEntity.Pop();
                                currentIndent = current.Item2;
                            }
                            else
                            {
                                if ((nextKeyword.Type == XSharpLexer.NAMESPACE) || IsTypeStart(nextKeyword.Type))
                                {
                                    // Do we have such block Type before in the list ?
                                    int found = nestedEntity.FindLastIndex((pair) => pair.Item1 == nextKeyword.Type);
                                    if (found > -1)
                                    {
                                        while (nestedEntity.Count - 1 >= found)
                                        {
                                            // Move back this opening Keyword
                                            // Close the Entity
                                            current = nestedEntity.Pop();
                                            currentIndent = current.Item2;
                                        }
                                    }
                                }
                            }
                        }
                        else if (current != null)
                        {
                            if (IsStartOfBlock(current.Item1) || IsMemberStart(current.Item1) ||
                                IsTypeStart(current.Item1) || IsAddOrRemove(current.Item1) ||
                                IsSetOrGet(current.Item1) || IsCaseOrOtherwise(current.Item1))

                            {
                                // Move back this opening Keyword
                                // Close the Entity
                                current = nestedEntity.Pop();
                                currentIndent = current.Item2;
                            }
                        }
                    }
                    else if (IsNext(openKeyword.Type) || IsEndOfBlock(openKeyword.Type))
                    {
                        // Move the Keyword back
                        if (nestedEntity.Count() > 0)
                        {
                            current = nestedEntity.Peek();
                            if (((current.Item1 == XSharpLexer.FOR) && (openKeyword.Type == XSharpLexer.NEXT)) ||
                                 ((current.Item1 == XSharpLexer.FOREACH) && (openKeyword.Type == XSharpLexer.NEXT)) ||
                                 ((current.Item1 == XSharpLexer.IF) && (openKeyword.Type == XSharpLexer.ENDIF)) ||
                                 ((current.Item1 == XSharpLexer.WHILE) && (openKeyword.Type == XSharpLexer.ENDDO)) ||
                                 ((current.Item1 == XSharpLexer.CASE) && (openKeyword.Type == XSharpLexer.ENDCASE)) ||
                                 ((current.Item1 == XSharpLexer.REPEAT) && (openKeyword.Type == XSharpLexer.UNTIL)) ||
                                 ((current.Item1 == XSharpLexer.PP_IFDEF) && (openKeyword.Type == XSharpLexer.PP_ENDIF)) ||
                                 ((current.Item1 == XSharpLexer.PP_IFNDEF) && (openKeyword.Type == XSharpLexer.PP_ENDIF))
                                 )
                            {
                                // Move back this opening Keyword
                                // Close the Entity
                                current = nestedEntity.Pop();
                                currentIndent = current.Item2;
                            }
                        }
                    }
                }
            }
            catch (Exception e)
            {
                WriteOutputMessage("GetLineIndentation : error " + e.Message);
            }
            // This should NOT happen
            if (currentIndent < 0)
            {
                currentIndent = 0;
            }
            // The number of needed indentation
            return currentIndent;
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
            if (GetBufferedTokens(out var xTokens))
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
            if (GetBufferedTokens(out var xTokens))
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
                    XToken keyword = GetFirstKeywordInLine(prevLine, out indentValue);
                    if (indentValue < 0)
                        indentValue = 0;
                    _lastIndentValue = indentValue;
                    if (alignOnPrev)
                        return _lastIndentValue;
                    // ok, now check what we have, starting the previous line
                    bool indentNextLine = false;
                    if (!keyword.isEmpty)// && !doSkipped)
                    {
                        // Start of a block of code ?
                        if (_memberKeywords.Contains(keyword))
                        {
                            if (_settings.IndentEntityContent)
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
                            XToken startToken = SearchMiddleKeyword(keyword, out var isMiddle);
                            int outdentValue = -1;
                            if (!startToken.isEmpty)
                            {
                                // Retrieve the Indentation for the previous line
                                outdentValue = AlignToSpecificTokens(line, new List<XToken> { startToken }, out var _);
                                indentNextLine = isMiddle;
                            }
                            else
                            {
                                // This is a keyword that has multiple possible first keywords
                                var startTokens = SearchSpecialMiddleKeyword(keyword);
                                if (startTokens != null)
                                {
                                    // The startToken is a list of possible tokens
                                    outdentValue = AlignToSpecificTokens(line, startTokens, out var firstKeyword);

                                    // Special handling for CASE and OTHERWISE
                                    // The SWITCH and DOCASE rules have the FormattingClags.Case set

                                    var rule = FormattingRule.GetStartRule(firstKeyword);
                                    if (!keyword.isEnd && !firstKeyword.isEmpty && rule.isEmpty)
                                    {
                                        if (rule.Flags.HasFlag(FormattingFlags.Case))
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
                                    WriteOutputMessage("Error indenting of current line ");
                                    XSettings.DisplayException(ex);
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
                WriteOutputMessage("SmartIndent.GetDesiredIndentation failed: ");
                XSettings.DisplayException(ex);
            }
            return _lastIndentValue;
        }

        private int AlignToSpecificTokens(ITextSnapshotLine currentLine, IList<XToken> tokenList, out XToken firstKeyword)
        {
            int indentValue = 0;
            bool found = false;
            firstKeyword = default;
            var context = new Stack<IList<XToken>>();
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
                    XToken currentKeyword;
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
                        }
                        if (tokens.Count > index)
                        {
                            token = tokens[index];
                        }
                        //
                        currentKeyword = new XToken(token.Type);
                        if (index < tokens.Count - 2)
                        {
                            var token2 = tokens[index + 2];
                            if (token.Type == XSharpLexer.DO)
                            {
                                // must be followed by whitespace and another token
                                if (tokens.Count > index && XSharpLexer.IsKeyword(token2.Type))
                                {
                                    currentKeyword = new XToken(token.Type, token2.Type);
                                }
                            }
                            else if (token.Type == XSharpLexer.BEGIN)
                            {
                                // must be followed by whitespace and another token
                                if (tokens.Count > index && XSharpLexer.IsKeyword(token2.Type))
                                {
                                    currentKeyword = new XToken(token.Type, token2.Type);
                                }
                            }
                        }
                        currentKeyword = FormattingRule.TranslateToken(currentKeyword);
                        if (tokenList.Contains(currentKeyword))
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
                        // Here we should also check for nested construct or we might get false positive...
                        //List<XToken> outdentTokens;
                        //if ((outdentTokens = searchSpecialOutdentKeyword(currentKeyword)) != null)
                        //{
                        //    context.Push(tokenList);
                        //    tokenList = outdentTokens;
                        //}
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
                XSettings.DisplayException(e);
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
        private XToken GetFirstKeywordInLine(ITextSnapshotLine line, out int minIndent)
        {
            minIndent = -1;
            string startOfLine = line.GetText();
            XToken keyword = default;
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
                        keyword = new XToken(token.Type);
                        // check for 2 keyword tokens
                        if (index < tokens.Count - 2)
                        { 
                            var token2 = tokens[index+2];

                            if (token.Type == XSharpLexer.END && XSharpLexer.IsKeyword(token2.Type))
                            {
                                keyword = new XToken(token.Type, token2.Type);
                            }
                            else if (token.Type == XSharpLexer.DO && XSharpLexer.IsKeyword(token2.Type))
                            {
                                keyword = new XToken(token.Type, token2.Type);
                            }
                            else if (token.Type == XSharpLexer.BEGIN && XSharpLexer.IsKeyword(token2.Type))
                            {
                                keyword = new XToken(token.Type, token2.Type);
                            }
                            else if (token.Type == XSharpLexer.LOCAL 
                                && (token2.Type == XSharpLexer.PROCEDURE || token2.Type == XSharpLexer.FUNCTION))
                            {
                                keyword = new XToken(token2.Type);
                            }
                            else if (token.Type == XSharpLexer.DEFINE && ParseOptions.Dialect == XSharpDialect.FoxPro)
                            {
                                index += 2;
                                // skip modifiers between DEFINE and CLASS
                                while ((XSharpLexer.IsModifier(token2.Type) || token2.Type == XSharpLexer.WS ) && index < tokens.Count-1)
                                {
                                    index++;
                                    token2 = tokens[index];
                                }
                                if (token2.Type == XSharpLexer.CLASS)
                                {
                                    keyword = new XToken(token.Type, token2.Type);
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
            return FormattingRule.TranslateToken(keyword);
        }
        private bool IsPPKeyword(int kw)
        {
            switch (kw)
            {
                case XSharpLexer.PP_IFDEF:
                case XSharpLexer.PP_IFNDEF:
                case XSharpLexer.PP_ELSE:
                case XSharpLexer.PP_ENDIF:
                case XSharpLexer.PP_REGION:
                case XSharpLexer.PP_ENDREGION:
                    return true;
            }
            return false;
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


        private void FormatLineCase(FormattingContext context, ITextEdit editSession, ITextSnapshotLine line)
        {
            if (XSettings.DebuggerIsRunning)
            {
                return;
            }
            if (!canFormatLine(line))
            {
                return;
            }

            if (XSettings.KeywordCase == KeywordCase.None)
            {
                return;
            }
            if (line.LineNumber == getCurrentLine())
            {
                // Come back later.
                registerLineForCaseSync(line.LineNumber);
                return;
            }
            WriteOutputMessage($"formatLineCaseV2({line.LineNumber + 1})");
            //
            context.MoveTo(line.Start);
            IToken token = context.GetToken(true);
            int workOnLine = -1;
            if (token != null)
                workOnLine = token.Line;
            while (token != null)
            {
                formatToken(editSession, 0, token);
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

        private void FormatLineCase_Next(FormattingLineContext context, ITextEdit editSession, ITextSnapshotLine line)
        {
            if (XSettings.DebuggerIsRunning)
            {
                return;
            }
            if (!canFormatLine(line))
            {
                return;
            }

            if (XSettings.KeywordCase == KeywordCase.None)
            {
                return;
            }
            if (line.LineNumber == getCurrentLine())
            {
                // Come back later.
                registerLineForCaseSync(line.LineNumber);
                return;
            }
            WriteOutputMessage($"formatLineCaseV2({line.LineNumber + 1})");
            //
            IToken token = context.GetToken(true);
            int workOnLine = -1;
            if (token != null)
                workOnLine = token.Line;
            while (token != null)
            {
                formatToken(editSession, 0, token);
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
