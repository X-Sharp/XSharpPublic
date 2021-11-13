using Microsoft.VisualStudio.Text;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Collections.Generic;
using Microsoft.VisualStudio.Text.Classification;
using System.Linq;
using System;
using LanguageService.CodeAnalysis.XSharp;
using XSharpModel;
using static XSharp.Parser.VsParser;
using LanguageService.CodeAnalysis.Text;

namespace XSharp.LanguageService
{
    partial class XSharpFormattingCommandHandler
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

        static XSharpFormattingCommandHandler()
        {
            getKeywords();
        }

        #endregion





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

        private bool getBufferedTokens(out XSharpTokens xTokens)
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
            //
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
                        copyWhiteSpaceFromPreviousLine(editSession, line);
                    }
                    else
                    {
                        switch ((EnvDTE.vsIndentStyle)_settings.IndentStyle)
                        {
                            case EnvDTE.vsIndentStyle.vsIndentStyleSmart:
                                indentation = getDesiredIndentation(line, editSession, alignOnPrev);
                                if (indentation == -1)
                                {
                                    copyWhiteSpaceFromPreviousLine(editSession, line);
                                }
                                else
                                {
                                    // but we may need to re-Format the previous line for Casing and Identifiers
                                    // so, do it before indenting the current line.
                                    lineNumber--;
                                    ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber);
                                    if (canFormatLine(prevLine))
                                    {
                                        this.formatLineCase(editSession, prevLine);
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
                    newIndent = new String(' ', desiredIndentation);
                }
                else
                {
                    // fill indent room with tabs and optionally also with one or more spaces
                    // if the indentsize is not the same as the tabsize
                    int numTabs = desiredIndentation / tabSize;
                    int numSpaces = desiredIndentation % tabSize;
                    newIndent = new String('\t', numTabs);
                    if (numSpaces != 0)
                    {
                        newIndent += new String(' ', numSpaces);
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
            var settings = _buffer.Properties.GetProperty<SourceCodeEditorSettings>(typeof(SourceCodeEditorSettings));
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
                if (getBufferedTokens(out var xTokens))
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
                //
                // wait until we can work
                while (_buffer.EditInProgress)
                {
                    System.Threading.Thread.Sleep(100);
                }
                // Create an Edit Sessions
                var editSession = _buffer.CreateEdit();
                try
                {
                    var lines = _buffer.CurrentSnapshot.Lines;
                    int indentSize = 0;
                    int lineContinue = 0;
                    int nextIndentSize = 0;
                    int moveAfterFormatting = 0;
                    List<Tuple<int, int>> nestedEntity = new List<Tuple<int, int>>();
                    List<ITextSnapshotLine> listDoc = new List<ITextSnapshotLine>();
                    List<ITextSnapshotLine> listMulti = new List<ITextSnapshotLine>();
                    IToken endToken = null;
                    // We are more forward, line per line
                    foreach (var snapLine in lines)
                    {
                        // Ignore Empty lines
                        if (snapLine.Length > 0)
                        {
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
                                // Certainly an Attribut, save for later indentation
                                if ((startToken.Type == XSharpLexer.LBRKT) && (endToken.Type == XSharpLexer.LINE_CONT))
                                {
                                    listMulti.Add(snapLine);
                                    continue;
                                }
                                // Not a continuing line
                                if (lineContinue == 0)
                                {
                                    indentSize = GetLineIndentation(snapLine, context, nextIndentSize, settings, out moveAfterFormatting, nestedEntity);
                                }
                            }
                            //
                            if (canFormatLine(snapLine))
                            {
                                formatLineCaseV2(context, editSession, snapLine);
                            }
                            // Do we have XMLDoc waiting ?
                            if (listDoc.Count > 0)
                            {
                                foreach (var docLine in listDoc)
                                {
                                    FormatLineIndent(editSession, docLine, indentSize * settings.IndentSize);
                                }
                                listDoc.Clear();
                            }
                            // Do we have Attributes waiting ?
                            if (listMulti.Count > 0)
                            {
                                foreach (var attrLine in listMulti)
                                {
                                    FormatLineIndent(editSession, attrLine, indentSize * settings.IndentSize);
                                }
                                listMulti.Clear();
                            }
                            // Ok, now format....
                            FormatLineIndent(editSession, snapLine, indentSize * settings.IndentSize);
                            //
                            nextIndentSize = indentSize;
                            nextIndentSize += moveAfterFormatting;
                            // The current line will continue
                            if (endToken?.Type == XSharpLexer.LINE_CONT)
                                lineContinue = 1;
                            else
                                lineContinue = 0;
                        }
                    }
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






        private void FormatDocument_backup()
        {
            WriteOutputMessage("FormatDocument() -->>");
            if (!_buffer.CheckEditAccess())
            {
                // can't edit !
                return;
            }
            var settings = _buffer.Properties.GetProperty<SourceCodeEditorSettings>(typeof(SourceCodeEditorSettings));
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
                if (getBufferedTokens(out var xTokens))
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
                //
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
                    int lineContinue = 0;
                    int nextIndentSize = 0;
                    int continueOffset = settings.IndentFactor;
                    int moveAfterFormatting = 0;
                    List<Tuple<int, int>> nestedEntity = new List<Tuple<int, int>>();
                    List<ITextSnapshotLine> listDoc = new List<ITextSnapshotLine>();
                    List<ITextSnapshotLine> listMulti = new List<ITextSnapshotLine>();
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
                                // The previous line was continuig
                                if (lineContinue == 1)
                                {
                                    // It wasn't an Attribute ?
                                    if (prevstart.Type != XSharpLexer.LBRKT)
                                    {
                                        indentSize = nextIndentSize + moveAfterFormatting;
                                    }
                                    else
                                    {
                                        lineAfterAttributes = true;
                                    }
                                }
                                else if (lineContinue > 1)
                                {
                                    indentSize = nextIndentSize + moveAfterFormatting;
                                }
                                else
                                {
                                    indentSize = GetLineIndentation(snapLine, context, nextIndentSize, settings, out moveAfterFormatting, nestedEntity);
                                }
                                prevstart = start;
                                // Line is continuing
                                if (end.Type == XSharpLexer.LINE_CONT)
                                {
                                    // It is the first continuing line
                                    if (lineContinue == 0)
                                    {
                                        // Flag line continuation
                                        lineContinue = 1;
                                        // Keep the previous Indentation
                                        nextIndentSize = indentSize;
                                    }
                                    else if (lineContinue == 1)
                                    {
                                        if (!lineAfterAttributes)
                                        {
                                            lineContinue = 2;
                                            nextIndentSize = indentSize;
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
                        FormatLineIndent(editSession, snapLine, indentSize * settings.IndentSize);
                        //
                        nextIndentSize = indentSize;
                        nextIndentSize += moveAfterFormatting;
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




        /// <summary>
        /// Calculate the indentation in characters
        /// </summary>
        private int GetLineIndentation(ITextSnapshotLine snapLine, FormattingContext context, int currentIndent, SourceCodeEditorSettings settings, out int moveAfterFormatting, List<Tuple<int, int>> nestedEntity)
        {
            // 
            moveAfterFormatting = 0;
            // Go to the begginning of the line
            context.MoveTo(snapLine.Start);
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
            switch (openKeyword.Type)
            {
                // Ignoring
                case XSharpLexer.ML_COMMENT:
                case XSharpLexer.SL_COMMENT:
                case XSharpLexer.USING:
                case XSharpLexer.PP_INCLUDE:
                case XSharpLexer.PP_DEFINE:
                case XSharpLexer.PP_REGION:
                    break;
                // Open Entity
                case XSharpLexer.CLASS:
                case XSharpLexer.INTERFACE:
                case XSharpLexer.STRUCTURE:
                case XSharpLexer.VOSTRUCT:
                case XSharpLexer.ENUM:
                    if (nestedEntity.Count() > 0)
                    {
                        current = nestedEntity.Peek();
                        switch (current.Item1)
                        {
                            case XSharpLexer.FUNCTION:
                            case XSharpLexer.PROCEDURE:
                                // Move back this opening Keyword
                                currentIndent = current.Item2;
                                nestedEntity.Pop();
                                break;
                        }
                    }
                    // Move inside this opening Keyword for the next line
                    // and indicate that as the minimum indenting size
                    moveAfterFormatting++;
                    nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    break;
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
                    if (nestedEntity.Count() > 0)
                    {
                        current = nestedEntity.Peek();
                        switch (current.Item1)
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
                                // Move back this opening Keyword
                                current = nestedEntity.Pop();
                                currentIndent = current.Item2;
                                break;
                        }
                    }
                    // Move inside this opening Keyword for the next line
                    moveAfterFormatting++;
                    nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    break;
                case XSharpLexer.ADD:
                case XSharpLexer.REMOVE:
                    if (nestedEntity.Count() > 0)
                    {
                        current = nestedEntity.Peek();
                        switch (current.Item1)
                        {
                            case XSharpLexer.EVENT:
                                // Move back this opening Keyword
                                //currentIndent--;
                                break;
                        }
                    }
                    // Move inside this opening Keyword for the next line
                    // and indicate that as the minimum indenting size
                    moveAfterFormatting++;
                    nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    break;
                case XSharpLexer.SET:
                case XSharpLexer.GET:
                    if (nestedEntity.Count() > 0)
                    {
                        current = nestedEntity.Peek();
                        switch (current.Item1)
                        {
                            case XSharpLexer.PROPERTY:
                                // Move back this opening Keyword
                                //currentIndent--;
                                break;
                        }
                    }
                    // Move inside this opening Keyword for the next line
                    // and indicate that as the minimum indenting size
                    moveAfterFormatting++;
                    nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    break;
                // Move inside these keywords
                case XSharpLexer.BEGIN:
                    // NAMESPACE ?
                    // Check the next one
                    context.MoveToNext();
                    nextKeyword = context.GetFirstToken(true);
                    if (nextKeyword != null)
                    {
                        if ( nextKeyword.Type == XSharpLexer.NAMESPACE)
                        {
                            // A NAMESPACE alwasy start in 0
                            currentIndent = 0;
                        }
                        context.MoveBack();
                        nestedEntity.Push(new Tuple<int, int>(nextKeyword.Type, currentIndent));
                    }
                    moveAfterFormatting++;
                    break;
                case XSharpLexer.DO:    // DO CASE, DO WHILE, ...
                    // Check the next one
                    context.MoveToNext();
                    nextKeyword = context.GetFirstToken(true);
                    if (nextKeyword != null)
                    {
                        context.MoveBack();
                        nestedEntity.Push(new Tuple<int, int>(nextKeyword.Type, currentIndent));
                    }
                    moveAfterFormatting++;
                    break;
                case XSharpLexer.IF:
                case XSharpLexer.FOR:
                case XSharpLexer.FOREACH:
                case XSharpLexer.WHILE:
                case XSharpLexer.SWITCH:
                case XSharpLexer.REPEAT:
                case XSharpLexer.PP_IFDEF:
                case XSharpLexer.PP_IFNDEF:
                case XSharpLexer.WITH:
                case XSharpLexer.TRY:
                    moveAfterFormatting++;
                    nestedEntity.Push(new Tuple<int, int>(openKeyword.Type, currentIndent));
                    break;
                // Move back keywords ( ELSE, ELSEIF, FINALLY, CATCH, RECOVER )
                case XSharpLexer.ELSE:
                case XSharpLexer.ELSEIF:
                //
                case XSharpLexer.FINALLY:
                case XSharpLexer.CATCH:
                //
                case XSharpLexer.RECOVER:
                //
                case XSharpLexer.PP_ELSE:
                    // Move back this opening Keyword
                    currentIndent--;
                    // Move inside this opening Keyword for the next line
                    moveAfterFormatting++;
                    break;
                // Move back keywords (or not) ( CASE, OTHERWISE )
                case XSharpLexer.CASE:
                case XSharpLexer.OTHERWISE:
                    // Some Users wants CASE/OTHERWISE to be aligned to the opening DO CASE
                    // Check for a setting
                    //if (settings.FormatAlignDoCase)
                    {
                        currentIndent--;
                    }
                    moveAfterFormatting++;
                    break;
                // Closing Keywords
                case XSharpLexer.END:
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
                            switch (nextKeyword.Type)
                            {
                                case XSharpLexer.NAMESPACE:
                                case XSharpLexer.CLASS:
                                case XSharpLexer.INTERFACE:
                                case XSharpLexer.STRUCTURE:
                                case XSharpLexer.VOSTRUCT:
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
                                    break;
                            }
                        }
                    }
                    else if (current != null)
                    {
                        switch (current.Item1)
                        {
                            case XSharpLexer.IF:
                            case XSharpLexer.WHILE:
                            case XSharpLexer.SWITCH:
                            case XSharpLexer.PP_IFDEF:
                            case XSharpLexer.PP_IFNDEF:
                            case XSharpLexer.WITH:
                            case XSharpLexer.TRY:
                            case XSharpLexer.CASE:
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
                            case XSharpLexer.CLASS:
                            case XSharpLexer.INTERFACE:
                            case XSharpLexer.STRUCTURE:
                            case XSharpLexer.VOSTRUCT:
                            case XSharpLexer.ENUM:
                            case XSharpLexer.ADD:
                            case XSharpLexer.REMOVE:
                            case XSharpLexer.SET:
                            case XSharpLexer.GET:
                                // Move back this opening Keyword
                                // Close the Entity
                                current = nestedEntity.Pop();
                                currentIndent = current.Item2;
                                break;
                        }
                    }
                    break;
                case XSharpLexer.NEXT:
                case XSharpLexer.ENDIF:
                case XSharpLexer.ENDDO:
                case XSharpLexer.ENDCASE:
                case XSharpLexer.UNTIL:
                case XSharpLexer.PP_ENDIF:
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
                    break;
            }
            // This should NOT happen
            if (currentIndent < 0)
            {
                currentIndent = 0;
            }
            // The number of needed indentation
            return currentIndent;
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
            if (getBufferedTokens(out var xTokens))
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
            tokens = getTokens(text);
            return tokens;
            //
        }

        private IList<IToken> getTokensInLine(ITextSnapshot snapshot, int start, int length)
        {
            IList<IToken> tokens = new List<IToken>();
            // Already been lexed ?
            if (getBufferedTokens(out var xTokens))
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


        #region SmartIndent

        // SmartIndent
        //private IEditorOptions _options;
        //


        /// <summary>
        /// the indentation is measured in # of characters
        /// </summary>
        /// <param name="line"></param>
        /// <param name="editSession"></param>
        /// <param name="alignOnPrev"></param>
        /// <returns></returns>
        private int getDesiredIndentation(ITextSnapshotLine line, ITextEdit editSession, bool alignOnPrev)
        {
            WriteOutputMessage($"getDesiredIndentation({line.LineNumber + 1})");
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
                    lineNumber--;
                    ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber);
                    string keyword = getFirstKeywordInLine(prevLine, out bool doSkipped, out indentValue);
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
                            if (!_settings.FormatAlignMethod)
                            {
                                indentValue += _settings.IndentSize;
                            }
                        }
                        else if (_specialCodeBlockKeywords.Contains(keyword))
                        {
                            if (!_settings.FormatAlignMethod)
                            {
                                indentValue += _settings.IndentSize;
                            }
                        }
                        else if (_indentKeywords.Contains(keyword))
                        {
                            indentValue += _settings.IndentSize;
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
                                    FormatLineIndent(editSession, prevLine, indentValue);
                                }
                                catch (Exception ex)
                                {
                                    WriteOutputMessage("Indentation of previous line failed");
                                    XSettings.DisplayException(ex);
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
                                    if (!_settings.FormatAlignDoCase)
                                    {
                                        indentValue += _settings.IndentSize;
                                    }
                                }
                                else
                                {
                                    // We could have "special" middle keyword : CASE or OTHERWISE
                                    startToken = searchSpecialMiddleKeyword(keyword);
                                    if (startToken != null)
                                    {
                                        // The startToken is a list of possible tokens
                                        specialIndentValue = alignToSpecificTokens(line, new List<string>(startToken.Split(new char[] { ',' })));
                                        // The can be aligned to SWITCH/DO CASE or indented
                                        if (!_settings.FormatAlignDoCase)
                                        {
                                            specialIndentValue += _settings.IndentSize;
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
                                        FormatLineIndent(editSession, prevLine, specialIndentValue);
                                    }
                                    indentValue = specialIndentValue + _settings.IndentSize;
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
                lineNumber--;
                while (lineNumber > 0)
                {
                    // We need to analyze the Previous line
                    lineNumber--;
                    ITextSnapshotLine line = currentLine.Snapshot.GetLineFromLineNumber(lineNumber);
                    var tokens = getTokensInLine(line);
                    string currentKeyword;
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
            WriteOutputMessage("FormatDocumentV2() -->>");
            if (!_buffer.CheckEditAccess())
            {
                // can't edit !
                return;
            }
            var settings = _buffer.Properties.GetProperty<SourceCodeEditorSettings>(typeof(SourceCodeEditorSettings));
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
                if (getBufferedTokens(out var xTokens))
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
                        if (startTokenType == -1)
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
                    int continueOffset = settings.IndentSize * settings.IndentFactor;
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
                                    indentSize = getDesiredIndentationInDocumentV2(context, snapLine, regions, out inComment, settings);
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
                        FormatLineIndent(editSession, snapLine, indentSize);

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

        //private int getLineLengthV2(ITextSnapshot snapshot, int start)
        //{
        //    int length = 0;
        //    bool found = false;
        //    char car;
        //    int currrentPos = start;
        //    int pos = 0;
        //    bool mayContinue = false;
        //    char[] newLine = Environment.NewLine.ToCharArray();
        //    do
        //    {
        //        car = snapshot[currrentPos];
        //        if (car == newLine[pos])
        //        {
        //            if (pos == newLine.Length - 1)
        //            {
        //                if (!mayContinue)
        //                {
        //                    found = true;
        //                    break;
        //                }
        //                pos = 0;
        //            }
        //            else
        //                pos++;
        //        }
        //        else
        //        {
        //            if (car == ';')
        //                mayContinue = true;
        //            else
        //                mayContinue = false;
        //            pos = 0;
        //        }
        //        //
        //        currrentPos++;
        //        if (currrentPos >= snapshot.Length)
        //        {
        //            break;
        //        }
        //    } while (!found);
        //    //
        //    if (found)
        //    {
        //        length = (currrentPos - start + 1) - newLine.Length;
        //    }
        //    return length;
        //}

        private int getDesiredIndentationInDocumentV2(FormattingContext context, ITextSnapshotLine snapLine, List<Region> regions, out bool inComment, SourceCodeEditorSettings settings)
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
                        WriteOutputMessage("FormatDocument : Error when moving in Tokens");
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
                                    WriteOutputMessage("FormatDocument : Error when moving in Tokens");
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
                    if (settings.FormatAlignDoCase)
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
                                    WriteOutputMessage("FormatDocument : Error when moving in Tokens");
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
                            if (XSharpLexer.IsComment(startTokenType))
                                continue;
                            break;
                    }
                    // We are between the opening Keyword and the closing Keyword
                    if (!settings.FormatAlignMethod)
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
                    if (!settings.FormatAlignMethod)
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
                    if (!settings.FormatAlignDoCase)
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
            return (indentValue * settings.IndentSize) + mlCmtSpaces;
        }

        private void formatLineCaseV2(FormattingContext context, ITextEdit editSession, ITextSnapshotLine line)
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
}
