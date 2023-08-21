//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;
using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using Microsoft.CodeAnalysis.Text;

namespace XSharp.Parser
{
    public static class VsParser
    {

        private static void GetLexerErrors(XSharpLexer lexer, BufferedTokenStream tokenStream, List<ParseErrorData> parseErrors)
        {
            // get lexer errors
            foreach (var error in lexer.LexErrors)
            {
                parseErrors.Add(error);
            }

        }

        private static void ReportErrors(List<ParseErrorData> parseErrors, IErrorListener listener)
        {
            foreach (var error in parseErrors)
            {

                string file = error.Node.SourceFileName;
                var ls = new LinePositionSpan() { Line = error.Node.Line, Column = error.Node.Column, FileName = error.Node.SourceFileName };
                var msg = ErrorFacts.GetMessage(error.Code, CultureInfo.CurrentCulture);
                if (ErrorFacts.IsWarning(error.Code))
                {
                    listener.ReportWarning(file, ls, error.Code.ToString(), msg, error.Args);
                }
                else
                {
                    listener.ReportError(file, ls, error.Code.ToString(), msg, error.Args);
                }
            }
        }

        private static bool LexerHelper(string sourceText, string fileName, CSharpParseOptions options,
            List<ParseErrorData> parseErrors, out ITokenStream tokens, out BufferedTokenStream ppStream, out List<string> includeFiles)
        {
            ppStream = null;
            includeFiles = null;
            var lexer = XSharpLexer.Create(sourceText, fileName, options);
            lexer.Options = options;
            var stream = lexer.GetTokenStream();
            tokens = stream;
            stream.Fill();
            GetLexerErrors(lexer, stream, parseErrors);

            // do we need to preprocess?
            // we are not interested in the pp output but we want the pp to modify the tokens from the lexer
            // so we can show UDC keywords and inactive PP regions
            if (options.ParseLevel >= ParseLevel.Parse)
            {
                if (lexer.HasPreprocessorTokens || !options.NoStdDef)
                {
                    var pp = new XSharpPreprocessor(lexer, tokens, options, fileName, Encoding.Unicode, SourceHashAlgorithm.None, parseErrors);
                    var ppTokens = pp.PreProcess();
                    ppStream = new CommonTokenStream(new XSharpListTokenSource(lexer, ppTokens));
                    includeFiles = new List<string>();
                    foreach (var file in pp.IncludedFiles)
                    {
                        includeFiles.Add(file.Key);
                    }
                }
                else
                {
                    ppStream = new CommonTokenStream(new XSharpListTokenSource(lexer, stream.GetTokens()));
                }
                ppStream.Fill();
            }
            return tokens != null;
        }
        [Obsolete("use the overload with the includeFiles parameter instead")]
        public static bool Parse(string sourceText, string fileName, CSharpParseOptions options, IErrorListener listener,
            out ITokenStream tokens, out XSharpParserRuleContext tree)
        {
            return Parse(sourceText, fileName, options, listener, out tokens, out tree, out _);
        }

        public static bool Parse(string sourceText, string fileName, CSharpParseOptions options, IErrorListener listener,
            out ITokenStream tokens, out XSharpParserRuleContext tree, out List<string> includeFiles)
        {
            tree = null;
            tokens = null;
            includeFiles = null;
            var parseErrors = ParseErrorData.NewBag();
            try
            {
                LexerHelper(sourceText, fileName, options, parseErrors, out tokens, out var ppStream, out includeFiles);
                var parser = new XSharpParser(ppStream);
                parser.Interpreter.tail_call_preserves_sll = false;     // default = true   Setting to FALSE will reduce memory used by parser
                parser.Options = options;
                tree = null;
                parser.RemoveErrorListeners();
                parser.Interpreter.PredictionMode = PredictionMode.Sll;
                parser.ErrorHandler = new BailErrorStrategy();
                try
                {
                    if (options.Dialect == XSharpDialect.FoxPro)
                    {
                        tree = parser.foxsource();
                    }
                    else
                    {
                        tree = parser.source();
                    }
                }
                catch (Exception)
                {
                    var errorListener = new XSharpErrorListener(fileName, parseErrors);
                    parser.AddErrorListener(errorListener);
                    parser.ErrorHandler = new XSharpErrorStrategy();
                    parser.Interpreter.PredictionMode = PredictionMode.Ll;
                    ppStream.Reset();
                    parser.Reset();
                    try
                    {
                        if (options.Dialect == XSharpDialect.FoxPro)
                        {
                            tree = parser.foxsource();
                        }
                        else
                        {
                            tree = parser.source();
                        }
                    }
                    catch (Exception)
                    {
                        tree = null;
                    }

                }
            }
            catch (Exception)
            {
                tree = null;
            }
            ReportErrors(parseErrors, listener);
            return tree != null;
        }
        [Obsolete("use the overload with the includeFiles parameter instead")]
        public static bool PreProcess(string sourceText, string fileName, CSharpParseOptions options, IErrorListener listener,
        out ITokenStream tokens)
        {
            return PreProcess(sourceText, fileName, options, listener, out tokens, out _);
        }


        public static bool PreProcess(string sourceText, string fileName, CSharpParseOptions options, IErrorListener listener,
                out ITokenStream tokens, out List<string> includeFiles)
        {
            tokens = null;
            includeFiles = null;
            var parseErrors = ParseErrorData.NewBag();
            try
            {
                LexerHelper(sourceText, fileName, options, parseErrors, out _, out var ppStream, out includeFiles);
                tokens = ppStream;
            }
            catch (Exception)
            {

            }
            ReportErrors(parseErrors, listener);
            return tokens != null;
        }
        [Obsolete("use the overload with the includeFiles parameter instead")]
        public static bool Lex(string sourceText, string fileName, CSharpParseOptions options, IErrorListener listener,
            out ITokenStream tokens)
        {
            return Lex(sourceText, fileName, options, listener, out tokens, out _);
        }
        public static bool Lex(string sourceText, string fileName, CSharpParseOptions options, IErrorListener listener,
            out ITokenStream tokens, out List<string> includeFiles)
        {
            tokens = null;
            includeFiles = null;
            var parseErrors = ParseErrorData.NewBag();
            try
            {
                LexerHelper(sourceText, fileName, options, parseErrors, out tokens, out _, out includeFiles);
            }
            catch (Exception)
            {
                ;
            }
            ReportErrors(parseErrors, listener);
            return tokens != null;
        }


        public interface IErrorListener
        {
            void ReportError(string fileName, LinePositionSpan span, string errorCode, string message, object[] args);
            void ReportWarning(string fileName, LinePositionSpan span, string errorCode, string message, object[] args);
        }
        internal class XSharpErrorListener : IAntlrErrorListener<IToken>
        {
            readonly String _fileName;
            readonly IList<ParseErrorData> _parseErrors;
            internal XSharpErrorListener(String FileName, IList<ParseErrorData> parseErrors) : base()
            {
                _fileName = FileName;
                _parseErrors = parseErrors;
            }

            public void SyntaxError(IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
            {
                if (e?.OffendingToken != null)
                {
                    //Debug.WriteLine(_fileName+"(" + e.OffendingToken.Line + "," + e.OffendingToken.Column + "): error: " + msg);
                    _parseErrors.Add(new ParseErrorData(e.OffendingToken, ErrorCode.ERR_ParserError, msg));
                }
                else if (offendingSymbol != null)
                {
                    //Debug.WriteLine(_fileName + "(" + offendingSymbol.Line + "," + offendingSymbol.Column + "): error: " + msg);
                    _parseErrors.Add(new ParseErrorData(offendingSymbol, ErrorCode.ERR_ParserError, msg));
                }
                else
                {
                    //Debug.WriteLine(_fileName + "(" + line + 1 + "," + charPositionInLine + 1 + "): error: " + msg);
                    _parseErrors.Add(new ParseErrorData(_fileName, ErrorCode.ERR_ParserError, msg));
                }
            }
        }

    }
}
