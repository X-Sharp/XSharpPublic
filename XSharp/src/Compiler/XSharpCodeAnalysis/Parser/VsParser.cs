using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Threading;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Tree;
using Antlr4.Runtime.Misc;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using System.Globalization;

namespace XSharp.Parser
{
    public static class VsParser
    {

        private static void GetLexerErrors(XSharpLexer lexer, BufferedTokenStream tokenStream, List<ParseErrorData> parseErrors)
        {
            // get lexer errors
            if (lexer.HasPragmas)
            {
                var pragmaTokens = tokenStream.FilterForChannel(0, tokenStream.Size - 1, XSharpLexer.PRAGMACHANNEL);
                foreach (var pragmaToken in pragmaTokens)
                {
                    parseErrors.Add(new ParseErrorData(pragmaToken, ErrorCode.WRN_PreProcessorWarning, "#pragma not (yet) supported, command is ignored"));
                }
            }
            foreach (var error in lexer.LexErrors)
            {
                parseErrors.Add(error);
            }

        }

        private static void ReportErrors(List<ParseErrorData> parseErrors, IErrorListener listener)
        {
            foreach (var error in parseErrors)
            {
                var loc = error.Node.GetLocation();
                var ls = loc.GetLineSpan().Span;
                var msg = ErrorFacts.GetMessage(error.Code, CultureInfo.CurrentCulture);
                if (ErrorFacts.IsWarning(error.Code))
                {
                    listener.ReportWarning(ls.Start.Line, ls.Start.Character, msg, error.Args);
                }
                else
                {
                    listener.ReportError(ls.Start.Line, ls.Start.Character, msg, error.Args);
                }
            }

        }

        public static bool Parse(string sourceText, string fileName, CSharpParseOptions options, IErrorListener listener, 
            out ITokenStream tokens, out XSharpParser.SourceContext tree)
        {
            tree = null;
            tokens = null;
            var parseErrors = ParseErrorData.NewBag();
            try
            {
                var lexer = XSharpLexer.Create(sourceText, fileName, options);
                lexer.AllowXBaseVariables = options.Dialect.AllowXBaseVariables();
                BufferedTokenStream tokenStream = lexer.GetTokenStream();
                tokenStream.Fill();
                tokens = (ITokenStream)tokenStream;

                GetLexerErrors(lexer, tokenStream, parseErrors);

                 // do we need to preprocess
                #region Determine if we really need the preprocessor
                bool mustPreprocess = true;
                if (lexer.HasPreprocessorTokens || !options.NoStdDef)
                {
                    // no need to pre process in partial compilation 
                    // if lexer does not contain UDCs, Messages or Includes
                    mustPreprocess = lexer.MustBeProcessed;
                }
                else
                {
                    mustPreprocess = false;

                }
                #endregion
                XSharpPreprocessor pp = null;
                BufferedTokenStream ppStream = null;
                pp = new XSharpPreprocessor(lexer, tokenStream, options, fileName, Encoding.Default, SourceHashAlgorithm.None, parseErrors);

                if (mustPreprocess)
                {
                    var ppTokens = pp.PreProcess();
                    ppStream = new CommonTokenStream(new ListTokenSource(ppTokens));
                }
                else
                {
                    // No Standard Defs and no preprocessor tokens in the lexer
                    // so we bypass the preprocessor and use the lexer token stream
                    ppStream = new CommonTokenStream(new ListTokenSource(tokenStream.GetTokens()));
                }
                ppStream.Fill();
                var parser = new XSharpParser(ppStream);
                parser.Interpreter.tail_call_preserves_sll = false;     // default = true   Setting to FALSE will reduce memory used by parser
                parser.Interpreter.reportAmbiguities = true;
                parser.AllowFunctionInsideClass = options.Dialect.AllowFunctionsInsideClass();
                parser.AllowNamedArgs = options.Dialect.AllowNamedArgs();
                parser.AllowXBaseVariables = options.Dialect.AllowXBaseVariables();
                tree = null;
                parser.RemoveErrorListeners();
                parser.Interpreter.PredictionMode = PredictionMode.Sll;
                parser.ErrorHandler = new BailErrorStrategy();
                try
                {
                    tree = parser.source();
                }
                catch (Exception)
                {
                    parser.ErrorHandler = new XSharpBailErrorStrategy(fileName, parseErrors);
                    parser.Interpreter.PredictionMode = PredictionMode.Ll;
                    ppStream.Reset();
                    parser.Reset();
                    try
                    {
                        tree = parser.source();
                    }
                    catch (Exception)
                    {
                        tree = null;
                    }

                }
            }
            catch (Exception )
            {
                tree = null;
            }
            ReportErrors(parseErrors, listener);
            return tree != null;
        }
        public static bool Lex(string sourceText, string fileName, CSharpParseOptions options, IErrorListener listener, out ITokenStream tokens)
        {
            tokens = null;
            var parseErrors = ParseErrorData.NewBag();
            try
            {
                var lexer = XSharpLexer.Create(sourceText, fileName, options);
                lexer.AllowXBaseVariables = options.Dialect.AllowXBaseVariables();
                var tokenStream = lexer.GetTokenStream();
                tokenStream.Fill();
                tokens = tokenStream; 
                GetLexerErrors(lexer, tokenStream, parseErrors);
            }
            catch (Exception)
            {
                 
            }
            ReportErrors(parseErrors, listener);
            return tokens != null;
        }


        public interface IErrorListener
        {
            void ReportError(int Line, int Column, string message, object[] args);
            void ReportWarning(int Line, int Column, string message, object[] args);
        }

    }


}
