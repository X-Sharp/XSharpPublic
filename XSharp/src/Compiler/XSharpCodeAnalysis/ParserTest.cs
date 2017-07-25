using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using System.Collections.Generic;
using System;
namespace ParserTest
{
    class ParserTest
    {
        public static void Main()
        {
            ITokenStream stream;
            var _fileName = @"c:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\test.prg";
            IList<ParseErrorData> parseErrors = ParseErrorData.NewBag();
            var filestream = new AntlrFileStream(_fileName);
            var lexer = new XSharpLexer(filestream);
            lexer.TokenFactory = XSharpTokenFactory.Default;
            stream = new CommonTokenStream(lexer, Lexer.DefaultTokenChannel);
            var parser = new XSharpParser(stream);
            parser.IsScript = false;
            parser.AllowFunctionInsideClass = false;
            parser.AllowGarbageAfterEnd = true;
            parser.AllowNamedArgs = false;
            parser.AllowXBaseVariables = false;
            parser.RemoveErrorListeners();

            parser.Interpreter.PredictionMode = PredictionMode.Sll;
            parser.ErrorHandler = new BailErrorStrategy();
            XSharpParserRuleContext tree;
            try
            {
                tree = parser.source();
            }
            catch (ParseCanceledException)
            {
                var errorListener = new XSharpErrorListener(_fileName, parseErrors);
                parser.AddErrorListener(errorListener);
                parser.ErrorHandler = new XSharpErrorStrategy();
                parser.Interpreter.PredictionMode = PredictionMode.Ll;
                parser.Reset();
                tree = parser.source();
            }
            // find parser errors (missing tokens etc)

            foreach (var e in lexer.LexErrors)
            {
                parseErrors.Add(e);
            }
            var walker = new ParseTreeWalker();
            var errchecker = new XSharpParseErrorAnalysis(parser, parseErrors);
            walker.Walk(errchecker, tree);

            foreach (var error in parseErrors)
            {

                System.Console.Write("{0} {1} ",error.Node.Position, error.Code);
                foreach (var arg in error.Args)
                {
                    System.Console.Write(arg.ToString());
                }
                System.Console.WriteLine("");
            }
            //System.Console.WriteLine(tree.ToStringTree(parser));
        }
    }
    internal class XSharpErrorListener : IAntlrErrorListener<IToken>
    {

        string _fileName;
        IList<ParseErrorData> _parseErrors;
        internal XSharpErrorListener(string FileName, IList<ParseErrorData> parseErrors) : base()
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
                _parseErrors.Add(new ParseErrorData(ErrorCode.ERR_ParserError, msg));
            }
        }
    }

}
namespace Microsoft.CodeAnalysis
{
    interface IMessageSerializable
    {

    }
    enum InternalErrorCode
    {
        Void = 0,
        Unknown = -1,
    }

    internal static class CaseInsensitiveComparison
    {
        internal static StringComparer Comparer => StringComparer.OrdinalIgnoreCase;
    }
    namespace Syntax.InternalSyntax
    {
        class SyntaxList<T>
        {

        }
    }

    namespace CSharp
    {

         public class CSharpParseOptions
        {
        }
        class DiagnosticInfo
        {

        }
        internal static class Extensions
        {
            internal static TNode WithDiagnosticsGreen<TNode>(this TNode node, DiagnosticInfo[] diagnostics) where TNode : class
            {
                return node;
            }
            internal static IDictionary<TKey, TValue> ToImmutableDictionary<TKey, TValue>(this Dictionary<TKey, TValue> dict)
            {
                return dict;
            }
            internal static IDictionary<TKey, TValue> ToImmutableDictionary<TKey, TValue>(this Dictionary<TKey, TValue> dict, StringComparer comparer)
            {
                return dict;
            }
        }
    }

    namespace CSharp.Syntax.InternalSyntax
    {
        internal class CSharpSyntaxNode
        {
            internal object XNode { get; set; }
            internal DiagnosticInfo[] GetDiagnostics() => null;
            internal CSharpSyntaxNode WithDiagnoticsGreen<TNode> (DiagnosticInfo[] args) => this;
        }
    }
}
namespace Roslyn.Utilities
{

}

namespace System.Collections.Immutable
{

}