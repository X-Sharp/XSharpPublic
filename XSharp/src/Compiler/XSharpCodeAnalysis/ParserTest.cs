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
        public static void Main(string[] args)
        {
            for (int i = 0; i < 10; i++)
            {
                Console.WriteLine("Iteration {0}", i);
                if (args.Length == 0)
                {
                    Parse(@"c:\XIDE\Prg\error.prg");
                }
                else
                {
                    foreach (string arg in args)
                    {
                        Console.WriteLine(arg);
                        var dir = System.IO.Path.GetDirectoryName(arg);
                        var mask = System.IO.Path.GetFileName(arg);
                        foreach (var file in System.IO.Directory.GetFiles(dir, mask))
                        {
                            Console.WriteLine("Parsing " + file);
                            var dt = DateTime.Now;
                            Parse(file);
                            TimeSpan took = DateTime.Now - dt;
                            Console.WriteLine("Parsing took : {0:s'.'fff}", took);
                        }
                    }
                }
            }

        }
        private static void Parse(string fileName)
        {
            ITokenStream stream;
            IList<ParseErrorData> parseErrors = ParseErrorData.NewBag();
            var filestream = new AntlrFileStream(fileName);
            var lexer = new XSharpLexer(filestream);
            lexer.TokenFactory = XSharpTokenFactory.Default;
            stream = new CommonTokenStream(lexer, Lexer.DefaultTokenChannel);
            var parser = new XSharpParser(stream);
            parser.IsScript = false;
            parser.AllowFunctionInsideClass = false;
            parser.AllowNamedArgs = false;
            parser.AllowXBaseVariables = false;
            parser.RemoveErrorListeners();

            parser.Interpreter.PredictionMode = PredictionMode.Sll;
            parser.Interpreter.reportAmbiguities = true;

            parser.Interpreter.enable_global_context_dfa = true; // default false
            parser.Interpreter.optimize_tail_calls = true;
            parser.Interpreter.tail_call_preserves_sll = true;
            //parser.Interpreter.userWantsCtxSensitive = true; // default true

            parser.ErrorHandler = new XSharpErrorStrategy();
            parser.AddErrorListener(new XSharpErrorListener(fileName, parseErrors, true));
            XSharpParserRuleContext tree;
            try
            {
                tree = parser.source();
            }
            catch (ParseCanceledException)
            {
                Console.WriteLine("Parse error, Errors from SLL mode");
                showErrors(parseErrors);
                parseErrors.Clear();
                parser.ErrorHandler = new XSharpErrorStrategy();
                parser.AddErrorListener(new XSharpErrorListener(fileName, parseErrors, true));
                parser.Interpreter.PredictionMode = PredictionMode.Ll;
                parser.Interpreter.force_global_context = true;
                parser.Interpreter.optimize_ll1 = false;
                parser.Interpreter.reportAmbiguities = true;
                parser.Reset();
                try
                {
                    tree = parser.source();
                }
                catch (Exception e)
                {
                    tree = null;
                    Console.WriteLine(e.Message);
                }
            }
            // find parser errors (missing tokens etc)

            foreach (var e in lexer.LexErrors)
            {
                parseErrors.Add(e);
            }
            var walker = new ParseTreeWalker();
            var errchecker = new XSharpParseErrorAnalysis(parser, parseErrors);
            if (tree != null)
            {
                walker.Walk(errchecker, tree);
            }
            Console.WriteLine("Parse error, Errors:");
            showErrors(parseErrors);

        }

        static void showErrors(IList<ParseErrorData> parseErrors)
        {
            foreach (var error in parseErrors)
            {
                var node = error.Node;
                XSharpToken token;
                if (node is XSharpToken)
                {
                    token = (XSharpToken)node;
                    System.Console.Write("Line {0} Column {1}", token.Line, token.Column);
                }
                else if (node is XSharpParserRuleContext)
                {
                    token = (XSharpToken)((XSharpParserRuleContext)node).Start;
                    System.Console.Write("Line {0} Column {1}", token.Line, token.Column);
                }
                else if (node is XTerminalNodeImpl)
                {
                    token = ((XTerminalNodeImpl)node).Symbol as XSharpToken;
                    System.Console.Write("Line {0} Column {1}", token.Line, token.Column);
                }
                else
                {
                    System.Console.Write(node.GetType());
                    System.Console.Write("Position {0} ", error.Node.Position);
                }
                foreach (var arg in error.Args)
                {
                    System.Console.Write(arg.ToString());
                }
                System.Console.WriteLine("");
            }
        }
    }

    internal class XSharpErrorListener : IAntlrErrorListener<IToken>
    {

        string _fileName;
        IList<ParseErrorData> _parseErrors;
        bool _cancelOnError = false;
        internal XSharpErrorListener(string FileName, IList<ParseErrorData> parseErrors, bool cancelOnError) : base()
        {
            _fileName = FileName;
            _parseErrors = parseErrors;
            _cancelOnError = true;
        }
        public void SyntaxError(IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
        {
            if (e?.OffendingToken != null)
            {
                _parseErrors.Add(new ParseErrorData(e.OffendingToken, ErrorCode.ERR_ParserError, msg));
                
            }
            else if (offendingSymbol != null)
            {
                _parseErrors.Add(new ParseErrorData(offendingSymbol, ErrorCode.ERR_ParserError, msg));
            }
            else
            {
                _parseErrors.Add(new ParseErrorData(ErrorCode.ERR_ParserError, msg));
            }
            if (_cancelOnError)
            {
                throw new ParseCanceledException(e);
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