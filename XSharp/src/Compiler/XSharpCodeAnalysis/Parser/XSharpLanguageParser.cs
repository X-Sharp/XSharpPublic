// Uncomment this define to dump time profiling info of the parsing phases.
//#define DUMP_TIMES

using System;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal partial class XSharpLanguageParser : SyntaxParser
    {
        private readonly String _fileName;
        private readonly SourceText _text;
        private readonly CSharpParseOptions _options;
        private readonly SyntaxListPool _pool = new SyntaxListPool(); // Don't need to reset this.
        private readonly SyntaxFactoryContext _syntaxFactoryContext; // Fields are resettable.
        private readonly ContextAwareSyntax _syntaxFactory; // Has context, the fields of which are resettable.

        private ITokenStream _lexerTokenStream;

#if DEBUG
        internal class XSharpErrorListener : IAntlrErrorListener<IToken>
        {

            String _fileName;
            IList<ParseErrorData> _parseErrors;
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
                    _parseErrors.Add(new ParseErrorData(new ErrorNodeImpl(e.OffendingToken), ErrorCode.ERR_ParserError, msg));
                }
                else if (offendingSymbol != null)
                {
                    //Debug.WriteLine(_fileName + "(" + offendingSymbol.Line + "," + offendingSymbol.Column + "): error: " + msg);
                    _parseErrors.Add(new ParseErrorData(new ErrorNodeImpl(offendingSymbol), ErrorCode.ERR_ParserError, msg));
                }
                else
                {
                    //Debug.WriteLine(_fileName + "(" + line + 1 + "," + charPositionInLine + 1 + "): error: " + msg);
                    _parseErrors.Add(new ParseErrorData(ErrorCode.ERR_ParserError, msg));
                }
            }
        }
#endif

        internal XSharpLanguageParser(
            String FileName,
            SourceText Text,
            CSharpParseOptions options,
            CSharp.CSharpSyntaxNode oldTree,
            IEnumerable<TextChangeRange> changes,
            LexerMode lexerMode = LexerMode.Syntax,
            CancellationToken cancellationToken = default(CancellationToken))
            : base(/*lexer*/null, lexerMode, oldTree, changes, allowModeReset: false,
                preLexIfNotIncremental: true, cancellationToken: cancellationToken)
        {
            _syntaxFactoryContext = new SyntaxFactoryContext();
            _syntaxFactory = new ContextAwareSyntax(_syntaxFactoryContext);
            _text = Text;
            _fileName = FileName;
            _options = options;
        }

        internal CompilationUnitSyntax ParseCompilationUnit()
        {
            return ParseWithStackGuard(
                ParseCompilationUnitCore,
                () => SyntaxFactory.CompilationUnit(
                        new SyntaxList<ExternAliasDirectiveSyntax>(),
                        new SyntaxList<UsingDirectiveSyntax>(),
                        new SyntaxList<AttributeListSyntax>(),
                        new SyntaxList<MemberDeclarationSyntax>(),
                        SyntaxFactory.Token(SyntaxKind.EndOfFileToken)));
        }

        internal CompilationUnitSyntax ParseCompilationUnitCore()
        {
#if false
            SyntaxToken tmp = null;
            SyntaxListBuilder initialBadNodes = null;
            var body = new NamespaceBodyBuilder(_pool);
            try
            {
                this.ParseNamespaceBody(ref tmp, ref body, ref initialBadNodes, SyntaxKind.CompilationUnit);

                var eof = this.EatToken(SyntaxKind.EndOfFileToken);
                var result = _syntaxFactory.CompilationUnit(body.Externs, body.Usings, body.Attributes, body.Members, eof);

                if (initialBadNodes != null)
                {
                    // attach initial bad nodes as leading trivia on first token
                    result = AddLeadingSkippedSyntax(result, initialBadNodes.ToListNode());
                    _pool.Free(initialBadNodes);
                }

                return result;
            }
            finally
            {
                body.Free(_pool);
            }
#endif
            ParserRuleContext tree;
            var stream = new AntlrInputStream(_text.ToString());
            var lexer = new XSharpLexer(stream);
            var tokens = new CommonTokenStream(lexer);
#if DEBUG && DUMP_TIMES
            DateTime t = DateTime.Now;
#endif
            tokens.Fill(); // Required due to the preprocessor
#if DEBUG && DUMP_TIMES
            {
                var ts = DateTime.Now - t;
                t += ts;
                Debug.WriteLine("Lexing completed in {0}",ts);
            }
#endif
            var parseErrors = ParseErrorData.NewBag();
            var pp = new XSharpPreprocessor(tokens, _options.PreprocessorSymbols, new List<string>(_options.IncludePaths) { System.IO.Path.GetDirectoryName(_fileName) }, _text.Encoding, _text.ChecksumAlgorithm, parseErrors);
            var pp_tokens = new CommonTokenStream(pp);
            var parser = new XSharpParser(pp_tokens);
#if DEBUG
            var errorListener = new XSharpErrorListener(_fileName, parseErrors);
            parser.AddErrorListener(errorListener);
#endif
#if DEBUG && DUMP_TIMES
            pp_tokens.Fill();
            {
                var ts = DateTime.Now - t;
                t += ts;
                Debug.WriteLine("Preprocessing completed in {0}",ts);
            }
#endif
            _lexerTokenStream = pp_tokens;
            parser.ErrorHandler = new XSharpErrorStrategy();
            parser.Interpreter.PredictionMode = PredictionMode.Sll;
            try
            {
                tree = parser.source();
            }
            catch (Exception)
            {
#if DEBUG
                Debug.WriteLine("Antlr: SLL parsing failed. Trying again in LL mode.");
#endif
                pp_tokens.Reset();
                parser.Reset();
                parser.Interpreter.PredictionMode = PredictionMode.Ll;
                tree = parser.source();
            }
#if DEBUG && DUMP_TIMES
            {
                var ts = DateTime.Now - t;
                t += ts;
                Debug.WriteLine("Parsing completed in {0}",ts);
            }
#endif

            var walker = new ParseTreeWalker();

//#if DEBUG
            /* Temporary solution to prevent crashes with invalid syntax */
            if (parser.NumberOfSyntaxErrors != 0 || (parseErrors.Count != 0 && parseErrors.Contains(p => p.Code != ErrorCode.WRN_ParserWarning)))
            {
                var failedTreeTransform = new XSharpTreeTransformation(parser, _options, _pool, _syntaxFactory);
                var eof = AddParserErrors(AddFileAsTrivia(SyntaxFactory.Token(SyntaxKind.EndOfFileToken)), parseErrors);
                eof.XNode = new ErrorNodeImpl(tree.Stop);
                var result = _syntaxFactory.CompilationUnit(
                    failedTreeTransform.GlobalEntities.Externs,
                    failedTreeTransform.GlobalEntities.Usings, 
                    failedTreeTransform.GlobalEntities.Attributes, 
                    failedTreeTransform.GlobalEntities.Members, 
                    eof);
                result.XNode = (XSharpParser.SourceContext)tree;
                result.XTokens = tokens;
                return result;
            }
//#endif

            if (parser.NumberOfSyntaxErrors != 0)
            {
                var errorAnalyzer = new XSharpParseErrorAnalysis(parser);
                walker.Walk(errorAnalyzer, tree);
#if DEBUG && DUMP_TIMES
                {
                    var ts = DateTime.Now - t;
                    t += ts;
                    Debug.WriteLine("Error analysis completed in {0}",ts);
                }
#endif
            }

            var treeTransform = new XSharpTreeTransformation(parser, _options, _pool, _syntaxFactory);
            try
            {
                walker.Walk(treeTransform, tree);
                var eof = SyntaxFactory.Token(SyntaxKind.EndOfFileToken);
                var result = _syntaxFactory.CompilationUnit(
                    treeTransform.GlobalEntities.Externs, treeTransform.GlobalEntities.Usings, 
                    treeTransform.GlobalEntities.Attributes, treeTransform.GlobalEntities.Members, eof);
                // TODO nvk: add parser warnings to tree diagnostic info
                result.XNode = (XSharpParser.SourceContext)tree;
                result.XTokens = tokens;
                return result;
            }
            finally
            {
#if DEBUG && DUMP_TIMES
                {
                    var ts = DateTime.Now - t;
                    t += ts;
                    Debug.WriteLine("Tree transform completed in {0}",ts);
                }
#endif
                treeTransform.Free();
            }
        }

        internal TNode ParseWithStackGuard<TNode>(Func<TNode> parseFunc, Func<TNode> createEmptyNodeFunc) where TNode : CSharpSyntaxNode
        {
            // If this value is non-zero then we are nesting calls to ParseWithStackGuard which should not be 
            // happening.  It's not a bug but it's inefficient and should be changed.
            //Debug.Assert(_recursionDepth == 0);

#if DEBUG
            return parseFunc();
#else
            try
            {
                return parseFunc();
            }
            // TODO (DevDiv workitem 966425): Replace exception name test with a type test once the type 
            // is available in the PCL
            catch (Exception ex) when (ex.GetType().Name == "InsufficientExecutionStackException")
            {
                return CreateForGlobalFailure(_lexerTokenStream?.Lt(1)?.StartIndex ?? 0, createEmptyNodeFunc());
            }
#endif
        }

        private TNode AddFileAsTrivia<TNode>(TNode node) where TNode  : CSharpSyntaxNode
        {
            var builder = new SyntaxListBuilder(1);
            builder.Add(SyntaxFactory.BadToken(null, _text.ToString(), null));
            var fileAsTrivia = _syntaxFactory.SkippedTokensTrivia(builder.ToList<SyntaxToken>());
            node = AddLeadingSkippedSyntax(node, fileAsTrivia);
            ForceEndOfFile(); // force the scanner to report that it is at the end of the input.
            return node;
        }

        private TNode AddParserErrors<TNode>(TNode node, IEnumerable<ParseErrorData> parseErrors) where TNode : CSharpSyntaxNode
        {
            if (!parseErrors.IsEmpty())
            {
                foreach (var e in parseErrors)
                {
                    if (e.Node != null)
                        node = node.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(e.Node.Position, e.Node.FullWidth, e.Code, e.Args));
                    else
                        node = node.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(e.Code, e.Args));
                }
            }
            else
            {
                node = node.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_ParserError, "Unknown error"));
            }
            return node;
        }

        private TNode CreateForGlobalFailure<TNode>(int position, TNode node) where TNode : CSharpSyntaxNode
        {
            // Turn the complete input into a single skipped token. This avoids running the lexer, and therefore
            // the preprocessor directive parser, which may itself run into the same problem that caused the
            // original failure.
            return AddError(AddFileAsTrivia(node), position, 0, ErrorCode.ERR_InsufficientStack);
        }

    }
}