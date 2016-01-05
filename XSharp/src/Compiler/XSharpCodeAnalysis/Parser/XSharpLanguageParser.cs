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
        private readonly SyntaxListPool _pool = new SyntaxListPool(); // Don't need to reset this.
        private readonly SyntaxFactoryContext _syntaxFactoryContext; // Fields are resettable.
        private readonly ContextAwareSyntax _syntaxFactory; // Has context, the fields of which are resettable.

#if DEBUG
        internal class XSharpErrorListener : IAntlrErrorListener<IToken>
        {

            String _fileName;
            internal XSharpErrorListener(String FileName) : base()
            {
                _fileName = FileName;
            }
            public void SyntaxError(IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
            {
                if (e?.OffendingToken != null)
                {
                    Debug.WriteLine(_fileName+"(" + e.OffendingToken.Line + "," + e.OffendingToken.Column + "): error: " + msg);
                }
                else if (offendingSymbol != null)
                {
                    Debug.WriteLine(_fileName + "(" + offendingSymbol.Line + "," + offendingSymbol.Column + "): error: " + msg);
                }
                else
                {
                    Debug.WriteLine(_fileName + "(" + line + 1 + "," + charPositionInLine + 1 + "): error: " + msg);
                }
            }
        }
#endif

        internal XSharpLanguageParser(
            String FileName,
            SourceText Text,
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
            var parser = new XSharpParser(tokens);
#if DEBUG
            var errorListener = new XSharpErrorListener(_fileName);
            parser.AddErrorListener(errorListener);
#endif
#if DEBUG && DUMP_TIMES
            DateTime t = DateTime.Now;
#endif
#if DEBUG && DUMP_TIMES
            {
                while (tokens.Fetch(1) > 0) { }
                tokens.Reset();
            }
            {
                var ts = DateTime.Now - t;
                t += ts;
                Debug.WriteLine("Lexing completed in {0}",ts);
            }
#endif
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
                tokens.Reset();
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
            if (parser.NumberOfSyntaxErrors != 0)
            {
                var failedTreeTransform = new XSharpTreeTransformation(parser, _pool, _syntaxFactory);
                var eof = SyntaxFactory.Token(SyntaxKind.EndOfFileToken).
                    WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_ParserError));
                eof.XNode = new TerminalNodeImpl(tree.Stop);
                var result = _syntaxFactory.CompilationUnit(
                    failedTreeTransform.GlobalEntities.Externs,
                    failedTreeTransform.GlobalEntities.Usings, 
                    failedTreeTransform.GlobalEntities.Attributes, 
                    failedTreeTransform.GlobalEntities.Members, 
                    eof);
                result.XNode = (XSharpParser.SourceContext)tree;
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

            var treeTransform = new XSharpTreeTransformation(parser, _pool, _syntaxFactory);
            try
            {
                walker.Walk(treeTransform, tree);
                var eof = SyntaxFactory.Token(SyntaxKind.EndOfFileToken);
                var result = _syntaxFactory.CompilationUnit(
                    treeTransform.GlobalEntities.Externs, treeTransform.GlobalEntities.Usings, 
                    treeTransform.GlobalEntities.Attributes, treeTransform.GlobalEntities.Members, eof);
                result.XNode = (XSharpParser.SourceContext)tree;
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
                return CreateForGlobalFailure(lexer.TextWindow.Position, createEmptyNodeFunc());
            }
#endif
        }

        private TNode CreateForGlobalFailure<TNode>(int position, TNode node) where TNode : CSharpSyntaxNode
        {
            // Turn the complete input into a single skipped token. This avoids running the lexer, and therefore
            // the preprocessor directive parser, which may itself run into the same problem that caused the
            // original failure.
            var builder = new SyntaxListBuilder(1);
            builder.Add(SyntaxFactory.BadToken(null, lexer.TextWindow.Text.ToString(), null));
            var fileAsTrivia = _syntaxFactory.SkippedTokensTrivia(builder.ToList<SyntaxToken>());
            node = AddLeadingSkippedSyntax(node, fileAsTrivia);
            ForceEndOfFile(); // force the scanner to report that it is at the end of the input.
            return AddError(node, position, 0, ErrorCode.ERR_InsufficientStack);
        }

    }
}