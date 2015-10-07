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
        private readonly SourceText _text;
        private readonly SyntaxListPool _pool = new SyntaxListPool(); // Don't need to reset this.
        private readonly SyntaxFactoryContext _syntaxFactoryContext; // Fields are resettable.
        private readonly ContextAwareSyntax _syntaxFactory; // Has context, the fields of which are resettable.

        //internal class XSharpErrorListener : IAntlrErrorListener<IToken>
        //{
        //    public int TotalErrors { get; private set; }
        //    internal XSharpErrorListener()
        //    {
        //        TotalErrors = 0;
        //    }
        //    public void SyntaxError(IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
        //    {
        //        TotalErrors += 1;
        //        /*if (e.OffendingToken != null)
        //        {
        //            _errors.Add("line :" + e.OffendingToken.Line + " column: " + e.OffendingToken.Column + " " + msg);
        //        }
        //        else
        //        {
        //            _errors.Add("line :" + line + 1 + " column: " + charPositionInLine + 1 + " " + msg);
        //        }*/
        //    }
        //}

        internal XSharpLanguageParser(
            //Lexer lexer,
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
            //var errorListener = new XSharpErrorListener();
            //parser.AddErrorListener(errorListener);
            parser.ErrorHandler = new XSharpErrorStrategy();
            parser.Interpreter.PredictionMode = PredictionMode.Sll;
            try
            {
                tree = parser.source();
            }
            catch (Exception)
            {
                tokens.Reset();
                parser.Reset();
                parser.Interpreter.PredictionMode = PredictionMode.Ll;
                tree = parser.source();
            }

            var walker = new ParseTreeWalker();

            //if (errorListener.TotalErrors != 0)
            if (parser.NumberOfSyntaxErrors != 0)
            {
                var errorAnalyzer = new XSharpParseErrorAnalysis(parser);
                walker.Walk(errorAnalyzer, tree);
            }

            var treeTransform = new XSharpTreeTransformation(parser, _pool, _syntaxFactory);
            try
            {
                walker.Walk(treeTransform, tree);
                var eof = SyntaxFactory.Token(SyntaxKind.EndOfFileToken);
                var result = _syntaxFactory.CompilationUnit(treeTransform.Externs, treeTransform.Usings, treeTransform.Attributes, treeTransform.Members, eof);
                result.XNode = (XSharpParser.SourceContext)tree;
                return result;
            }
            finally
            {
                treeTransform.Free();
            }
        }

        internal TNode ParseWithStackGuard<TNode>(Func<TNode> parseFunc, Func<TNode> createEmptyNodeFunc) where TNode : CSharpSyntaxNode
        {
            // If this value is non-zero then we are nesting calls to ParseWithStackGuard which should not be 
            // happening.  It's not a bug but it's inefficient and should be changed.
            //Debug.Assert(_recursionDepth == 0);

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