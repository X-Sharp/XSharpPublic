//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Uncomment this define to dump time profiling info of the parsing phases.
//#define DUMP_TIMES
#nullable disable
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Threading;
using System.Linq;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Tree;
using Antlr4.Runtime.Misc;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    using Microsoft.CodeAnalysis.Syntax.InternalSyntax;
    internal class XSharpBailErrorStrategy : BailErrorStrategy
    {
        readonly string _fileName;
        readonly IList<ParseErrorData> _parseErrors;
        internal XSharpBailErrorStrategy(String FileName, IList<ParseErrorData> parseErrors) : base()
        {
            _fileName = FileName;
            _parseErrors = parseErrors;
        }
        public override void ReportError(Parser recognizer, RecognitionException e)
        {
            if (e?.OffendingToken != null)
            {
                _parseErrors.Add(new ParseErrorData(e.OffendingToken, ErrorCode.ERR_ParserError, e.Message));
            }
            else
            {
                _parseErrors.Add(new ParseErrorData(_fileName, ErrorCode.ERR_ParserError, e.Message));
            }
            System.Diagnostics.Debug.WriteLine("Parsing aborted for : " + _fileName);
            System.Diagnostics.Debug.WriteLine("     error detected : " + e.Message);
        }
        public override void Recover(Parser recognizer, RecognitionException e)
        {
            ReportError(recognizer, e);
            base.Recover(recognizer, e);
        }
        public override IToken RecoverInline(Parser recognizer)
        {
            InputMismatchException e = new InputMismatchException(recognizer);
            ReportError(recognizer, e);
            return base.RecoverInline(recognizer);
        }
    }

    internal partial class XSharpLanguageParser : SyntaxParser
    {
        private readonly string _fileName;
        private readonly SourceText _text;
        private readonly CSharpParseOptions _options;
        private readonly SyntaxListPool _pool = new(); // Don't need to reset this.
        private readonly SyntaxFactoryContext _syntaxFactoryContext; // Fields are resettable.
        private readonly ContextAwareSyntax _syntaxFactory; // Has context, the fields of which are resettable.
        private readonly bool _isScript;
        private readonly bool _isMacroScript;

        private BufferedTokenStream _lexerTokenStream;
        private BufferedTokenStream _preprocessorTokenStream;
        //#endif

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
            _isScript = options.Kind == SourceCodeKind.Script;
            if (_isScript)
                _fileName = "Script";
            _isMacroScript = _isScript && options.MacroScript;
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
        XSharpParserRuleContext buildTree(XSharpParser parser)
        {
            XSharpParserRuleContext tree;
            if (_isScript)
            {
                if (_isMacroScript)
                    tree = parser.macroScript();
                else
                    tree = parser.script();
            }
            else if (_options.Dialect == XSharpDialect.FoxPro)
                tree = parser.foxsource();
            else
                tree = parser.source();
            return tree;

        }

        private string _GetInnerExceptionMessage(Exception e)
        {
            string msg = e.Message;
            while (e.InnerException != null)
            {
                e = e.InnerException;
                msg = e.Message;
            }
            return msg;
        }

        internal CompilationUnitSyntax ParseCompilationUnitCore()
        {
#if DEBUG && DUMP_TIMES
             DateTime t = DateTime.Now;
#endif
            if (_options.ShowIncludes)
            {
                _options.ConsoleOutput.WriteLine("Compiling {0}", _fileName);
            }
            var sourceText = _text.ToString();
            XSharpLexer lexer = null;
            XSharpPreprocessor pp = null;
            XSharpParserRuleContext tree = new XSharpParserRuleContext();
            XSharpParser parser = null;
            var parseErrors = ParseErrorData.NewBag();
            try
            {
                lexer = XSharpLexer.Create(sourceText, _fileName, _options);
                lexer.Options = _options;
                _lexerTokenStream = lexer.GetTokenStream();

            }
            catch (Exception e)
            {
                // Exception during Lexing 
                parseErrors.Add(new ParseErrorData(_fileName, ErrorCode.ERR_Internal, e.Message, e.StackTrace));
                // create empty token stream so we can continue the rest of the code
                _lexerTokenStream = new BufferedTokenStream(new XSharpListTokenSource(lexer, new List<IToken>()));
            }
#if DEBUG && DUMP_TIMES
            {
                var ts = DateTime.Now - t;
                t += ts;
                Debug.WriteLine("Lexing completed in {0}",ts);
            }
#endif  
            // do not pre-process when there were lexer exceptions
            if (lexer != null && parseErrors.Count == 0)
            {
                foreach (var e in lexer.LexErrors)
                {
                    parseErrors.Add(e);
                }
                BufferedTokenStream ppStream = null;
                try
                {
                    // Do we need to preprocess ?
                    _lexerTokenStream.Fill();

                    var mustPreprocess = true;
                    if (_options.MacroScript)
                    {
                        mustPreprocess = false;
                    }
                    else
                    {
                        if (!string.IsNullOrEmpty(_options.StdDefs))
                        {
                            // alternate header file defined
                            mustPreprocess = true;
                        }
                        else if (!_options.NoStdDef)
                        {
                            // normal standard header file (will be set later)
                            mustPreprocess = true;
                        }
                        else
                        {
                            mustPreprocess = lexer.HasPreprocessorTokens ||
                                _options.PreprocessorSymbols.Length > 0;
                        }
                    }
                    if (mustPreprocess)
                    {
                        pp = new XSharpPreprocessor(lexer, _lexerTokenStream, _options, _fileName, _text.Encoding, _text.ChecksumAlgorithm, parseErrors);
                        var ppTokens = pp.PreProcess();
                        ppStream = new CommonTokenStream(new XSharpListTokenSource(lexer, ppTokens));
                    }
                    else
                    {
                        // No Standard Defs and no preprocessor tokens in the lexer
                        // so we bypass the preprocessor and use the lexer tokenstream
                        // but if a .ppo is required we must use the preprocessor to
                        // write the source text to the .ppo file
                        if (_options.PreprocessorOutput && pp != null)
                        {
                            pp.writeToPPO(sourceText, false);
                        }
                        BufferedTokenStream ts = (BufferedTokenStream)_lexerTokenStream;
                        var tokens = ts.GetTokens();
                        // commontokenstream filters on tokens on the default channel. All other tokens are ignored
                        ppStream = new CommonTokenStream(new XSharpListTokenSource(lexer, tokens));
                    }
                    ppStream.Fill();
                    _preprocessorTokenStream = ppStream;

                }
                catch (Exception e)
                {
                    // Exception during Preprocessing
                    parseErrors.Add(new ParseErrorData(_fileName, ErrorCode.ERR_Internal, e.Message, e.StackTrace));
                    // create empty token stream so we can continue the rest of the code
                    _preprocessorTokenStream = new BufferedTokenStream(new XSharpListTokenSource(lexer, new List<IToken>()));
                }
            }
#if DEBUG && DUMP_TIMES
           {
                var ts = DateTime.Now - t;
                t += ts;
                Debug.WriteLine("Preprocessing completed in {0}",ts);
            }
#endif
            parser = new XSharpParser(_preprocessorTokenStream) { Options = _options };

            tree = new XSharpParserRuleContext();
            if (_options.ParseLevel != ParseLevel.Lex)
            {
                // When parsing in Sll mode we do not record any parser errors.
                // When this fails, then we try again with LL mode and then we record errors
                parser.RemoveErrorListeners();
                parser.Interpreter.PredictionMode = PredictionMode.Sll;
                // some options to have FAST parsing
                parser.Interpreter.tail_call_preserves_sll = false;
                parser.Interpreter.treat_sllk1_conflict_as_ambiguity = true;
                parser.ErrorHandler = new BailErrorStrategy();
                try
                {
                    tree = buildTree(parser);
                }
                catch (ParseCanceledException e)
                {
                    if (_options.Verbose)
                    {
                        string msg = _GetInnerExceptionMessage(e);
                        _options.ConsoleOutput.WriteLine("Antlr: SLL parsing failed with failure: " + msg + ". Trying again in LL mode.");
                    }
                    var errorListener = new XSharpErrorListener(_fileName, parseErrors);
                    parser.AddErrorListener(errorListener);
                    parser.ErrorHandler = new XSharpErrorStrategy();
                    // we need to set force_global_context to get proper error messages. This makes parsing slower
                    // but gives better messages
                    parser.Interpreter.treat_sllk1_conflict_as_ambiguity = false;
                    parser.Interpreter.force_global_context = true;
                    parser.Interpreter.enable_global_context_dfa = true;
                    parser.Interpreter.PredictionMode = PredictionMode.Ll;
                    _preprocessorTokenStream.Reset();
                    if (_options.Verbose && pp != null)
                    {
                        pp.DumpStats();
                    }
                    if (pp != null)
                    {
                        pp.Close();
                    }
                    parser.Reset();
                    try
                    {
                        tree = buildTree(parser);
                    }
                    catch (Exception e1)
                    {
                        // Cannot parse again. Must be a syntax error.
                        if (_options.Verbose)
                        {
                            string msg = _GetInnerExceptionMessage(e1);
                            _options.ConsoleOutput.WriteLine("Antlr: LL parsing also failed with failure: " + msg);
                        }
                    }
                }
            }// _options.ParseLevel != Lex
#if DEBUG && DUMP_TIMES
            {
                var ts = DateTime.Now - t;
                t += ts;
                Debug.WriteLine("Parsing completed in {0}",ts);
            }
#endif
            if (_options.DumpAST && tree != null)
            {
                string strTree = tree.ToStringTree();
                string file = System.IO.Path.ChangeExtension(_fileName, "ast");
                strTree = strTree.Replace(@"\r\n)))))", @"\r\n*)))))" + "\r\n");
                strTree = strTree.Replace(@"\r\n))))", @"\r\n*)))" + "\r\n");
                strTree = strTree.Replace(@"\r\n)))", @"\r\n*)))" + "\r\n");
                strTree = strTree.Replace(@"\r\n))", @"\r\n*))" + "\r\n");
                strTree = strTree.Replace(@"\r\n)", @"\r\n*)" + "\r\n");
                strTree = strTree.Replace(@"\r\n*)", @"\r\n)");
                System.IO.File.WriteAllText(file, strTree);
            }
            var walker = new ParseTreeWalker();
            List<PragmaBase> pragmas = new List<PragmaBase>();
            List<PragmaOption> pragmaoptions = new List<PragmaOption>();
            if (pp != null && pp.Pragmas?.Count > 0)
            {
                pragmas = pp.Pragmas;
                foreach (var pragma in pragmas)
                {
                    if (pragma is PragmaOption po)
                    {
                        pragmaoptions.Add(po);
                    }
                }
            }
            if (_options.ParseLevel == ParseLevel.Complete)
            {
                // check for parser errors, such as missing tokens
                // This adds items to the parseErrors list for missing
                // tokens and missing keywords
                try
                {
                    var errchecker = new XSharpParseErrorAnalysis(parser, parseErrors, _options, pragmaoptions);
                    walker.Walk(errchecker, tree);
                }
                catch (Exception e)
                {
                    parseErrors.Add(new ParseErrorData(_fileName, ErrorCode.ERR_Internal, e.Message, e.StackTrace));
                }
            }
            var treeTransform = CreateTransform(parser, _options, _pool, _syntaxFactory, _fileName);
            if (pragmas.Count > 0)
            {
                treeTransform.SetPragmas(pragmas);
            }
            bool hasErrors = false;
            var eof = SyntaxFactory.Token(SyntaxKind.EndOfFileToken);
            try
            {

                if (_options.ParseLevel < ParseLevel.Complete || parser.NumberOfSyntaxErrors != 0 ||
                    (parseErrors.Count != 0 && parseErrors.Contains(p => !ErrorFacts.IsWarning(p.Code))))
                {
                    if (pp != null)
                    {
                        eof = AddLeadingSkippedSyntax(eof, ParserErrorsAsTrivia(parseErrors, pp.IncludedFiles));
                    }
                    else
                    {
                        eof = AddLeadingSkippedSyntax(eof, ParserErrorsAsTrivia(parseErrors, new Dictionary<string, SourceText>()));
                    }
                    if (tree != null)
                    {
                        eof.XNode = new XTerminalNodeImpl(tree.Stop);
                    }
                    else
                    {
                        eof.XNode = new XTerminalNodeImpl(_lexerTokenStream.Get(_lexerTokenStream.Size - 1));
                    }
                    hasErrors = true;
                }

                if (!hasErrors)
                {
                    try
                    {
                        walker.Walk(treeTransform, tree);
                        if (treeTransform.ParseErrors.Count > 0)
                        {
                            parseErrors.AddRange(treeTransform.ParseErrors);
                        }
                    }
                    catch (Exception e)
                    {
                        parseErrors.Add(new ParseErrorData(_fileName, ErrorCode.ERR_Internal, e.Message, e.StackTrace));
                    }
                    if (parseErrors.Count > 0 && pp != null)
                    {
                        eof = AddLeadingSkippedSyntax(eof, ParserErrorsAsTrivia(parseErrors, pp.IncludedFiles));
                    }
                }
                var result = _syntaxFactory.CompilationUnit(
                        treeTransform.GlobalEntities.Externs,
                        treeTransform.GlobalEntities.Usings,
                        treeTransform.GlobalEntities.Attributes,
                        treeTransform.GlobalEntities.Members, eof);
                result.XNode = tree;
                tree.CsNode = result;
                result.XTokens = _lexerTokenStream;
                result.XPPTokens = _preprocessorTokenStream;
                result.HasDocComments = lexer.HasDocComments;
                if (!_options.MacroScript && !hasErrors)
                {
                    result.InitProcedures = treeTransform.GlobalEntities.InitProcedures;
                    result.Globals = treeTransform.GlobalEntities.Globals;
                    result.PragmaWarnings = treeTransform.GlobalEntities.PragmaWarnings;
                    result.PragmaOptions = treeTransform.GlobalEntities.PragmaOptions;
                    if (pp != null)
                    {
                        result.IncludedFiles = pp.IncludedFiles;
                    }
                    else
                    {
                        result.IncludedFiles = new Dictionary<string, SourceText>();
                    }
                    result.FileWidePublics = treeTransform.GlobalEntities.FileWidePublics;
                    result.HasPCall = treeTransform.GlobalEntities.HasPCall;
                    result.NeedsProcessing = treeTransform.GlobalEntities.NeedsProcessing;
                    if (_options.HasRuntime)
                    {
                        result.LiteralSymbols = ((XSharpTreeTransformationRT)treeTransform).LiteralSymbols;
                        result.LiteralPSZs = ((XSharpTreeTransformationRT)treeTransform).LiteralPSZs;
                    }
                }
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
                if (pp != null)
                    pp.Close();

            }
        }

        internal static XSharpTreeTransformationCore CreateTransform(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool,
                    ContextAwareSyntax syntaxFactory, string fileName)
        {
            switch (options.Dialect)
            {
                case XSharpDialect.Core:
                    return new XSharpTreeTransformationCore(parser, options, pool, syntaxFactory, fileName);
                case XSharpDialect.VO:
                case XSharpDialect.Vulcan:
                    return new XSharpTreeTransformationVO(parser, options, pool, syntaxFactory, fileName);
                case XSharpDialect.FoxPro:
                    return new XSharpTreeTransformationFox(parser, options, pool, syntaxFactory, fileName);
                case XSharpDialect.XPP:
                    return new XSharpTreeTransformationXPP(parser, options, pool, syntaxFactory, fileName);
                case XSharpDialect.Harbour:
                default:
                    return new XSharpTreeTransformationRT(parser, options, pool, syntaxFactory, fileName);
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
            catch (InsufficientExecutionStackException)
            {
                return CreateForGlobalFailure(_lexerTokenStream?.Lt(1)?.StartIndex ?? 0, createEmptyNodeFunc());
            }
#endif
        }

        private SkippedTokensTriviaSyntax FileAsTrivia(SourceText text)
        {
            var builder = new SyntaxListBuilder(1);
            builder.Add(SyntaxFactory.BadToken(null, text.ToString(), null));
            var fileAsTrivia = _syntaxFactory.SkippedTokensTrivia(builder.ToList<SyntaxToken>());
            ForceEndOfFile(); // force the scanner to report that it is at the end of the input.
            return fileAsTrivia;
        }

        private SkippedTokensTriviaSyntax ParserErrorsAsTrivia(List<ParseErrorData> parseErrors, IDictionary<string, SourceText> includes)
        {
            // create one syntax token per error
            // and one syntax token for the main file
            // these tokens will get as many errors as needed.
            // We are only including 1 syntax error per file (1003) and one parse error (9002)
            var textNode = SyntaxFactory.BadToken(null, _text.ToString(), null);
            var builder = new SyntaxListBuilder(parseErrors.Count + 1);
            if (!parseErrors.IsEmpty())
            {
                bool hasSyntaxError = false;
                bool hasParserError = false;
                foreach (var e in parseErrors)
                {
                    bool add = true;
                    if (e.Code == ErrorCode.ERR_SyntaxError)
                    {
                        add = !hasSyntaxError;
                        hasSyntaxError = true;
                    }
                    else if (e.Code == ErrorCode.ERR_ParserError)
                    {
                        add = !hasParserError;
                        hasParserError = true;
                    }
                    if (!add)
                    {
                        continue;
                    }
                    if (e.Node != null)
                    {
                        var node = e.Node;
                        var key = node.SourceFileName;
                        int pos = node.Position;
                        int len = node.FullWidth;
                        adjustLenPos(ref len, ref pos, e);
                        SourceText inc = null;
                        adjustlen(key, pos, ref len, out inc, includes);
                        var diag = new SyntaxDiagnosticInfo(pos, len, e.Code, e.Args);
                        if (inc != null)
                        {
                            var incNode = SyntaxFactory.BadToken(null, inc.ToString(), null);
                            incNode = incNode.WithAdditionalDiagnostics(diag);
                            incNode.XNode = e.Node;
                            builder.Add(incNode);
                        }
                        else
                        {
                            textNode = textNode.WithAdditionalDiagnostics(diag);
                        }
                        if (node.SourceSymbol != null && !ErrorFacts.IsWarning(e.Code))
                        {
                            // an error for a define could be on another location
                            var sym = node.SourceSymbol as XSharpToken;
                            key = sym.SourceName;
                            pos = sym.Position;
                            len = sym.FullWidth;
                            adjustLenPos(ref len, ref pos, e);
                            adjustlen(key, pos, ref len, out inc, includes);
                            var diagDefine = new SyntaxDiagnosticInfo(pos, len, ErrorCode.ERR_DefineIncorrectValue, sym.Text);
                            SourceText source = inc;
                            if (source == null)
                            {
                                source = _text;
                            }
                            var defnode = SyntaxFactory.BadToken(null, source.ToString(), null);
                            defnode = defnode.WithAdditionalDiagnostics(diagDefine);
                            defnode.XNode = new XTerminalNodeImpl(sym);
                            builder.Add(defnode);
                        }
                    }
                    else
                    {
                        textNode = textNode.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(e.Code, e.Args));
                    }
                }
            }
            else
            {
                if (_options.ParseLevel == ParseLevel.Complete)
                {
                    textNode = textNode.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_ParserError, "Unknown error"));
                }
            }
            builder.Add(textNode);
            return _syntaxFactory.SkippedTokensTrivia(builder.ToList<SyntaxToken>());
        }

        void adjustLenPos(ref int len, ref int pos, ParseErrorData e)
        {

            if (len <= 0 || pos < 0)
            {
                if (e.Node.Parent != null)
                {
                    var xNode = e.Node as IXParseTree;
                    pos = xNode.Position;
                    len = xNode.FullWidth;
                }
                if (pos < 0)
                    pos = 0;
                if (len <= 0)
                    len = 1;
            }
        }

        void adjustlen(string key, int pos, ref int len, out SourceText inc, IDictionary<string, SourceText> includes)
        {
            inc = null;
            if (key != null && includes.ContainsKey(key))
            {
                inc = includes[key];
                if (pos - 1 + len > inc.Length)
                {
                    len = inc.Length - pos + 1;
                }
            }
        }

        private TNode CreateForGlobalFailure<TNode>(int position, TNode node) where TNode : CSharpSyntaxNode
        {
            // Turn the complete input into a single skipped token. This avoids running the lexer, and therefore
            // the preprocessor directive parser, which may itself run into the same problem that caused the
            // original failure.
            return AddError(AddLeadingSkippedSyntax(node, FileAsTrivia(_text)), position, 0, ErrorCode.ERR_InsufficientStack);
        }

        internal static SyntaxTree ProcessTrees(SyntaxTree[] trees, CSharpParseOptions options)
        {
            if (trees.Length > 0)
            {
                var tree = trees[0];
                var lp = new XSharpLanguageParser(tree.FilePath, null, options, null, null);
                var newtree = lp.ProcessTreesImpl(trees, options);
                return newtree;
            }
            return null;
        }

        private string GetNsFullName(XP.Namespace_Context ns)
        {
            string name = ns.Name.GetText();
            while (ns.Parent is XP.Namespace_Context)
            {
                ns = ns.Parent as XP.Namespace_Context;
                name = ns.Name.GetText() + "." + name;
            }
            return name;
        }

        private MemberDeclarationSyntax WrapInNamespace(XSharpTreeTransformationCore trans, MemberDeclarationSyntax member,
            XP.Namespace_Context xns, string defaultNamespace)
        {
            if (xns != null || !string.IsNullOrEmpty(defaultNamespace))
            {
                var members = _pool.Allocate<MemberDeclarationSyntax>();
                string nsName;
                if (xns != null)
                    nsName = GetNsFullName(xns);
                else
                    nsName = defaultNamespace;
                members.Add(member);
                member = _syntaxFactory.NamespaceDeclaration(
                    attributeLists: default,
                    modifiers: default,
                    SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                    name: trans.GenerateQualifiedName(nsName),
                    openBraceToken: SyntaxFactory.OpenBraceToken,
                    externs: null,
                    usings: null,
                    members: members,
                    closeBraceToken: SyntaxFactory.CloseBraceToken,
                    semicolonToken: SyntaxFactory.SemicolonToken);
                _pool.Free(members);
            }
            return member;
        }

        /// <summary>
        /// Collects the types and the usings from their source file, because the body of the methods may contain references to code in this source
        /// such as (static) functions.
        /// </summary>
        internal struct PartialPropertyElement
        {
            internal XP.IPartialPropertyContext Type;
            internal IEnumerable<Syntax.UsingDirectiveSyntax> Usings;
            internal PartialPropertyElement(XP.IPartialPropertyContext type, IEnumerable<Syntax.UsingDirectiveSyntax> usings)
            {
                Type = type;
                Usings = usings;
            }
        }
    }
}
