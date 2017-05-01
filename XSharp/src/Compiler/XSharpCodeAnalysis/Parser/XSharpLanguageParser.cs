/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
// Uncomment this define to dump time profiling info of the parsing phases.
//#define DUMP_TIMES

using System;
using System.Collections.Generic;
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
namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal partial class XSharpLanguageParser : SyntaxParser
    {
        private readonly string _fileName;
        private readonly SourceText _text;
        private readonly CSharpParseOptions _options;
        private readonly SyntaxListPool _pool = new SyntaxListPool(); // Don't need to reset this.
        private readonly SyntaxFactoryContext _syntaxFactoryContext; // Fields are resettable.
        private readonly ContextAwareSyntax _syntaxFactory; // Has context, the fields of which are resettable.

        private ITokenStream _lexerTokenStream;
        private ITokenStream _preprocessorTokenStream;

        //#if DEBUG
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

            if (_options.ShowIncludes)  
            {
                _options.ConsoleOutput.WriteLine("Compiling {0}",_fileName);
            }
            var sourceText = _text.ToString();
            var lexer = XSharpLexer.Create(sourceText, _fileName, _options);
            _lexerTokenStream = lexer.GetTokenStream();
#if DEBUG && DUMP_TIMES
                        DateTime t = DateTime.Now;
#endif
#if DEBUG && DUMP_TIMES
            {
                var ts = DateTime.Now - t;
                t += ts;
                Debug.WriteLine("Lexing completed in {0}",ts);
            }
#endif
            var parseErrors = ParseErrorData.NewBag();
            var pp = new XSharpPreprocessor(_lexerTokenStream, _options, _fileName, _text.Encoding, _text.ChecksumAlgorithm, parseErrors);
            // commontokenstream filters on tokens on the default channel. All other tokens are ignored
			CommonTokenStream ppStream;
            if (lexer.HasPreprocessorTokens || !_options.NoStdDef)
            { 
	            var ppTokens = pp.PreProcess();
	            ppStream = new CommonTokenStream(new ListTokenSource(ppTokens));
            }
            else
            {
				// No Standard Defs and no preprocessor tokens in the lexer
				// so we bypass the preprocessor and use the lexer tokenstream
                // but if a .ppo is required we must use the preprocessor to
                // write the source text to the .ppo file
                if (_options.PreprocessorOutput)
                {
                    pp.writeToPPO(sourceText,false,false);
                }
                BufferedTokenStream ts = (BufferedTokenStream)_lexerTokenStream;
                var tokens = ts.GetTokens();
                ppStream = new CommonTokenStream(new ListTokenSource(tokens));
            }
            ppStream.Fill();
            _preprocessorTokenStream = ppStream;
            var parser = new XSharpParser(ppStream);
            // See https://github.com/tunnelvisionlabs/antlr4/blob/master/doc/optimized-fork.md
            // for info about optimization flags such as the next line
            //parser.Interpreter.enable_global_context_dfa = true;    // default = false
            //parser.Interpreter.tail_call_preserves_sll = false;     // default = true

            parser.AllowFunctionInsideClass = false;     // 
            if (_options.Dialect == XSharpDialect.VO)
            {
                parser.AllowXBaseVariables = true;
                parser.AllowNamedArgs = false;
                parser.AllowGarbageAfterEnd = true;
            }
            else if (_options.Dialect == XSharpDialect.Vulcan)
            {
                parser.AllowXBaseVariables = false;
                parser.AllowNamedArgs = false;
                parser.AllowGarbageAfterEnd = true;
            }
            else
            {                                      // memvar and private statements are not recognized in Vulcan
                parser.AllowXBaseVariables = false;
                parser.AllowNamedArgs = true;
                parser.AllowGarbageAfterEnd = false;
            }


#if DEBUG && DUMP_TIMES
           {
                var ts = DateTime.Now - t;
                t += ts;
                Debug.WriteLine("Preprocessing completed in {0}",ts);
            }
#endif
                parser.Interpreter.PredictionMode = PredictionMode.Sll;
            // When parsing in Sll mode we do not record any parser errors.
            // When this fails, then we try again with LL mode and then we record errors
            parser.RemoveErrorListeners();
            parser.ErrorHandler = new BailErrorStrategy();
            ParserRuleContext tree;
            try
            {
                tree = parser.source();
            }
            catch (ParseCanceledException e)
            {
                var errorListener = new XSharpErrorListener(_fileName, parseErrors);
                parser.AddErrorListener(errorListener);
                parser.ErrorHandler = new XSharpErrorStrategy();
                parser.Interpreter.PredictionMode = PredictionMode.Ll;
                if (_options.Verbose)
                {
                    Exception ex;
                    string msg = e.Message;
                    ex = e;
                    while (ex.InnerException != null)
                    {
                        ex = ex.InnerException;
                        msg = ex.Message;
                    }
                    _options.ConsoleOutput.WriteLine("Antlr: SLL parsing failed with failure: "+msg+". Trying again in LL mode.");
                }

                ppStream.Reset();
                if (_options.Verbose)
                {
                    pp.DumpStats();
                }
                pp.Close();
                parser.Reset();
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

            foreach (var e in lexer.LexErrors)
            {
                parseErrors.Add(e);
            }

            if (! _options.ParseOnly)
            {
                // check for parser errors, such as missing tokens
                // This adds items to the parseErrors list for missing
                // tokens and missing keywords
                var errchecker = new XSharpParseErrorAnalysis(parser, parseErrors);
                walker.Walk(errchecker, tree);
            }
            //

            XSharpTreeTransformation treeTransform;
            if (_options.IsDialectVO)
            {
                treeTransform = new XSharpVOTreeTransformation(parser, _options, _pool, _syntaxFactory, _fileName);
            }
            else
            {
                treeTransform = new XSharpTreeTransformation(parser, _options, _pool, _syntaxFactory, _fileName);
            }

            if ( _options.ParseOnly ||
                parser.NumberOfSyntaxErrors != 0 || 
                (parseErrors.Count != 0 && parseErrors.Contains(p => !ErrorFacts.IsWarning(p.Code))))
            {
                var eof = SyntaxFactory.Token(SyntaxKind.EndOfFileToken);
                eof = AddLeadingSkippedSyntax(eof, ParserErrorsAsTrivia(parseErrors, pp.IncludedFiles));
                eof.XNode = new XTerminalNodeImpl(tree.Stop);
                var result = _syntaxFactory.CompilationUnit(
                    treeTransform.GlobalEntities.Externs,
                    treeTransform.GlobalEntities.Usings,
                    treeTransform.GlobalEntities.Attributes,
                    treeTransform.GlobalEntities.Members,
                    eof);
                result.XNode = (XSharpParser.SourceContext)tree;
                result.XTokens = _lexerTokenStream;
                result.XPPTokens = _preprocessorTokenStream;
                result.IncludedFiles = pp.IncludedFiles;
                return result;
            }
//#endif
            try
            {
                walker.Walk(treeTransform, tree);
                var eof = SyntaxFactory.Token(SyntaxKind.EndOfFileToken);
                if (!parseErrors.IsEmpty())
                {
                    eof = AddLeadingSkippedSyntax(eof, ParserErrorsAsTrivia(parseErrors, pp.IncludedFiles));
                }
                var result = _syntaxFactory.CompilationUnit(
                    treeTransform.GlobalEntities.Externs, 
                    treeTransform.GlobalEntities.Usings,
                    treeTransform.GlobalEntities.Attributes, 
                    treeTransform.GlobalEntities.Members, eof);
                // TODO nvk: add parser warnings to tree diagnostic info
                result.XNode = (XSharpParser.SourceContext)tree;
                result.XTokens = _lexerTokenStream;
                result.XPPTokens = _preprocessorTokenStream;
                result.InitProcedures = treeTransform.GlobalEntities.InitProcedures;
                result.Globals = treeTransform.GlobalEntities.Globals;
                result.IncludedFiles = pp.IncludedFiles;
                result.HasPCall = treeTransform.GlobalEntities.HasPCall;
                result.NeedsProcessing = treeTransform.GlobalEntities.NeedsProcessing;
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

        private SkippedTokensTriviaSyntax FileAsTrivia(SourceText text)
        {
            var builder = new SyntaxListBuilder(1);
            builder.Add(SyntaxFactory.BadToken(null, text.ToString(), null));
            var fileAsTrivia = _syntaxFactory.SkippedTokensTrivia(builder.ToList<SyntaxToken>());
            ForceEndOfFile(); // force the scanner to report that it is at the end of the input.
            return fileAsTrivia;
        }

        private SkippedTokensTriviaSyntax ParserErrorsAsTrivia(List<ParseErrorData> parseErrors, IDictionary<string,SourceText> includes)
        {
            // create one syntax token per error
            // and one syntax token for the main file
            // these tokens will get as many errors as needed.
            var textNode = SyntaxFactory.BadToken(null, _text.ToString(), null);
            var builder = new SyntaxListBuilder(parseErrors.Count+1);
            if (!parseErrors.IsEmpty())
            {
                foreach (var e in parseErrors)
                {
                    if (e.Node != null)
                    {
                        var node = e.Node;
                        var key = node.SourceFileName;
                        int pos = node.Position;
                        int len = node.FullWidth;
                        if (node.SourceSymbol != null)
                        {
                            var sym = node.SourceSymbol as XSharpToken;
                            key = sym.SourceName;
                            pos = sym.Position;
                            len = sym.FullWidth;
                        }
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
                        SourceText inc = null;
                        if (key != null && includes.ContainsKey(key))
                        {
                            inc  = includes[key];
                            if (pos - 1 + len > inc.Length)
                            {
                                len = inc.Length - pos + 1;
                            }
                        }
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
                    }
                    else
                    {
                        textNode = textNode.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(e.Code, e.Args));
                    }
                }
            }
            else
            {
                if (! _options.SyntaxCheck)
                {
                    textNode = textNode.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_ParserError, "Unknown error"));
                }
            }
            builder.Add(textNode);
            return _syntaxFactory.SkippedTokensTrivia(builder.ToList<SyntaxToken>());
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
                var lp = new XSharpLanguageParser(tree.FilePath, null, options,null, null);
                return lp.processTrees(trees, options);
            }
            return null;
        }


        private SyntaxTree processTrees(SyntaxTree[] trees,CSharpParseOptions parseoptions)
        {
            // this method gives us the ability to check all the generated syntax trees,
            // add generated constructors to partial classes when none of the parts has a constructor
            // merge accesses and assigns from different source files into one property etc.
            if (! parseoptions.IsDialectVO)
                return null;
            var partialClasses = new Dictionary<string, List<XP.IPartialPropertyContext>>(StringComparer.OrdinalIgnoreCase);
            foreach (var tree in trees)
            {
                var compilationunit = tree.GetRoot() as Syntax.CompilationUnitSyntax;
                if (compilationunit.NeedsProcessing)
                {
                    foreach (var member in compilationunit.Members)
                    {
                        if (member is Syntax.NamespaceDeclarationSyntax)
                        {
                            processNameSpace(member as Syntax.NamespaceDeclarationSyntax, partialClasses);
                        }
                        else
                        {
                            var node = member.Green as CSharpSyntaxNode;
                            if (node.XNode is XP.EntityContext)
                            {
                                processType(node.XNode as XP.EntityContext, partialClasses);
                            }
                        }
                    }
                }
            }
            if (partialClasses.Count > 0)
            {
                // Create a new tree which shall have the generated constructors and properties
                // and return this tree to the caller.
                // we copy the attributes, modifiers etc from one of the class instances to make sure that
                // do not specify a conflicting modifier.
                var tr = trees[0];
                var cu = tr.GetRoot().Green as CompilationUnitSyntax;
                var trans = new XSharpVOTreeTransformation(null, _options, _pool, _syntaxFactory, tr.FilePath);
                var classes = _pool.Allocate<MemberDeclarationSyntax>();
                var clsmembers = _pool.Allocate<MemberDeclarationSyntax>();
                foreach (var element in partialClasses)
                {
                    var name = element.Key;
                    bool hasctor = false;
                    bool haspartialprop = false;
                    XP.IPartialPropertyContext ctxt = null;
                    foreach (var xnode in element.Value)
                    {
                        ctxt = xnode;
                        if (xnode.Data.HasCtor)
                        {
                            hasctor = true;
                        }
                        if (xnode.Data.PartialProps)
                        {
                            haspartialprop = true;
                        }
                    }
                    if (ctxt.CsNode is InterfaceDeclarationSyntax)
                    {
                        var ifdecl = ctxt.Get<InterfaceDeclarationSyntax>();
                        if (haspartialprop)
                        {
                            clsmembers.Clear();
                            var props = GeneratePartialProperties(element.Value);
                            if (props != null)
                            {
                                foreach (var prop in props)
                                {
                                    clsmembers.Add(prop);
                                }
                            }
                            if (clsmembers.Count > 0)
                            {
                                var decl = _syntaxFactory.InterfaceDeclaration(
                                trans.MakeCompilerGeneratedAttribute(false),
                                ifdecl.Modifiers,
                                ifdecl.Keyword,
                                ifdecl.Identifier,
                                ifdecl.TypeParameterList,
                                ifdecl.BaseList,
                                ifdecl.ConstraintClauses,
                                ifdecl.OpenBraceToken,
                                clsmembers,
                                ifdecl.CloseBraceToken,
                                null);
                                ifdecl.XGenerated = true;
                                classes.Add(decl);
                            }
                        }
                    }
                    else if (ctxt.CsNode is StructDeclarationSyntax)
                    {
                        var strucdecl = ctxt.Get<StructDeclarationSyntax>();
                        if (haspartialprop)
                        {
                            clsmembers.Clear();
                            var props = GeneratePartialProperties(element.Value);
                            if (props != null)
                            {
                                foreach (var prop in props)
                                {
                                    clsmembers.Add(prop);
                                }
                            }
                            if (clsmembers.Count > 0)
                            {
                                var decl = _syntaxFactory.StructDeclaration(
                                    trans.MakeCompilerGeneratedAttribute(false),
                                    strucdecl.Modifiers,
                                    strucdecl.Keyword,
                                    strucdecl.Identifier,
                                    strucdecl.TypeParameterList,
                                    strucdecl.BaseList,
                                    strucdecl.ConstraintClauses,
                                    strucdecl.OpenBraceToken,
                                    clsmembers,
                                    strucdecl.CloseBraceToken,
                                    null);
                                strucdecl.XGenerated = true;
                                classes.Add(decl);
                            }
                        }
                    }
                    else if (ctxt.CsNode is ClassDeclarationSyntax)
                    {
                        if (!hasctor || haspartialprop)
                        {
                            clsmembers.Clear();
                            var classdecl = ctxt.Get<ClassDeclarationSyntax>();
                            if (!hasctor)
                            {
                                var ctor = trans.GenerateDefaultCtor(classdecl.Identifier, ctxt as XP.Class_Context);
                                clsmembers.Add(ctor);
                            }
                            if (haspartialprop)
                            {
                                var props = GeneratePartialProperties(element.Value);
                                if (props != null)
                                {
                                    foreach (var prop in props)
                                    {
                                        clsmembers.Add(prop);
                                    }
                                }
                            }
                            if (clsmembers.Count > 0)
                            {

                                var decl = _syntaxFactory.ClassDeclaration(
                                trans.MakeCompilerGeneratedAttribute(false),
                                classdecl.Modifiers,
                                classdecl.Keyword,
                                classdecl.Identifier,
                                classdecl.TypeParameterList,
                                classdecl.BaseList,
                                classdecl.ConstraintClauses,
                                classdecl.OpenBraceToken,
                                clsmembers,
                                classdecl.CloseBraceToken,
                                null);
                                decl.XGenerated = true;
                                classes.Add(decl);
                            }
                        }
                    }
                }
                _pool.Free(clsmembers);
                var result = cu.Update(
                                    cu.Externs,
                                    cu.Usings,
                                    cu.AttributeLists,
                                    classes,
                                    cu.EndOfFileToken);
                
                var tree = CSharpSyntaxTree.Create((Syntax.CompilationUnitSyntax) result.CreateRed());
                _pool.Free(classes);
                return tree;
            }
            return null;

        }

        private List<MemberDeclarationSyntax> GeneratePartialProperties (List<XP.IPartialPropertyContext> classes)
        {
            var dict = new Dictionary<string, List<XP.MethodContext>>(StringComparer.OrdinalIgnoreCase);
            // Build list of unique names
            foreach (var clsctx in classes)
            {
                if (clsctx.PartialProperties != null)
                {
                    foreach (var  m in clsctx.PartialProperties)
                    {
                        var name = m.Id.GetText();
                        if (dict.ContainsKey(name))
                        {
                            dict[name].Add(m);
                        }
                        else
                        {
                            var list = new List<XP.MethodContext>();
                            list.Add(m);
                            dict.Add(name, list);
                        }
                    }
                }
            }
            // now we have a list of PropertyNames and methods
            XSharpTreeTransformation Xform;
            if (_options.IsDialectVO)
            {
                Xform = new XSharpVOTreeTransformation(null, _options, _pool, _syntaxFactory, _fileName);
            }
            else
            {
                Xform = new XSharpTreeTransformation(null, _options, _pool, _syntaxFactory, _fileName);
            }

            var result = new List<MemberDeclarationSyntax>();
            // For each unique name add a property
            foreach (var element in dict)
            {
                XP.MethodContext AccMet = null;
                XP.MethodContext AssMet = null;
                string originalName = null;
                foreach (var m in element.Value)
                {
                    originalName = m.Id.GetText();
                    if (m.T.Token.Type == XSharpLexer.ACCESS)
                    {
                        AccMet = m;
                    }
                    else
                    {
                        AssMet = m;
                    }
                    if (AccMet != null && AssMet != null)
                    {
                        break;
                    }
                }
                var prop = new XSharpTreeTransformation.SyntaxClassEntities.VoPropertyInfo();
                prop.AccessMethodCtx = AccMet;
                prop.AssignMethodCtx = AssMet;
                prop.idName = SyntaxFactory.Identifier(originalName);
                var propdecl = Xform.GenerateVoProperty(prop, null);
                result.Add(propdecl);
            }
            return result;
        }
        private static void processType(XP.EntityContext xnode, Dictionary<string, List<XP.IPartialPropertyContext>> partialClasses)
        {
            if (xnode != null && xnode.ChildCount == 1)
            {
                var cls = xnode.GetChild(0) as XP.IPartialPropertyContext;
                if (cls != null && (cls.Data.Partial  || cls.Data.PartialProps))
                {
                    var name = cls.Name;
                    if (!partialClasses.ContainsKey(name))
                    {
                        partialClasses.Add(name, new List<XP.IPartialPropertyContext>());
                    }
                    partialClasses[name].Add(cls);
                }
            }
        }
        private static void processNameSpace(Syntax.NamespaceDeclarationSyntax ns, Dictionary<string, List<XP.IPartialPropertyContext>> partialClasses)
        {
            foreach (var member in ns.Members)
            {
                if (member is Syntax.NamespaceDeclarationSyntax)
                {
                    processNameSpace(member as Syntax.NamespaceDeclarationSyntax, partialClasses);
                }
                else
                {
                    var node = member.Green as CSharpSyntaxNode;
                    if (node.XNode is XP.EntityContext)
                    {
                        processType(node.XNode as XP.EntityContext, partialClasses);
                    }
                }
            }

        }



    }
}