using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis;
using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.ExpressionEvaluator;

namespace LanguageService.CodeAnalysis.CSharp.ExpressionEvaluator
{
    internal static class XSyntaxHelpers
    {
        internal static XSharpSpecificCompilationOptions XSharpOptions { get; set; } = XSharpSpecificCompilationOptions.Default;

        internal static T ParseDebuggerInternal<T>(string source, CSharpParseOptions parseoptions) where T : InternalSyntax.CSharpSyntaxNode
        {
            string _fileName = "";
            var options = parseoptions
                .WithXSharpSpecificOptions(XSharpOptions)
                .WithMacroScript(false);

            var parseErrors = ParseErrorData.NewBag();

            T result = null;

            try
            {
                XSharpLexer lexer = XSharpLexer.Create(source.ToString(), _fileName, options);
                var lexerTokenStream = lexer.GetTokenStream();
                foreach (var e in lexer.LexErrors)
                {
                    parseErrors.Add(e);
                }

                var tokens = lexerTokenStream.GetTokens();
                // commontokenstream filters on tokens on the default channel. All other tokens are ignored
                var tokenStream = new Antlr4.Runtime.CommonTokenStream(new XSharpListTokenSource(lexer, tokens));
                tokenStream.Fill();
                tokenStream.Reset();

                var parser = new XSharpParser(tokenStream) { Options = options };
                var errorListener = new XSharpErrorListener(_fileName, parseErrors);
                parser.AddErrorListener(errorListener);
                parser.ErrorHandler = new InternalSyntax.XSharpErrorStrategy();
                // we need to set force_global_context to get proper error messages. This makes parsing slower
                // but gives better messages
                parser.Interpreter.treat_sllk1_conflict_as_ambiguity = false;
                parser.Interpreter.force_global_context = true;
                parser.Interpreter.enable_global_context_dfa = true;
                parser.Interpreter.PredictionMode = Antlr4.Runtime.Atn.PredictionMode.Ll;

                var _syntaxFactory = new InternalSyntax.ContextAwareSyntax(new InternalSyntax.SyntaxFactoryContext());
                var treeTransform = InternalSyntax.XSharpLanguageParser.CreateTransform(parser, options,
                    new Microsoft.CodeAnalysis.Syntax.InternalSyntax.SyntaxListPool(),
                    _syntaxFactory, _fileName);
                var walker = new Antlr4.Runtime.Tree.ParseTreeWalker();

                treeTransform.GlobalClassEntities = treeTransform.CreateClassEntities();
                treeTransform.ClassEntities.Push(treeTransform.GlobalClassEntities);

                IXParseTree tree;
                if (typeof(T) == typeof(InternalSyntax.ExpressionSyntax))
                {
                    tree = parser.expression();
                }
                else if (typeof(T) == typeof(InternalSyntax.StatementSyntax))
                {
                    tree = parser.statement();
                }
                else
                {
                    throw new Exception("Unsupported parse syntax type");
                }
                walker.Walk(treeTransform, tree);
                result = tree.Get<T>();

                var generated = treeTransform.ClassEntities.Pop();
                treeTransform.GlobalEntities.Members.AddRange(generated.Members);
                generated.Free();
            }
            catch (Exception e)
            {
                parseErrors.Add(new ParseErrorData(_fileName, ErrorCode.ERR_Internal, e.Message, e.StackTrace));
            }

            if (parseErrors.Count != 0)
            {
                var diags = new List<SyntaxDiagnosticInfo>();
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
                        diags.Add(new SyntaxDiagnosticInfo(e.Node.Position, e.Node.FullWidth, e.Code, e.Args));
                    }
                    else
                    {
                        diags.Add(new SyntaxDiagnosticInfo(e.Code, e.Args));
                    }
                }
                if (result == null)
                {
                    if (typeof(T) == typeof(InternalSyntax.ExpressionSyntax))
                    {
                        result = InternalSyntax.SyntaxFactory.IdentifierName(InternalSyntax.SyntaxFactory.Identifier(source))
                            .WithAdditionalDiagnostics(diags.ToArray()) as T;
                    }
                    else if (typeof(T) == typeof(InternalSyntax.StatementSyntax))
                    {
                        result = InternalSyntax.SyntaxFactory.EmptyStatement(attributeLists: default, InternalSyntax.SyntaxFactory.Token(SyntaxKind.SemicolonToken))
                            .WithAdditionalDiagnostics(diags.ToArray()) as T;
                    }
                    else
                    {
                        throw new Exception("Unsupported parse syntax type");
                    }
                }
            }

            return result;
        }
        
        internal static void UpdateRuntimeAssemblies(CSharpCompilation compilation)
        {
            var xoptions = XSharpOptions;
            xoptions.RuntimeAssemblies = RuntimeAssemblies.None;
            foreach (var f in compilation.ReferencedAssemblyNames)
            {
                xoptions.SetOptionFromReference(f.Name + ".dll");
            }
            XSharpOptions = xoptions;
            compilation.Options.SetXSharpSpecificOptions(xoptions);
        }
    }
}
