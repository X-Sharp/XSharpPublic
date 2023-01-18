// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Text;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;

namespace Microsoft.CodeAnalysis.CSharp.ExpressionEvaluator
{
    internal static class SyntaxHelpers
    {
        internal static readonly CSharpParseOptions ParseOptions = CSharpParseOptions.Default.WithLanguageVersion(LanguageVersionFacts.CurrentVersion);
#if XSHARP
        internal static CSharpParseOptions XSharpParseOptions { get; set; } = null;
#endif

        /// <summary>
        /// Parse expression. Returns null if there are any errors.
        /// </summary>
        internal static ExpressionSyntax? ParseExpression(
            this string expr,
            DiagnosticBag diagnostics,
            bool allowFormatSpecifiers,
            out ReadOnlyCollection<string>? formatSpecifiers)
        {
            // Remove trailing semi-colon if any. This is to support copy/paste
            // of (simple cases of) RHS of assignment in Watch window, not to
            // allow arbitrary syntax after the semi-colon, not even comments.
            if (RemoveSemicolonIfAny(ref expr))
            {
                // Format specifiers are not expected before a semi-colon.
                allowFormatSpecifiers = false;
            }

            var syntax = ParseDebuggerExpression(expr, consumeFullText: !allowFormatSpecifiers);
            diagnostics.AddRange(syntax.GetDiagnostics());
            formatSpecifiers = null;

            if (allowFormatSpecifiers)
            {
                var builder = ArrayBuilder<string>.GetInstance();
                if (ParseFormatSpecifiers(builder, expr, syntax.FullWidth, diagnostics) &&
                    builder.Count > 0)
                {
                    formatSpecifiers = new ReadOnlyCollection<string>(builder.ToArray());
                }

                builder.Free();
            }
            return diagnostics.HasAnyErrors() ? null : syntax;
        }

        internal static ExpressionSyntax? ParseAssignment(
            this string target,
            string expr,
            DiagnosticBag diagnostics)
        {
            var text = SourceText.From(expr);
            var expression = ParseDebuggerExpressionInternal(text, consumeFullText: true);
            // We're creating a SyntaxTree for just the RHS so that the Diagnostic spans for parse errors
            // will be correct (with respect to the original input text).  If we ever expose a SemanticModel
            // for debugger expressions, we should use this SyntaxTree.
            var syntaxTree = expression.CreateSyntaxTree(text);
            diagnostics.AddRange(syntaxTree.GetDiagnostics());
            if (diagnostics.HasAnyErrors())
            {
                return null;
            }

            // Any Diagnostic spans produced in binding will be offset by the length of the "target" expression text.
            // If we want to support live squiggles in debugger windows, SemanticModel, etc, we'll want to address this.
            var targetSyntax = ParseDebuggerExpressionInternal(SourceText.From(target), consumeFullText: true);
            Debug.Assert(!targetSyntax.GetDiagnostics().Any(), "The target of an assignment should never contain Diagnostics if we're being allowed to assign to it in the debugger.");

            var assignment = InternalSyntax.SyntaxFactory.AssignmentExpression(
                SyntaxKind.SimpleAssignmentExpression,
                targetSyntax,
                InternalSyntax.SyntaxFactory.Token(SyntaxKind.EqualsToken),
                expression);
            return assignment.MakeDebuggerExpression(SourceText.From(assignment.ToString()));
        }

        /// <summary>
        /// Parse statement. Returns null if there are any errors.
        /// </summary>
        internal static StatementSyntax? ParseStatement(
            this string expr,
            DiagnosticBag diagnostics)
        {
            var syntax = ParseDebuggerStatement(expr);
            diagnostics.AddRange(syntax.GetDiagnostics());
            return diagnostics.HasAnyErrors() ? null : syntax;
        }

        /// <summary>
        /// Return set of identifier tokens, with leading and
        /// trailing spaces and comma separators removed.
        /// </summary>
        private static bool ParseFormatSpecifiers(
            ArrayBuilder<string> builder,
            string expr,
            int offset,
            DiagnosticBag diagnostics)
        {
            bool expectingComma = true;
            int start = -1;
            int n = expr.Length;

            for (; offset < n; offset++)
            {
                var c = expr[offset];
                if (SyntaxFacts.IsWhitespace(c) || (c == ','))
                {
                    if (start >= 0)
                    {
                        var token = expr.Substring(start, offset - start);
                        if (expectingComma)
                        {
                            ReportInvalidFormatSpecifier(token, diagnostics);
                            return false;
                        }
                        builder.Add(token);
                        start = -1;
                        expectingComma = (c != ',');
                    }
                    else if (c == ',')
                    {
                        if (!expectingComma)
                        {
                            ReportInvalidFormatSpecifier(",", diagnostics);
                            return false;
                        }
                        expectingComma = false;
                    }
                }
                else if (start < 0)
                {
                    start = offset;
                }
            }

            if (start >= 0)
            {
                var token = expr.Substring(start);
                if (expectingComma)
                {
                    ReportInvalidFormatSpecifier(token, diagnostics);
                    return false;
                }
                builder.Add(token);
            }
            else if (!expectingComma)
            {
                ReportInvalidFormatSpecifier(",", diagnostics);
                return false;
            }

            // Verify format specifiers are valid identifiers.
            foreach (var token in builder)
            {
                if (!token.All(SyntaxFacts.IsIdentifierPartCharacter))
                {
                    ReportInvalidFormatSpecifier(token, diagnostics);
                    return false;
                }
            }

            return true;
        }

        private static void ReportInvalidFormatSpecifier(string token, DiagnosticBag diagnostics)
        {
            diagnostics.Add(ErrorCode.ERR_InvalidSpecifier, Location.None, token);
        }

        private static bool RemoveSemicolonIfAny(ref string str)
        {
            for (int i = str.Length - 1; i >= 0; i--)
            {
                var c = str[i];
                if (c == ';')
                {
                    str = str.Substring(0, i);
                    return true;
                }
                if (!SyntaxFacts.IsWhitespace(c))
                {
                    break;
                }
            }
            return false;
        }

        private static ExpressionSyntax ParseDebuggerExpression(string text, bool consumeFullText)
        {
            var source = SourceText.From(text);
            var expression = ParseDebuggerExpressionInternal(source, consumeFullText);
            return expression.MakeDebuggerExpression(source);
        }

#if XSHARP
        private static T ParseDebuggerInternal<T>(string source) where T : InternalSyntax.CSharpSyntaxNode
        {
            string _fileName = "";
            var options = XSharpParseOptions ?? ParseOptions.WithKind(SourceCodeKind.Script).WithMacroScript(false);

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
#endif
        private static InternalSyntax.ExpressionSyntax ParseDebuggerExpressionInternal(SourceText source, bool consumeFullText)
        {
#if XSHARP
            return ParseDebuggerInternal<InternalSyntax.ExpressionSyntax>(source.ToString());
#else
            using var lexer = new InternalSyntax.Lexer(source, ParseOptions, allowPreprocessorDirectives: false);
            using var parser = new InternalSyntax.LanguageParser(lexer, oldTree: null, changes: null, lexerMode: InternalSyntax.LexerMode.DebuggerSyntax);

            var node = parser.ParseExpression();
            if (consumeFullText)
                node = parser.ConsumeUnexpectedTokens(node);
            return node;
#endif
        }

        private static StatementSyntax ParseDebuggerStatement(string text)
        {
#if XSHARP
            var statement = ParseDebuggerInternal<InternalSyntax.StatementSyntax>(text);
            var syntaxTree = statement.CreateSyntaxTree(SourceText.From(text));
            return (StatementSyntax)syntaxTree.GetRoot();
#else
            var source = SourceText.From(text);
            using var lexer = new InternalSyntax.Lexer(source, ParseOptions);
            using var parser = new InternalSyntax.LanguageParser(lexer, oldTree: null, changes: null, lexerMode: InternalSyntax.LexerMode.DebuggerSyntax);

            var statement = parser.ParseStatement();
            var syntaxTree = statement.CreateSyntaxTree(source);
            return (StatementSyntax)syntaxTree.GetRoot();
#endif
        }

        private static SyntaxTree CreateSyntaxTree(this InternalSyntax.CSharpSyntaxNode root, SourceText text)
        {
            return CSharpSyntaxTree.CreateForDebugger((CSharpSyntaxNode)root.CreateRed(), text, ParseOptions);
        }

        private static ExpressionSyntax MakeDebuggerExpression(this InternalSyntax.ExpressionSyntax expression, SourceText text)
        {
            var syntaxTree = InternalSyntax.SyntaxFactory.ExpressionStatement(attributeLists: default, expression, InternalSyntax.SyntaxFactory.Token(SyntaxKind.SemicolonToken)).CreateSyntaxTree(text);
            return ((ExpressionStatementSyntax)syntaxTree.GetRoot()).Expression;
        }

        internal static string EscapeKeywordIdentifiers(string identifier)
        {
            return SyntaxFacts.IsKeywordKind(SyntaxFacts.GetKeywordKind(identifier)) ? "@" + identifier : identifier;
        }

        /// <remarks>
        /// We don't want to use the real lexer because we want to treat keywords as identifiers.
        /// Since the inputs are so simple, we'll just do the lexing ourselves.
        /// </remarks>
        internal static bool TryParseDottedName(string input, [NotNullWhen(true)] out NameSyntax? output)
        {
            var pooled = PooledStringBuilder.GetInstance();
            try
            {
                var builder = pooled.Builder;

                output = null;
                foreach (var ch in input)
                {
                    if (builder.Length == 0)
                    {
                        if (!SyntaxFacts.IsIdentifierStartCharacter(ch))
                        {
                            output = null;
                            return false;
                        }

                        builder.Append(ch);
                    }
                    else if (ch == '.')
                    {
                        var identifierName = SyntaxFactory.IdentifierName(builder.ToString());

                        builder.Clear();

                        output = output == null
                            ? (NameSyntax)identifierName
                            : SyntaxFactory.QualifiedName(output, identifierName);
                    }
                    else if (SyntaxFacts.IsIdentifierPartCharacter(ch))
                    {
                        builder.Append(ch);
                    }
                    else
                    {
                        output = null;
                        return false;
                    }
                }

                // There must be at least one character in the last identifier.
                if (builder.Length == 0)
                {
                    output = null;
                    return false;
                }

                var finalIdentifierName = SyntaxFactory.IdentifierName(builder.ToString());
                output = output == null
                    ? (NameSyntax)finalIdentifierName
                    : SyntaxFactory.QualifiedName(output, finalIdentifierName);

                return true;
            }
            finally
            {
                pooled.Free();
            }
        }

        internal static NameSyntax PrependExternAlias(IdentifierNameSyntax externAliasSyntax, NameSyntax nameSyntax)
        {
            if (nameSyntax is QualifiedNameSyntax qualifiedNameSyntax)
            {
                return SyntaxFactory.QualifiedName(
                    PrependExternAlias(externAliasSyntax, qualifiedNameSyntax.Left),
                    qualifiedNameSyntax.Right);
            }
            else
            {
                return SyntaxFactory.AliasQualifiedName(externAliasSyntax, (SimpleNameSyntax)nameSyntax);
            }
        }
    }
}
