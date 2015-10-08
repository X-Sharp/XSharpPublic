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
    internal class XSharpTreeTransformation : XSharpBaseListener
    {
        public SyntaxListBuilder<ExternAliasDirectiveSyntax> Externs;
        public SyntaxListBuilder<UsingDirectiveSyntax> Usings;
        public SyntaxListBuilder<AttributeListSyntax> Attributes;
        public SyntaxListBuilder<MemberDeclarationSyntax> Members;
        internal SyntaxListPool _pool;
        private readonly ContextAwareSyntax _syntaxFactory; // Has context, the fields of which are resettable.
        private XSharpParser _parser;

        public XSharpTreeTransformation(XSharpParser parser, SyntaxListPool pool, ContextAwareSyntax syntaxFactory)
        {
            Externs = pool.Allocate<ExternAliasDirectiveSyntax>();
            Usings = pool.Allocate<UsingDirectiveSyntax>();
            Attributes = pool.Allocate<AttributeListSyntax>();
            Members = pool.Allocate<MemberDeclarationSyntax>();
            _pool = pool;
            _syntaxFactory = syntaxFactory;
            _parser = parser;
        }

        internal void Free()
        {
            _pool.Free(Members);
            _pool.Free(Attributes);
            _pool.Free(Usings);
            _pool.Free(Externs);
        }

        public NameSyntax GenerateQualifiedName(string name)
        {
            string[] ids = name.Split('.');
            if (ids.Length == 1)
            {
                return _syntaxFactory.IdentifierName(SyntaxToken.Identifier(name));
            }
            else if (ids.Length > 1)
            {
                NameSyntax r = _syntaxFactory.IdentifierName(SyntaxToken.Identifier(ids[0]));
                for(int i = 1; i < ids.Length; i++)
                {
                    r = _syntaxFactory.QualifiedName(
                        r,
                        SyntaxFactory.MissingToken(SyntaxKind.DotToken),
                        _syntaxFactory.IdentifierName(SyntaxToken.Identifier(ids[i])) );
                }
                return r;
            }
            return null;
        }

        private void GenerateAttributeList(SyntaxListBuilder<AttributeListSyntax> attributeLists, params string[] attributeNames)
        {
            SeparatedSyntaxListBuilder<AttributeSyntax> attributes = _pool.AllocateSeparated<AttributeSyntax>();
            foreach (var attributeName in attributeNames)
            {
                if (attributes.Count > 0)
                {
                    attributes.AddSeparator(SyntaxFactory.MissingToken(SyntaxKind.CommaToken));
                }
                attributes.Add(_syntaxFactory.Attribute(
                    name: GenerateQualifiedName(attributeName),
                    argumentList: null));
            }
            attributeLists.Add(_syntaxFactory.AttributeList(
                openBracketToken: SyntaxFactory.MissingToken(SyntaxKind.OpenBracketToken),
                target: null,
                attributes: attributes,
                closeBracketToken: SyntaxFactory.MissingToken(SyntaxKind.CloseBracketToken)));
            _pool.Free(attributes);
        }

        private ClassDeclarationSyntax GenerateClass(string className, SyntaxListBuilder<MemberDeclarationSyntax> members)
        {
            SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
            GenerateAttributeList(attributeLists, "System.Runtime.CompilerServices.CompilerGenerated");
            SyntaxListBuilder modifiers = _pool.Allocate();
            modifiers.Add(SyntaxFactory.MissingToken(SyntaxKind.InternalKeyword));
            modifiers.Add(SyntaxFactory.MissingToken(SyntaxKind.StaticKeyword));
            var r = _syntaxFactory.ClassDeclaration(
                attributeLists: attributeLists,
                modifiers: modifiers.ToTokenList(),
                keyword: SyntaxFactory.MissingToken(SyntaxKind.ClassKeyword),
                identifier: SyntaxFactory.Identifier(className),
                typeParameterList: null,
                baseList: null, // BaseListSyntax baseList = _syntaxFactory.BaseList(colon, list)
                constraintClauses: default(SyntaxListBuilder<TypeParameterConstraintClauseSyntax>),
                openBraceToken: SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken));
            _pool.Free(attributeLists);
            _pool.Free(modifiers);
            return r;
        }

        private ClassDeclarationSyntax GenerateGlobalClass(string className, SyntaxListBuilder<MemberDeclarationSyntax> members)
        {
            SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
            GenerateAttributeList(attributeLists, 
                "System.Runtime.CompilerServices.CompilerGenerated",
                "System.Runtime.CompilerServices.CompilerGlobalScope");
            SyntaxListBuilder modifiers = _pool.Allocate();
            modifiers.Add(SyntaxFactory.MissingToken(SyntaxKind.PartialKeyword));
            modifiers.Add(SyntaxFactory.MissingToken(SyntaxKind.PublicKeyword));
            modifiers.Add(SyntaxFactory.MissingToken(SyntaxKind.StaticKeyword));
            var r = _syntaxFactory.ClassDeclaration(
                attributeLists: attributeLists,
                modifiers: modifiers.ToTokenList(),
                keyword: SyntaxFactory.MissingToken(SyntaxKind.ClassKeyword),
                identifier: SyntaxFactory.Identifier(className),
                typeParameterList: null,
                baseList: null, // BaseListSyntax baseList = _syntaxFactory.BaseList(colon, list)
                constraintClauses: default(SyntaxListBuilder<TypeParameterConstraintClauseSyntax>),
                openBraceToken: SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken) );
            _pool.Free(attributeLists);
            _pool.Free(modifiers);
            return r;
        }

        private MethodDeclarationSyntax GenerateMainMethod(string startMethodName)
        {
            SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
            GenerateAttributeList(attributeLists, "System.Runtime.CompilerServices.CompilerGenerated");
            SyntaxListBuilder modifiers = _pool.Allocate();
            modifiers.Add(SyntaxFactory.MissingToken(SyntaxKind.StaticKeyword));
            ParameterListSyntax paramList;
            {
                var parameters = _pool.AllocateSeparated<ParameterSyntax>();
                paramList = _syntaxFactory.ParameterList(SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                    parameters,
                    SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken));
                _pool.Free(parameters);
            }
            BlockSyntax blockBody;
            {
                var statements = _pool.Allocate<StatementSyntax>();
                {
                    ArgumentListSyntax argList = _syntaxFactory.ArgumentList(
                        openParenToken: SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken), 
                        arguments: default(SeparatedSyntaxList<ArgumentSyntax>), 
                        closeParenToken: SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken));
                    statements.Add(_syntaxFactory.ExpressionStatement(
                        expression: _syntaxFactory.InvocationExpression(GenerateQualifiedName(startMethodName), argList), 
                        semicolonToken: SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
                }
                blockBody = _syntaxFactory.Block(
                    openBraceToken: SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken), 
                    statements: statements,
                    closeBraceToken: SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken));
                _pool.Free(statements);
            }
            var r = _syntaxFactory.MethodDeclaration(
                attributeLists: attributeLists,
                modifiers: modifiers.ToTokenList(),
                returnType: _syntaxFactory.PredefinedType(SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword)),
                explicitInterfaceSpecifier: null,
                identifier: SyntaxFactory.Identifier("Main"),
                typeParameterList: null,
                parameterList: paramList,
                constraintClauses: default(SyntaxListBuilder<TypeParameterConstraintClauseSyntax>),
                body: blockBody,
                expressionBody: null,
                semicolonToken: SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken));
            _pool.Free(attributeLists);
            _pool.Free(modifiers);
            return r;
        }

        protected virtual TNode WithAdditionalDiagnostics<TNode>(TNode node, params DiagnosticInfo[] diagnostics) where TNode : CSharpSyntaxNode
        {
            DiagnosticInfo[] existingDiags = node.GetDiagnostics();
            int existingLength = existingDiags.Length;
            if (existingLength == 0)
            {
                return node.WithDiagnosticsGreen(diagnostics);
            }
            else
            {
                DiagnosticInfo[] result = new DiagnosticInfo[existingDiags.Length + diagnostics.Length];
                existingDiags.CopyTo(result, 0);
                diagnostics.CopyTo(result, existingLength);
                return node.WithDiagnosticsGreen(result);
            }
        }

        public override void VisitErrorNode([NotNull] IErrorNode node)
        {
        }

        public override void VisitTerminal(ITerminalNode node)
        {
        }

        public override void ExitEveryRule([NotNull] ParserRuleContext context)
        {
            if (context.HasErrors() && context.CsNode != null && context.CsNode is CSharpSyntaxNode)
            {
                foreach (var e in context.ErrorData)
                {
                    var csNode = (CSharpSyntaxNode)context.CsNode;
                    context.Put(WithAdditionalDiagnostics(csNode,
                        new SyntaxDiagnosticInfo(csNode.GetLeadingTriviaWidth(), csNode.Width, e.Code, e.Args)));
                }
            }
        }

        public override void ExitEntity([NotNull] XSharpParser.EntityContext context)
        {
        }

        public override void ExitEof([NotNull] XSharpParser.EofContext context)
        {
        }

        public override void ExitEos([NotNull] XSharpParser.EosContext context)
        {
        }

        public override void ExitSource([NotNull] XSharpParser.SourceContext context)
        {
            SyntaxListBuilder<MemberDeclarationSyntax> startClassMembers = _pool.Allocate<MemberDeclarationSyntax>();
            startClassMembers.Add(GenerateMainMethod("Xs$Globals.Start"));
            Members.Add( GenerateClass("Xs$Local", startClassMembers) );
            _pool.Free(startClassMembers);
        }

        public override void ExitFunction([NotNull] XSharpParser.FunctionContext context)
        {
            SyntaxListBuilder<MemberDeclarationSyntax> globalClassMembers = _pool.Allocate<MemberDeclarationSyntax>();
            {
                SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
                GenerateAttributeList(attributeLists, "System.Runtime.CompilerServices.CompilerGenerated");
                SyntaxListBuilder modifiers = _pool.Allocate();
                modifiers.Add(SyntaxFactory.MissingToken(SyntaxKind.StaticKeyword));
                modifiers.Add(SyntaxFactory.MissingToken(SyntaxKind.PublicKeyword));
                var m = _syntaxFactory.MethodDeclaration(
                    attributeLists: attributeLists,
                    modifiers: modifiers.ToTokenList(),
                    returnType: context.Type.Get<TypeSyntax>(),
                    explicitInterfaceSpecifier: null,
                    identifier: context.Id.Get<SyntaxToken>(),
                    typeParameterList: null,
                    parameterList: context.ParamList.Get<ParameterListSyntax>(),
                    constraintClauses: default(SyntaxListBuilder<TypeParameterConstraintClauseSyntax>),
                    body: context.StmtBlk.Get<BlockSyntax>(),
                    expressionBody: null,
                    semicolonToken: SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken));
                context.Put(m);
                globalClassMembers.Add(m);
                _pool.Free(modifiers);
                _pool.Free(attributeLists);
            }
            Members.Add(GenerateGlobalClass("Xs$Globals", globalClassMembers));
            _pool.Free(globalClassMembers);
        }

        public override void ExitProcedure([NotNull] XSharpParser.ProcedureContext context)
        {
            SyntaxListBuilder<MemberDeclarationSyntax> globalClassMembers = _pool.Allocate<MemberDeclarationSyntax>();
            {
                SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
                GenerateAttributeList(attributeLists, "System.Runtime.CompilerServices.CompilerGenerated");
                SyntaxListBuilder modifiers = _pool.Allocate();
                modifiers.Add(SyntaxFactory.MissingToken(SyntaxKind.StaticKeyword));
                modifiers.Add(SyntaxFactory.MissingToken(SyntaxKind.PublicKeyword));
                var m = _syntaxFactory.MethodDeclaration(
                    attributeLists: attributeLists,
                    modifiers: modifiers.ToTokenList(),
                    returnType: _syntaxFactory.PredefinedType(SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword)),
                    explicitInterfaceSpecifier: null,
                    identifier: context.Id.Get<SyntaxToken>(),
                    typeParameterList: null,
                    parameterList: context.ParamList.Get<ParameterListSyntax>(),
                    constraintClauses: default(SyntaxListBuilder<TypeParameterConstraintClauseSyntax>),
                    body: context.StmtBlk.Get<BlockSyntax>(),
                    expressionBody: null,
                    semicolonToken: SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken));
                context.Put(m);
                globalClassMembers.Add(m);
                _pool.Free(modifiers);
                _pool.Free(attributeLists);
            }
            Members.Add(GenerateGlobalClass("Xs$Globals", globalClassMembers));
            _pool.Free(globalClassMembers);
        }

        public override void ExitParameterList([NotNull] XSharpParser.ParameterListContext context)
        {
            var @params = _pool.AllocateSeparated<ParameterSyntax>();
            foreach (var paramCtx in context._Params)
            {
                @params.Add(paramCtx.Get<ParameterSyntax>());
            }
            context.Put(_syntaxFactory.ParameterList(
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                @params,
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken)));
            _pool.Free(@params);
        }

        public override void ExitParameter([NotNull] XSharpParser.ParameterContext context)
        {
            SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Modifiers)
            {
                if (m.Type != XSharpParser.AS && m.Type != XSharpParser.IS)
                    modifiers.Add(m.Syntax());
            }
            context.Put(_syntaxFactory.Parameter(
                attributeLists: attributeLists,
                modifiers: modifiers.ToTokenList(),
                type: context.Type.Get<TypeSyntax>(),
                identifier: context.Id.Get<SyntaxToken>(),
                @default: _syntaxFactory.EqualsValueClause(
                    SyntaxFactory.MissingToken(SyntaxKind.EqualsToken),
                    context.Default.Get<ExpressionSyntax>())));
            _pool.Free(attributeLists);
            _pool.Free(modifiers);
        }

        public override void ExitStatementBlock([NotNull] XSharpParser.StatementBlockContext context)
        {
            var openBrace = SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken);
            var closeBrace = SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken);
            var statements = _pool.Allocate<StatementSyntax>();
            foreach (var stmtCtx in context._Stmts)
            {
                statements.Add(stmtCtx.Get<StatementSyntax>());
            }
            context.Put(_syntaxFactory.Block(openBrace, statements, closeBrace));
            _pool.Free(statements);
        }

        public override void ExitReturnStmt([NotNull] XSharpParser.ReturnStmtContext context)
        {
            var @return = SyntaxFactory.MissingToken(SyntaxKind.ReturnKeyword);
            ExpressionSyntax arg = null;
            var _semicolon = SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken);
            context.Put(_syntaxFactory.ReturnStatement(@return, arg, _semicolon));
        }

        public override void ExitExpressionStmt([NotNull] XSharpParser.ExpressionStmtContext context)
        {
            var _semicolon = SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken);
            var statements = _pool.Allocate<StatementSyntax>();
            foreach (var exprCtx in context._Exprs)
            {
                statements.Add(_syntaxFactory.ExpressionStatement(exprCtx.Get<ExpressionSyntax>(), _semicolon));
            }
            var openBrace = SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken);
            var closeBrace = SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken);
            context.Put(_syntaxFactory.Block(openBrace, statements, closeBrace));
            _pool.Free(statements);
        }

        public override void ExitExpressionList([NotNull] XSharpParser.ExpressionListContext context)
        {
        }

        public override void ExitAccessMember([NotNull] XSharpParser.AccessMemberContext context)
         {
            context.Put(
                    _syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                            context.Left.Get<ExpressionSyntax>(),
                            SyntaxFactory.MissingToken(SyntaxKind.DotToken),
                            context.Right.Get<IdentifierNameSyntax>()));
        }

        public override void ExitPostfixExpression([NotNull] XSharpParser.PostfixExpressionContext context)
        {
            SyntaxKind kind;
            switch (context.Op.Type)
            {
                case XSharpParser.INC:
                    kind = SyntaxKind.PostIncrementExpression;
                    break;
                case XSharpParser.DEC:
                    kind = SyntaxKind.PostDecrementExpression;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            context.Put(_syntaxFactory.PostfixUnaryExpression(
                kind,
                context.Expr.Get<ExpressionSyntax>(),
                context.Op.Syntax()));
        }

        public override void ExitPrefixExpression([NotNull] XSharpParser.PrefixExpressionContext context)
        {
            SyntaxKind kind;
            switch (context.Op.Type)
            {
                case XSharpParser.PLUS:
                    kind = SyntaxKind.UnaryPlusExpression;
                    break;
                case XSharpParser.MINUS:
                    kind = SyntaxKind.UnaryMinusExpression;
                    break;
                case XSharpParser.TILDE:
                    kind = SyntaxKind.BitwiseNotExpression;
                    break;
                case XSharpParser.ADDROF:
                    kind = SyntaxKind.AddressOfExpression;
                    break;
                case XSharpParser.INC:
                    kind = SyntaxKind.PreIncrementExpression;
                    break;
                case XSharpParser.DEC:
                    kind = SyntaxKind.PreDecrementExpression;
                    break;
                case XSharpParser.LOGIC_NOT:
                    kind = SyntaxKind.LogicalNotExpression;
                    break;
                case XSharpParser.LOGIC_XOR:
                    kind = SyntaxKind.BitwiseNotExpression;
                    break;
                case XSharpParser.NOT:
                    kind = SyntaxKind.LogicalNotExpression;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            context.Put(_syntaxFactory.PrefixUnaryExpression(
                kind,
                context.Op.Syntax(),
                context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitBinaryExpression([NotNull] XSharpParser.BinaryExpressionContext context)
        {
            context.Put(_syntaxFactory.BinaryExpression(
                context.Op.ExpressionKind(),
                context.Left.Get<ExpressionSyntax>(),
                context.Op.Syntax(),
                context.Right.Get<ExpressionSyntax>()));
        }

        public override void ExitAssignmentExpression([NotNull] XSharpParser.AssignmentExpressionContext context)
        {
            context.Put(_syntaxFactory.AssignmentExpression(
                context.Op.ExpressionKind(),
                context.Left.Get<ExpressionSyntax>(),
                context.Op.Syntax(),
                context.Right.Get<ExpressionSyntax>()));
        }

        public override void ExitMethodCall([NotNull] XSharpParser.MethodCallContext context)
        {
            var expr = context.Expr.Get<ExpressionSyntax>();
            ArgumentListSyntax argList;
            if (context.ArgList != null)
                argList = context.ArgList.Get<ArgumentListSyntax>();
            else
            {
                var openParen = SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken);
                var closeParen = SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken);
                var args = default(SeparatedSyntaxList<ArgumentSyntax>);
                argList = _syntaxFactory.ArgumentList(openParen, args, closeParen);
            }
            context.Put(_syntaxFactory.InvocationExpression(expr, argList));
        }

        public override void ExitCtorCall([NotNull] XSharpParser.CtorCallContext context)
        {
            var type = context.Type.Get<TypeSyntax>();
            ArgumentListSyntax argList;
            if (context.ArgList != null)
                argList = context.ArgList.Get<ArgumentListSyntax>();
            else
            {
                var openParen = SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken);
                var closeParen = SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken);
                var args = default(SeparatedSyntaxList<ArgumentSyntax>);
                argList = _syntaxFactory.ArgumentList(openParen, args, closeParen);
            }
            context.Put(_syntaxFactory.ObjectCreationExpression(
                SyntaxFactory.MissingToken(SyntaxKind.NewKeyword),
                type, 
                argList,
                initializer: null));
        }

        public override void ExitArrayAccess([NotNull] XSharpParser.ArrayAccessContext context)
        {
            var expr = context.Expr.Get<ExpressionSyntax>();
            var openBracket = SyntaxFactory.MissingToken(SyntaxKind.OpenBracketToken);
            var closeBracket = SyntaxFactory.MissingToken(SyntaxKind.CloseBracketToken);
            BracketedArgumentListSyntax argList;
            if (context.ArgList != null)
            {
                var args = _pool.AllocateSeparated<ArgumentSyntax>();
                foreach(var e in context.ArgList._Exprs)
                {
                    if (args.Count != 0)
                        args.AddSeparator(SyntaxFactory.MissingToken(SyntaxKind.CommaToken));
                    args.Add(_syntaxFactory.Argument(null, null, e.Get<ExpressionSyntax>()));
                }
                argList = _syntaxFactory.BracketedArgumentList(openBracket, args, closeBracket);
                _pool.Free(args);
            }
            else
            {
                var args = default(SeparatedSyntaxList<ArgumentSyntax>);
                argList = _syntaxFactory.BracketedArgumentList(openBracket, args, closeBracket);
            }
            context.Put(_syntaxFactory.ElementAccessExpression(
                expr,
                argList));
        }

        public override void ExitNameExpression([NotNull] XSharpParser.NameExpressionContext context)
        {
            context.Put(context.Name.Get<NameSyntax>());
        }

        public override void ExitTypeExpression([NotNull] XSharpParser.TypeExpressionContext context)
        {
            context.Put(context.Type.Get<TypeSyntax>());
        }

        public override void ExitIifExpression([NotNull] XSharpParser.IifExpressionContext context)
        {
            context.Put(context.Expr.Get<ExpressionSyntax>());
        }

        public override void ExitParenExpression([NotNull] XSharpParser.ParenExpressionContext context)
        {
            context.Put(_syntaxFactory.ParenthesizedExpression(
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken)));
        }

        public override void ExitTypeCast([NotNull] XSharpParser.TypeCastContext context)
        {
            context.Put(_syntaxFactory.CastExpression(
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                context.Type.Get<TypeSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken),
                context.Expr.Get<TypeSyntax>()));
        }

        public override void ExitSizeOfExpression([NotNull] XSharpParser.SizeOfExpressionContext context)
        {
            context.Put(_syntaxFactory.SizeOfExpression(
                SyntaxFactory.MissingToken(SyntaxKind.SizeOfKeyword),
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                context.Type.Get<TypeSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken)));
        }

        public override void ExitTypeOfExpression([NotNull] XSharpParser.TypeOfExpressionContext context)
        {
            context.Put(_syntaxFactory.TypeOfExpression(
                SyntaxFactory.MissingToken(SyntaxKind.TypeOfKeyword),
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                context.Type.Get<TypeSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken)));
        }

        public override void ExitArgumentList([NotNull] XSharpParser.ArgumentListContext context)
        {
            var args = _pool.AllocateSeparated<ArgumentSyntax>();
            var openParen = SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken);
            var closeParen = SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken);
            foreach (var argCtx in context._Args)
            {
                args.Add(argCtx.Get<ArgumentSyntax>());
            }
            context.Put(_syntaxFactory.ArgumentList(openParen, args, closeParen));
            _pool.Free(args);
        }

        public override void ExitArgument([NotNull] XSharpParser.ArgumentContext context)
        {
            context.Put(_syntaxFactory.Argument(null, null, context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitQualifiedName([NotNull] XSharpParser.QualifiedNameContext context)
        {
            context.Put(_syntaxFactory.QualifiedName(context.Left.Get<NameSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.DotToken),
                _syntaxFactory.IdentifierName(context.Right.Get<SyntaxToken>())));
        }

        public override void ExitSimpleName([NotNull] XSharpParser.SimpleNameContext context)
        {
            context.Put(_syntaxFactory.IdentifierName(context.Id.Get<SyntaxToken>()));
        }

        public override void ExitGenericName([NotNull] XSharpParser.GenericNameContext context)
        {
            context.Put(_syntaxFactory.GenericName(context.Id.Get<SyntaxToken>(), context.GenericArgList.Get<TypeArgumentListSyntax>()));
        }

        public override void ExitGenericArgumentList([NotNull] XSharpParser.GenericArgumentListContext context)
        {
            var types = _pool.AllocateSeparated<TypeSyntax>();
            foreach (var type in context._GenericArgs)
            {
                if (types.Count != 0)
                    types.AddSeparator(SyntaxFactory.MissingToken(SyntaxKind.CommaToken));
                types.Add(context.Get<TypeSyntax>());
            }
            context.Put(_syntaxFactory.TypeArgumentList(
                SyntaxFactory.MissingToken(SyntaxKind.LessThanToken),
                types.ToList(),
                SyntaxFactory.MissingToken(SyntaxKind.GreaterThanToken)
                ));
            _pool.Free(types);
        }

        public override void ExitIdentifierName([NotNull] XSharpParser.IdentifierNameContext context)
        {
            context.Put(_syntaxFactory.IdentifierName(context.Id.Get<SyntaxToken>()));
        }

        public override void ExitPtrDatatype([NotNull] XSharpParser.PtrDatatypeContext context)
        {
            context.Put(
                _syntaxFactory.PointerType(context.TypeName.Get<TypeSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.AsteriskToken)));
        }

        public override void ExitArrayDatatype([NotNull] XSharpParser.ArrayDatatypeContext context)
        {
            var ranks = _pool.Allocate<ArrayRankSpecifierSyntax>();
            foreach (var rankCtx in context._Ranks)
            {
                ranks.Add(rankCtx.Get<ArrayRankSpecifierSyntax>());
            }
            context.Put(_syntaxFactory.ArrayType(context.TypeName.Get<TypeSyntax>(), ranks));
            _pool.Free(ranks);
        }

        public override void ExitArrayRank([NotNull] XSharpParser.ArrayRankContext context)
        {
            var sizes = _pool.AllocateSeparated<ExpressionSyntax>();
            var omittedArraySizeExpressionInstance = _syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MissingToken(SyntaxKind.OmittedArraySizeExpressionToken));
            foreach (var comma in context.COMMA())
            {
                sizes.Add(omittedArraySizeExpressionInstance);
                sizes.AddSeparator(SyntaxFactory.MissingToken(SyntaxKind.CommaToken));
            }
            sizes.Add(omittedArraySizeExpressionInstance);
            context.Put(_syntaxFactory.ArrayRankSpecifier(
                SyntaxFactory.MissingToken(SyntaxKind.OpenBracketToken),
                sizes,
                SyntaxFactory.MissingToken(SyntaxKind.CloseBracketToken)));
            _pool.Free(sizes);
        }

        public override void ExitSimpleDatatype([NotNull] XSharpParser.SimpleDatatypeContext context)
        {
            context.Put(context.TypeName.Get<TypeSyntax>());
        }

        public override void ExitTypeName([NotNull] XSharpParser.TypeNameContext context)
        {
            if (context.NativeType != null)
                context.Put(context.NativeType.Get<PredefinedTypeSyntax>());
            else if (context.Name != null)
                context.Put(context.Name.Get<NameSyntax>());
        }

        public override void ExitLiteralExpression([NotNull] XSharpParser.LiteralExpressionContext context)
        {
            context.Put(context.Literal.Get<LiteralExpressionSyntax>());
        }

        public override void ExitLiteralArrayExpression([NotNull] XSharpParser.LiteralArrayExpressionContext context)
        {
            context.Put(context.LiteralArray.Get<InitializerExpressionSyntax>());
        }

        public override void ExitIif([NotNull] XSharpParser.IifContext context)
        {
            context.Put(_syntaxFactory.ConditionalExpression(
                context.Cond.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.QuestionToken),
                context.TrueExpr.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.ColonToken),
                context.FalseExpr.Get<ExpressionSyntax>()));
        }

        public override void ExitLiteralArray([NotNull] XSharpParser.LiteralArrayContext context)
        {
            var openBrace = SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken);
            var closeBrace = SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken);
            if (context.ExprList != null)
            {
                var exprs = _pool.AllocateSeparated<ExpressionSyntax>();
                foreach (var e in context.ExprList._Exprs)
                {
                    if (exprs.Count != 0)
                        exprs.AddSeparator(SyntaxFactory.MissingToken(SyntaxKind.CommaToken));
                    exprs.Add(e.Get<ExpressionSyntax>());
                }
                context.Put(_syntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression, openBrace, exprs, closeBrace));
                _pool.Free(exprs);
            }
            else
            {
                var exprs = default(SeparatedSyntaxList<ExpressionSyntax>);
                context.Put(_syntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression, openBrace, exprs, closeBrace));
            }
        }

        public override void ExitCodeblockExpression([NotNull] XSharpParser.CodeblockExpressionContext context)
        {
            context.Put(context.CbExpr.Get<LambdaExpressionSyntax>());
        }

        public override void ExitCodeblock([NotNull] XSharpParser.CodeblockContext context)
        {
            context.Put(_syntaxFactory.ParenthesizedLambdaExpression(
                asyncKeyword: null,
                parameterList: context.CbParamList.Get<ParameterListSyntax>(),
                arrowToken: SyntaxFactory.MissingToken(SyntaxKind.EqualsGreaterThanToken), 
                body: context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitCodeblockParamList([NotNull] XSharpParser.CodeblockParamListContext context)
        {
            var @params = _pool.AllocateSeparated<ParameterSyntax>();
            foreach (var idCtx in context._Ids)
            {
                SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
                SyntaxListBuilder modifiers = _pool.Allocate();
                @params.Add(_syntaxFactory.Parameter(
                    attributeLists: attributeLists,
                    modifiers: modifiers.ToTokenList(),
                    type: null,
                    identifier: idCtx.Get<SyntaxToken>(),
                    @default: null));
                _pool.Free(attributeLists);
                _pool.Free(modifiers);
            }
            context.Put(_syntaxFactory.ParameterList(
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                @params,
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken)));
            _pool.Free(@params);
        }

        public override void ExitLiteralValue([NotNull] XSharpParser.LiteralValueContext context)
        {
            context.Put(_syntaxFactory.LiteralExpression(context.Token.ExpressionKind(), context.Token.Syntax()));
        }

        public override void ExitIdentifier([NotNull] XSharpParser.IdentifierContext context)
        {
            context.Put(context.Token.Syntax());
        }

        public override void ExitNativeType([NotNull] XSharpParser.NativeTypeContext context)
        {
            context.Put(_syntaxFactory.PredefinedType(context.Token.Syntax()));
        }

        public override void ExitAccessModifier([NotNull] XSharpParser.AccessModifierContext context)
        {
            context.Put(_syntaxFactory.PredefinedType(context.Token.Syntax()));
        }
    }
}