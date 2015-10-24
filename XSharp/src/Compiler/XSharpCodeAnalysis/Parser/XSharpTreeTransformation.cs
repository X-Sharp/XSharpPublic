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
        public static SyntaxTree DefaultXSharpSyntaxTree = GenerateDefaultTree();

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

        SyntaxList<SyntaxToken> TokenList(params SyntaxKind[] kinds)
        {
            var rb = _pool.Allocate();
            foreach(var k in kinds) {
                rb.Add(SyntaxFactory.MissingToken(k));
            }
            var r = rb.ToTokenList();
            _pool.Free(rb);
            return r;
        }

        SyntaxList<SyntaxToken> EmptyList()
        {
            var rb = _pool.Allocate();
            var r = rb.ToTokenList();
            _pool.Free(rb);
            return r;
        }

        SyntaxList<T> EmptyList<T>() where T : CSharpSyntaxNode
        {
            var rb = _pool.Allocate<T>();
            var r = rb.ToList();
            _pool.Free(rb);
            return r;
        }

        SeparatedSyntaxList<T> EmptySeparatedList<T>() where T : CSharpSyntaxNode
        {
            var rb = _pool.AllocateSeparated<T>();
            var r = rb.ToList();
            _pool.Free(rb);
            return r;
        }

        TypeSyntax VoidType()
        {
            return _syntaxFactory.PredefinedType(SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword));
        }

        TypeSyntax EmptyType()
        {
            return _syntaxFactory.PredefinedType(SyntaxFactory.MissingToken(SyntaxKind.VoidKeyword));
        }

        ParameterListSyntax EmptyParameterList()
        {
            return _syntaxFactory.ParameterList(
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                EmptySeparatedList<ParameterSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken));
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

        private ClassDeclarationSyntax GenerateGlobalClass(string className, params MemberDeclarationSyntax[] members)
        {
            SyntaxListBuilder<MemberDeclarationSyntax> globalClassMembers = _pool.Allocate<MemberDeclarationSyntax>();
            SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
            if (members.Length > 0) {
                foreach(var m in members)
                    globalClassMembers.Add(m);
            }
            else {
                GenerateAttributeList(attributeLists, 
                    "System.Runtime.CompilerServices.CompilerGenerated",
                    "System.Runtime.CompilerServices.CompilerGlobalScope");
            }
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
                members: globalClassMembers,
                closeBraceToken: SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken) );
            _pool.Free(attributeLists);
            _pool.Free(modifiers);
            _pool.Free(globalClassMembers);
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

        public static SyntaxTree GenerateDefaultTree()
        {
            var t = new XSharpTreeTransformation(null, new SyntaxListPool(), new ContextAwareSyntax(new SyntaxFactoryContext()));

            /*SyntaxListBuilder<MemberDeclarationSyntax> startClassMembers = t._pool.Allocate<MemberDeclarationSyntax>();
            startClassMembers.Add(t.GenerateMainMethod("Xs$Globals.Start"));
            t.Members.Add( t.GenerateClass("Xs$Local", startClassMembers) );
            _pool.Free(startClassMembers);*/

            t.Members.Add(t.GenerateGlobalClass("Xs$Globals"));

            var eof = SyntaxFactory.Token(SyntaxKind.EndOfFileToken);
            return CSharpSyntaxTree.Create(
                (Syntax.CompilationUnitSyntax)t._syntaxFactory.CompilationUnit(t.Externs, t.Usings, t.Attributes, t.Members, eof).CreateRed());
        }

        public override void VisitErrorNode([NotNull] IErrorNode node)
        {
        }

        public override void VisitTerminal(ITerminalNode node)
        {
        }

        public override void EnterEveryRule([NotNull] ParserRuleContext context)
        {
#if DEBUG
            var s = context.GetType().ToString();
            Debug.WriteLine("{0}Enter: {1}",new string(' ',context.Depth()),s.Substring(s.LastIndexOfAny(".+".ToCharArray())+1));
#endif
        }

        public override void ExitEveryRule([NotNull] ParserRuleContext context)
        {
            if (context.HasErrors() && context.CsNode != null && context.CsNode is CSharpSyntaxNode)
            {
                foreach (var e in context.ErrorData)
                {
                    var csNode = (CSharpSyntaxNode)context.CsNode;
                    context.Put(csNode.WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(csNode.GetLeadingTriviaWidth(), csNode.Width, e.Code, e.Args)));
                }
            }
#if DEBUG
            var s = context.GetType().ToString();
            Debug.WriteLine("{0}Exit: {1}",new string(' ',context.Depth()),s.Substring(s.LastIndexOfAny(".+".ToCharArray())+1));
#endif
        }

        public override void ExitSource([NotNull] XSharpParser.SourceContext context)
        {
            foreach(var entityCtx in context._Entities)
            {
                var s = entityCtx.CsNode;
                if (s is MemberDeclarationSyntax)
                    Members.Add(s as MemberDeclarationSyntax);
                else if (s is UsingDirectiveSyntax)
                    Usings.Add(s as UsingDirectiveSyntax);
                else if (s is AttributeListSyntax)
                    Attributes.Add(s as AttributeListSyntax);
                else if (s is ExternAliasDirectiveSyntax)
                    Externs.Add(s as ExternAliasDirectiveSyntax);
            }
            Usings.Add(_syntaxFactory.UsingDirective(SyntaxFactory.MissingToken(SyntaxKind.UsingKeyword), 
                SyntaxFactory.MissingToken(SyntaxKind.StaticKeyword), 
                null,
                _syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$Globals")),
                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitNamespace_([NotNull] XSharpParser.Namespace_Context context)
        {
            var externs = _pool.Allocate<ExternAliasDirectiveSyntax>();
            var usings = _pool.Allocate<UsingDirectiveSyntax>();
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            foreach(var entityCtx in context._Entities)
            {
                var s = entityCtx.CsNode;
                if (s is MemberDeclarationSyntax)
                    members.Add(s as MemberDeclarationSyntax);
                else if (s is UsingDirectiveSyntax)
                    usings.Add(s as UsingDirectiveSyntax);
                else if (s is AttributeListSyntax)
                    //Attributes.Add(s as AttributeListSyntax);
                    context.AddError(new ParseErrorData(entityCtx, ErrorCode.ERR_AttributesNotAllowed));
                else if (s is ExternAliasDirectiveSyntax)
                    externs.Add(s as ExternAliasDirectiveSyntax);
            }
            context.Put(_syntaxFactory.NamespaceDeclaration(SyntaxFactory.MissingToken(SyntaxKind.NamespaceKeyword),
                name: context.Name.Get<NameSyntax>(),
                openBraceToken: SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken),
                externs: externs,
                usings: usings,
                members: members,
                closeBraceToken: SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
            _pool.Free(externs);
            _pool.Free(usings);
            _pool.Free(members);
        }

        public override void ExitEntity([NotNull] XSharpParser.EntityContext context)
        {
            var ch = context.children[0];
            if (ch is XSharpParser.FunctionContext || ch is XSharpParser.ProcedureContext || ch is XSharpParser.VoglobalContext)
                Members.Add(GenerateGlobalClass("Xs$Globals", ch.Get<MemberDeclarationSyntax>()));
            else
                context.Put(ch.Get<CSharpSyntaxNode>());
        }

        public override void ExitEof([NotNull] XSharpParser.EofContext context)
        {
        }

        public override void ExitEos([NotNull] XSharpParser.EosContext context)
        {
        }

        public override void ExitUsing_([NotNull] XSharpParser.Using_Context context)
        {
            context.Put(_syntaxFactory.UsingDirective(SyntaxFactory.MissingToken(SyntaxKind.UsingKeyword),
                staticKeyword: null,
                alias: null,
                name: context.Name.Get<NameSyntax>(),
                semicolonToken: SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitClassvar([NotNull] XSharpParser.ClassvarContext context)
        {
            context.Put(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(),
                null,
                (context.Initializer != null) ? _syntaxFactory.EqualsValueClause(SyntaxFactory.MissingToken(SyntaxKind.EqualsToken),
                    context.Initializer.Get<ExpressionSyntax>()) : null));
        }

        public override void ExitVodefine([NotNull] XSharpParser.VodefineContext context)
        {
            var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            variables.Add(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(),
                null,
                _syntaxFactory.EqualsValueClause(SyntaxFactory.MissingToken(SyntaxKind.EqualsToken),
                    context.Expr.Get<ExpressionSyntax>())));
            context.Put(_syntaxFactory.FieldDeclaration(
                EmptyList<AttributeListSyntax>(),
                TokenList(SyntaxKind.StaticKeyword,SyntaxKind.PublicKeyword),
                _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$var")), variables),
                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
            _pool.Free(variables);
        }

        public override void ExitInterface_([NotNull] XSharpParser.Interface_Context context)
        {
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            foreach(var mCtx in context._Members) {
                members.Add(mCtx.Get<MemberDeclarationSyntax>());
            }
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            foreach(var pCtx in context._Parents) {
                baseTypes.Add(_syntaxFactory.SimpleBaseType(pCtx.Get<TypeSyntax>()));
            }
            context.Put(_syntaxFactory.InterfaceDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: SyntaxFactory.MissingToken(SyntaxKind.InterfaceKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MissingToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: null,
                openBraceToken: SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null));
            _pool.Free(members);
            _pool.Free(baseTypes);
        }

        public override void ExitInterfaceModifiers([NotNull] XSharpParser.InterfaceModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitClass_([NotNull] XSharpParser.Class_Context context)
        {
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            foreach(var mCtx in context._Members) {
                members.Add(mCtx.Get<MemberDeclarationSyntax>());
            }
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            baseTypes.Add(_syntaxFactory.SimpleBaseType(context.BaseType?.Get<TypeSyntax>() 
                ?? _syntaxFactory.PredefinedType(SyntaxFactory.MissingToken(SyntaxKind.ObjectKeyword))));
            foreach(var iCtx in context._Implements) {
                baseTypes.Add(_syntaxFactory.SimpleBaseType(iCtx.Get<TypeSyntax>()));
            }
            context.Put(_syntaxFactory.ClassDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: SyntaxFactory.MissingToken(SyntaxKind.ClassKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MissingToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: null,
                openBraceToken: SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null));
            _pool.Free(members);
            _pool.Free(baseTypes);
        }

        public override void ExitClassModifiers([NotNull] XSharpParser.ClassModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitMethod([NotNull] XSharpParser.MethodContext context)
        {
            context.Put(_syntaxFactory.MethodDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                returnType: context.Type?.Get<TypeSyntax>() ?? EmptyType(),
                explicitInterfaceSpecifier: null,
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                constraintClauses: default(SyntaxListBuilder<TypeParameterConstraintClauseSyntax>),
                body: context.StmtBlk.Get<BlockSyntax>(),
                expressionBody: null,
                semicolonToken: (context.StmtBlk != null) ? null : SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitMethodtype([NotNull] XSharpParser.MethodtypeContext context)
        {
            if (context.Token.Type != XSharpParser.METHOD) {
                context.AddError(new ParseErrorData(ErrorCode.ERR_FeatureNotAvailableInVersion1, context.Token));
            }
        }

        public override void ExitConstructorModifiers([NotNull] XSharpParser.ConstructorModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitDestructorModifiers([NotNull] XSharpParser.DestructorModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitMemberModifiers([NotNull] XSharpParser.MemberModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitAttributes([NotNull] XSharpParser.AttributesContext context)
        {
            var attributeLists = _pool.Allocate<AttributeListSyntax>();
            foreach(var attrBlkCtx in context._AttrBlk) {
                attributeLists.Add(attrBlkCtx.Get<AttributeListSyntax>());
            }
            context.PutList(attributeLists.ToList());
            _pool.Free(attributeLists);
        }

        public override void ExitAttributeBlock([NotNull] XSharpParser.AttributeBlockContext context)
        {
            var attributes = _pool.AllocateSeparated<AttributeSyntax>();
            foreach(var attrCtx in context._Attributes) {
                if (attributes.Count > 0)
                {
                    attributes.AddSeparator(SyntaxFactory.MissingToken(SyntaxKind.CommaToken));
                }
                attributes.Add(attrCtx.Get<AttributeSyntax>());
            }
            context.Put(_syntaxFactory.AttributeList(
                SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken),
                context.Target.Get<AttributeTargetSpecifierSyntax>(),
                attributes,
                SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken)));
            _pool.Free(attributes);
        }

        public override void ExitAttributeTarget([NotNull] XSharpParser.AttributeTargetContext context)
        {
            context.Put(_syntaxFactory.AttributeTargetSpecifier(
                context.Id?.Get<SyntaxToken>() ?? context.Kw.Get<SyntaxToken>(),
                SyntaxFactory.MissingToken(SyntaxKind.ColonToken)));
        }

        public override void ExitAttribute([NotNull] XSharpParser.AttributeContext context)
        {
            var arguments = _pool.AllocateSeparated<AttributeArgumentSyntax>();
            foreach(var paramCtx in context._Params) {
                if (arguments.Count != 0) {
                    arguments.AddSeparator(SyntaxFactory.MissingToken(SyntaxKind.CommaToken));
                }
                arguments.Add(paramCtx.Get<AttributeArgumentSyntax>());
            }
            context.Put(_syntaxFactory.Attribute(
                name: context.Name.Get<NameSyntax>(),
                argumentList: _syntaxFactory.AttributeArgumentList(SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                    arguments,
                    SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken))));
            _pool.Free(arguments);
        }

        public override void ExitPropertyAttributeParam([NotNull] XSharpParser.PropertyAttributeParamContext context)
        {
            context.Put(_syntaxFactory.AttributeArgument(
                _syntaxFactory.NameEquals(context.Name.Get<IdentifierNameSyntax>(), 
                    SyntaxFactory.MissingToken(SyntaxKind.EqualsToken)), 
                null, context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitExprAttributeParam([NotNull] XSharpParser.ExprAttributeParamContext context)
        {
            context.Put(_syntaxFactory.AttributeArgument(null, null, context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitGlobalAttributes([NotNull] XSharpParser.GlobalAttributesContext context)
        {
            var attributes = _pool.AllocateSeparated<AttributeSyntax>();
            foreach(var attrCtx in context._Attributes) {
                if (attributes.Count > 0)
                {
                    attributes.AddSeparator(SyntaxFactory.MissingToken(SyntaxKind.CommaToken));
                }
                attributes.Add(attrCtx.Get<AttributeSyntax>());
            }
            context.Put(_syntaxFactory.AttributeList(
                SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken),
                context.Target.Get<AttributeTargetSpecifierSyntax>(),
                attributes,
                SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken)));
            _pool.Free(attributes);
        }

        public override void ExitGlobalAttributeTarget([NotNull] XSharpParser.GlobalAttributeTargetContext context)
        {
            context.Put(_syntaxFactory.AttributeTargetSpecifier(
                context.Token.SyntaxKeywordIdentifier(),
                SyntaxFactory.MissingToken(SyntaxKind.ColonToken)));
        }

        public override void EnterVoglobal([NotNull] XSharpParser.VoglobalContext context)
        {
            if (context.Const != null) {
                if (context.Modifiers != null)
                    context.Modifiers._Tokens.Add(context.Const);
                else {
                    context.Modifiers = new XSharpParser.FuncprocModifiersContext(context,0);
                    context.Modifiers.PutList(TokenList(SyntaxKind.ConstKeyword,SyntaxKind.StaticKeyword));
                }
            }
        }

        public override void ExitVoglobal([NotNull] XSharpParser.VoglobalContext context)
        {
            var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            foreach(var varCtx in context._Var) {
                variables.Add(varCtx.Get<VariableDeclaratorSyntax>());
            }
            context.Put(_syntaxFactory.FieldDeclaration(
                context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                context.Modifiers?.GetList<SyntaxToken>() ?? TokenList(SyntaxKind.StaticKeyword),
                _syntaxFactory.VariableDeclaration(context.DataType.Get<TypeSyntax>(), variables),
                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
            _pool.Free(variables);
        }

        public override void ExitFunction([NotNull] XSharpParser.FunctionContext context)
        {
            context.Put(_syntaxFactory.MethodDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenList(SyntaxKind.StaticKeyword),
                returnType: context.Type?.Get<TypeSyntax>() ?? EmptyType(),
                explicitInterfaceSpecifier: null,
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                constraintClauses: default(SyntaxListBuilder<TypeParameterConstraintClauseSyntax>),
                body: context.StmtBlk.Get<BlockSyntax>(),
                expressionBody: null,
                semicolonToken: (context.StmtBlk != null) ? null : SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitProcedure([NotNull] XSharpParser.ProcedureContext context)
        {
            context.Put(_syntaxFactory.MethodDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenList(SyntaxKind.StaticKeyword),
                returnType: VoidType(),
                explicitInterfaceSpecifier: null,
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                constraintClauses: default(SyntaxListBuilder<TypeParameterConstraintClauseSyntax>),
                body: context.StmtBlk.Get<BlockSyntax>(),
                expressionBody: null,
                semicolonToken: (context.StmtBlk != null) ? null : SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
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
            context.Put(_syntaxFactory.Parameter(
                attributeLists: attributeLists,
                modifiers: context.Modifiers.GetList<SyntaxToken>(),
                type: context.Type.Get<TypeSyntax>(),
                identifier: context.Id.Get<SyntaxToken>(),
                @default: _syntaxFactory.EqualsValueClause(
                    SyntaxFactory.MissingToken(SyntaxKind.EqualsToken),
                    context.Default.Get<ExpressionSyntax>())));
            _pool.Free(attributeLists);
        }

        public override void ExitParameterDeclMods([NotNull] XSharpParser.ParameterDeclModsContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                if (m.Type != XSharpParser.AS && m.Type != XSharpParser.IS)
                    modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitFuncprocModifiers([NotNull] XSharpParser.FuncprocModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            if (!modifiers.Any(SyntaxKind.StaticKeyword))
                modifiers.Add(SyntaxFactory.MissingToken(SyntaxKind.StaticKeyword));
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitStatementBlock([NotNull] XSharpParser.StatementBlockContext context)
        {
            var openBrace = SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken);
            var closeBrace = SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken);
            var statements = _pool.Allocate<StatementSyntax>();
            foreach (var stmtCtx in context._Stmts)
            {
                if (stmtCtx is XSharpParser.DeclarationStmtContext) {
                    var ld = ((XSharpParser.DeclarationStmtContext)stmtCtx).localdecl();
                    if (ld is XSharpParser.CommonLocalDeclContext)
                        foreach (var decl in (ld as XSharpParser.CommonLocalDeclContext)._LocalVars) statements.Add(decl.Get<StatementSyntax>());
                    else if (ld is XSharpParser.StaticLocalDeclContext)
                        foreach (var decl in (ld as XSharpParser.StaticLocalDeclContext)._LocalVars) statements.Add(decl.Get<StatementSyntax>());
                    else if (ld is XSharpParser.VarLocalDeclContext)
                        foreach (var decl in (ld as XSharpParser.VarLocalDeclContext)._ImpliedVars) statements.Add(decl.Get<StatementSyntax>());
                }
                else
                    statements.Add(stmtCtx.Get<StatementSyntax>());
            }
            context.Put(_syntaxFactory.Block(openBrace, statements, closeBrace));
            _pool.Free(statements);
        }

        public override void ExitExpressionList([NotNull] XSharpParser.ExpressionListContext context)
        {
        }

        public override void ExitDeclarationStmt([NotNull] XSharpParser.DeclarationStmtContext context)
        {
            context.Put(context.localdecl().Get<StatementSyntax>());
        }

        public override void EnterCommonLocalDecl([NotNull] XSharpParser.CommonLocalDeclContext context)
        {
            XSharpParser.DatatypeContext t = null;
            for(var i = context._LocalVars.Count-1; i >= 0; i--) {
                var locCtx = context._LocalVars[i];
                if (locCtx.DataType != null)
                    t = locCtx.DataType;
                else if (t != null)
                    locCtx.DataType = t;
                else {
                    locCtx.DataType = new XSharpParser.DatatypeContext(context,0);
                    locCtx.DataType.Put(_syntaxFactory.PredefinedType(SyntaxFactory.MissingToken(SyntaxKind.IntKeyword)));
                    context.AddError(new ParseErrorData(locCtx.DataType, ErrorCode.ERR_SyntaxError, locCtx.DataType));
                }
            }
        }

        public override void EnterStaticLocalDecl([NotNull] XSharpParser.StaticLocalDeclContext context)
        {
            XSharpParser.DatatypeContext t = null;
            for(var i = context._LocalVars.Count-1; i >= 0; i--) {
                var locCtx = context._LocalVars[i];
                if (locCtx.DataType != null)
                    t = locCtx.DataType;
                else if (t != null)
                    locCtx.DataType = t;
                else {
                    locCtx.DataType = new XSharpParser.DatatypeContext(context,0);
                    locCtx.DataType.Put(_syntaxFactory.PredefinedType(SyntaxFactory.MissingToken(SyntaxKind.IntKeyword)));
                    context.AddError(new ParseErrorData(locCtx.DataType, ErrorCode.ERR_SyntaxError, locCtx.DataType));
                }
            }
        }

        public override void ExitCommonLocalDecl([NotNull] XSharpParser.CommonLocalDeclContext context)
        {
            context.Put(context._localvar?.Get<StatementSyntax>());
        }

        public override void ExitStaticLocalDecl([NotNull] XSharpParser.StaticLocalDeclContext context)
        {
            context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)).
                WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(0, 1, ErrorCode.ERR_FeatureNotAvailableInVersion1, context)));
        }

        public override void ExitVarLocalDecl([NotNull] XSharpParser.VarLocalDeclContext context)
        {
            context.Put(context._impliedvar?.Get<StatementSyntax>());
        }

        public override void ExitLocalvar([NotNull] XSharpParser.LocalvarContext context)
        {
            var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            variables.Add(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(),
                null,
                (context.Expression != null) ? _syntaxFactory.EqualsValueClause(SyntaxFactory.MissingToken(SyntaxKind.EqualsToken),
                    context.Expression.Get<ExpressionSyntax>()) : null));
            var modifiers = _pool.Allocate();
            if (context.Const != null)
                modifiers.Add(SyntaxFactory.MissingToken(SyntaxKind.ConstKeyword));
            context.Put(_syntaxFactory.LocalDeclarationStatement(
                modifiers.ToTokenList(),
                _syntaxFactory.VariableDeclaration(context.DataType.Get<TypeSyntax>(), variables),
                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
            _pool.Free(variables);
            _pool.Free(modifiers);
        }

        public override void ExitImpliedvar([NotNull] XSharpParser.ImpliedvarContext context)
        {
            var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            variables.Add(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(),null,
                (context.Expression != null) ? _syntaxFactory.EqualsValueClause(SyntaxFactory.MissingToken(SyntaxKind.EqualsToken),
                    context.Expression.Get<ExpressionSyntax>()) : null));
            var modifiers = _pool.Allocate();
            if (context.Const != null)
                modifiers.Add(SyntaxFactory.MissingToken(SyntaxKind.ConstKeyword));
            context.Put(_syntaxFactory.LocalDeclarationStatement(
                modifiers.ToTokenList(),
                _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$var")), variables),
                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
            _pool.Free(variables);
            _pool.Free(modifiers);
        }

        public override void ExitXbasedeclStmt([NotNull] XSharpParser.XbasedeclStmtContext context)
        {
            context.Put(context.xbasedecl().Get<StatementSyntax>());
        }

        public override void ExitXbasedecl([NotNull] XSharpParser.XbasedeclContext context)
        {
            context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)).
                WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(0, 1, ErrorCode.ERR_FeatureNotAvailableInVersion1, context)));
        }

        public override void ExitWhileStmt([NotNull] XSharpParser.WhileStmtContext context)
        {
            context.Put(_syntaxFactory.WhileStatement(SyntaxFactory.MissingToken(SyntaxKind.WhileKeyword),
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>()));
        }

        public override void ExitRepeatStmt([NotNull] XSharpParser.RepeatStmtContext context)
        {
            context.Put(_syntaxFactory.DoStatement(SyntaxFactory.MissingToken(SyntaxKind.DoKeyword),
                context.StmtBlk.Get<BlockSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.WhileKeyword),
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken),
                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitForStmt([NotNull] XSharpParser.ForStmtContext context)
        {
            object blockStmts = null;
            ExpressionSyntax initExpr, whileExpr, incrExpr;
            if (context.Step == null) {
                context.Step = new XSharpParser.LiteralExpressionContext(new XSharpParser.ExpressionContext());
                context.Step.Put(_syntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(null,"1",1,null)));
            }
            switch (context.Dir.Type) {
                case XSharpParser.UPTO:
                    initExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MissingToken(SyntaxKind.EqualsToken),
                        context.InitExpr.Get<ExpressionSyntax>());
                    whileExpr = _syntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MissingToken(SyntaxKind.LessThanEqualsToken),
                        context.FinalExpr.Get<ExpressionSyntax>());
                    incrExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.AddAssignmentExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MissingToken(SyntaxKind.PlusEqualsToken),
                        context.Step.Get<ExpressionSyntax>());
                    break;
                case XSharpParser.DOWNTO:
                    initExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MissingToken(SyntaxKind.EqualsToken),
                        context.InitExpr.Get<ExpressionSyntax>());
                    whileExpr = _syntaxFactory.BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MissingToken(SyntaxKind.GreaterThanEqualsToken),
                        context.FinalExpr.Get<ExpressionSyntax>());
                    incrExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.SubtractAssignmentExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MissingToken(SyntaxKind.MinusEqualsToken),
                        context.Step.Get<ExpressionSyntax>());
                    break;
                case XSharpParser.TO:
                default:
                    var startToken = SyntaxFactory.Identifier("Xs$ForStart$"+context.Dir.StartIndex);
                    var endToken = SyntaxFactory.Identifier("Xs$ForEnd$"+context.Dir.StartIndex);
                    var indToken = SyntaxFactory.Identifier("Xs$ForInd$"+context.Dir.StartIndex);
                    var stmts = _pool.Allocate<StatementSyntax>();
                    blockStmts = stmts;
                    {
                        var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
                        variables.Add(_syntaxFactory.VariableDeclarator(startToken,null,
                            _syntaxFactory.EqualsValueClause(SyntaxFactory.MissingToken(SyntaxKind.EqualsToken),
                                context.InitExpr.Get<ExpressionSyntax>())));
                        var modifiers = _pool.Allocate();
                        stmts.Add(_syntaxFactory.LocalDeclarationStatement(
                            modifiers.ToTokenList(),
                            _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$var")), variables),
                            SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
                        _pool.Free(variables);
                        _pool.Free(modifiers);
                    }
                    {
                        var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
                        variables.Add(_syntaxFactory.VariableDeclarator(endToken,null,
                            _syntaxFactory.EqualsValueClause(SyntaxFactory.MissingToken(SyntaxKind.EqualsToken),
                                context.FinalExpr.Get<ExpressionSyntax>())));
                        var modifiers = _pool.Allocate();
                        stmts.Add(_syntaxFactory.LocalDeclarationStatement(
                            modifiers.ToTokenList(),
                            _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$var")), variables),
                            SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
                        _pool.Free(variables);
                        _pool.Free(modifiers);
                    }
                    {
                        var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
                        variables.Add(_syntaxFactory.VariableDeclarator(indToken,null,
                            _syntaxFactory.EqualsValueClause(SyntaxFactory.MissingToken(SyntaxKind.EqualsToken),
                                _syntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression,
                                    _syntaxFactory.IdentifierName(startToken),
                                    SyntaxFactory.MissingToken(SyntaxKind.LessThanEqualsToken),
                                    _syntaxFactory.IdentifierName(endToken)))));
                        var modifiers = _pool.Allocate();
                        stmts.Add(_syntaxFactory.LocalDeclarationStatement(
                            modifiers.ToTokenList(),
                            _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$var")), variables),
                            SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
                        _pool.Free(variables);
                        _pool.Free(modifiers);
                    }
                    initExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MissingToken(SyntaxKind.EqualsToken),
                        _syntaxFactory.IdentifierName(startToken));
                    whileExpr = _syntaxFactory.ConditionalExpression(
                        _syntaxFactory.IdentifierName(indToken),
                        SyntaxFactory.MissingToken(SyntaxKind.QuestionToken),
                        _syntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression,
                            context.Iter.Get<ExpressionSyntax>(),
                            SyntaxFactory.MissingToken(SyntaxKind.LessThanEqualsToken),
                            _syntaxFactory.IdentifierName(endToken)),
                        SyntaxFactory.MissingToken(SyntaxKind.ColonToken),
                        _syntaxFactory.BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression,
                            context.Iter.Get<ExpressionSyntax>(),
                            SyntaxFactory.MissingToken(SyntaxKind.GreaterThanEqualsToken),
                            _syntaxFactory.IdentifierName(endToken)));
                    incrExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.AddAssignmentExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MissingToken(SyntaxKind.PlusEqualsToken),
                        _syntaxFactory.ConditionalExpression(
                            _syntaxFactory.IdentifierName(indToken),
                            SyntaxFactory.MissingToken(SyntaxKind.QuestionToken),
                            context.Step.Get<ExpressionSyntax>(),
                            SyntaxFactory.MissingToken(SyntaxKind.ColonToken),
                            _syntaxFactory.PrefixUnaryExpression(SyntaxKind.UnaryMinusExpression,
                                SyntaxFactory.MissingToken(SyntaxKind.MinusToken),
                                context.Step.Get<ExpressionSyntax>())));
                    break;
            }
            var init = _pool.AllocateSeparated<ExpressionSyntax>();
            init.Add(initExpr);
            var incr = _pool.AllocateSeparated<ExpressionSyntax>();
            incr.Add(incrExpr);
            context.Put(_syntaxFactory.ForStatement(SyntaxFactory.MissingToken(SyntaxKind.ForKeyword),
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                null,
                init,
                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken),
                whileExpr,
                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken),
                incr,
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>()));
            _pool.Free(init);
            _pool.Free(incr);
            if (blockStmts != null) {
                context.Put(_syntaxFactory.Block(
                    SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken),
                    (SyntaxListBuilder<StatementSyntax>)blockStmts,
                    SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken)));
                _pool.Free((SyntaxListBuilder<StatementSyntax>)blockStmts);
            }
        }

        public override void ExitForeachStmt([NotNull] XSharpParser.ForeachStmtContext context)
        {
            context.Put(_syntaxFactory.ForEachStatement(SyntaxFactory.MissingToken(SyntaxKind.ForEachKeyword),
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                context.Type?.Get<TypeSyntax>() ?? _syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$var")),
                context.Id.Get<SyntaxToken>(),
                SyntaxFactory.MissingToken(SyntaxKind.InKeyword),
                context.Container.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>()));
        }

        public override void ExitIfStmt([NotNull] XSharpParser.IfStmtContext context)
        {
            context.Put(context.IfStmt.Get<IfStatementSyntax>());
        }

        public override void ExitIfElseBlock([NotNull] XSharpParser.IfElseBlockContext context)
        {
            context.Put(_syntaxFactory.IfStatement(SyntaxFactory.MissingToken(SyntaxKind.IfKeyword),
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                context.Cond.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>(),
                (context.ElseIfBlock != null) ? 
                    _syntaxFactory.ElseClause(SyntaxFactory.MissingToken(SyntaxKind.ElseKeyword), context.ElseIfBlock.Get<IfStatementSyntax>())
                : (context.ElseBlock != null) ?
                    _syntaxFactory.ElseClause(SyntaxFactory.MissingToken(SyntaxKind.ElseKeyword), context.ElseBlock.Get<BlockSyntax>())
                : null));
        }

        public override void ExitCaseStmt([NotNull] XSharpParser.CaseStmtContext context)
        {
            context.Put((StatementSyntax)context.CaseStmt?.Get<IfStatementSyntax>() ?? 
                _syntaxFactory.EmptyStatement(SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitCaseBlock([NotNull] XSharpParser.CaseBlockContext context)
        {
            if (context.Key.Type == XSharpParser.OTHERWISE)
                context.Put(context.StmtBlk.Get<StatementSyntax>());
            else {
                context.Put(_syntaxFactory.IfStatement(SyntaxFactory.MissingToken(SyntaxKind.IfKeyword),
                    SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                    context.Cond.Get<ExpressionSyntax>(),
                    SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken),
                    context.StmtBlk.Get<BlockSyntax>(),
                    (context.NextCase == null) ? null :
                        _syntaxFactory.ElseClause(SyntaxFactory.MissingToken(SyntaxKind.ElseKeyword),
                            context.NextCase.Get<StatementSyntax>())));
            }
        }

        public override void ExitExitStmt([NotNull] XSharpParser.ExitStmtContext context)
        {
            context.Put(_syntaxFactory.BreakStatement(SyntaxFactory.MissingToken(SyntaxKind.BreakKeyword),
                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitLoopStmt([NotNull] XSharpParser.LoopStmtContext context)
        {
            context.Put(_syntaxFactory.ContinueStatement(SyntaxFactory.MissingToken(SyntaxKind.ContinueKeyword),
                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
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

        public override void ExitBreakStmt([NotNull] XSharpParser.BreakStmtContext context)
        {
            context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)).
                WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(0, 1, ErrorCode.ERR_FeatureNotAvailableInVersion1, context)));
        }

        public override void ExitThrowStmt([NotNull] XSharpParser.ThrowStmtContext context)
        {
            context.Put(_syntaxFactory.ThrowStatement(SyntaxFactory.MissingToken(SyntaxKind.ThrowKeyword),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitTryStmt([NotNull] XSharpParser.TryStmtContext context)
        {
            var catches = _pool.Allocate<CatchClauseSyntax>();
            foreach (var catchCtx in context._CatchBlock)
            {
                catches.Add(catchCtx.Get<CatchClauseSyntax>());
            }
            context.Put(_syntaxFactory.TryStatement(SyntaxFactory.MissingToken(SyntaxKind.TryKeyword),
                context.StmtBlk.Get<BlockSyntax>(),
                catches,
                _syntaxFactory.FinallyClause(SyntaxFactory.MissingToken(SyntaxKind.FinallyKeyword),
                    context.FinBlock.Get<BlockSyntax>())));
            _pool.Free(catches);
        }

        public override void ExitCatchBlock([NotNull] XSharpParser.CatchBlockContext context)
        {
            context.Put(_syntaxFactory.CatchClause(SyntaxFactory.MissingToken(SyntaxKind.CatchKeyword),
                _syntaxFactory.CatchDeclaration(
                    SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                    context.Type.Get<TypeSyntax>(),
                    context.Id.Get<SyntaxToken>(),
                    SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken)),
                null,
                context.StmtBlk.Get<BlockSyntax>()));
        }

        public override void ExitLockStmt([NotNull] XSharpParser.LockStmtContext context)
        {
            context.Put(_syntaxFactory.LockStatement(SyntaxFactory.MissingToken(SyntaxKind.LockKeyword),
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>()));
        }

        public override void ExitScopeStmt([NotNull] XSharpParser.ScopeStmtContext context)
        {
            context.Put(context.StmtBlk.Get<BlockSyntax>());
        }

        public override void ExitReturnStmt([NotNull] XSharpParser.ReturnStmtContext context)
        {
            context.Put(_syntaxFactory.ReturnStatement(SyntaxFactory.MissingToken(SyntaxKind.ReturnKeyword), 
                context.Expr?.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitYieldStmt([NotNull] XSharpParser.YieldStmtContext context)
        {
            context.Put(_syntaxFactory.YieldStatement(SyntaxKind.YieldReturnStatement,
                SyntaxFactory.MissingToken(SyntaxKind.YieldKeyword),
                SyntaxFactory.MissingToken(SyntaxKind.ReturnKeyword),
                context.Expr?.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitSwitchStmt([NotNull] XSharpParser.SwitchStmtContext context)
        {
            var sections = _pool.Allocate<SwitchSectionSyntax>();
            foreach(var switchBlkCtx in context._SwitchBlock) {
                sections.Add(switchBlkCtx.Get<SwitchSectionSyntax>());
            }
            context.Put(_syntaxFactory.SwitchStatement(SyntaxFactory.MissingToken(SyntaxKind.SwitchKeyword),
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken),
                SyntaxFactory.MissingToken(SyntaxKind.OpenBraceToken),
                sections,
                SyntaxFactory.MissingToken(SyntaxKind.CloseBraceToken)));
            _pool.Free(sections);
        }

        public override void ExitSwitchBlock([NotNull] XSharpParser.SwitchBlockContext context)
        {
            var labels = _pool.Allocate<SwitchLabelSyntax>();
            labels.Add(_syntaxFactory.CaseSwitchLabel(context.Key.SyntaxKeyword(),
                context.Const?.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.ColonToken)));
            var stmts = _pool.Allocate<StatementSyntax>();
            if (context.StmtBlk._Stmts.Count > 0) {
                stmts.Add(context.StmtBlk.Get<BlockSyntax>());
                stmts.Add(_syntaxFactory.BreakStatement(SyntaxFactory.MissingToken(SyntaxKind.BreakKeyword),
                    SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken))); // TODO: only add it if missing!
            }
            context.Put(_syntaxFactory.SwitchSection(labels, stmts));
            _pool.Free(labels);
            _pool.Free(stmts);
        }

        public override void ExitUsingStmt([NotNull] XSharpParser.UsingStmtContext context)
        {
            // TODO: variable declarations in using expr
            context.Put(_syntaxFactory.UsingStatement(SyntaxFactory.MissingToken(SyntaxKind.UsingKeyword),
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                null,
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken),
                context.Stmtblk.Get<BlockSyntax>()));
        }

        public override void ExitQoutStmt([NotNull] XSharpParser.QoutStmtContext context)
        {
            context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MissingToken(SyntaxKind.SemicolonToken)).
                WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(0, 1, ErrorCode.ERR_FeatureNotAvailableInVersion1, context)));
        }

        public override void ExitUnsafeStmt([NotNull] XSharpParser.UnsafeStmtContext context)
        {
            context.Put(_syntaxFactory.UnsafeStatement(SyntaxFactory.MissingToken(SyntaxKind.UnsafeKeyword),
                context.StmtBlk.Get<BlockSyntax>()));
        }

        public override void ExitCheckedStmt([NotNull] XSharpParser.CheckedStmtContext context)
        {
            context.Put(_syntaxFactory.CheckedStatement(context.Ch.StatementKind(),
                context.Ch.SyntaxKeyword(),
                context.StmtBlk.Get<BlockSyntax>()));
        }

        public override void ExitAccessMember([NotNull] XSharpParser.AccessMemberContext context)
         {
            context.Put(_syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                context.Left.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.DotToken),
                context.Right.Get<IdentifierNameSyntax>()));
        }

        public override void ExitPostfixExpression([NotNull] XSharpParser.PostfixExpressionContext context)
        {
            context.Put(_syntaxFactory.PostfixUnaryExpression(
                context.Op.ExpressionKindPostfixOp(),
                context.Expr.Get<ExpressionSyntax>(),
                context.Op.SyntaxOp()));
        }

        public override void ExitPrefixExpression([NotNull] XSharpParser.PrefixExpressionContext context)
        {
            context.Put(_syntaxFactory.PrefixUnaryExpression(
                context.Op.ExpressionKindPrefixOp(),
                context.Op.SyntaxOp(),
                context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitBinaryExpression([NotNull] XSharpParser.BinaryExpressionContext context)
        {
            context.Put(_syntaxFactory.BinaryExpression(
                context.Op.ExpressionKindBinaryOp(),
                context.Left.Get<ExpressionSyntax>(),
                context.Op.SyntaxOp(),
                context.Right.Get<ExpressionSyntax>()));
        }

        public override void ExitAssignmentExpression([NotNull] XSharpParser.AssignmentExpressionContext context)
        {
            context.Put(_syntaxFactory.AssignmentExpression(
                context.Op.ExpressionKindBinaryOp(),
                context.Left.Get<ExpressionSyntax>(),
                context.Op.SyntaxOp(),
                context.Right.Get<ExpressionSyntax>()));
        }

        public override void ExitCheckedExpression([NotNull] XSharpParser.CheckedExpressionContext context)
        {
            context.Put(_syntaxFactory.CheckedExpression(context.ch.ExpressionKind(),
                context.ch.SyntaxKeyword(),
                SyntaxFactory.MissingToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MissingToken(SyntaxKind.CloseParenToken)));
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
            context.Put(_syntaxFactory.LiteralExpression(context.Token.ExpressionKindLiteral(), context.Token.SyntaxLiteralValue()));
        }

        public override void ExitIdentifier([NotNull] XSharpParser.IdentifierContext context)
        {
            context.Put(context.Token?.SyntaxIdentifier()
                ?? context.XsToken?.Token.SyntaxIdentifier()
                ?? context.VnToken?.Token.SyntaxIdentifier());
        }

        public override void ExitKeyword([NotNull] XSharpParser.KeywordContext context)
        {
            context.Put(context.KwXs?.Token.SyntaxKeywordIdentifier()
                ?? context.KwVn?.Token.SyntaxKeywordIdentifier()
                ?? context.KwVo?.Token.SyntaxKeywordIdentifier());
        }

        public override void ExitKeywordxs([NotNull] XSharpParser.KeywordxsContext context)
        {
        }

        public override void ExitKeywordvn([NotNull] XSharpParser.KeywordvnContext context)
        {
        }

        public override void ExitKeywordvo([NotNull] XSharpParser.KeywordvoContext context)
        {
        }

        public override void ExitNativeType([NotNull] XSharpParser.NativeTypeContext context)
        {
            context.Put(_syntaxFactory.PredefinedType(context.Token.SyntaxNativeType()));
        }

    }
}