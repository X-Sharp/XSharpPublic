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
                rb.Add(SyntaxFactory.MakeToken(k));
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

        ArgumentListSyntax EmptyArgumentList()
        {
            return _syntaxFactory.ArgumentList(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken), 
                    default(SeparatedSyntaxList<ArgumentSyntax>), 
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
        }

        SyntaxList<T> MakeList<T>(System.Collections.IEnumerable t) where T : InternalSyntax.CSharpSyntaxNode
        {
            if (t == null)
                return default(SyntaxList<T>);
            var l = _pool.Allocate<T>();
            foreach (var item in t) {
                if (item != null)
                    l.Add(((IParseTree)item).Get<T>());
            }
            var list = l.ToList();
            _pool.Free(l);
            return list;
        }

        SyntaxList<T> MakeList<T>(params T[] items) where T : InternalSyntax.CSharpSyntaxNode
        {
            var l = _pool.Allocate<T>();
            foreach (var item in items) {
                if (item != null)
                    l.Add(item);
            }
            var list = l.ToList();
            _pool.Free(l);
            return list;
        }

        SeparatedSyntaxList<T> MakeSeparatedList<T>(System.Collections.IEnumerable t) where T : InternalSyntax.CSharpSyntaxNode
        {
            if (t == null)
                return default(SeparatedSyntaxList<T>);
            var l = _pool.AllocateSeparated<T>();
            foreach (var item in t) {
                if (item != null) {
                    if (l.Count>0)
                        l.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    l.Add(((IParseTree)item).Get<T>());
                }
            }
            var list = l.ToList();
            _pool.Free(l);
            return list;
        }

        SeparatedSyntaxList<T> MakeSeparatedList<T>(params T[] items) where T : InternalSyntax.CSharpSyntaxNode
        {
            var l = _pool.AllocateSeparated<T>();
            foreach (var item in items) {
                if (item != null) {
                    if (l.Count>0)
                        l.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    l.Add(item);
                }
            }
            var list = l.ToList();
            _pool.Free(l);
            return list;
        }

        TypeSyntax VoidType()
        {
            return _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.VoidKeyword));
        }

        TypeSyntax EmptyType()
        {
            return _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.VoidKeyword));
        }

        ParameterListSyntax EmptyParameterList()
        {
            return _syntaxFactory.ParameterList(
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                EmptySeparatedList<ParameterSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
        }

        public LiteralExpressionSyntax GenerateLiteral(string text)
        {
            return _syntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression,SyntaxFactory.Literal(null,"",text,null));
        }

        public NameEqualsSyntax GenerateNameEquals(string name)
        {
            return _syntaxFactory.NameEquals(
                _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(name)),
                SyntaxFactory.MakeToken(SyntaxKind.EqualsToken));
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
                        SyntaxFactory.MakeToken(SyntaxKind.DotToken),
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
                    attributes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                }
                attributes.Add(_syntaxFactory.Attribute(
                    name: GenerateQualifiedName(attributeName),
                    argumentList: null));
            }
            attributeLists.Add(_syntaxFactory.AttributeList(
                openBracketToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                target: null,
                attributes: attributes,
                closeBracketToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
            _pool.Free(attributes);
        }

        private ClassDeclarationSyntax GenerateClass(string className, SyntaxListBuilder<MemberDeclarationSyntax> members)
        {
            SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
            GenerateAttributeList(attributeLists, "System.Runtime.CompilerServices.CompilerGenerated");
            SyntaxListBuilder modifiers = _pool.Allocate();
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.InternalKeyword));
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
            var r = _syntaxFactory.ClassDeclaration(
                attributeLists: attributeLists,
                modifiers: modifiers.ToTokenList(),
                keyword: SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword),
                identifier: SyntaxFactory.Identifier(className),
                typeParameterList: null,
                baseList: null, // BaseListSyntax baseList = _syntaxFactory.BaseList(colon, list)
                constraintClauses: default(SyntaxListBuilder<TypeParameterConstraintClauseSyntax>),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
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
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.PartialKeyword));
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.PublicKeyword));
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
            var r = _syntaxFactory.ClassDeclaration(
                attributeLists: attributeLists,
                modifiers: modifiers.ToTokenList(),
                keyword: SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword),
                identifier: SyntaxFactory.Identifier(className),
                typeParameterList: null,
                baseList: null, // BaseListSyntax baseList = _syntaxFactory.BaseList(colon, list)
                constraintClauses: default(SyntaxListBuilder<TypeParameterConstraintClauseSyntax>),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: globalClassMembers,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken) );
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
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
            ParameterListSyntax paramList;
            {
                var parameters = _pool.AllocateSeparated<ParameterSyntax>();
                paramList = _syntaxFactory.ParameterList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    parameters,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
                _pool.Free(parameters);
            }
            BlockSyntax blockBody;
            {
                var statements = _pool.Allocate<StatementSyntax>();
                {
                    ArgumentListSyntax argList = _syntaxFactory.ArgumentList(
                        openParenToken: SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken), 
                        arguments: default(SeparatedSyntaxList<ArgumentSyntax>), 
                        closeParenToken: SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
                    statements.Add(_syntaxFactory.ExpressionStatement(
                        expression: _syntaxFactory.InvocationExpression(GenerateQualifiedName(startMethodName), argList), 
                        semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                }
                blockBody = _syntaxFactory.Block(
                    openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken), 
                    statements: statements,
                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
                _pool.Free(statements);
            }
            var r = _syntaxFactory.MethodDeclaration(
                attributeLists: attributeLists,
                modifiers: modifiers.ToTokenList(),
                returnType: VoidType(),
                explicitInterfaceSpecifier: null,
                identifier: SyntaxFactory.Identifier("Main"),
                typeParameterList: null,
                parameterList: paramList,
                constraintClauses: default(SyntaxListBuilder<TypeParameterConstraintClauseSyntax>),
                body: blockBody,
                expressionBody: null,
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
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
#if DEBUG && false
            var s = context.GetType().ToString();
            s = s.Substring(s.LastIndexOfAny(".+".ToCharArray())+1);
            s = s.Replace("Context","");
            Debug.WriteLine("{0}=> ({1},{2}) {3} '{4}'",new string(' ',context.Depth()),context.Start.Line,context.Start.Column,s,context.Start.Text);
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
#if DEBUG && false
            var s = context.GetType().ToString();
            s = s.Substring(s.LastIndexOfAny(".+".ToCharArray())+1);
            s = s.Replace("Context","");
            Debug.WriteLine("{0}<= ({1},{2}) {3} '{4}'",new string(' ',context.Depth()),context.Start.Line,context.Start.Column,s,context.Start.Text);
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
            Usings.Add(_syntaxFactory.UsingDirective(SyntaxFactory.MakeToken(SyntaxKind.UsingKeyword), 
                SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword), 
                null,
                _syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$Globals")),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
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
            context.Put(_syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                name: context.Name.Get<NameSyntax>(),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                externs: externs,
                usings: usings,
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
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
            context.Put(_syntaxFactory.UsingDirective(SyntaxFactory.MakeToken(SyntaxKind.UsingKeyword),
                staticKeyword: context.Static == null ? null : context.Static.SyntaxKeyword(),
                alias: context.Alias == null ? null : _syntaxFactory.NameEquals(context.Alias.Get<IdentifierNameSyntax>(),SyntaxFactory.MakeToken(SyntaxKind.EqualsToken)),
                name: context.Name.Get<NameSyntax>(),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitVodefine([NotNull] XSharpParser.VodefineContext context)
        {
            var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            variables.Add(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(),
                null,
                _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    context.Expr.Get<ExpressionSyntax>())));
            context.Put(_syntaxFactory.FieldDeclaration(
                EmptyList<AttributeListSyntax>(),
                TokenList(SyntaxKind.StaticKeyword,SyntaxKind.PublicKeyword),
                _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$var")), variables),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
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
                if (baseTypes.Count>0)
                    baseTypes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                baseTypes.Add(_syntaxFactory.SimpleBaseType(pCtx.Get<TypeSyntax>()));
            }
            context.Put(_syntaxFactory.InterfaceDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: SyntaxFactory.MakeToken(SyntaxKind.InterfaceKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MakeToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
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
                ?? _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.ObjectKeyword))));
            foreach(var iCtx in context._Implements) {
                baseTypes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                baseTypes.Add(_syntaxFactory.SimpleBaseType(iCtx.Get<TypeSyntax>()));
            }
            context.Put(_syntaxFactory.ClassDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MakeToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
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

        public override void ExitStructure_([NotNull] XSharpParser.Structure_Context context)
        {
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            foreach(var mCtx in context._Members) {
                members.Add(mCtx.Get<MemberDeclarationSyntax>());
            }
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            foreach(var iCtx in context._Implements) {
                if (baseTypes.Count>0)
                    baseTypes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                baseTypes.Add(_syntaxFactory.SimpleBaseType(iCtx.Get<TypeSyntax>()));
            }
            context.Put(_syntaxFactory.StructDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MakeToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null));
            _pool.Free(members);
            _pool.Free(baseTypes);
        }

        public override void ExitStructureModifiers([NotNull] XSharpParser.StructureModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitDelegate_([NotNull] XSharpParser.Delegate_Context context)
        {
            context.Put(_syntaxFactory.DelegateDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                delegateKeyword: SyntaxFactory.MakeToken(SyntaxKind.DelegateKeyword),
                returnType: context.Type.Get<TypeSyntax>(),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitDelegateModifiers([NotNull] XSharpParser.DelegateModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitEnum_([NotNull] XSharpParser.Enum_Context context)
        {
            context.Put(_syntaxFactory.EnumDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                enumKeyword: SyntaxFactory.MakeToken(SyntaxKind.EnumKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                baseList: default(BaseListSyntax),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: MakeSeparatedList<EnumMemberDeclarationSyntax>(context._Members),
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null));
        }

        public override void ExitEnumModifiers([NotNull] XSharpParser.EnumModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitEnummember([NotNull] XSharpParser.EnummemberContext context)
        {
            context.Put(_syntaxFactory.EnumMemberDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                identifier: context.Id.Get<SyntaxToken>(),
                equalsValue: _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    context.Expr.Get<ExpressionSyntax>())));
        }

        public override void ExitEvent_([NotNull] XSharpParser.Event_Context context)
        {
            /*context.Put(_syntaxFactory.EventDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                eventKeyword: SyntaxFactory.MakeToken(SyntaxKind.EventKeyword),
                type: context.Type.Get<TypeSyntax>(),
                explicitInterfaceSpecifier: null,
                identifier: context.Id.Get<SyntaxToken>(),
                accessorList: null));*/
            context.Put(_syntaxFactory.EventFieldDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                eventKeyword: SyntaxFactory.MakeToken(SyntaxKind.EventKeyword),
                declaration: _syntaxFactory.VariableDeclaration(
                    context.Type.Get<TypeSyntax>(),
                    MakeSeparatedList<VariableDeclaratorSyntax>(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(),null, null))),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitEventModifiers([NotNull] XSharpParser.EventModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitClassvars([NotNull] XSharpParser.ClassvarsContext context)
        {
            context.Put(_syntaxFactory.FieldDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                declaration: _syntaxFactory.VariableDeclaration(
                    type: context.DataType.Get<TypeSyntax>(),
                    variables: MakeSeparatedList<VariableDeclaratorSyntax>(context._Var)),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitClassvarModifiers([NotNull] XSharpParser.ClassvarModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitClassvar([NotNull] XSharpParser.ClassvarContext context)
        {
            context.Put(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(),
                null,
                (context.Initializer != null) ? _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    context.Initializer.Get<ExpressionSyntax>()) : null));
        }

        public override void ExitProperty([NotNull] XSharpParser.PropertyContext context)
        {
            var isInInterface = context.isInInterface();
            if (isInInterface) {
                if (context.Auto != null) {
                    context.AddError(new ParseErrorData(context.AUTO(), ErrorCode.ERR_InterfaceMemberHasBody));
                }
                else if (context.Multi != null) {
                    context.AddError(new ParseErrorData(context.Multi, ErrorCode.ERR_InterfaceMemberHasBody));
                }
                else {
                    foreach(var aCtx in context._LineAccessors) {
                        if (aCtx.Expr != null && aCtx.ExprList != null) {
                            if (aCtx.Expr != null)
                                context.AddError(new ParseErrorData(aCtx.Expr, ErrorCode.ERR_InterfaceMemberHasBody));
                            else
                                context.AddError(new ParseErrorData(aCtx.ExprList, ErrorCode.ERR_InterfaceMemberHasBody));
                        }
                    }
                }
            }
            if (context.Modifiers?._EXTERN != null) {
                if (context.Auto != null) {
                    context.AddError(new ParseErrorData(context.AUTO(), ErrorCode.ERR_ExternHasBody));
                }
                else if (context.Multi != null) {
                    context.AddError(new ParseErrorData(context.Multi, ErrorCode.ERR_ExternHasBody));
                }
                else {
                    foreach(var aCtx in context._LineAccessors) {
                        if (aCtx.Expr != null && aCtx.ExprList != null) {
                            if (aCtx.Expr != null)
                                context.AddError(new ParseErrorData(aCtx.Expr, ErrorCode.ERR_ExternHasBody));
                            else
                                context.AddError(new ParseErrorData(aCtx.ExprList, ErrorCode.ERR_ExternHasBody));
                        }
                    }
                }
            }
            if (context.Modifiers?._ABSTRACT != null) {
                if (context.Modifiers?._EXTERN != null) {
                    context.AddError(new ParseErrorData(context.Modifiers, ErrorCode.ERR_AbstractAndExtern));
                }
                if (context.Auto != null) {
                    context.AddError(new ParseErrorData(context.AUTO(), ErrorCode.ERR_AbstractHasBody));
                }
                else if (context.Multi != null) {
                    context.AddError(new ParseErrorData(context.Multi, ErrorCode.ERR_AbstractHasBody));
                }
                else {
                    foreach(var aCtx in context._LineAccessors) {
                        if (aCtx.Expr != null && aCtx.ExprList != null) {
                            if (aCtx.Expr != null)
                                context.AddError(new ParseErrorData(aCtx.Expr, ErrorCode.ERR_AbstractHasBody));
                            else
                                context.AddError(new ParseErrorData(aCtx.ExprList, ErrorCode.ERR_AbstractHasBody));
                        }
                    }
                }
            }
            if (context.ParamList == null)
                context.Put(_syntaxFactory.PropertyDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                    type: context.Type.Get<TypeSyntax>(),
                    explicitInterfaceSpecifier: null,
                    identifier: context.Id.Get<SyntaxToken>(),
                    accessorList: _syntaxFactory.AccessorList(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        (context.Auto != null) ? 
                            ((context._AutoAccessors?.Count ?? 0) > 0) ? MakeList<AccessorDeclarationSyntax>(context._AutoAccessors) :
                            MakeList(_syntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration,null,null,
                                    SyntaxFactory.MakeToken(SyntaxKind.GetKeyword),null,SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)),
                                _syntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration,null,null,
                                    SyntaxFactory.MakeToken(SyntaxKind.SetKeyword),null,SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))) :
                        ((context._LineAccessors?.Count ?? 0) > 0) ? MakeList<AccessorDeclarationSyntax>(context._LineAccessors) :
                        MakeList<AccessorDeclarationSyntax>(context._Accessors),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)),
                    expressionBody: null,
                    initializer: context.Initializer != null ? _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                        context.Initializer.Get<ExpressionSyntax>()) : null,
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            else {
                if (context.Auto != null)
                    context.AddError(new ParseErrorData(context.AUTO(),ErrorCode.ERR_SyntaxError,SyntaxFactory.MakeToken(SyntaxKind.GetKeyword)));
                context.Put(_syntaxFactory.IndexerDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                    type: context.Type.Get<TypeSyntax>(),
                    explicitInterfaceSpecifier: null,
                    thisKeyword: SyntaxFactory.MakeToken(SyntaxKind.ThisKeyword),
                    parameterList: context.ParamList.Get<BracketedParameterListSyntax>(),
                    accessorList: _syntaxFactory.AccessorList(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        (context.Auto != null) ? 
                            ((context._AutoAccessors?.Count ?? 0) > 0) ? MakeList<AccessorDeclarationSyntax>(context._AutoAccessors) :
                            MakeList(_syntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration,null,null,
                                    SyntaxFactory.MakeToken(SyntaxKind.GetKeyword),null,SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)),
                                _syntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration,null,null,
                                    SyntaxFactory.MakeToken(SyntaxKind.SetKeyword),null,SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))) :
                        ((context._LineAccessors?.Count ?? 0) > 0) ? MakeList<AccessorDeclarationSyntax>(context._LineAccessors) :
                        MakeList<AccessorDeclarationSyntax>(context._Accessors),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)),
                    expressionBody: null,
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
        }

        public override void ExitPropertyParameterList([NotNull] XSharpParser.PropertyParameterListContext context)
        {
            var @params = _pool.AllocateSeparated<ParameterSyntax>();
            foreach (var paramCtx in context._Params)
            {
                if (@params.Count>0)
                    @params.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                @params.Add(paramCtx.Get<ParameterSyntax>());
            }
            context.Put(_syntaxFactory.BracketedParameterList(
                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                @params,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
            _pool.Free(@params);
        }

        public override void ExitPropertyAutoAccessor([NotNull] XSharpParser.PropertyAutoAccessorContext context)
        {
            context.Put(_syntaxFactory.AccessorDeclaration(context.Key.AccessorKind(),
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: context.Key.SyntaxKeyword(),
                body: null,
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitPropertyLineAccessor([NotNull] XSharpParser.PropertyLineAccessorContext context)
        {
            context.Put(_syntaxFactory.AccessorDeclaration(context.Key.AccessorKind(),
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: context.Key.SyntaxKeyword(),
                body: context.Key.Type == XSharpParser.GET ? 
                    context.Expr == null ? null : _syntaxFactory.Block(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        MakeList<StatementSyntax>(_syntaxFactory.ReturnStatement(SyntaxFactory.MakeToken(SyntaxKind.ReturnKeyword),
                            context.Expr.Get<ExpressionSyntax>(),SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)) :
                    context.ExprList == null ? null : _syntaxFactory.Block(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        context.ExprList.GetList<StatementSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken))
                    ,
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitExpressionListStmt([NotNull] XSharpParser.ExpressionListStmtContext context)
        {
            var stmts = _pool.Allocate<StatementSyntax>();
            foreach(var eCtx in context._Exprs) {
                stmts.Add(_syntaxFactory.ExpressionStatement(eCtx.Get<ExpressionSyntax>(),SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
            context.PutList(stmts.ToList());
            _pool.Free(stmts);
        }

        public override void ExitPropertyAccessor([NotNull] XSharpParser.PropertyAccessorContext context)
        {
            context.Put(_syntaxFactory.AccessorDeclaration(context.Key.AccessorKind(),
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: context.Key.SyntaxKeyword(),
                body: context.StmtBlk.Get<BlockSyntax>(),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitMethod([NotNull] XSharpParser.MethodContext context)
        {
            var isInInterface = context.isInInterface();
            if (isInInterface && context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0) {
                context.AddError(new ParseErrorData(context.Id, ErrorCode.ERR_InterfaceMemberHasBody));
            }
            if (isInInterface && context.ClassId != null) {
                context.AddError(new ParseErrorData(context.ClassId, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            if (context.Modifiers?._EXTERN != null) {
                if (context.StmtBlk?._Stmts?.Count > 0) {
                    context.AddError(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody));
                }
                context.StmtBlk = null;
            }
            if (context.Modifiers?._ABSTRACT != null) {
                if (context.Modifiers?._EXTERN != null) {
                    context.AddError(new ParseErrorData(context.Modifiers, ErrorCode.ERR_AbstractAndExtern));
                }
                if (context.StmtBlk?._Stmts?.Count > 0) {
                    context.AddError(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_AbstractHasBody));
                }
                context.StmtBlk = null;
            }
            context.Put(_syntaxFactory.MethodDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                returnType: context.Type?.Get<TypeSyntax>() ?? EmptyType(),
                explicitInterfaceSpecifier: null,
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                body: isInInterface ? null : context.StmtBlk.Get<BlockSyntax>(),
                expressionBody: null,
                semicolonToken: (!isInInterface && context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            if (context.ClassId != null) {
                context.Put(_syntaxFactory.ClassDeclaration(
                    attributeLists: EmptyList<AttributeListSyntax>(),
                    modifiers: TokenList(SyntaxKind.PartialKeyword),
                    keyword: SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword),
                    identifier: context.ClassId.Get<SyntaxToken>(),
                    typeParameterList: default(TypeParameterListSyntax),
                    baseList: default(BaseListSyntax),
                    constraintClauses: default(SyntaxList<TypeParameterConstraintClauseSyntax>),
                    openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    members: MakeList<MemberDeclarationSyntax>(context.Get<MethodDeclarationSyntax>()),
                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                    semicolonToken: null));
            }
        }

        public override void ExitTypeparameters([NotNull] XSharpParser.TypeparametersContext context)
        {
            var parameters = _pool.AllocateSeparated<TypeParameterSyntax>();
            foreach(var tpCtx in context._TypeParams) {
                if (parameters.Count>0)
                    parameters.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                parameters.Add(tpCtx.Get<TypeParameterSyntax>());
            }
            context.Put(_syntaxFactory.TypeParameterList(SyntaxFactory.MakeToken(SyntaxKind.LessThanToken),
                parameters,
                SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken)));
            _pool.Free(parameters);
        }

        public override void ExitTypeparameter([NotNull] XSharpParser.TypeparameterContext context)
        {
            context.Put(_syntaxFactory.TypeParameter(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                varianceKeyword: context.VarianceKeyword.SyntaxKeyword(),
                identifier: context.Id.Get<SyntaxToken>()));
        }

        public override void ExitTypeparameterconstraintsclause([NotNull] XSharpParser.TypeparameterconstraintsclauseContext context)
        {
            var constraints = _pool.AllocateSeparated<TypeParameterConstraintSyntax>();
            foreach(var cCtx in context._Constraints) {
                if (constraints.Count > 0)
                    constraints.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                constraints.Add(cCtx.Get<TypeParameterConstraintSyntax>());
            }
            context.Put(_syntaxFactory.TypeParameterConstraintClause(
                SyntaxFactory.MakeToken(SyntaxKind.WhereKeyword),
                context.Name.Get<IdentifierNameSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                constraints));
            _pool.Free(constraints);
        }

        public override void ExitTypeConstraint([NotNull] XSharpParser.TypeConstraintContext context)
        {
            context.Put(_syntaxFactory.TypeConstraint(context.Type.Get<TypeSyntax>()));
        }

        public override void ExitClassOrStructConstraint([NotNull] XSharpParser.ClassOrStructConstraintContext context)
        {
            context.Put(_syntaxFactory.ClassOrStructConstraint(
                context.Key.ConstraintKind(),
                context.Key.SyntaxKeyword()));
        }

        public override void ExitConstructorConstraint([NotNull] XSharpParser.ConstructorConstraintContext context)
        {
            context.Put(_syntaxFactory.ConstructorConstraint(
                SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
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
            if (modifiers.Any(SyntaxKind.VirtualKeyword) && !modifiers.Any(SyntaxKind.NewKeyword))
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.OverrideKeyword));
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitOperator_([NotNull] XSharpParser.Operator_Context context)
        {
            if (context.Modifiers?._EXTERN != null) {
                if (context.StmtBlk?._Stmts?.Count > 0) {
                    context.AddError(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody));
                }
                context.StmtBlk = null;
            }
            if (context.Conversion != null)
                context.Put(_syntaxFactory.ConversionOperatorDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                    implicitOrExplicitKeyword: context.Conversion.Get<SyntaxToken>(),
                    operatorKeyword: SyntaxFactory.MakeToken(SyntaxKind.OperatorKeyword),
                    type: context.Type?.Get<TypeSyntax>() ?? EmptyType(),
                    parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                    body: context.StmtBlk.Get<BlockSyntax>(),
                    expressionBody: null,
                    semicolonToken: (context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            else
                context.Put(_syntaxFactory.OperatorDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                    returnType: context.Type?.Get<TypeSyntax>() ?? EmptyType(),
                    operatorKeyword: SyntaxFactory.MakeToken(SyntaxKind.OperatorKeyword),
                    operatorToken: context.Operation.Get<SyntaxToken>(),
                    parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                    body: context.StmtBlk.Get<BlockSyntax>(),
                    expressionBody: null,
                    semicolonToken: (context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitOperatorModifiers([NotNull] XSharpParser.OperatorModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitOverloadedOps([NotNull] XSharpParser.OverloadedOpsContext context)
        {
            context.Put(context.Token.SyntaxOp());
        }

        public override void ExitConversionOps([NotNull] XSharpParser.ConversionOpsContext context)
        {
            context.Put(context.Token.SyntaxKeyword());
        }

        public override void ExitClsmethod([NotNull] XSharpParser.ClsmethodContext context)
        {
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitClsctor([NotNull] XSharpParser.ClsctorContext context)
        {
            if (context.Modifiers?._EXTERN != null) {
                if (context.StmtBlk?._Stmts?.Count > 0) {
                    context.AddError(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody));
                }
                context.StmtBlk = null;
            }
            if (context.isInInterface()) {
                context.AddError(new ParseErrorData(context.CONSTRUCTOR(), ErrorCode.ERR_InterfacesCantContainConstructors));
            }
            else {
                context.Put(_syntaxFactory.ConstructorDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                    identifier: (context.Parent as XSharpParser.Class_Context).Id.Get<SyntaxToken>(),
                    parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                    initializer: context.Chain == null ? null : 
                        _syntaxFactory.ConstructorInitializer(context.Chain.CtorInitializerKind(),
                            SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                            context.Chain.SyntaxKeyword(), 
                            context.ArgList?.Get<ArgumentListSyntax>() ?? EmptyArgumentList()),
                    body: context.StmtBlk.Get<BlockSyntax>(),
                    semicolonToken: (context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
        }

        public override void ExitClsdtor([NotNull] XSharpParser.ClsdtorContext context)
        {
            if (context.Modifiers?._EXTERN != null) {
                if (context.StmtBlk?._Stmts?.Count > 0) {
                    context.AddError(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody));
                }
                context.StmtBlk = null;
            }
            if (context.isInInterface()) {
                context.AddError(new ParseErrorData(context.DESTRUCTOR(), ErrorCode.ERR_InterfacesCantContainConstructors));
            }
            else {
                context.Put(_syntaxFactory.DestructorDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                    tildeToken: SyntaxFactory.MakeToken(SyntaxKind.TildeToken),
                    identifier: (context.Parent as XSharpParser.Class_Context).Id.Get<SyntaxToken>(),
                    parameterList: EmptyParameterList(),
                    body: context.StmtBlk.Get<BlockSyntax>(),
                    semicolonToken: (context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
        }

        public override void ExitClsvars([NotNull] XSharpParser.ClsvarsContext context)
        {
            if (context.isInInterface()) {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCantContainFields));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitClsproperty([NotNull] XSharpParser.ClspropertyContext context)
        {
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitClsoperator([NotNull] XSharpParser.ClsoperatorContext context)
        {
            if (context.isInInterface()) {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCantContainOperators));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitNestedStructure([NotNull] XSharpParser.NestedStructureContext context)
        {
            if (context.isInInterface()) {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitNestedClass([NotNull] XSharpParser.NestedClassContext context)
        {
            if (context.isInInterface()) {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitNestedDelegate([NotNull] XSharpParser.NestedDelegateContext context)
        {
            if (context.isInInterface()) {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitNestedEnum([NotNull] XSharpParser.NestedEnumContext context)
        {
            if (context.isInInterface()) {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitNestedEvent([NotNull] XSharpParser.NestedEventContext context)
        {
            
        }

        public override void ExitNestedInterface([NotNull] XSharpParser.NestedInterfaceContext context)
        {
            if (context.isInInterface()) {
                context.AddError(new ParseErrorData(context.Member, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            else
                context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitClsfunction([NotNull] XSharpParser.ClsfunctionContext context)
        {
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitClsprocedure([NotNull] XSharpParser.ClsprocedureContext context)
        {
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
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
                    attributes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                }
                attributes.Add(attrCtx.Get<AttributeSyntax>());
            }
            context.Put(_syntaxFactory.AttributeList(
                SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                context.Target.Get<AttributeTargetSpecifierSyntax>(),
                attributes,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)));
            _pool.Free(attributes);
        }

        public override void ExitAttributeTarget([NotNull] XSharpParser.AttributeTargetContext context)
        {
            context.Put(_syntaxFactory.AttributeTargetSpecifier(
                context.Id?.Get<SyntaxToken>() ?? context.Kw.Get<SyntaxToken>(),
                SyntaxFactory.MakeToken(SyntaxKind.ColonToken)));
        }

        public override void ExitAttribute([NotNull] XSharpParser.AttributeContext context)
        {
            var arguments = _pool.AllocateSeparated<AttributeArgumentSyntax>();
            foreach(var paramCtx in context._Params) {
                if (arguments.Count != 0) {
                    arguments.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                }
                arguments.Add(paramCtx.Get<AttributeArgumentSyntax>());
            }
            context.Put(_syntaxFactory.Attribute(
                name: context.Name.Get<NameSyntax>(),
                argumentList: _syntaxFactory.AttributeArgumentList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    arguments,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))));
            _pool.Free(arguments);
        }

        public override void ExitPropertyAttributeParam([NotNull] XSharpParser.PropertyAttributeParamContext context)
        {
            context.Put(_syntaxFactory.AttributeArgument(
                _syntaxFactory.NameEquals(context.Name.Get<IdentifierNameSyntax>(), 
                    SyntaxFactory.MakeToken(SyntaxKind.EqualsToken)), 
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
                    attributes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                }
                attributes.Add(attrCtx.Get<AttributeSyntax>());
            }
            context.Put(_syntaxFactory.AttributeList(
                SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                context.Target.Get<AttributeTargetSpecifierSyntax>(),
                attributes,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)));
            _pool.Free(attributes);
        }

        public override void ExitGlobalAttributeTarget([NotNull] XSharpParser.GlobalAttributeTargetContext context)
        {
            context.Put(_syntaxFactory.AttributeTargetSpecifier(
                context.Token.SyntaxKeywordIdentifier(),
                SyntaxFactory.MakeToken(SyntaxKind.ColonToken)));
        }

        public override void EnterVoglobal([NotNull] XSharpParser.VoglobalContext context)
        {
            if (context.Const != null) {
                if (context.Modifiers != null)
                    context.Modifiers._Tokens.Add(context.Const);
                else {
                    context.Modifiers = new XSharpParser.FuncprocModifiersContext(context,0);
                    context.Modifiers.PutList(TokenList(SyntaxKind.ConstKeyword,SyntaxKind.StaticKeyword,SyntaxKind.PublicKeyword));
                }
            }
        }

        public override void ExitVoglobal([NotNull] XSharpParser.VoglobalContext context)
        {
            var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            foreach(var varCtx in context._Var) {
                if (variables.Count>0)
                    variables.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                variables.Add(varCtx.Get<VariableDeclaratorSyntax>());
            }
            context.Put(_syntaxFactory.FieldDeclaration(
                context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                context.Modifiers?.GetList<SyntaxToken>() ?? TokenList(SyntaxKind.StaticKeyword,SyntaxKind.PublicKeyword),
                _syntaxFactory.VariableDeclaration(context.DataType.Get<TypeSyntax>(), variables),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            _pool.Free(variables);
        }

        public override void ExitNestedPragma([NotNull] XSharpParser.NestedPragmaContext context)
        {
            // TODO
        }

        public override void ExitPragmaOptions([NotNull] XSharpParser.PragmaOptionsContext context)
        {
            // TODO
        }

        public override void ExitPragmaswitch([NotNull] XSharpParser.PragmaswitchContext context)
        {
            // TODO
        }

        public override void ExitPragmaWarnings([NotNull] XSharpParser.PragmaWarningsContext context)
        {
            // TODO
        }

        public override void ExitVodll([NotNull] XSharpParser.VodllContext context)
        {
            context.Put(_syntaxFactory.MethodDeclaration(
                attributeLists: MakeList(
                    _syntaxFactory.AttributeList(
                        openBracketToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                        target: null,
                        attributes: MakeSeparatedList(
                            _syntaxFactory.Attribute(
                                name: GenerateQualifiedName("System.Runtime.InteropServices.DllImport"),
                                argumentList: _syntaxFactory.AttributeArgumentList(
                                    openParenToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                                    arguments: MakeSeparatedList(
                                        _syntaxFactory.AttributeArgument(null,null,context.Dll.Get<ExpressionSyntax>()),
                                        context.Entrypoint == null ? null : 
                                            _syntaxFactory.AttributeArgument(GenerateNameEquals("EntryPoint"),null,context.Entrypoint.Get<ExpressionSyntax>()),
                                        context.CallingConvention?.Get<AttributeArgumentSyntax>()
                                    ),
                                    closeParenToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken))
                                )
                            ),
                        closeBracketToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken))
                    ),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenList(SyntaxKind.StaticKeyword,SyntaxKind.PublicKeyword),
                returnType: context.Type?.Get<TypeSyntax>() ?? (context.T.Type == XSharpParser.FUNCTION ? EmptyType() : VoidType()),
                explicitInterfaceSpecifier: null,
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                constraintClauses: default(SyntaxList<TypeParameterConstraintClauseSyntax>),
                body: null,
                expressionBody: null,
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitDllcallconv([NotNull] XSharpParser.DllcallconvContext context)
        {
            string conv = null;
            switch (context.Cc.Type) {
                case XSharpParser.CLIPPER:
                case XSharpParser.STRICT:
                    break;
                case XSharpParser.PASCAL:
                    conv = "System.Runtime.InteropServices.CallingConvention.StdCall";
                    break;
                case XSharpParser.THISCALL:
                    conv = "System.Runtime.InteropServices.CallingConvention.ThisCall";
                    break;
                case XSharpParser.FASTCALL:
                    conv = "System.Runtime.InteropServices.CallingConvention.Cdecl";
                    break;
            }
            if (conv != null && conv != "") {
                context.Put(_syntaxFactory.AttributeArgument(
                    _syntaxFactory.NameEquals(
                        _syntaxFactory.IdentifierName(SyntaxFactory.Identifier("CallingConvention")),
                        SyntaxFactory.MakeToken(SyntaxKind.EqualsToken)),
                    null,
                    GenerateQualifiedName(conv)));
            }
        }

        public override void ExitVostruct([NotNull] XSharpParser.VostructContext context)
        {
            // TODO
        }

        public override void ExitVostructmember([NotNull] XSharpParser.VostructmemberContext context)
        {
            // TODO
        }

        public override void ExitVounion([NotNull] XSharpParser.VounionContext context)
        {
            // TODO
        }

        public override void ExitArraysub([NotNull] XSharpParser.ArraysubContext context)
        {
            // TODO
        }

        public override void ExitFunction([NotNull] XSharpParser.FunctionContext context)
        {
            var isInInterface = context.isInInterface();
            if (isInInterface && context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0) {
                context.AddError(new ParseErrorData(context.Id, ErrorCode.ERR_InterfaceMemberHasBody));
            }
            context.Put(_syntaxFactory.MethodDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenList(SyntaxKind.StaticKeyword,SyntaxKind.PublicKeyword),
                returnType: context.Type?.Get<TypeSyntax>() ?? EmptyType(),
                explicitInterfaceSpecifier: null,
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                body: isInInterface ? null : context.StmtBlk.Get<BlockSyntax>(),
                expressionBody: null,
                semicolonToken: (!isInInterface && context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitProcedure([NotNull] XSharpParser.ProcedureContext context)
        {
            var isInInterface = context.isInInterface();
            if (isInInterface && context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0) {
                context.AddError(new ParseErrorData(context.Id, ErrorCode.ERR_InterfaceMemberHasBody));
            }
            context.Put(_syntaxFactory.MethodDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenList(SyntaxKind.StaticKeyword,SyntaxKind.PublicKeyword),
                returnType: VoidType(),
                explicitInterfaceSpecifier: null,
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                body: isInInterface ? null : context.StmtBlk.Get<BlockSyntax>(),
                expressionBody: null,
                semicolonToken: (!isInInterface && context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitCallingconvention([NotNull] XSharpParser.CallingconventionContext context)
        {
            // TODO nvk (calling convention is ignored for now)
        }

        public override void ExitParameterList([NotNull] XSharpParser.ParameterListContext context)
        {
            var @params = _pool.AllocateSeparated<ParameterSyntax>();
            foreach (var paramCtx in context._Params)
            {
                if (@params.Count>0)
                    @params.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                @params.Add(paramCtx.Get<ParameterSyntax>());
            }
            context.Put(_syntaxFactory.ParameterList(
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                @params,
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
            _pool.Free(@params);
        }

        public override void ExitParameter([NotNull] XSharpParser.ParameterContext context)
        {
            context.Put(_syntaxFactory.Parameter(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                type: context.Type.Get<TypeSyntax>(),
                identifier: context.Id.Get<SyntaxToken>(),
                @default: context.Default == null ? null : _syntaxFactory.EqualsValueClause(
                    SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    context.Default.Get<ExpressionSyntax>())));
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
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
            if (!modifiers.Any(SyntaxKind.PublicKeyword) && !modifiers.Any(SyntaxKind.InternalKeyword))
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.PublicKeyword));
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitStatementBlock([NotNull] XSharpParser.StatementBlockContext context)
        {
            var openBrace = SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken);
            var closeBrace = SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken);
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
                    locCtx.DataType.Put(_syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.IntKeyword)));
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
                    locCtx.DataType.Put(_syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.IntKeyword)));
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
            context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)).
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
                (context.Expression != null) ? _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    context.Expression.Get<ExpressionSyntax>()) : null));
            var modifiers = _pool.Allocate();
            if (context.Const != null)
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.ConstKeyword));
            context.Put(_syntaxFactory.LocalDeclarationStatement(
                modifiers.ToTokenList(),
                _syntaxFactory.VariableDeclaration(context.DataType.Get<TypeSyntax>(), variables),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            _pool.Free(variables);
            _pool.Free(modifiers);
        }

        public override void ExitImpliedvar([NotNull] XSharpParser.ImpliedvarContext context)
        {
            var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            variables.Add(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(),null,
                (context.Expression != null) ? _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    context.Expression.Get<ExpressionSyntax>()) : null));
            var modifiers = _pool.Allocate();
            if (context.Const != null)
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.ConstKeyword));
            context.Put(_syntaxFactory.LocalDeclarationStatement(
                modifiers.ToTokenList(),
                _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$var")), variables),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            _pool.Free(variables);
            _pool.Free(modifiers);
        }

        public override void ExitXbasedeclStmt([NotNull] XSharpParser.XbasedeclStmtContext context)
        {
            context.Put(context.xbasedecl().Get<StatementSyntax>());
        }

        public override void ExitXbasedecl([NotNull] XSharpParser.XbasedeclContext context)
        {
            context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)).
                WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(0, 1, ErrorCode.ERR_FeatureNotAvailableInVersion1, context)));
        }

        public override void ExitWhileStmt([NotNull] XSharpParser.WhileStmtContext context)
        {
            context.Put(_syntaxFactory.WhileStatement(SyntaxFactory.MakeToken(SyntaxKind.WhileKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>()));
        }

        public override void ExitRepeatStmt([NotNull] XSharpParser.RepeatStmtContext context)
        {
            context.Put(_syntaxFactory.DoStatement(SyntaxFactory.MakeToken(SyntaxKind.DoKeyword),
                context.StmtBlk.Get<BlockSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.WhileKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
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
                        SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                        context.InitExpr.Get<ExpressionSyntax>());
                    whileExpr = _syntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.LessThanEqualsToken),
                        context.FinalExpr.Get<ExpressionSyntax>());
                    incrExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.AddAssignmentExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.PlusEqualsToken),
                        context.Step.Get<ExpressionSyntax>());
                    break;
                case XSharpParser.DOWNTO:
                    initExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                        context.InitExpr.Get<ExpressionSyntax>());
                    whileExpr = _syntaxFactory.BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.GreaterThanEqualsToken),
                        context.FinalExpr.Get<ExpressionSyntax>());
                    incrExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.SubtractAssignmentExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.MinusEqualsToken),
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
                            _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                                context.InitExpr.Get<ExpressionSyntax>())));
                        var modifiers = _pool.Allocate();
                        stmts.Add(_syntaxFactory.LocalDeclarationStatement(
                            modifiers.ToTokenList(),
                            _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$var")), variables),
                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                        _pool.Free(variables);
                        _pool.Free(modifiers);
                    }
                    {
                        var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
                        variables.Add(_syntaxFactory.VariableDeclarator(endToken,null,
                            _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                                context.FinalExpr.Get<ExpressionSyntax>())));
                        var modifiers = _pool.Allocate();
                        stmts.Add(_syntaxFactory.LocalDeclarationStatement(
                            modifiers.ToTokenList(),
                            _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$var")), variables),
                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                        _pool.Free(variables);
                        _pool.Free(modifiers);
                    }
                    {
                        var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
                        variables.Add(_syntaxFactory.VariableDeclarator(indToken,null,
                            _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                                _syntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression,
                                    _syntaxFactory.IdentifierName(startToken),
                                    SyntaxFactory.MakeToken(SyntaxKind.LessThanEqualsToken),
                                    _syntaxFactory.IdentifierName(endToken)))));
                        var modifiers = _pool.Allocate();
                        stmts.Add(_syntaxFactory.LocalDeclarationStatement(
                            modifiers.ToTokenList(),
                            _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$var")), variables),
                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                        _pool.Free(variables);
                        _pool.Free(modifiers);
                    }
                    initExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                        _syntaxFactory.IdentifierName(startToken));
                    whileExpr = _syntaxFactory.ConditionalExpression(
                        _syntaxFactory.IdentifierName(indToken),
                        SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                        _syntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression,
                            context.Iter.Get<ExpressionSyntax>(),
                            SyntaxFactory.MakeToken(SyntaxKind.LessThanEqualsToken),
                            _syntaxFactory.IdentifierName(endToken)),
                        SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                        _syntaxFactory.BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression,
                            context.Iter.Get<ExpressionSyntax>(),
                            SyntaxFactory.MakeToken(SyntaxKind.GreaterThanEqualsToken),
                            _syntaxFactory.IdentifierName(endToken)));
                    incrExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.AddAssignmentExpression,
                        context.Iter.Get<ExpressionSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.PlusEqualsToken),
                        _syntaxFactory.ConditionalExpression(
                            _syntaxFactory.IdentifierName(indToken),
                            SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                            context.Step.Get<ExpressionSyntax>(),
                            SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                            _syntaxFactory.PrefixUnaryExpression(SyntaxKind.UnaryMinusExpression,
                                SyntaxFactory.MakeToken(SyntaxKind.MinusToken),
                                context.Step.Get<ExpressionSyntax>())));
                    break;
            }
            var init = _pool.AllocateSeparated<ExpressionSyntax>();
            init.Add(initExpr);
            var incr = _pool.AllocateSeparated<ExpressionSyntax>();
            incr.Add(incrExpr);
            context.Put(_syntaxFactory.ForStatement(SyntaxFactory.MakeToken(SyntaxKind.ForKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                null,
                init,
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken),
                whileExpr,
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken),
                incr,
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>()));
            _pool.Free(init);
            _pool.Free(incr);
            if (blockStmts != null) {
                context.Put(_syntaxFactory.Block(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    (SyntaxListBuilder<StatementSyntax>)blockStmts,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)));
                _pool.Free((SyntaxListBuilder<StatementSyntax>)blockStmts);
            }
        }

        public override void ExitForeachStmt([NotNull] XSharpParser.ForeachStmtContext context)
        {
            context.Put(_syntaxFactory.ForEachStatement(SyntaxFactory.MakeToken(SyntaxKind.ForEachKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Type?.Get<TypeSyntax>() ?? _syntaxFactory.IdentifierName(SyntaxFactory.Identifier("Xs$var")),
                context.Id.Get<SyntaxToken>(),
                SyntaxFactory.MakeToken(SyntaxKind.InKeyword),
                context.Container.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>()));
        }

        public override void ExitIfStmt([NotNull] XSharpParser.IfStmtContext context)
        {
            context.Put(context.IfStmt.Get<IfStatementSyntax>());
        }

        public override void ExitIfElseBlock([NotNull] XSharpParser.IfElseBlockContext context)
        {
            context.Put(_syntaxFactory.IfStatement(SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Cond.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>(),
                (context.ElseIfBlock != null) ? 
                    _syntaxFactory.ElseClause(SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword), context.ElseIfBlock.Get<IfStatementSyntax>())
                : (context.ElseBlock != null) ?
                    _syntaxFactory.ElseClause(SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword), context.ElseBlock.Get<BlockSyntax>())
                : null));
        }

        public override void ExitCaseStmt([NotNull] XSharpParser.CaseStmtContext context)
        {
            context.Put((StatementSyntax)context.CaseStmt?.Get<IfStatementSyntax>() ?? 
                _syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitCaseBlock([NotNull] XSharpParser.CaseBlockContext context)
        {
            if (context.Key.Type == XSharpParser.OTHERWISE)
                context.Put(context.StmtBlk.Get<StatementSyntax>());
            else {
                context.Put(_syntaxFactory.IfStatement(SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    context.Cond.Get<ExpressionSyntax>(),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                    context.StmtBlk.Get<BlockSyntax>(),
                    (context.NextCase == null) ? null :
                        _syntaxFactory.ElseClause(SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword),
                            context.NextCase.Get<StatementSyntax>())));
            }
        }

        public override void ExitExitStmt([NotNull] XSharpParser.ExitStmtContext context)
        {
            context.Put(_syntaxFactory.BreakStatement(SyntaxFactory.MakeToken(SyntaxKind.BreakKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitLoopStmt([NotNull] XSharpParser.LoopStmtContext context)
        {
            context.Put(_syntaxFactory.ContinueStatement(SyntaxFactory.MakeToken(SyntaxKind.ContinueKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitExpressionStmt([NotNull] XSharpParser.ExpressionStmtContext context)
        {
            var _semicolon = SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken);
            var statements = _pool.Allocate<StatementSyntax>();
            foreach (var exprCtx in context._Exprs)
            {
                statements.Add(_syntaxFactory.ExpressionStatement(exprCtx.Get<ExpressionSyntax>(), _semicolon));
            }
            var openBrace = SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken);
            var closeBrace = SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken);
            context.Put(_syntaxFactory.Block(openBrace, statements, closeBrace));
            _pool.Free(statements);
        }

        public override void ExitBreakStmt([NotNull] XSharpParser.BreakStmtContext context)
        {
            context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)).
                WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(0, 1, ErrorCode.ERR_FeatureNotAvailableInVersion1, context)));
        }

        public override void ExitThrowStmt([NotNull] XSharpParser.ThrowStmtContext context)
        {
            context.Put(_syntaxFactory.ThrowStatement(SyntaxFactory.MakeToken(SyntaxKind.ThrowKeyword),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitTryStmt([NotNull] XSharpParser.TryStmtContext context)
        {
            var catches = _pool.Allocate<CatchClauseSyntax>();
            foreach (var catchCtx in context._CatchBlock)
            {
                catches.Add(catchCtx.Get<CatchClauseSyntax>());
            }
            context.Put(_syntaxFactory.TryStatement(SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                context.StmtBlk.Get<BlockSyntax>(),
                catches,
                _syntaxFactory.FinallyClause(SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                    context.FinBlock.Get<BlockSyntax>())));
            _pool.Free(catches);
        }

        public override void ExitCatchBlock([NotNull] XSharpParser.CatchBlockContext context)
        {
            context.Put(_syntaxFactory.CatchClause(SyntaxFactory.MakeToken(SyntaxKind.CatchKeyword),
                _syntaxFactory.CatchDeclaration(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    context.Type.Get<TypeSyntax>(),
                    context.Id.Get<SyntaxToken>(),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)),
                null,
                context.StmtBlk.Get<BlockSyntax>()));
        }

        public override void ExitSeqStmt([NotNull] XSharpParser.SeqStmtContext context)
        {
            // TODO
        }

        public override void ExitRecoverBlock([NotNull] XSharpParser.RecoverBlockContext context)
        {
            // TODO
        }

        public override void ExitLockStmt([NotNull] XSharpParser.LockStmtContext context)
        {
            context.Put(_syntaxFactory.LockStatement(SyntaxFactory.MakeToken(SyntaxKind.LockKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>()));
        }

        public override void ExitScopeStmt([NotNull] XSharpParser.ScopeStmtContext context)
        {
            context.Put(context.StmtBlk.Get<BlockSyntax>());
        }

        public override void ExitReturnStmt([NotNull] XSharpParser.ReturnStmtContext context)
        {
            context.Put(_syntaxFactory.ReturnStatement(SyntaxFactory.MakeToken(SyntaxKind.ReturnKeyword), 
                context.Expr?.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitYieldStmt([NotNull] XSharpParser.YieldStmtContext context)
        {
            context.Put(_syntaxFactory.YieldStatement(SyntaxKind.YieldReturnStatement,
                SyntaxFactory.MakeToken(SyntaxKind.YieldKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.ReturnKeyword),
                context.Expr?.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitSwitchStmt([NotNull] XSharpParser.SwitchStmtContext context)
        {
            var sections = _pool.Allocate<SwitchSectionSyntax>();
            foreach(var switchBlkCtx in context._SwitchBlock) {
                sections.Add(switchBlkCtx.Get<SwitchSectionSyntax>());
            }
            context.Put(_syntaxFactory.SwitchStatement(SyntaxFactory.MakeToken(SyntaxKind.SwitchKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                sections,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)));
            _pool.Free(sections);
        }

        public override void ExitSwitchBlock([NotNull] XSharpParser.SwitchBlockContext context)
        {
            var labels = _pool.Allocate<SwitchLabelSyntax>();
            labels.Add(_syntaxFactory.CaseSwitchLabel(context.Key.SyntaxKeyword(),
                context.Const?.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.ColonToken)));
            var stmts = _pool.Allocate<StatementSyntax>();
            if (context.StmtBlk._Stmts.Count > 0) {
                stmts.Add(context.StmtBlk.Get<BlockSyntax>());
                stmts.Add(_syntaxFactory.BreakStatement(SyntaxFactory.MakeToken(SyntaxKind.BreakKeyword),
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))); // TODO: only add it if missing!
            }
            context.Put(_syntaxFactory.SwitchSection(labels, stmts));
            _pool.Free(labels);
            _pool.Free(stmts);
        }

        public override void ExitUsingStmt([NotNull] XSharpParser.UsingStmtContext context)
        {
            // TODO: variable declarations in using expr
            context.Put(_syntaxFactory.UsingStatement(SyntaxFactory.MakeToken(SyntaxKind.UsingKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                null,
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                context.Stmtblk.Get<BlockSyntax>()));
        }

        public override void ExitQoutStmt([NotNull] XSharpParser.QoutStmtContext context)
        {
            context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)).
                WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(0, 1, ErrorCode.ERR_FeatureNotAvailableInVersion1, context)));
        }

        public override void ExitUnsafeStmt([NotNull] XSharpParser.UnsafeStmtContext context)
        {
            context.Put(_syntaxFactory.UnsafeStatement(SyntaxFactory.MakeToken(SyntaxKind.UnsafeKeyword),
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
            if (context.Q != null)
                context.Put(_syntaxFactory.ConditionalAccessExpression(
                    context.Left.Get<ExpressionSyntax>(),
                    SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                    _syntaxFactory.MemberBindingExpression(SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                        context.Right.Get<IdentifierNameSyntax>())));
            else
                context.Put(_syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                    context.Left.Get<ExpressionSyntax>(),
                    SyntaxFactory.MakeToken(SyntaxKind.DotToken),
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
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
        }

        public override void ExitMethodCall([NotNull] XSharpParser.MethodCallContext context)
        {
            var expr = context.Expr.Get<ExpressionSyntax>();
            ArgumentListSyntax argList;
            if (context.ArgList != null)
                argList = context.ArgList.Get<ArgumentListSyntax>();
            else
            {
                var openParen = SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken);
                var closeParen = SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken);
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
                var openParen = SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken);
                var closeParen = SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken);
                var args = default(SeparatedSyntaxList<ArgumentSyntax>);
                argList = _syntaxFactory.ArgumentList(openParen, args, closeParen);
            }
            context.Put(_syntaxFactory.ObjectCreationExpression(
                SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                type, 
                argList,
                initializer: null));
        }

        public override void ExitArrayAccess([NotNull] XSharpParser.ArrayAccessContext context)
        {
            var expr = context.Expr.Get<ExpressionSyntax>();
            BracketedArgumentListSyntax argList;
            if (context.ArgList != null)
                argList = context.ArgList.Get<BracketedArgumentListSyntax>();
            else
            {
                var openBracket = SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken);
                var closeBracket = SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken);
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
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
        }

        public override void ExitTypeCheckExpression([NotNull] XSharpParser.TypeCheckExpressionContext context)
        {
            context.Put(_syntaxFactory.BinaryExpression(
                SyntaxKind.IsExpression,
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.IsKeyword),
                context.Type.Get<ExpressionSyntax>()));
        }

        public override void ExitTypeCast([NotNull] XSharpParser.TypeCastContext context)
        {
            context.Put(_syntaxFactory.CastExpression(
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Type.Get<TypeSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitSizeOfExpression([NotNull] XSharpParser.SizeOfExpressionContext context)
        {
            context.Put(_syntaxFactory.SizeOfExpression(
                SyntaxFactory.MakeToken(SyntaxKind.SizeOfKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Type.Get<TypeSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
        }

        public override void ExitTypeOfExpression([NotNull] XSharpParser.TypeOfExpressionContext context)
        {
            context.Put(_syntaxFactory.TypeOfExpression(
                SyntaxFactory.MakeToken(SyntaxKind.TypeOfKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Type.Get<TypeSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
        }

        public override void ExitBracketedArgumentList([NotNull] XSharpParser.BracketedArgumentListContext context)
        {
            var args = _pool.AllocateSeparated<ArgumentSyntax>();
            foreach (var argCtx in context._Args)
            {
                if (args.Count != 0)
                    args.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                args.Add(argCtx.Get<ArgumentSyntax>());
            }
            context.Put(_syntaxFactory.BracketedArgumentList(
                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken), 
                args, 
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
            _pool.Free(args);
        }

        public override void ExitArgumentList([NotNull] XSharpParser.ArgumentListContext context)
        {
            var args = _pool.AllocateSeparated<ArgumentSyntax>();
            var openParen = SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken);
            var closeParen = SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken);
            foreach (var argCtx in context._Args)
            {
                if (args.Count != 0)
                    args.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                args.Add(argCtx.Get<ArgumentSyntax>());
            }
            context.Put(_syntaxFactory.ArgumentList(openParen, args, closeParen));
            _pool.Free(args);
        }

        public override void ExitArgument([NotNull] XSharpParser.ArgumentContext context)
        {
            context.Put(_syntaxFactory.Argument(
                context.Name == null ? null : _syntaxFactory.NameColon(context.Name.Get<IdentifierNameSyntax>(), SyntaxFactory.MakeToken(SyntaxKind.ColonToken)), 
                context.RefOut?.SyntaxKeyword(), context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitQualifiedName([NotNull] XSharpParser.QualifiedNameContext context)
        {
            context.Put(_syntaxFactory.QualifiedName(context.Left.Get<NameSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.DotToken),
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
                    types.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                types.Add(context.Get<TypeSyntax>());
            }
            context.Put(_syntaxFactory.TypeArgumentList(
                SyntaxFactory.MakeToken(SyntaxKind.LessThanToken),
                types.ToList(),
                SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken)
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
                SyntaxFactory.MakeToken(SyntaxKind.AsteriskToken)));
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
            var omittedArraySizeExpressionInstance = _syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MakeToken(SyntaxKind.OmittedArraySizeExpressionToken));
            foreach (var comma in context.COMMA())
            {
                sizes.Add(omittedArraySizeExpressionInstance);
                sizes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
            }
            sizes.Add(omittedArraySizeExpressionInstance);
            context.Put(_syntaxFactory.ArrayRankSpecifier(
                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                sizes,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
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

        public override void ExitAwaitExpression([NotNull] XSharpParser.AwaitExpressionContext context)
        {
            context.Put(_syntaxFactory.AwaitExpression(SyntaxFactory.MakeToken(SyntaxKind.AwaitKeyword),context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitSelfExpression([NotNull] XSharpParser.SelfExpressionContext context)
        {
            context.Put(_syntaxFactory.ThisExpression(context.Key.SyntaxKeyword()));
        }

        public override void ExitSuperExpression([NotNull] XSharpParser.SuperExpressionContext context)
        {
            context.Put(_syntaxFactory.BaseExpression(context.Key.SyntaxKeyword()));
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
                SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                context.TrueExpr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                context.FalseExpr.Get<ExpressionSyntax>()));
        }

        public override void ExitLiteralArray([NotNull] XSharpParser.LiteralArrayContext context)
        {
            var openBrace = SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken);
            var closeBrace = SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken);
            if ((context._Exprs?.Count ?? 0) > 0 )
            {
                var exprs = _pool.AllocateSeparated<ExpressionSyntax>();
                foreach (var e in context._Exprs)
                {
                    if (exprs.Count != 0)
                        exprs.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
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
                arrowToken: SyntaxFactory.MakeToken(SyntaxKind.EqualsGreaterThanToken), 
                body: context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitCodeblockParamList([NotNull] XSharpParser.CodeblockParamListContext context)
        {
            var @params = _pool.AllocateSeparated<ParameterSyntax>();
            foreach (var idCtx in context._Ids)
            {
                if (@params.Count>0)
                    @params.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
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
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                @params,
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
            _pool.Free(@params);
        }

        public override void ExitLiteralValue([NotNull] XSharpParser.LiteralValueContext context)
        {
            context.Put(_syntaxFactory.LiteralExpression(context.Token.ExpressionKindLiteral(), context.Token.SyntaxLiteralValue()));
        }

        public override void ExitIdentifierString([NotNull] XSharpParser.IdentifierStringContext context)
        {
            context.Put(context.Token?.SyntaxLiteralValue()
                ?? context.XsToken?.Token.SyntaxLiteralValue()
                ?? context.VnToken?.Token.SyntaxLiteralValue()
                ?? context.VoToken?.Token.SyntaxLiteralValue());
        }

        public override void ExitIdentifier([NotNull] XSharpParser.IdentifierContext context)
        {
            context.Put(context.Token?.SyntaxIdentifier()
                ?? context.XsToken?.Token.SyntaxIdentifier()
                ?? context.VnToken?.Token.SyntaxIdentifier()
                ?? context.VoToken?.Token.SyntaxIdentifier());
        }

        public override void ExitKeyword([NotNull] XSharpParser.KeywordContext context)
        {
            context.Put(context.KwXs?.Token.SyntaxKeywordIdentifier()
                ?? context.KwVn?.Token.SyntaxKeywordIdentifier()
                ?? context.KwVo?.Token.SyntaxKeywordIdentifier());
        }

        public override void ExitKeywordxs([NotNull] XSharpParser.KeywordxsContext context)
        {
            // caught by the keyword/identifier rule
        }

        public override void ExitKeywordvn([NotNull] XSharpParser.KeywordvnContext context)
        {
            // caught by the keyword/identifier rule
        }

        public override void ExitKeywordvo([NotNull] XSharpParser.KeywordvoContext context)
        {
            // caught by the keyword/identifier rule
        }

        public override void ExitNativeType([NotNull] XSharpParser.NativeTypeContext context)
        {
            context.Put(_syntaxFactory.PredefinedType(context.Token.SyntaxNativeType()));
        }

    }
}