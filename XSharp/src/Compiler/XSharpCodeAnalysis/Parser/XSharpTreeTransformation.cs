// Uncomment this define to dump the AST to the debug console.
//#define DUMP_TREE

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
        internal class SyntaxEntities
        {
            internal SyntaxListPool _pool;
            public SyntaxListBuilder<ExternAliasDirectiveSyntax> Externs;
            public SyntaxListBuilder<UsingDirectiveSyntax> Usings;
            public SyntaxListBuilder<AttributeListSyntax> Attributes;
            public SyntaxListBuilder<MemberDeclarationSyntax> Members;

            internal SyntaxEntities(SyntaxListPool pool) {
                Externs = pool.Allocate<ExternAliasDirectiveSyntax>();
                Usings = pool.Allocate<UsingDirectiveSyntax>();
                Attributes = pool.Allocate<AttributeListSyntax>();
                Members = pool.Allocate<MemberDeclarationSyntax>();
                _pool = pool;
            }

            internal void Free()
            {
                _pool.Free(Members);
                _pool.Free(Attributes);
                _pool.Free(Usings);
                _pool.Free(Externs);
            }
        }

        internal class SyntaxClassEntities
        {
            internal class VoPropertyInfo {
                internal SyntaxToken idName;
                internal XSharpParser.MethodContext AccessMethodCtx = null;
                internal XSharpParser.MethodContext AssignMethodCtx = null;
            }

            internal SyntaxListPool _pool;
            public SyntaxListBuilder<MemberDeclarationSyntax> Members;
            public Dictionary<string,VoPropertyInfo> VoProperties;

            internal SyntaxClassEntities(SyntaxListPool pool) {
                Members = pool.Allocate<MemberDeclarationSyntax>();
                _pool = pool;
            }

            internal void Free()
            {
                _pool.Free(Members);
            }

            internal void AddVoPropertyAccessor(XSharpParser.MethodContext accessor)
            {
                if (VoProperties == null)
                    VoProperties = new Dictionary<string, VoPropertyInfo>(CaseInsensitiveComparison.Comparer);
                string name = accessor.Id.Get<SyntaxToken>().Text;
                VoPropertyInfo propertyInfo;
                if (!VoProperties.TryGetValue(name,out propertyInfo)) {
                    propertyInfo = new VoPropertyInfo();
                    propertyInfo.idName = accessor.Id.Get<SyntaxToken>();
                    VoProperties.Add(name,propertyInfo);
                }
                switch (accessor.T.Token.Type) {
                    case XSharpParser.ACCESS:
                        if (propertyInfo.AccessMethodCtx != null)
                            accessor.AddError(new ParseErrorData(ErrorCode.ERR_DuplicateAccessor, accessor));
                        else
                            propertyInfo.AccessMethodCtx = accessor;
                        break;
                    case XSharpParser.ASSIGN:
                        if (propertyInfo.AssignMethodCtx != null)
                            accessor.AddError(new ParseErrorData(ErrorCode.ERR_DuplicateAccessor, accessor));
                        else
                            propertyInfo.AssignMethodCtx = accessor;
                        break;
                    default:
                        break;
                }
            }
        }

        public const string GlobalClassName = "Xs$Globals";
        const string ImpliedTypeName = "Xs$var";
        const string ForStartNamePrefix = "Xs$ForStart$";
        const string ForEndNamePrefix = "Xs$ForEnd$";
        const string ForIndNamePrefix = "Xs$ForInd$";
        const string StaticLocalFieldNamePrefix = "Xs$StaticLocal$";
        const string StaticLocalInitFieldNameSuffix = "$init";
        const string StaticLocalLockFieldNameSuffix = "$lock";
        const string EventFieldNamePrefix = "Xs$Event$";
        const string VoPropertyAccessPrefix = "Xs$Access$";
        const string VoPropertyAssignPrefix = "Xs$Assign$";

        public static SyntaxTree DefaultXSharpSyntaxTree = GenerateDefaultTree();
        private static int _unique = 0;

        internal SyntaxListPool _pool;
        private readonly ContextAwareSyntax _syntaxFactory; // Has context, the fields of which are resettable.
        private XSharpParser _parser;
        private readonly CSharpParseOptions _options;
        internal SyntaxEntities GlobalEntities;
        internal SyntaxClassEntities GlobalClassEntities;
        internal Stack<SyntaxClassEntities> ClassEntities = new Stack<SyntaxClassEntities> ();

        public XSharpTreeTransformation(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool, ContextAwareSyntax syntaxFactory)
        {
            _pool = pool;
            _syntaxFactory = syntaxFactory;
            _parser = parser;
            _options = options;
            GlobalEntities = CreateEntities();
        }

        internal void Free()
        {
            GlobalEntities.Free();
        }

        internal SyntaxEntities CreateEntities() {
            return new SyntaxEntities(_pool);
        }

        internal SyntaxClassEntities CreateClassEntities() {
            return new SyntaxClassEntities(_pool);
        }

        internal string UniqueNameSuffix {
            get { return "$"+_unique++; }
        }

        internal T FixPosition<T>(T r, IToken t) where T: ParserRuleContext
        {
            r.Start = r.Stop = t;
            return r;
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

        SyntaxList<SyntaxToken> TokenListWithDefaultVisibility(Boolean inInterface = false,  params SyntaxKind[] kinds)
        {
            var rb = _pool.Allocate();
            foreach(var k in kinds) {
                rb.Add(SyntaxFactory.MakeToken(k));
            }
            if (!inInterface)
                rb.FixDefaultVisibility();
            var r = rb.ToTokenList();
            _pool.Free(rb);
            return r;
        }

        SyntaxList<SyntaxToken> DefaultMethodModifiers(bool inInterface = false)
        {
            var rb = _pool.Allocate();
            if (!inInterface)
            {
                rb.FixDefaultVisibility();
                if (_options.VirtualInstanceMethods)
                    rb.FixDefaultVirtual();
                else
                    rb.FixDefaultMethod();
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

        BracketedArgumentListSyntax EmptyBracketedArgumentList()
        {
            return _syntaxFactory.BracketedArgumentList(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken), 
                    default(SeparatedSyntaxList<ArgumentSyntax>), 
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken));
        }

        SyntaxList<T> MakeList<T>(System.Collections.IEnumerable t) where T : InternalSyntax.CSharpSyntaxNode
        {
            if (t == null)
                return default(SyntaxList<T>);
            var l = _pool.Allocate<T>();
            foreach (var item in t) {
                if (item != null) {
                    if (((IParseTree)item).CsNode is SyntaxList<T>)
                        l.AddRange(((IParseTree)item).GetList<T>());
                    else
                        l.Add(((IParseTree)item).Get<T>());
                }
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

        ArgumentListSyntax MakeArgumentList(params ArgumentSyntax[] items)
        {
            return _syntaxFactory.ArgumentList(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken), 
                    MakeSeparatedList<ArgumentSyntax>(items), 
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
        }

        ArrayRankSpecifierSyntax MakeArrayRankSpeicifier(int ranks)
        {
            var sizes = _pool.AllocateSeparated<ExpressionSyntax>();
            for(int i = 0; i < ranks; i++) {
                if (i > 0)
                    sizes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                sizes.Add(_syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MakeToken(SyntaxKind.OmittedArraySizeExpressionToken)));
            }
            var r = _syntaxFactory.ArrayRankSpecifier(SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                sizes,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken));
            _pool.Free(sizes);
            return r;
        }

        TypeSyntax VoidType()
        {
            return _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.VoidKeyword));
        }

        TypeSyntax MissingType()
        {
            return _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.ObjectKeyword))
                .WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_TypeExpected));
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
            string idName = ids[0];
            string alias = null;
            int cc = idName.IndexOf("::");
            if (cc >= 0) {
                alias = idName.Substring(0,cc);
                idName = idName.Substring(cc+2);
            }
            NameSyntax r = _syntaxFactory.IdentifierName(SyntaxToken.Identifier(idName));
            if (alias != null) {
                if (string.Compare(alias,"global",StringComparison.OrdinalIgnoreCase) == 0)
                    r = _syntaxFactory.AliasQualifiedName(
                        _syntaxFactory.IdentifierName(SyntaxFactory.MakeToken(SyntaxKind.GlobalKeyword, alias)),
                        SyntaxFactory.MakeToken(SyntaxKind.ColonColonToken),
                        (SimpleNameSyntax)r);
                else
                    r = _syntaxFactory.AliasQualifiedName(
                        _syntaxFactory.IdentifierName(SyntaxToken.Identifier(alias)),
                        SyntaxFactory.MakeToken(SyntaxKind.ColonColonToken),
                        (SimpleNameSyntax)r);
            }
            for(int i = 1; i < ids.Length; i++)
            {
                r = _syntaxFactory.QualifiedName(
                    r,
                    SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                    _syntaxFactory.IdentifierName(SyntaxToken.Identifier(ids[i])) );
            }
            return r;
        }

        public NameSyntax GenerateGlobalQualifiedNameFromList(string name, params string[] dotNames)
        {
            NameSyntax r = _syntaxFactory.IdentifierName(SyntaxToken.Identifier(name));
            r = _syntaxFactory.AliasQualifiedName(
                _syntaxFactory.IdentifierName(SyntaxFactory.MakeToken(SyntaxKind.GlobalKeyword, "global")),
                SyntaxFactory.MakeToken(SyntaxKind.ColonColonToken),
                (SimpleNameSyntax)r);
            foreach (var dotName in dotNames)
            {
                r = _syntaxFactory.QualifiedName(
                    r,
                    SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                    _syntaxFactory.IdentifierName(SyntaxToken.Identifier(dotName)));
            }
            return r;
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
            GenerateAttributeList(attributeLists, "global::System.Runtime.CompilerServices.CompilerGenerated");
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

        private NamespaceDeclarationSyntax GenerateNamespace(string name, SyntaxList<MemberDeclarationSyntax> members)
        {
            var externs = _pool.Allocate<ExternAliasDirectiveSyntax>();
            var usings = _pool.Allocate<UsingDirectiveSyntax>();
            var r = _syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                name: _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(name)),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                externs: externs,
                usings: usings,
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            _pool.Free(externs);
            _pool.Free(usings);
            return r;
        }

        private MemberDeclarationSyntax GenerateGlobalClass(string className, SyntaxList<MemberDeclarationSyntax> members)
        {
            SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
            SyntaxListBuilder modifiers = _pool.Allocate();
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.PartialKeyword));
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.PublicKeyword));
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
            var r = /*GenerateNamespace(_options.ModuleName, MakeList<MemberDeclarationSyntax>(*/
                _syntaxFactory.ClassDeclaration(
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
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken) ) /*))*/;
            _pool.Free(attributeLists);
            _pool.Free(modifiers);
            return r;
        }

        private MemberDeclarationSyntax GenerateGlobalClass(string className, params MemberDeclarationSyntax[] members)
        {
            SyntaxListBuilder<MemberDeclarationSyntax> globalClassMembers = _pool.Allocate<MemberDeclarationSyntax>();
            SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
            if (members.Length == 0) {
                members = new MemberDeclarationSyntax[] {
                    _syntaxFactory.FieldDeclaration(EmptyList<AttributeListSyntax>(),
                        TokenList(SyntaxKind.ConstKeyword,SyntaxKind.PublicKeyword),
                        _syntaxFactory.VariableDeclaration(_syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.IntKeyword)), 
                            MakeSeparatedList<VariableDeclaratorSyntax>(
                                _syntaxFactory.VariableDeclarator(SyntaxFactory.Identifier("Xs$Dummy"),null,
                                    _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                                        _syntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                                            SyntaxFactory.Literal(null,"",0,null))))
                                )),
                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))
                };
                GenerateAttributeList(attributeLists, 
                    "global::System.Runtime.CompilerServices.CompilerGenerated",
                    "global::System.Runtime.CompilerServices.CompilerGlobalScope");
            }
            if (members.Length > 0) {
                foreach(var m in members)
                    globalClassMembers.Add(m);
            }
            SyntaxListBuilder modifiers = _pool.Allocate();
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.PartialKeyword));
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.PublicKeyword));
            modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
            var r = /*GenerateNamespace(_options.ModuleName, MakeList<MemberDeclarationSyntax>(*/
                _syntaxFactory.ClassDeclaration(
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
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken) ) /*))*/;
            _pool.Free(attributeLists);
            _pool.Free(modifiers);
            _pool.Free(globalClassMembers);
            return r;
        }

        private MethodDeclarationSyntax GenerateMainMethod(string startMethodName)
        {
            SyntaxListBuilder<AttributeListSyntax> attributeLists = _pool.Allocate<AttributeListSyntax>();
            GenerateAttributeList(attributeLists, "global::System.Runtime.CompilerServices.CompilerGenerated");
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

        internal PropertyDeclarationSyntax GenerateVoProperty(SyntaxClassEntities.VoPropertyInfo vop) {
            var getMods = _pool.Allocate();
            var setMods = _pool.Allocate();
            var outerMods = _pool.Allocate();
            int getVisLvl;
            int setVisLvl;
            if (vop.AccessMethodCtx != null) {
                if (vop.AccessMethodCtx.Modifiers != null)
                    getMods.AddRange(vop.AccessMethodCtx.Modifiers.GetList<SyntaxToken>());
                else if (!vop.AccessMethodCtx.isInInterface()) {
                    getMods.FixDefaultVisibility();
                    if (_options.VirtualInstanceMethods)
                        getMods.FixDefaultVirtual();
                    else
                        getMods.FixDefaultMethod();
                }
                getVisLvl = getMods.GetVisibilityLevel();
            }
            else
                getVisLvl = 15;
            if (vop.AssignMethodCtx != null) {
                if (vop.AssignMethodCtx.Modifiers != null)
                    setMods.AddRange(vop.AssignMethodCtx.Modifiers.GetList<SyntaxToken>());
                else if (!vop.AssignMethodCtx.isInInterface()) {
                    setMods.FixDefaultVisibility();
                    if (_options.VirtualInstanceMethods)
                        setMods.FixDefaultVirtual();
                    else
                        setMods.FixDefaultMethod();
                }
                setVisLvl = setMods.GetVisibilityLevel();
            }
            else
                setVisLvl = 15;
            if (getVisLvl <= setVisLvl) {
                outerMods.AddRange(getMods);
                getMods.Clear();
            }
            else {
                outerMods.AddRange(setMods);
                setMods.Clear();
            }
            var rawMods = getVisLvl <= setVisLvl ? setMods : getMods;
            var innerMods = _pool.Allocate();
            for (int i = 0; i < rawMods.Count; i++) {
                var t = rawMods[i];
                if (!outerMods.Any(t.Kind)) {
                    if (!SyntaxFacts.IsAccessibilityModifier(t.Kind))
                        t = t.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_BadMemberFlag, t));
                    innerMods.Add(t);
                }
            }
            _pool.Free(rawMods);
            if (getVisLvl <= setVisLvl) {
                setMods = innerMods;
            }
            else {
                getMods = innerMods;
            }

            TypeSyntax voPropType;
            if (vop.AccessMethodCtx != null)
            {
                voPropType = vop.AccessMethodCtx.Type?.Get<TypeSyntax>() ?? MissingType();
            }
            else if (vop.AssignMethodCtx != null && vop.AssignMethodCtx.ParamList != null && vop.AssignMethodCtx.ParamList._Params?.Count > 0)
                voPropType = vop.AssignMethodCtx.ParamList._Params[0].Type?.Get<TypeSyntax>() ?? MissingType();
            else
                voPropType = MissingType();

            var accessors = _pool.Allocate<AccessorDeclarationSyntax>();
            if (vop.AccessMethodCtx != null) {
                bool isInInterfaceOrAbstract = vop.AccessMethodCtx.isInInterface() || outerMods.Any(SyntaxKind.AbstractKeyword) || outerMods.Any(SyntaxKind.ExternKeyword);
                accessors.Add(
                    _syntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration,EmptyList<AttributeListSyntax>(),getMods.ToTokenList(),
                        SyntaxFactory.MakeToken(SyntaxKind.GetKeyword),
                        isInInterfaceOrAbstract ? null
                        : _syntaxFactory.Block(
                            SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                            MakeList<StatementSyntax>(_syntaxFactory.ReturnStatement(SyntaxFactory.MakeToken(SyntaxKind.ReturnKeyword),
                                _syntaxFactory.InvocationExpression(_syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier(VoPropertyAccessPrefix+vop.idName.Text)),
                                    MakeArgumentList()),
                                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))),
                            SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)),
                        isInInterfaceOrAbstract ? SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)
                        : null)
                    );
            }
            if (vop.AssignMethodCtx != null) {
                bool isInInterfaceOrAbstract = vop.AssignMethodCtx.isInInterface() || outerMods.Any(SyntaxKind.AbstractKeyword) || outerMods.Any(SyntaxKind.ExternKeyword);
                accessors.Add(
                    _syntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration,EmptyList<AttributeListSyntax>(),setMods.ToTokenList(),
                        SyntaxFactory.MakeToken(SyntaxKind.SetKeyword),
                        isInInterfaceOrAbstract ? null
                        : _syntaxFactory.Block(
                            SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                            MakeList<StatementSyntax>(_syntaxFactory.ExpressionStatement(
                                _syntaxFactory.InvocationExpression(_syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier(VoPropertyAssignPrefix+vop.idName.Text)),
                                    MakeArgumentList(_syntaxFactory.Argument(null,null,_syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier("value"))))),
                                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))),
                            SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)),
                        isInInterfaceOrAbstract ? SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)
                        : null)
                    );
            }

            var prop = _syntaxFactory.PropertyDeclaration(
                attributeLists: EmptyList<AttributeListSyntax>(),
                modifiers: outerMods.ToTokenList(),
                type: voPropType,
                explicitInterfaceSpecifier: null,
                identifier: vop.idName,
                accessorList: _syntaxFactory.AccessorList(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    accessors,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)),
                expressionBody: null,
                initializer: null,
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

            _pool.Free(accessors);
            _pool.Free(getMods);
            _pool.Free(setMods);
            _pool.Free(outerMods);

            return prop;
        }

        public static SyntaxTree GenerateDefaultTree()
        {
            var t = new XSharpTreeTransformation(null, CSharpParseOptions.Default, new SyntaxListPool(), new ContextAwareSyntax(new SyntaxFactoryContext()));

            t.GlobalEntities.Members.Add(t.GenerateGlobalClass(GlobalClassName));

            var eof = SyntaxFactory.Token(SyntaxKind.EndOfFileToken);
            return CSharpSyntaxTree.Create(
                (Syntax.CompilationUnitSyntax)t._syntaxFactory.CompilationUnit(
                    t.GlobalEntities.Externs, t.GlobalEntities.Usings, t.GlobalEntities.Attributes, t.GlobalEntities.Members, eof).CreateRed());
        }

        public override void VisitErrorNode([NotNull] IErrorNode node)
        {
        }

        public override void VisitTerminal(ITerminalNode node)
        {
        }

        public override void EnterEveryRule([NotNull] ParserRuleContext context)
        {
#if DEBUG && DUMP_TREE
            var s = context.GetType().ToString();
            s = s.Substring(s.LastIndexOfAny(".+".ToCharArray())+1);
            s = s.Replace("Context","");
            Debug.WriteLine("{0}=> ({1},{2}) {3} [{4}] <{5}>",new string(' ',context.Depth()),context.Start.Line,context.Start.Column,s,context.Start.Text,XSharpParser.DefaultVocabulary.GetSymbolicName(context.Start.Type));
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
#if DEBUG && DUMP_TREE
            var s = context.GetType().ToString();
            s = s.Substring(s.LastIndexOfAny(".+".ToCharArray())+1);
            s = s.Replace("Context","");
            Debug.WriteLine("{0}<= ({1},{2}) {3} '{4}'",new string(' ',context.Depth()),context.Start.Line,context.Start.Column,s,context.Start.Text);
#endif
        }

        public override void EnterSource([NotNull] XSharpParser.SourceContext context)
        {
            GlobalClassEntities = CreateClassEntities();
            ClassEntities.Push(GlobalClassEntities);
        }

        public override void ExitSource([NotNull] XSharpParser.SourceContext context)
        {
            var globalMembers = GlobalEntities.Members;
            if (!string.IsNullOrEmpty(_options.DefaultNamespace))
            {
                globalMembers = _pool.Allocate<MemberDeclarationSyntax>();
            }
            foreach(var entityCtx in context._Entities)
            {
                var s = entityCtx.CsNode;
                if (s is NamespaceDeclarationSyntax)
                    GlobalEntities.Members.Add(s as MemberDeclarationSyntax);
                else if (s is MemberDeclarationSyntax)
                    globalMembers.Add(s as MemberDeclarationSyntax);
                else if (s is UsingDirectiveSyntax)
                    GlobalEntities.Usings.Add(s as UsingDirectiveSyntax);
                else if (s is AttributeListSyntax)
                    GlobalEntities.Attributes.Add(s as AttributeListSyntax);
                else if (s is ExternAliasDirectiveSyntax)
                    GlobalEntities.Externs.Add(s as ExternAliasDirectiveSyntax);
            }

            var generated = ClassEntities.Pop();
            if(generated.Members.Count > 0) {
                globalMembers.Add(GenerateGlobalClass(GlobalClassName, generated.Members));
            }
            generated.Free();

            if (!string.IsNullOrEmpty(_options.DefaultNamespace))
            {
                GlobalEntities.Members.Add(_syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                    name: GenerateQualifiedName(_options.DefaultNamespace),
                    openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    externs: MakeList<ExternAliasDirectiveSyntax>(),
                    usings: MakeList<UsingDirectiveSyntax>(),
                    members: globalMembers,
                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                GlobalEntities.Usings.Add(_syntaxFactory.UsingDirective(SyntaxFactory.MakeToken(SyntaxKind.UsingKeyword),
                    null,
                    null,
                    GenerateQualifiedName(_options.DefaultNamespace),
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }

            // Add: using static Xs$Globals
            //if (generated.Members.Count > 0)
            //{
            GlobalEntities.Usings.Add(_syntaxFactory.UsingDirective(SyntaxFactory.MakeToken(SyntaxKind.UsingKeyword),
					SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword),
					null,
					//GenerateGlobalQualifiedNameFromList(_options.ModuleName,GlobalClassName),
                    _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(GlobalClassName)),
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
			//}

			// Add: using System
            bool alreadyUsingSystem = false;
            for(int i=0; i<GlobalEntities.Usings.Count; i++) {
                if (CaseInsensitiveComparison.Compare(GlobalEntities.Usings[i].Name.ToString(),"System") == 0)
                    alreadyUsingSystem = true;
            }
            if (!alreadyUsingSystem) {
			    GlobalEntities.Usings.Add(_syntaxFactory.UsingDirective(SyntaxFactory.MakeToken(SyntaxKind.UsingKeyword), 
                    null,
                    null,
                    _syntaxFactory.IdentifierName(SyntaxFactory.Identifier("System")),
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
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
            if (ch is XSharpParser.FunctionContext || ch is XSharpParser.ProcedureContext || ch is XSharpParser.VoglobalContext
                 || ch is XSharpParser.VodefineContext || ch is XSharpParser.VodllContext) {
                if (ch.CsNode != null)
                    GlobalEntities.Members.Add(GenerateGlobalClass(GlobalClassName, ch.Get<MemberDeclarationSyntax>()));
            }
            else
                context.Put(ch.Get<CSharpSyntaxNode>());
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
            // RvdH May need to change to PUBLIC STATIC later. 
            // Const does not support unsafe types such as Ptr, but has the advantage
            // that it is in-lined when used
            // We can probably inspect the type and depending on the type switch between
            // public Const and public Static
            context.Put(_syntaxFactory.FieldDeclaration(
                EmptyList<AttributeListSyntax>(),
                TokenList(SyntaxKind.PublicKeyword, SyntaxKind.ConstKeyword),
                _syntaxFactory.VariableDeclaration(context.DataType.Get<TypeSyntax>(), variables),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            _pool.Free(variables);
        }

        public override void EnterInterface_([NotNull] XSharpParser.Interface_Context context)
        {
            ClassEntities.Push(CreateClassEntities());
        }

        public override void ExitInterface_([NotNull] XSharpParser.Interface_Context context)
        {
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            foreach(var mCtx in context._Members) {
                if (mCtx.CsNode != null)
                    members.Add(mCtx.Get<MemberDeclarationSyntax>());
            }
            var generated = ClassEntities.Pop();
            if(generated.Members.Count > 0) {
                members.AddRange(generated.Members);
            }
            if (generated.VoProperties != null) {
                foreach(var vop in generated.VoProperties.Values) {
                    members.Add(GenerateVoProperty(vop));
                }
            }


            generated.Free();
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            foreach(var pCtx in context._Parents) {
                if (baseTypes.Count>0)
                    baseTypes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                baseTypes.Add(_syntaxFactory.SimpleBaseType(pCtx.Get<TypeSyntax>()));
            }
            MemberDeclarationSyntax m = _syntaxFactory.InterfaceDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                keyword: SyntaxFactory.MakeToken(SyntaxKind.InterfaceKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MakeToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            _pool.Free(members);
            _pool.Free(baseTypes);
            if (context.Namespace != null) {
                m = _syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                    name: context.Namespace.Get<NameSyntax>(),
                    openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    externs: EmptyList<ExternAliasDirectiveSyntax>(),
                    usings: EmptyList<UsingDirectiveSyntax>(),
                    members: MakeList<MemberDeclarationSyntax>(m),
                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

            }
            context.Put(m);
        }

        public override void ExitInterfaceModifiers([NotNull] XSharpParser.InterfaceModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            modifiers.FixDefaultVisibility();
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void EnterClass_([NotNull] XSharpParser.Class_Context context)
        {
            ClassEntities.Push(CreateClassEntities());
        }

        public override void ExitClass_([NotNull] XSharpParser.Class_Context context)
        {
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            foreach(var mCtx in context._Members) {
                if (mCtx.CsNode != null)
                    members.Add(mCtx.Get<MemberDeclarationSyntax>());
            }
            var generated = ClassEntities.Pop();
            if(generated.Members.Count > 0) {
                members.AddRange(generated.Members);
            }
            if (generated.VoProperties != null) {
                foreach(var vop in generated.VoProperties.Values) {
                    members.Add(GenerateVoProperty(vop));
                }
            }
            generated.Free();
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            baseTypes.Add(_syntaxFactory.SimpleBaseType(context.BaseType?.Get<TypeSyntax>() 
                ?? _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.ObjectKeyword))));
            foreach(var iCtx in context._Implements) {
                baseTypes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                baseTypes.Add(_syntaxFactory.SimpleBaseType(iCtx.Get<TypeSyntax>()));
            }
            MemberDeclarationSyntax m = _syntaxFactory.ClassDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                keyword: SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MakeToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            _pool.Free(members);
            _pool.Free(baseTypes);
            if (context.Namespace != null) {
                m = _syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                    name: context.Namespace.Get<NameSyntax>(),
                    openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    externs: EmptyList<ExternAliasDirectiveSyntax>(),
                    usings: EmptyList<UsingDirectiveSyntax>(),
                    members: MakeList<MemberDeclarationSyntax>(m),
                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

            }
            context.Put(m);
        }

        public override void ExitClassModifiers([NotNull] XSharpParser.ClassModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            modifiers.FixDefaultVisibility();
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void EnterStructure_([NotNull] XSharpParser.Structure_Context context)
        {
            ClassEntities.Push(CreateClassEntities());
        }

        public override void ExitStructure_([NotNull] XSharpParser.Structure_Context context)
        {
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            foreach(var mCtx in context._Members) {
                if (mCtx.CsNode != null)
                    members.Add(mCtx.Get<MemberDeclarationSyntax>());
            }
            var generated = ClassEntities.Pop();
            if(generated.Members.Count > 0) {
                members.AddRange(generated.Members);
            }
            if (generated.VoProperties != null) {
                foreach(var vop in generated.VoProperties.Values) {
                    members.Add(GenerateVoProperty(vop));
                }
            }
            generated.Free();
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            foreach(var iCtx in context._Implements) {
                if (baseTypes.Count>0)
                    baseTypes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                baseTypes.Add(_syntaxFactory.SimpleBaseType(iCtx.Get<TypeSyntax>()));
            }
            MemberDeclarationSyntax m = _syntaxFactory.StructDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MakeToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            _pool.Free(members);
            _pool.Free(baseTypes);
            if (context.Namespace != null) {
                m = _syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                    name: context.Namespace.Get<NameSyntax>(),
                    openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    externs: EmptyList<ExternAliasDirectiveSyntax>(),
                    usings: EmptyList<UsingDirectiveSyntax>(),
                    members: MakeList<MemberDeclarationSyntax>(m),
                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

            }
            context.Put(m);
        }

        public override void ExitStructureModifiers([NotNull] XSharpParser.StructureModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            modifiers.FixDefaultVisibility();
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitDelegate_([NotNull] XSharpParser.Delegate_Context context)
        {
            MemberDeclarationSyntax m = _syntaxFactory.DelegateDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                delegateKeyword: SyntaxFactory.MakeToken(SyntaxKind.DelegateKeyword),
                returnType: context.Type.Get<TypeSyntax>(),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            if (context.Namespace != null) {
                m = _syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                    name: context.Namespace.Get<NameSyntax>(),
                    openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    externs: EmptyList<ExternAliasDirectiveSyntax>(),
                    usings: EmptyList<UsingDirectiveSyntax>(),
                    members: MakeList<MemberDeclarationSyntax>(m),
                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

            }
            context.Put(m);
        }

        public override void ExitDelegateModifiers([NotNull] XSharpParser.DelegateModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            modifiers.FixDefaultVisibility();
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitEnum_([NotNull] XSharpParser.Enum_Context context)
        {
            MemberDeclarationSyntax m = _syntaxFactory.EnumDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                enumKeyword: SyntaxFactory.MakeToken(SyntaxKind.EnumKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                baseList: default(BaseListSyntax),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: MakeSeparatedList<EnumMemberDeclarationSyntax>(context._Members),
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            if (context.Namespace != null) {
                m = _syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                    name: context.Namespace.Get<NameSyntax>(),
                    openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    externs: EmptyList<ExternAliasDirectiveSyntax>(),
                    usings: EmptyList<UsingDirectiveSyntax>(),
                    members: MakeList<MemberDeclarationSyntax>(m),
                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

            }
            context.Put(m);
        }

        public override void ExitEnumModifiers([NotNull] XSharpParser.EnumModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            modifiers.FixDefaultVisibility();
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitEnummember([NotNull] XSharpParser.EnummemberContext context)
        {
            context.Put(_syntaxFactory.EnumMemberDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                identifier: context.Id.Get<SyntaxToken>(),
                equalsValue: context.Expr == null ? null : _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    context.Expr.Get<ExpressionSyntax>())));
        }

        public override void ExitEvent_([NotNull] XSharpParser.Event_Context context)
        {
            if (context.ExplicitIface != null) {
                string evtFldName = EventFieldNamePrefix + context.Id.Get<SyntaxToken>();
                ClassEntities.Peek().Members.Add(
                    _syntaxFactory.FieldDeclaration(
                        EmptyList<AttributeListSyntax>(),
                        TokenList(SyntaxKind.StaticKeyword,SyntaxKind.InternalKeyword),
                        _syntaxFactory.VariableDeclaration(context.Type.Get<TypeSyntax>(), 
                            MakeSeparatedList(_syntaxFactory.VariableDeclarator(SyntaxFactory.Identifier(evtFldName), null, null))),
                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))
                    );
                var mods = context.Modifiers?.GetList<SyntaxToken>() ?? DefaultMethodModifiers(context.isInInterface());
                //if (context.ExplicitIface != null)
                {
                    var m = _pool.Allocate();
                    foreach (var mod in mods)
                    {
                        if (mod.Kind != SyntaxKind.VirtualKeyword && mod.Kind != SyntaxKind.OverrideKeyword && mod.Kind != SyntaxKind.PublicKeyword)
                            m.Add(mod);
                    }
                    mods = m.ToTokenList();
                    _pool.Free(m);
                }
                context.Put(_syntaxFactory.EventDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: mods,
                    eventKeyword: SyntaxFactory.MakeToken(SyntaxKind.EventKeyword),
                    type: context.Type.Get<TypeSyntax>(),
                    explicitInterfaceSpecifier: _syntaxFactory.ExplicitInterfaceSpecifier(
                        name: context.ExplicitIface.Get<NameSyntax>(),
                        dotToken: SyntaxFactory.MakeToken(SyntaxKind.DotToken)),
                    identifier: context.Id.Get<SyntaxToken>(),
                    accessorList: _syntaxFactory.AccessorList(
                        openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        accessors: MakeList(
                            _syntaxFactory.AccessorDeclaration(SyntaxKind.AddAccessorDeclaration,
                                attributeLists: EmptyList<AttributeListSyntax>(),
                                modifiers: EmptyList(),
                                keyword: SyntaxFactory.MakeToken(SyntaxKind.AddKeyword),
                                body: _syntaxFactory.Block(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                                    statements: _syntaxFactory.LockStatement(SyntaxFactory.MakeToken(SyntaxKind.LockKeyword),
                                        SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                        _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(evtFldName)),
                                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                                        _syntaxFactory.ExpressionStatement(
                                            _syntaxFactory.AssignmentExpression(SyntaxKind.AddAssignmentExpression,
                                                _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(evtFldName)),
                                                SyntaxFactory.MakeToken(SyntaxKind.PlusEqualsToken),
                                                _syntaxFactory.IdentifierName(SyntaxFactory.Identifier("value"))),
                                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))),
                                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)),
                                semicolonToken: null),
                            _syntaxFactory.AccessorDeclaration(SyntaxKind.RemoveAccessorDeclaration,
                                attributeLists: EmptyList<AttributeListSyntax>(),
                                modifiers: EmptyList(),
                                keyword: SyntaxFactory.MakeToken(SyntaxKind.RemoveKeyword),
                                body: _syntaxFactory.Block(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                                    statements: _syntaxFactory.LockStatement(SyntaxFactory.MakeToken(SyntaxKind.LockKeyword),
                                        SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                        _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(evtFldName)),
                                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                                        _syntaxFactory.ExpressionStatement(
                                            _syntaxFactory.AssignmentExpression(SyntaxKind.SubtractAssignmentExpression,
                                                _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(evtFldName)),
                                                SyntaxFactory.MakeToken(SyntaxKind.MinusEqualsToken),
                                                _syntaxFactory.IdentifierName(SyntaxFactory.Identifier("value"))),
                                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))),
                                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)),
                                semicolonToken: null)
                            ),
                        closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken))));
            }
            else {
                context.Put(_syntaxFactory.EventFieldDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? DefaultMethodModifiers(context.isInInterface()),
                    eventKeyword: SyntaxFactory.MakeToken(SyntaxKind.EventKeyword),
                    declaration: _syntaxFactory.VariableDeclaration(
                        context.Type.Get<TypeSyntax>(),
                        MakeSeparatedList<VariableDeclaratorSyntax>(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(),null, null))),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
        }

        public override void ExitEventModifiers([NotNull] XSharpParser.EventModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            if (!context.Parent.isInInterface())
            {
                modifiers.FixDefaultVisibility();
                if (_options.VirtualInstanceMethods)
                    modifiers.FixDefaultVirtual();
                else
                    modifiers.FixDefaultMethod();
            }
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitClassvars([NotNull] XSharpParser.ClassvarsContext context)
        {
            var varList = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            var varType = context.Vars.DataType.Get<TypeSyntax>();
            foreach (var varCtx in context.Vars._Var) {
                bool isDim = varCtx.Dim != null && varCtx.ArraySub != null;
                if (isDim) {
                    ClassEntities.Peek().Members.Add(_syntaxFactory.FieldDeclaration(
                        attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                        modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                        declaration: _syntaxFactory.VariableDeclaration(
                            type: _syntaxFactory.ArrayType(varType, MakeArrayRankSpeicifier(varCtx.ArraySub._ArrayIndex.Count)),
                            variables: MakeSeparatedList(varCtx.Get<VariableDeclaratorSyntax>())),
                        semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                }
                else {
                    if (varList.Count > 0)
                        varList.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    varList.Add(varCtx.Get<VariableDeclaratorSyntax>());
                }
            }
            if (varList.Count > 0) {
                context.Put(_syntaxFactory.FieldDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                    declaration: _syntaxFactory.VariableDeclaration(
                        type: varType,
                        variables: varList),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
            _pool.Free(varList);
        }

        public override void ExitClassvarModifiers([NotNull] XSharpParser.ClassvarModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            modifiers.FixDefaultVisibility();
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitClassVarList([NotNull] XSharpParser.ClassVarListContext context)
        {
            foreach(var cvCtx in context._Var)
                VisitClassvar(cvCtx);
        }

        public override void EnterClassvar([NotNull] XSharpParser.ClassvarContext context)
        {
            bool isDim = context.Dim != null;
            bool hasArraySub = context.ArraySub != null;
            if (isDim && !hasArraySub) {
                context.AddError(new ParseErrorData(context.DIM(), ErrorCode.ERR_ArrayInitializerExpected));
            }
            if (!isDim && hasArraySub) {
                context.ArraySub.AddError(new ParseErrorData(ErrorCode.ERR_FeatureNotAvailableInVersion1,"Indexed Class variable"));
            }
        }

        public override void ExitClassvar([NotNull] XSharpParser.ClassvarContext context)
        {
            // nvk: Not handled here due to datatype, which is processed later
        }

        public void VisitClassvar([NotNull] XSharpParser.ClassvarContext context)
        {
            bool isDim = context.Dim != null && context.ArraySub != null;
            var initExpr = context.Initializer?.Get<ExpressionSyntax>();
            if (isDim) {
                var varType = ((XSharpParser.ClassVarListContext)context.Parent).DataType.Get<TypeSyntax>();
                if (initExpr == null) {
                    initExpr = _syntaxFactory.ArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                        _syntaxFactory.ArrayType(varType,context.ArraySub.Get<ArrayRankSpecifierSyntax>()),
                        null);
                }
            }
            context.Put(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(),
                null,
                (initExpr == null) ? null : _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), initExpr)));
        }

        public override void ExitProperty([NotNull] XSharpParser.PropertyContext context)
        {
            var isInInterface = context.isInInterface();
            var isExtern = context.Modifiers?._EXTERN != null;
            var isAbstract = context.Modifiers?._ABSTRACT != null;
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
            if (isExtern) {
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
            if (isAbstract) {
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
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? DefaultMethodModifiers(isInInterface);
            if (context.ExplicitIface != null)
            {
                var m = _pool.Allocate();
                foreach (var mod in mods)
                {
                    if (mod.Kind != SyntaxKind.VirtualKeyword && mod.Kind != SyntaxKind.OverrideKeyword && mod.Kind != SyntaxKind.PublicKeyword)
                        m.Add(mod);
                }
                mods = m.ToTokenList();
                _pool.Free(m);
            }
            if (context.ParamList == null)
                context.Put(_syntaxFactory.PropertyDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: mods,
                    type: context.Type.Get<TypeSyntax>(),
                    explicitInterfaceSpecifier: context.ExplicitIface == null ? null : _syntaxFactory.ExplicitInterfaceSpecifier(
                        name: context.ExplicitIface.Get<NameSyntax>(),
                        dotToken: SyntaxFactory.MakeToken(SyntaxKind.DotToken)),
                    identifier: context.Id.Get<SyntaxToken>(),
                    accessorList: _syntaxFactory.AccessorList(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        (context.Auto != null) ? 
                            ((context._AutoAccessors?.Count ?? 0) > 0) ? MakeList<AccessorDeclarationSyntax>(context._AutoAccessors) :
                            MakeList(_syntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration,EmptyList<AttributeListSyntax>(),EmptyList(),
                                    SyntaxFactory.MakeToken(SyntaxKind.GetKeyword),null,SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)),
                                _syntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration,EmptyList<AttributeListSyntax>(),EmptyList(),
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
                    modifiers: mods,
                    type: context.Type.Get<TypeSyntax>(),
                    explicitInterfaceSpecifier: context.ExplicitIface == null ? null : _syntaxFactory.ExplicitInterfaceSpecifier(
                        name: context.ExplicitIface.Get<NameSyntax>(),
                        dotToken: SyntaxFactory.MakeToken(SyntaxKind.DotToken)),
                    thisKeyword: SyntaxFactory.MakeToken(SyntaxKind.ThisKeyword),
                    parameterList: context.ParamList.Get<BracketedParameterListSyntax>(),
                    accessorList: _syntaxFactory.AccessorList(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        (context.Auto != null) ? 
                            (context._AutoAccessors?.Count > 0) ? MakeList<AccessorDeclarationSyntax>(context._AutoAccessors) :
                            MakeList(_syntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration,EmptyList<AttributeListSyntax>(),EmptyList(),
                                    SyntaxFactory.MakeToken(SyntaxKind.GetKeyword),null,SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)),
                                _syntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration,EmptyList<AttributeListSyntax>(),EmptyList(),
                                    SyntaxFactory.MakeToken(SyntaxKind.SetKeyword),null,SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))) :
                        (context._LineAccessors?.Count > 0) ? MakeList<AccessorDeclarationSyntax>(context._LineAccessors) :
                        MakeList<AccessorDeclarationSyntax>(context._Accessors),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)),
                    expressionBody: null, // TODO: (grammar) expressionBody methods
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
            bool forceBody = false;
            if (context.Key.Type == XSharpParser.SET && context.ExprList == null)
            {
                var property = context.Parent as XSharpParser.PropertyContext;
                var isExtern = property.Modifiers?._EXTERN != null;
                var isAbstract = property.Modifiers?._ABSTRACT != null;
                if (!isExtern && !isAbstract && !property.isInInterface() && property._LineAccessors.Count > 1 &&
                    (property._LineAccessors[0].Expr != null || property._LineAccessors[1].Expr != null))
                {
                    forceBody = true;
                }
            }
            context.Put(_syntaxFactory.AccessorDeclaration(context.Key.AccessorKind(),
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList(),
                keyword: context.Key.SyntaxKeyword(),
                body: context.Key.Type == XSharpParser.GET ? 
                    ( context.Expr == null ? null : _syntaxFactory.Block(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        MakeList<StatementSyntax>(_syntaxFactory.ReturnStatement(SyntaxFactory.MakeToken(SyntaxKind.ReturnKeyword),
                            context.Expr.Get<ExpressionSyntax>(),SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)) )
                    : (context.ExprList == null && !forceBody) ? null 
                    : _syntaxFactory.Block(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        context.ExprList?.GetList<StatementSyntax>() ?? EmptyList<StatementSyntax>(),
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
            var idName = context.Id.Get<SyntaxToken>();
            var isInInterface = context.isInInterface();
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? DefaultMethodModifiers(isInInterface);
            var isExtern = mods.Any(SyntaxKind.ExternKeyword);
            var isAbstract = mods.Any(SyntaxKind.AbstractKeyword);
            var hasNoBody = isInInterface || isExtern || isAbstract;
            if (isInInterface && context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0) {
                context.AddError(new ParseErrorData(context.Id, ErrorCode.ERR_InterfaceMemberHasBody));
            }
            if (isInInterface && context.ClassId != null) {
                context.AddError(new ParseErrorData(context.ClassId, ErrorCode.ERR_InterfacesCannotContainTypes));
            }
            if (isAbstract) {
                if (isExtern) {
                    context.AddError(new ParseErrorData(context.Modifiers, ErrorCode.ERR_AbstractAndExtern));
                }
                if (context.StmtBlk?._Stmts?.Count > 0) {
                    context.AddError(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_AbstractHasBody));
                }
                context.StmtBlk = null;
            }
            else if (isExtern) {
                if (context.StmtBlk?._Stmts?.Count > 0) {
                    context.AddError(new ParseErrorData(context.StmtBlk, ErrorCode.ERR_ExternHasBody));
                }
                context.StmtBlk = null;
            }
            bool actualDeclaration = true;
            if (context.T.Token.Type != XSharpParser.METHOD) {
                switch (context.T.Token.Type) {
                    case XSharpParser.ACCESS:
                        idName = SyntaxFactory.Identifier(VoPropertyAccessPrefix + context.Id.GetText());
                        idName.XNode = context.Id;
                        break;
                    case XSharpParser.ASSIGN:
                        idName = SyntaxFactory.Identifier(VoPropertyAssignPrefix + context.Id.GetText());
                        idName.XNode = context.Id;
                        break;
                }
                var vomods = _pool.Allocate();
                vomods.Add(SyntaxFactory.MakeToken(SyntaxKind.PrivateKeyword));
                if (mods.Any(SyntaxKind.StaticKeyword))
                    vomods.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
                if (mods.Any(SyntaxKind.UnsafeKeyword))
                    vomods.Add(SyntaxFactory.MakeToken(SyntaxKind.UnsafeKeyword));
                if (hasNoBody)
                    actualDeclaration = false;
                mods = vomods.ToTokenList();
                _pool.Free(vomods);
            }
            else
            {
                if (context.ParamList?._Params.Count > 0 && context.ParamList?._Params[0].Self != null && !mods.Any(SyntaxKind.StaticKeyword))
                {
                    var m = _pool.Allocate();
                    m.AddRange(mods);
                    m.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
                    mods = m.ToTokenList();
                    _pool.Free(m);
                }
            }
            if (context.ExplicitIface != null)
            {
                var m = _pool.Allocate();
                foreach(var mod in mods)
                {
                    if (mod.Kind != SyntaxKind.VirtualKeyword && mod.Kind != SyntaxKind.OverrideKeyword && mod.Kind != SyntaxKind.PublicKeyword)
                        m.Add(mod);
                }
                mods = m.ToTokenList();
                _pool.Free(m);
            }
            if (actualDeclaration) {
                MemberDeclarationSyntax m = _syntaxFactory.MethodDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: mods,
                    returnType: context.Type?.Get<TypeSyntax>() ?? (context.T.Token.Type == XSharpParser.ASSIGN ? VoidType() : MissingType()),
                    explicitInterfaceSpecifier: context.ExplicitIface == null ? null : _syntaxFactory.ExplicitInterfaceSpecifier(
                        name: context.ExplicitIface.Get<NameSyntax>(),
                        dotToken: SyntaxFactory.MakeToken(SyntaxKind.DotToken)),
                    identifier: idName,
                    typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                    parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                    constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                    body: hasNoBody ? null : context.StmtBlk?.Get<BlockSyntax>(),
                    expressionBody: null, // TODO: (grammar) expressionBody methods
                    semicolonToken: (!hasNoBody && context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                if (context.ClassId != null) {
                    m = _syntaxFactory.ClassDeclaration(
                        attributeLists: EmptyList<AttributeListSyntax>(),
                        modifiers: TokenList(SyntaxKind.PartialKeyword),
                        keyword: SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword),
                        identifier: context.ClassId.Get<SyntaxToken>(),
                        typeParameterList: default(TypeParameterListSyntax),
                        baseList: default(BaseListSyntax),
                        constraintClauses: default(SyntaxList<TypeParameterConstraintClauseSyntax>),
                        openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        members: MakeList<MemberDeclarationSyntax>(m),
                        closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                        semicolonToken: null);
                    if (context.Namespace != null) {
                        m = _syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                            name: context.Namespace.Get<NameSyntax>(),
                            openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                            externs: EmptyList<ExternAliasDirectiveSyntax>(),
                            usings: EmptyList<UsingDirectiveSyntax>(),
                            members: MakeList<MemberDeclarationSyntax>(m),
                            closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                            semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

                    }
                }
                context.Put(m);
            }
            if (context.T.Token.Type != XSharpParser.METHOD) {
                ClassEntities.Peek().AddVoPropertyAccessor(context);
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
                varianceKeyword: context.VarianceKeyword?.SyntaxKeyword(),
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
            // nvk: Handled by the method rule
        }

        public override void ExitConstructorModifiers([NotNull] XSharpParser.ConstructorModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            if (!modifiers.Any(SyntaxKind.StaticKeyword))
                modifiers.FixDefaultVisibility();
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
            if (!context.Parent.isInInterface())
            {
                modifiers.FixDefaultVisibility();
                if (_options.VirtualInstanceMethods)
                    modifiers.FixDefaultVirtual();
                else
                    modifiers.FixDefaultMethod();
            }
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
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(false, SyntaxKind.StaticKeyword),
                    implicitOrExplicitKeyword: context.Conversion.Get<SyntaxToken>(),
                    operatorKeyword: SyntaxFactory.MakeToken(SyntaxKind.OperatorKeyword),
                    type: context.Type?.Get<TypeSyntax>() ?? MissingType(),
                    parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                    body: context.StmtBlk.Get<BlockSyntax>(),
                    expressionBody: null, // TODO: (grammar) expressionBody methods
                    semicolonToken: (context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            else
                context.Put(_syntaxFactory.OperatorDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(false, SyntaxKind.StaticKeyword),
                    returnType: context.Type?.Get<TypeSyntax>() ?? MissingType(),
                    operatorKeyword: SyntaxFactory.MakeToken(SyntaxKind.OperatorKeyword),
                    operatorToken: context.Operation.Get<SyntaxToken>(),
                    parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                    body: context.StmtBlk.Get<BlockSyntax>(),
                    expressionBody: null, // TODO: (grammar) expressionBody methods
                    semicolonToken: (context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitOperatorModifiers([NotNull] XSharpParser.OperatorModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            if (!modifiers.Any(SyntaxKind.StaticKeyword))
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
            modifiers.FixDefaultVisibility();
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
            if (context.Member.CsNode != null)
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
                var parentId = (context.Parent as XSharpParser.Class_Context)?.Id.Get<SyntaxToken>()
                    ?? (context.Parent as XSharpParser.Structure_Context)?.Id.Get<SyntaxToken>()
                    ?? (context.Parent as XSharpParser.Interface_Context)?.Id.Get<SyntaxToken>();
                context.Put(_syntaxFactory.ConstructorDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                    identifier: parentId,
                    parameterList: context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
                    initializer: context.Chain == null ? null : 
                        _syntaxFactory.ConstructorInitializer(context.Chain.CtorInitializerKind(),
                            SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                            context.Chain.SyntaxKeyword(), 
                            context.ArgList?.Get<ArgumentListSyntax>() ?? EmptyArgumentList()),
                    body: context.StmtBlk?.Get<BlockSyntax>(),
                    semicolonToken: (context.StmtBlk?._Stmts?.Count > 0) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
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
                var parentId = (context.Parent as XSharpParser.Class_Context)?.Id.Get<SyntaxToken>()
                    ?? (context.Parent as XSharpParser.Structure_Context)?.Id.Get<SyntaxToken>()
                    ?? (context.Parent as XSharpParser.Interface_Context)?.Id.Get<SyntaxToken>();
                context.Put(_syntaxFactory.DestructorDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? EmptyList<SyntaxToken>(),
                    tildeToken: SyntaxFactory.MakeToken(SyntaxKind.TildeToken),
                    identifier: parentId,
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
            else if (context.Member.CsNode != null)
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
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
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
                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                context.Target?.Get<AttributeTargetSpecifierSyntax>(),
                attributes,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
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
            if (context._Params != null) {
                foreach (var paramCtx in context._Params) {
                    if (arguments.Count != 0) {
                        arguments.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    }
                    arguments.Add(paramCtx.Get<AttributeArgumentSyntax>());
                }
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
                null, // TODO: (grammar) name: attr arg syntax?
                context.Expr.Get<ExpressionSyntax>()));
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
                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                context.Target.Get<AttributeTargetSpecifierSyntax>(),
                attributes,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
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
                    context.Modifiers = FixPosition(new XSharpParser.FuncprocModifiersContext(context,0),context.Start);
                    context.Modifiers.PutList(TokenList(SyntaxKind.ConstKeyword,SyntaxKind.StaticKeyword,SyntaxKind.PublicKeyword));
                }
            }
        }

        public override void ExitVoglobal([NotNull] XSharpParser.VoglobalContext context)
        {
            var varList = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            var varType = context.Vars.DataType?.Get<TypeSyntax>() ?? MissingType();
            foreach (var varCtx in context.Vars._Var) {
                bool isDim = varCtx.Dim != null && varCtx.ArraySub != null;
                if (isDim) {
                    GlobalClassEntities.Members.Add(_syntaxFactory.FieldDeclaration(
                        attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                        modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(false, SyntaxKind.StaticKeyword),
                        declaration: _syntaxFactory.VariableDeclaration(
                            type: _syntaxFactory.ArrayType(varType, MakeArrayRankSpeicifier(varCtx.ArraySub._ArrayIndex.Count)),
                            variables: MakeSeparatedList(varCtx.Get<VariableDeclaratorSyntax>())),
                        semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                }
                else {
                    if (varList.Count > 0)
                        varList.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    varList.Add(varCtx.Get<VariableDeclaratorSyntax>());
                }
            }
            if (varList.Count > 0) {
                context.Put(_syntaxFactory.FieldDeclaration(
                    attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(false, SyntaxKind.StaticKeyword),
                    declaration: _syntaxFactory.VariableDeclaration(
                        type: varType,
                        variables: varList),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
            _pool.Free(varList);
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

        public override void EnterVodll([NotNull] XSharpParser.VodllContext context)
        {
            if (context.Modifiers != null) {
                context.Modifiers._Tokens.Add(_parser.TokenFactory.Create(XSharpParser.EXTERN,""));
            }
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
                                name: GenerateQualifiedName("global::System.Runtime.InteropServices.DllImport"),
                                argumentList: _syntaxFactory.AttributeArgumentList(
                                    openParenToken: SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                    arguments: MakeSeparatedList(
                                        _syntaxFactory.AttributeArgument(null,null,context.Dll.Get<ExpressionSyntax>()),
                                        context.Entrypoint != null ? _syntaxFactory.AttributeArgument(GenerateNameEquals("EntryPoint"),null,context.Entrypoint.Get<ExpressionSyntax>())
                                            : context.Ordinal != null ? _syntaxFactory.AttributeArgument(GenerateNameEquals("EntryPoint"), null,
                                                    _syntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, context.Ordinal.SyntaxLiteralValue()))
                                            : null,
                                        context.CharSet != null ? _syntaxFactory.AttributeArgument(GenerateNameEquals("Charset"), null,
                                                _syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, GenerateQualifiedName("global::System.Runtime.InteropServices.CharSet"), 
                                                    SyntaxFactory.MakeToken(SyntaxKind.DotToken), _syntaxFactory.IdentifierName(context.CharSet.SyntaxIdentifier())))
                                            : null,
                                        context.CallingConvention?.Get<AttributeArgumentSyntax>()
                                    ),
                                    closeParenToken: SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))
                                )
                            ),
                        closeBracketToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken))
                    ),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(false, SyntaxKind.StaticKeyword,SyntaxKind.ExternKeyword),
                returnType: context.Type?.Get<TypeSyntax>() ?? (context.T.Type == XSharpParser.FUNCTION ? MissingType() : VoidType()),
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
                    conv = "global::System.Runtime.InteropServices.CallingConvention.StdCall";
                    break;
                case XSharpParser.THISCALL:
                    conv = "global::System.Runtime.InteropServices.CallingConvention.ThisCall";
                    break;
                case XSharpParser.FASTCALL:
                    conv = "global::System.Runtime.InteropServices.CallingConvention.Cdecl";
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
            MemberDeclarationSyntax m = _syntaxFactory.StructDeclaration(
                attributeLists: MakeList(
                    _syntaxFactory.AttributeList(
                        openBracketToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                        target: null,
                        attributes: MakeSeparatedList(
                            _syntaxFactory.Attribute(
                                name: GenerateQualifiedName("global::System.Runtime.InteropServices.StructLayout"),
                                argumentList: _syntaxFactory.AttributeArgumentList(
                                    openParenToken: SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                    arguments: MakeSeparatedList(
                                        _syntaxFactory.AttributeArgument(null,null,GenerateQualifiedName("global::System.Runtime.InteropServices.LayoutKind.Sequential")),
                                        _syntaxFactory.AttributeArgument(GenerateNameEquals("Pack"),null,
                                            context.Alignment == null ?
                                                _syntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(null, "8", 8, null))
                                                : _syntaxFactory.LiteralExpression(context.Alignment.ExpressionKindLiteral(), context.Alignment.SyntaxLiteralValue()))
                                    ),
                                    closeParenToken: SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))
                                )
                            ),
                        closeBracketToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken))
                    ),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: null,
                constraintClauses: null,
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: (context._Members?.Count > 0) ? MakeList<MemberDeclarationSyntax>(context._Members) : EmptyList<MemberDeclarationSyntax>(),
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null );
            if (context.Namespace != null) {
                m = _syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                    name: context.Namespace.Get<NameSyntax>(),
                    openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    externs: EmptyList<ExternAliasDirectiveSyntax>(),
                    usings: EmptyList<UsingDirectiveSyntax>(),
                    members: MakeList<MemberDeclarationSyntax>(m),
                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

            }
            context.Put(m);
        }

        public override void ExitVostructmember([NotNull] XSharpParser.VostructmemberContext context)
        {
            bool isDim = context.Dim != null;
            var varType = context.DataType.Get<TypeSyntax>();
            if (isDim) {
                varType = _syntaxFactory.ArrayType(varType, MakeArrayRankSpeicifier(context.ArraySub._ArrayIndex.Count));
            }
            context.Put(_syntaxFactory.FieldDeclaration(
                EmptyList<AttributeListSyntax>(),
                TokenList(SyntaxKind.PublicKeyword),
                _syntaxFactory.VariableDeclaration(varType, 
                    MakeSeparatedList(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(), null, null))),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitVounion([NotNull] XSharpParser.VounionContext context)
        {
            MemberDeclarationSyntax m = _syntaxFactory.StructDeclaration(
                attributeLists: MakeList(
                    _syntaxFactory.AttributeList(
                        openBracketToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                        target: null,
                        attributes: MakeSeparatedList(
                            _syntaxFactory.Attribute(
                                name: GenerateQualifiedName("global::System.Runtime.InteropServices.StructLayout"),
                                argumentList: _syntaxFactory.AttributeArgumentList(
                                    openParenToken: SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                    arguments: MakeSeparatedList(
                                        _syntaxFactory.AttributeArgument(null,null,GenerateQualifiedName("global::System.Runtime.InteropServices.LayoutKind.Explicit"))
                                    ),
                                    closeParenToken: SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))
                                )
                            ),
                        closeBracketToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken))
                    ),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: null,
                constraintClauses: null,
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: (context._Members?.Count > 0) ? MakeList<MemberDeclarationSyntax>(context._Members) : EmptyList<MemberDeclarationSyntax>(),
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            if (context.Namespace != null) {
                m = _syntaxFactory.NamespaceDeclaration(SyntaxFactory.MakeToken(SyntaxKind.NamespaceKeyword),
                    name: context.Namespace.Get<NameSyntax>(),
                    openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    externs: EmptyList<ExternAliasDirectiveSyntax>(),
                    usings: EmptyList<UsingDirectiveSyntax>(),
                    members: MakeList<MemberDeclarationSyntax>(m),
                    closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));

            }
            context.Put(m);
        }

        public override void ExitVounionmember([NotNull] XSharpParser.VounionmemberContext context)
        {
            bool isDim = context.Dim != null;
            var varType = context.DataType.Get<TypeSyntax>();
            if (isDim) {
                varType = _syntaxFactory.ArrayType(varType, MakeArrayRankSpeicifier(context.ArraySub._ArrayIndex.Count));
            }
            context.Put(_syntaxFactory.FieldDeclaration(
                MakeList(
                    _syntaxFactory.AttributeList(
                        openBracketToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                        target: null,
                        attributes: MakeSeparatedList(
                            _syntaxFactory.Attribute(
                                name: GenerateQualifiedName("global::System.Runtime.InteropServices.FieldOffset"),
                                argumentList: _syntaxFactory.AttributeArgumentList(
                                    openParenToken: SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                    arguments: MakeSeparatedList(
                                        _syntaxFactory.AttributeArgument(null,null,
                                            _syntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(null,"0",0,null))
                                        )
                                    ),
                                    closeParenToken: SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))
                                )
                            ),
                        closeBracketToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken))
                    ),
                TokenList(SyntaxKind.PublicKeyword),
                _syntaxFactory.VariableDeclaration(varType, 
                    MakeSeparatedList(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(), null, null))),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitArraysub([NotNull] XSharpParser.ArraysubContext context)
        {
            context.Put(_syntaxFactory.ArrayRankSpecifier(
                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                MakeSeparatedList<ExpressionSyntax>(context._ArrayIndex),
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
        }

        public override void ExitFunction([NotNull] XSharpParser.FunctionContext context)
        {
            var isInInterface = context.isInInterface();
            if (isInInterface && context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0) {
                context.AddError(new ParseErrorData(context.Id, ErrorCode.ERR_InterfaceMemberHasBody));
            }
            context.Put(_syntaxFactory.MethodDeclaration(
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(isInInterface, SyntaxKind.StaticKeyword),
                returnType: context.Type?.Get<TypeSyntax>() ?? MissingType(),
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
                modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(isInInterface, SyntaxKind.StaticKeyword),
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
            // TODO nvk (calling convention is silently ignored for now)
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

        public override void EnterParameter([NotNull] XSharpParser.ParameterContext context)
        {
            if (context.Self != null)
            {
                context.Modifiers._Tokens.Add(context.Self);
            }
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
            modifiers.FixDefaultVisibility();
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
            modifiers.FixDefaultVisibility();
            context.PutList(modifiers.ToTokenList());
            _pool.Free(modifiers);
        }

        public override void ExitVotypeModifiers([NotNull] XSharpParser.VotypeModifiersContext context)
        {
            SyntaxListBuilder modifiers = _pool.Allocate();
            foreach (var m in context._Tokens)
            {
                modifiers.AddCheckUnique(m.SyntaxKeyword());
            }
            modifiers.FixDefaultVisibility();
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
                if (stmtCtx.CsNode is SyntaxList<StatementSyntax>)
                    statements.AddRange(stmtCtx.GetList<StatementSyntax>());
                else
                    statements.Add(stmtCtx.Get<StatementSyntax>());
            }
            context.Put(_syntaxFactory.Block(openBrace, statements, closeBrace));
            _pool.Free(statements);
        }

        public override void ExitDeclarationStmt([NotNull] XSharpParser.DeclarationStmtContext context)
        {
            context.PutList(context.Decl.GetList<StatementSyntax>());
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
            }
        }

        public override void ExitCommonLocalDecl([NotNull] XSharpParser.CommonLocalDeclContext context)
        {
            foreach(var lvCtx in context._LocalVars)
                VisitLocalvar(lvCtx);
            context.PutList(MakeList<StatementSyntax>(context._LocalVars));
        }

        public override void ExitVarLocalDecl([NotNull] XSharpParser.VarLocalDeclContext context)
        {
            context.PutList(MakeList<StatementSyntax>(context._ImpliedVars));
        }

        public override void EnterLocalvar([NotNull] XSharpParser.LocalvarContext context)
        {
            bool isDim = context.Dim != null;
            bool hasArraySub = context.ArraySub != null;
            if (isDim && !hasArraySub) {
                context.AddError(new ParseErrorData(context.DIM(), ErrorCode.ERR_ArrayInitializerExpected));
            }
            if (!isDim && hasArraySub) {
                context.ArraySub.AddError(new ParseErrorData(ErrorCode.ERR_FeatureNotAvailableInVersion1,"Indexed Local"));
            }
        }

        public override void ExitLocalvar([NotNull] XSharpParser.LocalvarContext context)
        {
            // nvk: Do nothing here. It will be handled by the visitor after Datatype(s) are processed.
        }

        private void VisitLocalvar([NotNull] XSharpParser.LocalvarContext context)
        {
            bool isConst = context.Const != null;
            bool isStatic = (context.Parent as XSharpParser.CommonLocalDeclContext).Static != null;
            bool isDim = context.Dim != null && context.ArraySub != null;
            string staticName = null;
            var varType = context.DataType?.Get<TypeSyntax>() ?? MissingType();
            var initExpr = context.Expression?.Get<ExpressionSyntax>();
            if (isDim) {
                if (initExpr == null) {
                    initExpr = _syntaxFactory.ArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                        _syntaxFactory.ArrayType(varType,context.ArraySub.Get<ArrayRankSpecifierSyntax>()),
                        null);
                }
                varType = _syntaxFactory.ArrayType(varType, MakeArrayRankSpeicifier(context.ArraySub._ArrayIndex.Count));
            }
            if (isStatic) {
                staticName = StaticLocalFieldNamePrefix+context.Id.Get<SyntaxToken>().Text+UniqueNameSuffix;
                ClassEntities.Peek().Members.Add(
                    _syntaxFactory.FieldDeclaration(
                        EmptyList<AttributeListSyntax>(),
                        TokenList(SyntaxKind.StaticKeyword,SyntaxKind.PrivateKeyword),
                        _syntaxFactory.VariableDeclaration(varType, 
                            MakeSeparatedList(_syntaxFactory.VariableDeclarator(SyntaxFactory.Identifier(staticName), null, null))),
                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))
                    );
                if (initExpr != null) {
                    ClassEntities.Peek().Members.Add(
                        _syntaxFactory.FieldDeclaration(
                            EmptyList<AttributeListSyntax>(),
                            TokenList(SyntaxKind.StaticKeyword,SyntaxKind.PrivateKeyword),
                            _syntaxFactory.VariableDeclaration(_syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.BoolKeyword)), 
                                MakeSeparatedList(_syntaxFactory.VariableDeclarator(SyntaxFactory.Identifier(staticName+StaticLocalInitFieldNameSuffix), null, 
                                    _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), 
                                        _syntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression, SyntaxFactory.MakeToken(SyntaxKind.TrueKeyword)))))),
                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))
                        );
                    ClassEntities.Peek().Members.Add(
                        _syntaxFactory.FieldDeclaration(
                            EmptyList<AttributeListSyntax>(),
                            TokenList(SyntaxKind.StaticKeyword,SyntaxKind.PrivateKeyword),
                            _syntaxFactory.VariableDeclaration(_syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.ObjectKeyword)), 
                                MakeSeparatedList(_syntaxFactory.VariableDeclarator(SyntaxFactory.Identifier(staticName+StaticLocalLockFieldNameSuffix), null, 
                                    _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), 
                                        _syntaxFactory.ObjectCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                                            _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.ObjectKeyword)),
                                            EmptyArgumentList(), null))))),
                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))
                        );
                }
            }
            var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            variables.Add(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(), null,
                isStatic ? _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(staticName)))
                : (initExpr == null) ? null : _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), initExpr)));
            var modifiers = _pool.Allocate();
            if (isConst)
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.ConstKeyword));
            if (isStatic)
                modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.RefKeyword));
            if (!isStatic) {
                context.Put(_syntaxFactory.LocalDeclarationStatement(
                    modifiers.ToTokenList(),
                    _syntaxFactory.VariableDeclaration(varType, variables),
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
            else {
                var decl = _pool.Allocate<StatementSyntax>();
                decl.Add(_syntaxFactory.LocalDeclarationStatement(
                    modifiers.ToTokenList(),
                    _syntaxFactory.VariableDeclaration(varType, variables),
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                if (initExpr != null) {
                    decl.Add(_syntaxFactory.IfStatement(SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                        SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                        _syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier(staticName+StaticLocalInitFieldNameSuffix)),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                        _syntaxFactory.LockStatement(SyntaxFactory.MakeToken(SyntaxKind.LockKeyword),
                            SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                            _syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier(staticName+StaticLocalLockFieldNameSuffix)),
                            SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                            _syntaxFactory.IfStatement(SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                _syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier(staticName+StaticLocalInitFieldNameSuffix)),
                                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                                _syntaxFactory.Block(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                                    MakeList<StatementSyntax>(
                                        _syntaxFactory.ExpressionStatement(
                                            _syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                                                _syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier(staticName)),
                                                SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                                                initExpr),
                                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)),
                                        _syntaxFactory.ExpressionStatement(
                                            _syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                                                _syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier(staticName+StaticLocalInitFieldNameSuffix)),
                                                SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                                                _syntaxFactory.LiteralExpression(SyntaxKind.FalseLiteralExpression, SyntaxFactory.MakeToken(SyntaxKind.FalseKeyword))),
                                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))
                                        ),
                                    SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)),
                                null)),
                        null));
                }
                context.PutList<StatementSyntax>(decl);
                _pool.Free(decl);
            }
            _pool.Free(variables);
            _pool.Free(modifiers);
        }

        public override void ExitImpliedvar([NotNull] XSharpParser.ImpliedvarContext context)
        {
            bool isConst = context.Const != null;
            bool isStatic = (context.Parent as XSharpParser.VarLocalDeclContext).Static != null;
            var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            variables.Add(_syntaxFactory.VariableDeclarator(context.Id.Get<SyntaxToken>(),null,
                (context.Expression == null) ? null :
                _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), context.Expression.Get<ExpressionSyntax>())));
            var modifiers = _pool.Allocate();
            if (isConst)
                context.AddError(new ParseErrorData(ErrorCode.ERR_ImplicitlyTypedVariableCannotBeConst));
            if (isStatic)
                context.AddError(new ParseErrorData(ErrorCode.ERR_BadVarDecl));
            context.Put(_syntaxFactory.LocalDeclarationStatement(
                modifiers.ToTokenList(),
                _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier(ImpliedTypeName)), variables),
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
                WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_FeatureNotAvailableInVersion1,  context.T.Text+" statement" )));
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
                _syntaxFactory.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, SyntaxFactory.MakeToken(SyntaxKind.ExclamationToken),
                    context.Expr.Get<ExpressionSyntax>()),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitForStmt([NotNull] XSharpParser.ForStmtContext context)
        {
            object blockStmts = null;
            ExpressionSyntax assignExpr, whileExpr, incrExpr, iterExpr, initExpr;
            if (context.AssignExpr != null)
            {
                if (!(context.AssignExpr is XSharpParser.AssignmentExpressionContext))
                {
                    context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                    context.AddError(new ParseErrorData(context.Dir, ErrorCode.ERR_SyntaxError, ":="));
                    return;
                }
                if ((context.AssignExpr as XSharpParser.AssignmentExpressionContext).Op.Type != XSharpParser.ASSIGN_OP)
                {
                    context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                    context.AddError(new ParseErrorData((context.AssignExpr as XSharpParser.AssignmentExpressionContext).Op, ErrorCode.ERR_SyntaxError, ":="));
                    return;
                }
                iterExpr = (context.AssignExpr as XSharpParser.AssignmentExpressionContext).Left.Get<ExpressionSyntax>();
                initExpr = (context.AssignExpr as XSharpParser.AssignmentExpressionContext).Right.Get<ExpressionSyntax>();
                assignExpr = context.AssignExpr.Get<ExpressionSyntax>();
            }
            else
            {
                iterExpr = _syntaxFactory.IdentifierName(context.ForIter.Get<SyntaxToken>());
                initExpr = context.Expr.Get<ExpressionSyntax>();
                assignExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                    iterExpr,
                    SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                    initExpr);
            }
            if (context.Step == null) {
                context.Step = FixPosition(new XSharpParser.PrimaryExpressionContext(FixPosition(new XSharpParser.ExpressionContext(),context.Stop)),context.Stop);
                context.Step.Put(_syntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(null,"1",1,null)));
            }
            switch (context.Dir.Type) {
                case XSharpParser.UPTO:
                    whileExpr = _syntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression,
                        iterExpr,
                        SyntaxFactory.MakeToken(SyntaxKind.LessThanEqualsToken),
                        context.FinalExpr.Get<ExpressionSyntax>());
                    incrExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.AddAssignmentExpression,
                        iterExpr,
                        SyntaxFactory.MakeToken(SyntaxKind.PlusEqualsToken),
                        context.Step.Get<ExpressionSyntax>());
                    break;
                case XSharpParser.DOWNTO:
                    whileExpr = _syntaxFactory.BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression,
                        iterExpr,
                        SyntaxFactory.MakeToken(SyntaxKind.GreaterThanEqualsToken),
                        context.FinalExpr.Get<ExpressionSyntax>());
                    incrExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.SubtractAssignmentExpression,
                        iterExpr,
                        SyntaxFactory.MakeToken(SyntaxKind.MinusEqualsToken),
                        context.Step.Get<ExpressionSyntax>());
                    break;
                case XSharpParser.TO:
                default:
                    var startToken = SyntaxFactory.Identifier(ForStartNamePrefix+context.Dir.StartIndex);
                    var endToken = SyntaxFactory.Identifier(ForEndNamePrefix+context.Dir.StartIndex);
                    var indToken = SyntaxFactory.Identifier(ForIndNamePrefix+context.Dir.StartIndex);
                    var stmts = _pool.Allocate<StatementSyntax>();
                    blockStmts = stmts;
                    {
                        var variables = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
                        variables.Add(_syntaxFactory.VariableDeclarator(startToken,null,
                            _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                                initExpr)));
                        var modifiers = _pool.Allocate();
                        stmts.Add(_syntaxFactory.LocalDeclarationStatement(
                            modifiers.ToTokenList(),
                            _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier(ImpliedTypeName)), variables),
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
                            _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier(ImpliedTypeName)), variables),
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
                            _syntaxFactory.VariableDeclaration(_syntaxFactory.IdentifierName(SyntaxFactory.Identifier(ImpliedTypeName)), variables),
                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
                        _pool.Free(variables);
                        _pool.Free(modifiers);
                    }
                    whileExpr = _syntaxFactory.ConditionalExpression(
                        _syntaxFactory.IdentifierName(indToken),
                        SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                        _syntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression,
                            iterExpr,
                            SyntaxFactory.MakeToken(SyntaxKind.LessThanEqualsToken),
                            _syntaxFactory.IdentifierName(endToken)),
                        SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                        _syntaxFactory.BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression,
                            iterExpr,
                            SyntaxFactory.MakeToken(SyntaxKind.GreaterThanEqualsToken),
                            _syntaxFactory.IdentifierName(endToken)));
                    incrExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.AddAssignmentExpression,
                        iterExpr,
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
            var decl = default(VariableDeclarationSyntax);
            var init = _pool.AllocateSeparated<ExpressionSyntax>();
            if (context.ForDecl != null)
            {
                decl = _syntaxFactory.VariableDeclaration(
                    context.Type?.Get<TypeSyntax>() ?? _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(ImpliedTypeName)),
                    MakeSeparatedList(_syntaxFactory.VariableDeclarator(
                        context.ForIter.Get<SyntaxToken>(),
                        null,
                        _syntaxFactory.EqualsValueClause(SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), initExpr))));
            }
            else
            {
                init.Add(assignExpr);
            }
            var incr = _pool.AllocateSeparated<ExpressionSyntax>();
            incr.Add(incrExpr);
            var forStmt = _syntaxFactory.ForStatement(SyntaxFactory.MakeToken(SyntaxKind.ForKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                decl,
                init,
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken),
                whileExpr,
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken),
                incr,
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                context.StmtBlk.Get<BlockSyntax>());
            _pool.Free(init);
            _pool.Free(incr);
            if (blockStmts == null)
                context.Put(forStmt);
            else {
                var stmts = (SyntaxListBuilder<StatementSyntax>)blockStmts;
                stmts.Add(forStmt);
                context.Put(_syntaxFactory.Block(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    stmts,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)));
                _pool.Free(stmts);
            }
        }

        public override void ExitForeachStmt([NotNull] XSharpParser.ForeachStmtContext context)
        {
            context.Put(_syntaxFactory.ForEachStatement(SyntaxFactory.MakeToken(SyntaxKind.ForEachKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                context.Type?.Get<TypeSyntax>() ?? _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(ImpliedTypeName)),
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
            // TODO: sequence/break/recover are not supported yet
            context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)).
                WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_FeatureNotAvailableInVersion1, "BREAK statement")));
        }

        public override void ExitThrowStmt([NotNull] XSharpParser.ThrowStmtContext context)
        {
            context.Put(_syntaxFactory.ThrowStatement(SyntaxFactory.MakeToken(SyntaxKind.ThrowKeyword),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void ExitTryStmt([NotNull] XSharpParser.TryStmtContext context)
        {
            if (!(context._CatchBlock.Count > 0) && context.FinBlock == null) {
                var cb = FixPosition(new XSharpParser.CatchBlockContext(context,0), context.Stop);
                cb.StmtBlk = FixPosition(new XSharpParser.StatementBlockContext(cb,0), context.Stop);
                this.ExitStatementBlock(cb.StmtBlk);
                this.ExitCatchBlock(cb);
                context._CatchBlock.Add(cb);
            }
            var catches = _pool.Allocate<CatchClauseSyntax>();
            foreach (var catchCtx in context._CatchBlock)
            {
                catches.Add(catchCtx.Get<CatchClauseSyntax>());
            }
            context.Put(_syntaxFactory.TryStatement(SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                context.StmtBlk.Get<BlockSyntax>(),
                catches,
                context.FinBlock == null ? null : _syntaxFactory.FinallyClause(SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                    context.FinBlock.Get<BlockSyntax>())));
            _pool.Free(catches);
        }

        public override void ExitCatchBlock([NotNull] XSharpParser.CatchBlockContext context)
        {
            context.Put(_syntaxFactory.CatchClause(SyntaxFactory.MakeToken(SyntaxKind.CatchKeyword),
                context.Id == null ? null : _syntaxFactory.CatchDeclaration(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    context.Type.Get<TypeSyntax>(),
                    context.Id.Get<SyntaxToken>(),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)),
                null, // TODO: (grammar) catch filters?
                context.StmtBlk.Get<BlockSyntax>()));
        }

        public override void ExitSeqStmt([NotNull] XSharpParser.SeqStmtContext context)
        {
            // TODO: sequence/break/recover are not supported yet
            context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)).
                WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_FeatureNotAvailableInVersion1, "BEGIN SEQUENCE statement")));
        }

        public override void ExitRecoverBlock([NotNull] XSharpParser.RecoverBlockContext context)
        {
            // TODO: sequence/break/recover are not supported yet
            context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)).
                WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_FeatureNotAvailableInVersion1, "RECOVER statement" )));
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
                // Add check for switch block without statements and insert a Jump to the next Switch block in the statement list
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
            var kw = context.Key.SyntaxKeyword();
            if (kw.Kind == SyntaxKind.CaseKeyword)
            {
                labels.Add(_syntaxFactory.CaseSwitchLabel(kw, context.Const?.Get<ExpressionSyntax>(),
                    SyntaxFactory.MakeToken(SyntaxKind.ColonToken)));
            }
            else
            {
                labels.Add(_syntaxFactory.DefaultSwitchLabel(kw, SyntaxFactory.MakeToken(SyntaxKind.ColonToken)));
            }
            var stmts = _pool.Allocate<StatementSyntax>();
            if (context.StmtBlk._Stmts.Count > 0) {
                stmts.Add(context.StmtBlk.Get<BlockSyntax>());
                stmts.Add(_syntaxFactory.BreakStatement(SyntaxFactory.MakeToken(SyntaxKind.BreakKeyword),
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))); // TODO: add it implicitly in the backend (catch error CS0163)
            }
            context.Put(_syntaxFactory.SwitchSection(labels, stmts));
            _pool.Free(labels);
            _pool.Free(stmts);
        }

        public override void ExitUsingStmt([NotNull] XSharpParser.UsingStmtContext context)
        {
            context.Put(_syntaxFactory.UsingStatement(SyntaxFactory.MakeToken(SyntaxKind.UsingKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                null, // TODO: (grammar) using variable declarations?
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                context.Stmtblk.Get<BlockSyntax>()));
        }

        public override void ExitQoutStmt([NotNull] XSharpParser.QoutStmtContext context)
        {
            if (context.Q.Type == XSharpParser.QQMARK && !(context._Exprs?.Count > 0)) {
                context.Put(_syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
            else {
                var block = _pool.Allocate<StatementSyntax>();
                if (context.Q.Type == XSharpParser.QMARK)
                    block.Add(_syntaxFactory.ExpressionStatement(
                        _syntaxFactory.InvocationExpression(
                            GenerateQualifiedName("global::System.Console.WriteLine"),
                            EmptyArgumentList()
                        ),
                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)
                    ));
                bool first = true;
                if (context._Exprs != null) {
                    foreach(var eCtx in context._Exprs) {
                        if (!first) {
                            block.Add(_syntaxFactory.ExpressionStatement(
                                _syntaxFactory.InvocationExpression(
                                    GenerateQualifiedName("global::System.Console.Write"),
                                    MakeArgumentList(_syntaxFactory.Argument(null,null,
                                        _syntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression,SyntaxFactory.Literal(null," "," ",null))
                                    ))
                                ),
                                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)
                            ));
                        }
                        // TODO: numeric formatting!
                        block.Add(_syntaxFactory.ExpressionStatement(
                            _syntaxFactory.InvocationExpression(
                                GenerateQualifiedName("global::System.Console.Write"),
                                MakeArgumentList(_syntaxFactory.Argument(null,null,eCtx.Get<ExpressionSyntax>()))
                            ),
                            SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)
                        ));
                        first = false;
                    }
                }
                context.Put(_syntaxFactory.Block(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    block,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken)
                ));
                _pool.Free(block);
            }
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

        public override void ExitCondAccessExpr([NotNull] XSharpParser.CondAccessExprContext context)
        {
#if false // nvk: check not needed because it is a separate rule now!
            switch (context.Right.Start.Type) {
                case XSharpParser.DOT:
                case XSharpParser.COLON:
                case XSharpParser.LBRKT:
                    break;
                default:
                    context.AddError(new ParseErrorData(context.Right.Start,ErrorCode.ERR_SyntaxError,"."));
                    break;
            }
#endif
            context.Put(_syntaxFactory.ConditionalAccessExpression(
                context.Left.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                context.Right.Get<ExpressionSyntax>()
            ));
        }

        public override void ExitBoundAccessMember([NotNull] XSharpParser.BoundAccessMemberContext context)
        {
            context.Put(_syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                context.Name.Get<SimpleNameSyntax>()));
        }

        public override void ExitBoundArrayAccess([NotNull] XSharpParser.BoundArrayAccessContext context)
        {
            context.Put(_syntaxFactory.ElementAccessExpression(
                context.Expr.Get<ExpressionSyntax>(),
                context.ArgList?.Get<BracketedArgumentListSyntax>() 
                    ?? _syntaxFactory.BracketedArgumentList(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                        default(SeparatedSyntaxList<ArgumentSyntax>),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken))));
        }

        public override void ExitBoundMethodCall([NotNull] XSharpParser.BoundMethodCallContext context)
        {
            context.Put(_syntaxFactory.InvocationExpression(
                context.Expr.Get<ExpressionSyntax>(),
                context.ArgList?.Get<ArgumentListSyntax>()
                    ?? _syntaxFactory.ArgumentList(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                        default(SeparatedSyntaxList<ArgumentSyntax>),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))));
        }

        public override void ExitBoundCondAccessExpr([NotNull] XSharpParser.BoundCondAccessExprContext context)
        {
            context.Put(_syntaxFactory.ConditionalAccessExpression(
                context.Left.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                context.Right.Get<ExpressionSyntax>()
            ));
        }

        public override void ExitBindMemberAccess([NotNull] XSharpParser.BindMemberAccessContext context)
        {
            context.Put(_syntaxFactory.MemberBindingExpression(
                SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                context.Name.Get<SimpleNameSyntax>()));
        }

        public override void ExitBindArrayAccess([NotNull] XSharpParser.BindArrayAccessContext context)
        {
            context.Put(_syntaxFactory.ElementBindingExpression(
                context.ArgList?.Get<BracketedArgumentListSyntax>() ?? EmptyBracketedArgumentList()
            ));
        }

        public override void ExitAccessMember([NotNull] XSharpParser.AccessMemberContext context)
         {
            context.Put(_syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                context.Name.Get<SimpleNameSyntax>()));
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
                context.Op.SyntaxPrefixOp(),
                context.Expr.Get<ExpressionSyntax>()));
        }

        public override void ExitBinaryExpression([NotNull] XSharpParser.BinaryExpressionContext context)
        {
            switch (context.Op.Type) {
                case XSharpParser.EXP:
                    context.Put(_syntaxFactory.InvocationExpression(GenerateQualifiedName("global::System.Math.Pow"), 
                        _syntaxFactory.ArgumentList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                            MakeSeparatedList(_syntaxFactory.Argument(null,null,context.Left.Get<ExpressionSyntax>()),
                                _syntaxFactory.Argument(null,null,context.Right.Get<ExpressionSyntax>())), 
                            SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))));
                    break;
                case XSharpParser.SUBSTR:
                    context.Put(
                        _syntaxFactory.BinaryExpression(
                            SyntaxKind.CoalesceExpression,
                            _syntaxFactory.BinaryExpression(SyntaxKind.GreaterThanExpression,
                                _syntaxFactory.ConditionalAccessExpression(
                                    context.Left.Get<ExpressionSyntax>(),
                                    SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                                    _syntaxFactory.InvocationExpression(
                                        _syntaxFactory.MemberBindingExpression(SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                                            _syntaxFactory.IdentifierName(SyntaxFactory.Identifier("IndexOf"))
                                        ),
                                        _syntaxFactory.ArgumentList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                            MakeSeparatedList(
                                                _syntaxFactory.Argument(null,null,_syntaxFactory.BinaryExpression(
                                                    SyntaxKind.CoalesceExpression,
                                                    context.Right.Get<ExpressionSyntax>(),
                                                    SyntaxFactory.MakeToken(SyntaxKind.QuestionQuestionToken),
                                                    _syntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, SyntaxFactory.Literal(null,"","",null))
                                                )),
                                                _syntaxFactory.Argument(null,null,GenerateQualifiedName("global::System.StringComparison.Ordinal"))
                                            ), 
                                            SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)
                                        )
                                    )
                                ),
                                SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken),
                                _syntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(null, "", -1, null))
                            ),
                            SyntaxFactory.MakeToken(SyntaxKind.QuestionQuestionToken),
                            _syntaxFactory.LiteralExpression(SyntaxKind.FalseLiteralExpression, SyntaxFactory.MakeToken(SyntaxKind.FalseKeyword))
                        )
                    );
                    break;
                case XSharpParser.ASSIGN_EXP:
                    context.Put(_syntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        context.Left.Get<ExpressionSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                        _syntaxFactory.InvocationExpression(GenerateQualifiedName("global::System.Math.Pow"), 
                            _syntaxFactory.ArgumentList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                MakeSeparatedList(_syntaxFactory.Argument(null,null,context.Left.Get<ExpressionSyntax>()),
                                    _syntaxFactory.Argument(null,null,context.Right.Get<ExpressionSyntax>())), 
                                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)))));
                    break;
                default:
                    context.Put(_syntaxFactory.BinaryExpression(
                        context.Op.ExpressionKindBinaryOp(),
                        context.Left.Get<ExpressionSyntax>(),
                        context.Op.SyntaxOp(),
                        context.Right.Get<ExpressionSyntax>()));
                    break;
            }
        }

        public override void ExitAssignmentExpression([NotNull] XSharpParser.AssignmentExpressionContext context)
        {
            context.Put(_syntaxFactory.AssignmentExpression(
                context.Op.ExpressionKindBinaryOp(),
                context.Left.Get<ExpressionSyntax>(),
                context.Op.SyntaxOp(),
                context.Right.Get<ExpressionSyntax>()));
        }

        public override void ExitPrimaryExpression([NotNull] XSharpParser.PrimaryExpressionContext context)
        {
            context.Put(context.Expr.Get<ExpressionSyntax>());
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
            if (!(context.Type is XSharpParser.ArrayDatatypeContext)) {
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
                    initializer: null)); // TODO: (grammar) object creation initializer
            }
            else {
                var type = (context.Type as XSharpParser.ArrayDatatypeContext).TypeName.Get<TypeSyntax>();
                var arrayType = context.Type.Get<ArrayTypeSyntax>();
                int ranks = 0;
                foreach(var rankSpec in arrayType.RankSpecifiers) {
                    ranks += rankSpec.Sizes.Count;
                }
                if (ranks != context.ArgList?._Args?.Count)
                    context.AddError(new ParseErrorData(ErrorCode.ERR_NoConstructors,arrayType));
                var sizes = _pool.AllocateSeparated<ExpressionSyntax>();
                if (context.ArgList?._Args != null) {
                    foreach (var size in context.ArgList?._Args) {
                        if (size.Name != null)
                            context.AddError(new ParseErrorData(size,ErrorCode.ERR_BadNamedArgument,size));
                        if (size.RefOut != null)
                            context.AddError(new ParseErrorData(size,ErrorCode.ERR_BadTypeArgument,size));
                        if (sizes.Count > 0)
                            sizes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                        sizes.Add(size.Expr.Get<ExpressionSyntax>());
                    }
                }
                context.Put(_syntaxFactory.ArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                    _syntaxFactory.ArrayType(type,
                        MakeList(_syntaxFactory.ArrayRankSpecifier(
                            SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                            sizes,
                            SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)))),
                    null));
                _pool.Free(sizes);
            }
        }

        public override void ExitDelegateCtorCall([NotNull] XSharpParser.DelegateCtorCallContext context)
        {
            if (((context.Obj as XSharpParser.PrimaryExpressionContext)?.Expr as XSharpParser.LiteralExpressionContext)?.Literal.Token.Type == XSharpParser.NULL) {
                context.Put(_syntaxFactory.CastExpression(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    context.Type.Get<TypeSyntax>(),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                    context.Func.Get<NameSyntax>()
                    ));
            }
            else {
                var fobj = context.Obj.Get<ExpressionSyntax>();
                SimpleNameSyntax fname;
                if (context.Func.CsNode is SimpleNameSyntax) {
                    fname = context.Func.Get<SimpleNameSyntax>();
                }
                else {
                    var fCtx = context.Func as XSharpParser.QualifiedNameContext;
                    if (fCtx != null) {
                        fobj = _syntaxFactory.CastExpression(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                            fCtx.Left.Get<NameSyntax>(),
                            SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                            fobj);
                        fname = fCtx.Right.Get<SimpleNameSyntax>();
                    }
                    else {
                        fname = _syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier("<missing>"));
                        context.AddError(new ParseErrorData(fCtx, ErrorCode.ERR_IdentifierExpected));
                    }
                }
                context.Put(_syntaxFactory.CastExpression(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    context.Type.Get<TypeSyntax>(),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                    _syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                        fobj,
                        SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                        fname)
                    ));
            }
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
            if (context.Type != null)
                context.Put(context.Type.Get<TypeSyntax>());
            else
                context.Put(context.XType.Get<TypeSyntax>());
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

        public override void ExitIntrinsicExpression([NotNull] XSharpParser.IntrinsicExpressionContext context)
        {
            var kind = context.Op.ExpressionKindBinaryOp();
            var syntax = context.Op.SyntaxOp();
            if (kind == SyntaxKind.BitwiseNotExpression) {
                if (context._Exprs?.Count > 1) {
                    context.AddError(new ParseErrorData(context.COMMA()[0], ErrorCode.ERR_CloseParenExpected));
                }
                context.Put(_syntaxFactory.PrefixUnaryExpression(
                    kind,
                    syntax,
                    context._Exprs?[0].Get<ExpressionSyntax>()));
            }
            else {
                var e = context._Exprs?[0].Get<ExpressionSyntax>();
                for (int i = 1; i < context._Exprs?.Count; i++) {
                    context.Put(_syntaxFactory.BinaryExpression(
                        kind,
                        e,
                        syntax,
                        context._Exprs[i].Get<ExpressionSyntax>()));
                }
            }
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

        public override void ExitVoConversionExpression([NotNull] XSharpParser.VoConversionExpressionContext context)
        {
            if (context.Type != null)
            {
                context.Put(_syntaxFactory.CastExpression(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    context.Type.Get<TypeSyntax>(),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                    context.Expr.Get<ExpressionSyntax>()));
            }
            else if (context.XType != null)
            {
                context.Put(_syntaxFactory.CastExpression(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    context.XType.Get<TypeSyntax>(),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                    context.Expr.Get<ExpressionSyntax>()));
            }
        }

        public override void ExitVoCastExpression([NotNull] XSharpParser.VoCastExpressionContext context)
        {
            context.Put(_syntaxFactory.CheckedExpression(SyntaxKind.UncheckedExpression,
                SyntaxFactory.MakeToken(SyntaxKind.UncheckedKeyword),
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                _syntaxFactory.CastExpression(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    context.Type.Get<TypeSyntax>(),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                    context.Expr.Get<ExpressionSyntax>()),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)));
        }

        public override void ExitVoCastPtrExpression([NotNull] XSharpParser.VoCastPtrExpressionContext context)
        {
            context.Put(_syntaxFactory.CastExpression(
                SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                _syntaxFactory.PointerType(context.Type.Get<TypeSyntax>(),SyntaxFactory.MakeToken(SyntaxKind.AsteriskToken)),
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                _syntaxFactory.PrefixUnaryExpression(SyntaxKind.AddressOfExpression,
                    SyntaxFactory.MakeToken(SyntaxKind.AmpersandToken),
                    context.Expr.Get<ExpressionSyntax>())));
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

        public override void ExitDefaultExpression([NotNull] XSharpParser.DefaultExpressionContext context)
        {
            context.Put(_syntaxFactory.DefaultExpression(
                SyntaxFactory.MakeToken(SyntaxKind.DefaultKeyword),
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

        public override void ExitQualifiedNameDot([NotNull] XSharpParser.QualifiedNameDotContext context)
        {
            context.Put(_syntaxFactory.QualifiedName(context.Left.Get<NameSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                context.Right.Get<SimpleNameSyntax>()));
        }

        public override void ExitSimpleOrAliasedNameDot([NotNull] XSharpParser.SimpleOrAliasedNameDotContext context)
        {
            context.Put(context.Name.Get<NameSyntax>());
        }

        public override void ExitQualifiedName([NotNull] XSharpParser.QualifiedNameContext context)
        {
            context.Put(_syntaxFactory.QualifiedName(context.Left.Get<NameSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                context.Right.Get<SimpleNameSyntax>()));
        }

        public override void ExitAliasQualifiedName([NotNull] XSharpParser.AliasQualifiedNameContext context)
        {
            context.Put(_syntaxFactory.AliasQualifiedName(context.Alias.Get<IdentifierNameSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.ColonColonToken),
                context.Right.Get<SimpleNameSyntax>()));
        }

        public override void ExitSimpleOrAliasedName([NotNull] XSharpParser.SimpleOrAliasedNameContext context)
        {
            context.Put(context.Name.Get<NameSyntax>());
        }

        public override void ExitGlobalQualifiedName([NotNull] XSharpParser.GlobalQualifiedNameContext context)
        {
            context.Put(_syntaxFactory.AliasQualifiedName(_syntaxFactory.IdentifierName(context.Global.SyntaxKeyword()),
                SyntaxFactory.MakeToken(SyntaxKind.ColonColonToken),
                context.Right.Get<SimpleNameSyntax>()));
        }

        public override void ExitIdentifierOrGenericName([NotNull] XSharpParser.IdentifierOrGenericNameContext context)
        {
            context.Put(context.Name.Get<SimpleNameSyntax>());
        }

        public override void ExitSimpleName([NotNull] XSharpParser.SimpleNameContext context)
        {
            if (context.GenericArgList == null)
                context.Put(_syntaxFactory.IdentifierName(context.Id.Get<SyntaxToken>()));
            else
                context.Put(_syntaxFactory.GenericName(context.Id.Get<SyntaxToken>(), context.GenericArgList.Get<TypeArgumentListSyntax>()));
        }

        public override void ExitGenericArgumentList([NotNull] XSharpParser.GenericArgumentListContext context)
        {
            var types = _pool.AllocateSeparated<TypeSyntax>();
            foreach (var type in context._GenericArgs)
            {
                if (types.Count != 0)
                    types.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                types.Add(type.Get<TypeSyntax>());
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
            foreach (var comma in context._Commas)
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

        public override void ExitNullableDatatype([NotNull] XSharpParser.NullableDatatypeContext context)
        {
            context.Put(_syntaxFactory.NullableType(context.TypeName.Get<TypeSyntax>(), SyntaxFactory.MakeToken(SyntaxKind.QuestionToken)));
        }

        public override void ExitTypeName([NotNull] XSharpParser.TypeNameContext context)
        {
            if (context.NativeType != null)
                context.Put(context.NativeType.Get<TypeSyntax>());
            else if (context.XType != null)
                context.Put(context.XType.Get<TypeSyntax>());
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
            context.Put(context.LiteralArray.Get<ExpressionSyntax>());
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
            var initializer = _syntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression, 
                SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken), 
                (context._Exprs?.Count ?? 0) == 0 ? default(SeparatedSyntaxList<ExpressionSyntax>)
                    : MakeSeparatedList<ExpressionSyntax>(context._Exprs), 
                SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
            if (context.Type != null)
                context.Put(_syntaxFactory.ArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                    _syntaxFactory.ArrayType(context.Type.Get<TypeSyntax>(),
                    MakeList(_syntaxFactory.ArrayRankSpecifier(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                        MakeSeparatedList<ExpressionSyntax>(
                            _syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MakeToken(SyntaxKind.OmittedArraySizeExpressionToken))),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)))),
                    initializer));
            else
                context.Put(_syntaxFactory.ImplicitArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                    EmptyList(),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken),
                    initializer));
        }

        public override void ExitCodeblockExpression([NotNull] XSharpParser.CodeblockExpressionContext context)
        {
            context.Put(context.CbExpr.Get<LambdaExpressionSyntax>());
        }

        public override void ExitCodeblock([NotNull] XSharpParser.CodeblockContext context)
        {
            context.Put(_syntaxFactory.ParenthesizedLambdaExpression(
                asyncKeyword: null,
                parameterList: context.CbParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList(),
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
                    type: null, // TODO: (grammar) codeblock param type
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

        public override void ExitQueryExpression([NotNull] XSharpParser.QueryExpressionContext context)
        {
            context.Put(context.Query.Get<QueryExpressionSyntax>());
        }

        public override void ExitLinqQuery([NotNull] XSharpParser.LinqQueryContext context)
        {
            context.Put(_syntaxFactory.QueryExpression(
                context.From.Get<FromClauseSyntax>(),
                context.Body.Get<QueryBodySyntax>()
                ));
        }

        public override void ExitFromClause([NotNull] XSharpParser.FromClauseContext context)
        {
            context.Put(_syntaxFactory.FromClause(SyntaxFactory.MakeToken(SyntaxKind.FromKeyword),
                context.Type?.Get<TypeSyntax>(),
                context.Id.Get<SyntaxToken>(),
                SyntaxFactory.MakeToken(SyntaxKind.InKeyword),
                context.Expr.Get<ExpressionSyntax>()
                ));
        }

        public override void ExitQueryBody([NotNull] XSharpParser.QueryBodyContext context)
        {
            context.Put(_syntaxFactory.QueryBody(
                MakeList<QueryClauseSyntax>(context._Bodyclauses),
                context.SorG.Get<SelectOrGroupClauseSyntax>(),
                context.Continuation?.Get<QueryContinuationSyntax>()
                ));
        }

        public override void ExitFromBodyClause([NotNull] XSharpParser.FromBodyClauseContext context)
        {
            context.Put(context.From.Get<FromClauseSyntax>());
        }

        public override void ExitLetClause([NotNull] XSharpParser.LetClauseContext context)
        {
            context.Put(_syntaxFactory.LetClause(
                SyntaxFactory.MakeToken(SyntaxKind.LetKeyword),
                context.Id.Get<SyntaxToken>(),
                SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                context.Expr.Get<ExpressionSyntax>()
                ));
        }

        public override void ExitWhereClause([NotNull] XSharpParser.WhereClauseContext context)
        {
            context.Put(_syntaxFactory.WhereClause(
                SyntaxFactory.MakeToken(SyntaxKind.WhereKeyword),
                context.Expr.Get<ExpressionSyntax>()
                ));
        }

        public override void ExitJoinClause([NotNull] XSharpParser.JoinClauseContext context)
        {
            context.Put(_syntaxFactory.JoinClause(
                SyntaxFactory.MakeToken(SyntaxKind.JoinKeyword),
                context.Type?.Get<TypeSyntax>(),
                context.Id.Get<SyntaxToken>(),
                SyntaxFactory.MakeToken(SyntaxKind.InKeyword),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.OnKeyword),
                context.OnExpr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                context.EqExpr.Get<ExpressionSyntax>(),
                context.Into?.Get<JoinIntoClauseSyntax>()
                ));
        }

        public override void ExitJoinIntoClause([NotNull] XSharpParser.JoinIntoClauseContext context)
        {
            context.Put(_syntaxFactory.JoinIntoClause(
                SyntaxFactory.MakeToken(SyntaxKind.IntoKeyword),
                context.Id.Get<SyntaxToken>()));
        }

        public override void ExitOrderbyClause([NotNull] XSharpParser.OrderbyClauseContext context)
        {
            context.Put(_syntaxFactory.OrderByClause(
                SyntaxFactory.MakeToken(SyntaxKind.OrderByKeyword),
                MakeSeparatedList<OrderingSyntax>(context._Orders)
                ));
        }

        public override void ExitOrdering([NotNull] XSharpParser.OrderingContext context)
        {
            SyntaxToken direction;
            SyntaxKind kind;
            if (context.Direction != null && context.Direction.Type == XSharpParser.DESCENDING)
            {
                direction = SyntaxFactory.MakeToken(SyntaxKind.DescendingKeyword);
                kind = SyntaxKind.DescendingOrdering;
                }
            else
            {
                direction = SyntaxFactory.MakeToken(SyntaxKind.AscendingKeyword);
                kind = SyntaxKind.AscendingOrdering;
            }
            context.Put(_syntaxFactory.Ordering(kind,context.Expr.Get<ExpressionSyntax>(),direction));

        }

        public override void ExitSelectClause([NotNull] XSharpParser.SelectClauseContext context)
        {
            context.Put(_syntaxFactory.SelectClause(
                SyntaxFactory.MakeToken(SyntaxKind.SelectKeyword),
                context.Expr.Get<ExpressionSyntax>()
                ));
        }

        public override void ExitGroupClause([NotNull] XSharpParser.GroupClauseContext context)
        {
            context.Put(_syntaxFactory.GroupClause(
                SyntaxFactory.MakeToken(SyntaxKind.GroupKeyword),
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.ByKeyword),
                context.ByExpr.Get<ExpressionSyntax>()
                ));
        }

        public override void ExitQueryContinuation([NotNull] XSharpParser.QueryContinuationContext context)
        {
            context.Put(_syntaxFactory.QueryContinuation(
                SyntaxFactory.MakeToken(SyntaxKind.IntoKeyword),
                context.Id.Get<SyntaxToken>(),
                context.Body.Get<QueryBodySyntax>()
                ));
        }

        public override void ExitLiteralValue([NotNull] XSharpParser.LiteralValueContext context)
        {
            //if (context.Token.Type == XSharpParser.MACRO)
            //{
            //    // Todo: replace Token with proper value
            //    switch (context.ToString().ToLowerInvariant())
            //    {
            //        case "__arraybase__":
            //        case "__clr2__":
            //        case "__clr4__":
            //        case "__clrversion__":
            //        case "__datetime__":
            //        case "__date__":
            //        case "__debug__":
            //        case "__entity__":
            //        case "__file__":
            //        case "__line__":
            //        case "__module__":
            //        case "__sig__":
            //        case "__srcloc__":
            //        case "__sysdir__":
            //        case "__time__":
            //        case "__utctime__":
            //        case "__version__":
            //        case "__windir__":
            //        case "__windrive__":
            //            break;
            //        default:
            //            break;
            //    }
            //}

            context.Put(_syntaxFactory.LiteralExpression(context.Token.ExpressionKindLiteral(), context.Token.SyntaxLiteralValue()));
        }

        public override void ExitIdentifierString([NotNull] XSharpParser.IdentifierStringContext context)
        {
            context.Put(_syntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression,
                context.Token?.SyntaxLiteralValue()
                ?? context.XsToken?.Token.SyntaxLiteralValue()
                ?? context.VnToken?.Token.SyntaxLiteralValue()));
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
            switch (context.Token.Type) {
                case XSharpParser.PTR:
                    context.Put(_syntaxFactory.PointerType(VoidType(),SyntaxFactory.MakeToken(SyntaxKind.AsteriskToken)));
                    break;
                //case XSharpParser.PSZ:
                //    context.Put(GenerateQualifiedName("global::System.IntPtr"));
                //    break;
                default:
                    context.Put(_syntaxFactory.PredefinedType(context.Token.SyntaxNativeType()));
                    break;
            }
        }
        public override void ExitXbaseType([NotNull] XSharpParser.XbaseTypeContext context)
        {
            context.Put(_syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.ObjectKeyword))
                .WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_FeatureNotAvailableInVersion1, context.Token.Text )));
        }


    }
}