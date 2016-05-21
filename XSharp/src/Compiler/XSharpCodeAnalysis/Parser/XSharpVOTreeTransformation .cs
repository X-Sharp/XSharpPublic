// Uncomment this define to dump the AST to the debug console.
//#define DUMP_TREE

using System;
using System.Linq;
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
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal class XSharpVOTreeTransformation : XSharpTreeTransformation {


        const string VoPszList = "Xs$PszList";
        const string ClipperArgs = "Xs$Args";
        const string ClipperPCount = "Xs$PCount";
        const string RecoverVarName = "Xs$Obj";

        private TypeSyntax _usualType;
        private TypeSyntax _floatType;
        private TypeSyntax _arrayType;
        private TypeSyntax _dateType;
        private TypeSyntax _symbolType;
        private TypeSyntax _pszType;
        private TypeSyntax _codeblockType;
 
        public XSharpVOTreeTransformation(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool, ContextAwareSyntax syntaxFactory) : 
            base(parser, options, pool, syntaxFactory)
        {
            if (_options.VulcanRTFuncsIncluded)
            {
                _usualType = GenerateQualifiedName("global::Vulcan.__Usual");
                _floatType = GenerateQualifiedName("global::Vulcan.__VOFloat");
                _dateType = GenerateQualifiedName("global::Vulcan.__VODate");
                _arrayType = GenerateQualifiedName("global::Vulcan.__Array");
                _symbolType = GenerateQualifiedName("global::Vulcan.__Symbol");
                _pszType = GenerateQualifiedName("global::Vulcan.__Psz");
                _codeblockType = GenerateQualifiedName("global::Vulcan.Codeblock");
            }
            else
            {
                _usualType = _objectType;
                _floatType = _objectType;
                _dateType = _objectType;
                _arrayType = _objectType;
                _symbolType = _objectType;
                _pszType = _objectType;
                _codeblockType = _objectType;
            }
        }

        internal CSharpSyntaxNode NoUsual(CSharpSyntaxNode node)
        {
            return NoRtFuncs(node,"USUAL");
        }
        internal CSharpSyntaxNode NoRtFuncs(CSharpSyntaxNode node, string type)
        {
            return GenerateNoRuntimeError(node, type, "VulcanRTFuncs.DLL");
        }

        internal CSharpSyntaxNode NoRt(CSharpSyntaxNode node, string type)
        {
            return GenerateNoRuntimeError(node, type, "VulcanRT.DLL");
        }

        internal CSharpSyntaxNode NoRt(string type)
        {
            CSharpSyntaxNode node = _syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            return NoRt(node, type);
        }

        internal ExpressionSyntax GenerateMemVarPut(string memvar, ExpressionSyntax right)
        {
            string method = "global::VulcanRTFuncs.Functions.__MemVarPut";
            var arg1 = MakeArgument(GenerateLiteral(memvar));
            var arg2 = MakeArgument(right);
            var args = MakeArgumentList(arg1, arg2);
            var expr = GenerateMethodCall(method, args);
            expr = (ExpressionSyntax)NotInDialect(expr, "MEMVAR");
            // if (!_options.VulcanRTFuncsIncluded)
            //{
            //    expr = (ExpressionSyntax)NoRtFuncs(expr, "FIELD");
            //}
            return expr;
        }

        internal ExpressionSyntax GenerateMemVarGet(string memvar)
        {
            string method = "global::VulcanRTFuncs.Functions.__MemVarGet";
            var arg1 = MakeArgument(GenerateLiteral(memvar));
            var args = MakeArgumentList(arg1);
            var expr = GenerateMethodCall(method, args);
            expr = (ExpressionSyntax)NotInDialect(expr, "MEMVAR");
            //todo: Implement MemVarGet
            // if (!_options.VulcanRTFuncsIncluded)
            //{
            //    expr = (ExpressionSyntax)NoRtFuncs(expr, "MEMVAR");
            //}
            return expr;
        }

        internal ExpressionSyntax GenerateFieldSet(string alias, string field, ExpressionSyntax right)
        {
            string method = "";
            ArgumentListSyntax args;
            var argField = MakeArgument(GenerateLiteral(field));
            var argValue = MakeArgument(right);
            if (!String.IsNullOrEmpty(alias))
            {
                method= "global::VulcanRTFuncs.Functions.__FieldSetWa";
                var argWA = MakeArgument(GenerateLiteral(alias));
                args = MakeArgumentList(argWA, argField, argValue);
            }
            else
            {
                method = "global::VulcanRTFuncs.Functions.__FieldSet";
                args = MakeArgumentList(argField, argValue);
            }
            var expr = GenerateMethodCall(method, args);
            if (!_options.VulcanRTFuncsIncluded)
            {
                expr = (ExpressionSyntax)NoRtFuncs(expr, "FIELD");
            }
            return expr;
        }

        internal ExpressionSyntax GenerateFieldGet(string alias, string field)
        {
            string method ;
            ArgumentListSyntax args;
            var argField= MakeArgument(GenerateLiteral(field));
            if (string.IsNullOrEmpty(alias))
            {
                method = "global::VulcanRTFuncs.Functions.__FieldGet";
                args = MakeArgumentList(argField);
            }
            else
            {
                method = "global::VulcanRTFuncs.Functions.__FieldGetWa";
                var argWA = MakeArgument(GenerateLiteral(alias));
                args = MakeArgumentList(argWA, argField);
            }

            var expr = GenerateMethodCall(method, args);
            if (!_options.VulcanRTFuncsIncluded)
            {
                expr = (ExpressionSyntax)NoRtFuncs(expr, "FIELD");
            }
            return expr;
        }

        private ExpressionSyntax GenerateNIL()
        {
            TypeSyntax type = _usualType;
            if (!_options.VulcanRTFuncsIncluded)
            {
                type = (TypeSyntax)NoUsual(type);
            }
            return _syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                            type,
                            SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                            GenerateSimpleName("_NIL"));

        }


        protected override void Check4ClipperCC(XP.IEntityContext context, 
            XP.ParameterListContext parameters, IToken Convention, XP.DatatypeContext returnType, bool mustHaveReturnType)
        {
            bool isEntryPoint = false;
            bool hasConvention = false;
            if (context is XP.FunctionContext)
            {
                var fc = context as XP.FunctionContext;
                isEntryPoint = fc.Id.GetText().ToLower() == "start";
            }
            context.Data.MustHaveReturnType = mustHaveReturnType;
            context.Data.HasMissingReturnType = (returnType == null);
            context.Data.HasTypedParameter = false;
            if (Convention != null)
            {
                context.Data.HasClipperCallingConvention = (Convention.Type == XP.CLIPPER);
                hasConvention = true;
            }
            int paramCount = 0;
            if (parameters != null && parameters._Params != null)
                paramCount = parameters._Params.Count;
            // Function Foo or Function Foo() without convention
            if ( paramCount == 0 && !hasConvention )
            {
                context.Data.HasClipperCallingConvention = _options.VOClipperCallingConvention && !isEntryPoint;
            }
            if (paramCount > 0 )
            {
                bool bHasTypedParameter = false;
                foreach (XP.ParameterContext par in parameters._Params)
                {
                    if (par.Type != null || par.Self != null)
                    {
                        bHasTypedParameter = true;
                        break;
                    }
                }
                context.Data.HasTypedParameter = bHasTypedParameter;
                if (!context.Data.HasClipperCallingConvention && !isEntryPoint && !hasConvention)
                    context.Data.HasClipperCallingConvention = !bHasTypedParameter;
            }
        }

        protected override void ImplementClipperAndPSZ(XP.IEntityContext context,
            ref  SyntaxList<AttributeListSyntax> attributes, ref ParameterListSyntax parameters, ref BlockSyntax body,
            ref TypeSyntax dataType )
        {
            if (context.Data.HasTypedParameter && context.Data.HasClipperCallingConvention)
            {
                parameters = parameters.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_TypedParametersWithClipperCallingConvention));
                return;
            }
            if (_options.IsDialectVO && body != null && (context.Data.HasClipperCallingConvention || context.Data.UsesPSZ ))
            {
                var stmts = _pool.Allocate<StatementSyntax>();
                ExpressionSyntax assignExpr;
                ExpressionSyntax ifExpr;
                StatementSyntax exprStmt;
                if (context.Data.HasClipperCallingConvention)
                {

                    if (context.Data.UsesPCount || parameters.Parameters.Count > 0  )
                    {
                        // VAR Xs$PCount  := 0
                        // IF Xs$Args != NULL
                        //    Xs$PCount := Xs$Args.Length
                        // ENDIF
                        stmts.Add(GenerateLocalDecl(ClipperPCount, _impliedType, GenerateLiteral("0", 0)));
                        assignExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                                GenerateSimpleName(ClipperPCount),
                                SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                                            _syntaxFactory.MemberAccessExpression(
                                                SyntaxKind.SimpleMemberAccessExpression,
                                                GenerateSimpleName(ClipperArgs),
                                                 SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                                                GenerateSimpleName("Length")));
                        exprStmt = _syntaxFactory.ExpressionStatement(assignExpr, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                        ifExpr = _syntaxFactory.BinaryExpression(
                                SyntaxKind.NotEqualsExpression,
                                GenerateSimpleName(ClipperArgs),
                                SyntaxFactory.MakeToken(SyntaxKind.ExclamationEqualsToken),
                                SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression,
                                    SyntaxFactory.MakeToken(SyntaxKind.NullKeyword)));
                        stmts.Add(GenerateIfStatement(ifExpr, exprStmt));
                    }

                    // LOCAL oPar1 as USUAL                <== For Clipper Calling Convention
                    // LOCAL oPar2 as USUAL                .
                    // IF Xs$ArgCount > 0                .
                    //   oPar1 := Xs$Args[0]                 .
                    // ENDIF                               <== For Clipper Calling Convention
                    // IF Xs$ArgCount > 1              .
                    //    oPar1 := Xs$Args[1]              .
                    // ENDIF                             .
                    var nilExpr = GenerateNIL();
                    var paramNames = new List<String>();
                    for (int i =0; i < parameters.Parameters.Count ; i++)
                    {
                        ParameterSyntax parm = parameters.Parameters[i];
                        string name = parm.Identifier.Text;
                        paramNames.Add(name);
                        stmts.Add(GenerateLocalDecl(name, _usualType, nilExpr));
                    }
                    int iAdd = 1;
                    if (_options.ArrayZero)
                        iAdd = 0;
                    StatementSyntax LastStmt = null;
                    for (int i = paramNames.Count-1; i >= 0 ; i--)
                    {
                        var name = paramNames[i];
                        var indices = _pool.AllocateSeparated<ArgumentSyntax>();
                        indices.Add(MakeArgument(GenerateLiteral("", i+iAdd)));
                        assignExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                            GenerateSimpleName(name), 
                            SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                                _syntaxFactory.ElementAccessExpression(
                                GenerateSimpleName(ClipperArgs),
                                _syntaxFactory.BracketedArgumentList(
                                    SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                                    indices,
                                    SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken))));
                        _pool.Free(indices);
                        exprStmt = _syntaxFactory.ExpressionStatement(assignExpr, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                        ifExpr = _syntaxFactory.BinaryExpression(
                            SyntaxKind.GreaterThanExpression,
                            GenerateSimpleName(ClipperPCount),
                            SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken),
                            GenerateLiteral(i.ToString(), i));
                        StatementSyntax ifBody;
                        if (LastStmt != null)
                        {
                            var ifbodystmts = _pool.Allocate<StatementSyntax>();
                            ifbodystmts.Add(exprStmt);
                            ifbodystmts.Add(LastStmt);
                            ifBody = MakeBlock(ifbodystmts);
                            _pool.Free(ifbodystmts);
                        }
                        else
                        {
                            ifBody = exprStmt;
                        }
                        LastStmt = GenerateIfStatement(ifExpr, ifBody);
                    }
                    stmts.Add(LastStmt);
                    // Now Change argument to X$Args PARAMS USUAL[]
                    var sizes = _pool.AllocateSeparated<ExpressionSyntax>();
                    var omittedArraySizeExpressionInstance = _syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MakeToken(SyntaxKind.OmittedArraySizeExpressionToken));
                    sizes.Add(omittedArraySizeExpressionInstance);
                    var rank = _syntaxFactory.ArrayRankSpecifier(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                        sizes,
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken));
                    _pool.Free(sizes);
                    var atype = _syntaxFactory.ArrayType(_usualType, rank) ;
                    if (!_options.VulcanRTFuncsIncluded)
                    {
                        atype = (ArrayTypeSyntax) NoUsual(atype);
                    }
                    SyntaxListBuilder modifiers = _pool.Allocate();
                    modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.ParamsKeyword));
                    var par = _syntaxFactory.Parameter(
                                    EmptyList<AttributeListSyntax>(),
                                    modifiers.ToList(),
                                    type: atype,
                                    identifier: SyntaxFactory.Identifier(ClipperArgs),
                                    @default: null );
                    var param = _pool.AllocateSeparated<ParameterSyntax>();
                    param.Add(par);
                    parameters = _syntaxFactory.ParameterList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                        param.ToList(),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
                    _pool.Free(param);
                    if (!_options.VulcanRTIncluded)
                    {
                        parameters = (ParameterListSyntax)NoRt(parameters, "Clipper Calling Convention");
                    }
                    // Finally add ClipperCallingConventionAttribute to the method
                    // using the names from the paramNames list
                    // [Vulcan.Internal.ClipperCallingConvention(new string[] { "a", "b" })]
                    // make sure that existing attributes are not removed!
                    var attrs = _pool.Allocate<AttributeListSyntax>();
                    attrs.AddRange(attributes);
                    var names = new List<ExpressionSyntax>();
                    foreach (var name in paramNames)
                    {
                        names.Add(GenerateLiteral(name));
                    }

                    attrs.Add(_syntaxFactory.AttributeList(
                        openBracketToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                        target: null,
                        attributes: MakeSeparatedList(_syntaxFactory.Attribute(
                            name: GenerateQualifiedName("global::Vulcan.Internal.ClipperCallingConvention"),
                            argumentList: _syntaxFactory.AttributeArgumentList(
                                openParenToken: SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                                arguments: MakeSeparatedList(
                                    _syntaxFactory.AttributeArgument(null, null,
                                        _syntaxFactory.ArrayCreationExpression(
                                            SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                                            _syntaxFactory.ArrayType(_syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.StringKeyword)),rank),
                                            _syntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression,
                                                SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                                                MakeSeparatedList<ExpressionSyntax>(names.ToArray()),
                                                SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken))))),
                                closeParenToken: SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)))),
                        closeBracketToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
                    attributes = attrs;
                    _pool.Free(attrs);
                }
                FinallyClauseSyntax finallyClause = null;
                if (context.Data.UsesPSZ)
                {
                    // VAR Xs$PszList := List<IntPtr>{}
                    var listOfIntPtr = _syntaxFactory.QualifiedName(GenerateQualifiedName("global::System.Collections.Generic"),
                        SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                        _syntaxFactory.GenericName(SyntaxFactory.MakeIdentifier("List"), 
                            _syntaxFactory.TypeArgumentList(
                                SyntaxFactory.MakeToken(SyntaxKind.LessThanToken),
                                MakeSeparatedList<TypeSyntax>(_ptrType),
                                SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken)
                                )));
                    var expr = CreateObject(listOfIntPtr, EmptyArgumentList(), null);
                    stmts.Add(GenerateLocalDecl(VoPszList, _impliedType, expr));
                    finallyClause = _syntaxFactory.FinallyClause(
                        SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                        MakeBlock(MakeList<StatementSyntax>(
                            GenerateExpressionStatement(
                                GenerateMethodCall("global::Vulcan.Internal.CompilerServices.String2PszRelease",
                                    MakeArgumentList(MakeArgument(GenerateSimpleName(VoPszList))))))));
                }
                // TRY
                //    original body
                // FINALLY                                             < == Always
                //    CompilerServices.String2PszRelease(Xs$PszList)   < == only for HasPsz(), otherwise empty
                //    And assign Byref params back to Xs$Args
                // END TRY

                // Note: When there is nothing in the finallyClause then the optimizer will remove the try statement which is OK.
                var tryStmt = finallyClause == null ? (StatementSyntax)body : // nvk: It doesn't hurt to optimize the tree (and avoid unnecessary diagnostics...)
                    _syntaxFactory.TryStatement(
                    SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                    body,
                    null,
                    finallyClause);
                stmts.Add(tryStmt);
                body = MakeBlock(stmts);
                _pool.Free(stmts);
            }
            // Add missing return type when needed. OBJECT or USUAL depending on the dialect.
            // Of course the return value must be specified as well, but that should be done in the return statement
            // when /vo9 is enabled.
            if (context.Data.HasMissingReturnType && context.Data.MustHaveReturnType)
            {
                if (_options.IsDialectVO && _options.VOUntypedAllowed)
                {
                    dataType = _usualType;
                    if (!_options.VulcanRTFuncsIncluded)
                    {
                        dataType = (TypeSyntax)NoUsual(dataType);
                    }
                }
                else
                    dataType = MissingType();

            }
        }




        public override void ExitSource([NotNull] XP.SourceContext context)
        {
            if (_options.VulcanRTFuncsIncluded)
            {
                // Add using Vulcan
                AddUsingWhenMissing(GlobalEntities.Usings, "Vulcan", false);

                // Add using Static VulcanRTFuncs.Functions
                AddUsingWhenMissing(GlobalEntities.Usings, "VulcanRTFuncs.Functions", true);

                // Add using Vulcan.VO
                // AddUsingWhenMissing(GlobalEntities.Usings, "Vulcan.VO",true),
            }
            base.ExitSource(context);
        }

        public override void ExitClsctor([NotNull] XP.ClsctorContext context)
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
                var attributes = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
                var parameters = context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList();
                var body = context.StmtBlk?.Get<BlockSyntax>();
                TypeSyntax returntype = null;
                if (context.Chain != null && _options.IsDialectVO)
                {
                    var chainArgs = context.ArgList?.Get<ArgumentListSyntax>() ?? EmptyArgumentList();
                    var chainExpr = _syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                        context.Chain.Type == XP.SELF ? (ExpressionSyntax)_syntaxFactory.ThisExpression(context.Chain.SyntaxKeyword()) : _syntaxFactory.BaseExpression(context.Chain.SyntaxKeyword()),
                        SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                        _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(".ctor")));
                    body = MakeBlock(MakeList<StatementSyntax>(
                        _syntaxFactory.ExpressionStatement(_syntaxFactory.InvocationExpression(chainExpr, chainArgs), SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)),
                        body));
                    context.Chain = null;
                }
                ImplementClipperAndPSZ(context, ref attributes, ref parameters, ref body, ref returntype);
                var parentId = (context.Parent as XP.Class_Context)?.Id.Get<SyntaxToken>()
                    ?? (context.Parent as XP.Structure_Context)?.Id.Get<SyntaxToken>()
                    ?? (context.Parent as XP.Interface_Context)?.Id.Get<SyntaxToken>();
                context.Put(_syntaxFactory.ConstructorDeclaration(
                    attributeLists: attributes,
                    modifiers: context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(),
                    identifier: parentId,
                    parameterList: parameters,
                    initializer: context.Chain == null ? null : 
                        _syntaxFactory.ConstructorInitializer(context.Chain.CtorInitializerKind(),
                            SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                            context.Chain.SyntaxKeyword(),
                            context.ArgList?.Get<ArgumentListSyntax>() ?? EmptyArgumentList()),
                    body: body,
                    semicolonToken: (context.StmtBlk?._Stmts?.Count > 0) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
        }




        public override void ExitCallingconvention([NotNull] XP.CallingconventionContext context)
        {
            // TODO nvk (calling convention is silently ignored for now)
            if (context.Convention.Type == XP.CLIPPER && context.Parent == CurrentEntity)
            {
                CurrentEntity.Data.HasClipperCallingConvention = true;
            }
            base.ExitCallingconvention(context); 
        }

        protected override TypeSyntax _getParameterType([NotNull] XP.ParameterContext context) {
            TypeSyntax type = context.Type?.Get<TypeSyntax>();
            if(type == null) {
                if(CurrentEntity.Data.HasTypedParameter && _options.VOUntypedAllowed) {
                    type = _usualType;
                    if(!_options.VulcanRTFuncsIncluded) {
                        type = (TypeSyntax)NoUsual(type);
                    }
                } else
                    type = MissingType();
            }
            return type;
        }

        protected override TypeSyntax _getMissingLocalType() {
            TypeSyntax varType;
            if(_options.VOUntypedAllowed) {
                varType = _usualType;
                if(!_options.VulcanRTFuncsIncluded) {
                    varType = (TypeSyntax)NoUsual(varType);
                }
            } else
                varType = MissingType();
            return varType;
        }


        public override void EnterMethod([NotNull] XP.MethodContext context) {
            base.EnterMethod(context);
            if(context.T.Token.Type != XP.METHOD ) {
                context.Data.HasClipperCallingConvention = false;
                context.Data.HasTypedParameter = true;          // this will set all missing types to USUAL
            }
        }


        public override void EnterXbasedecl([NotNull] XP.XbasedeclContext context)
        {
            // declare memvars
            if (context.T.Type == XP.MEMVAR)
            {
                foreach (var memvar in context._Vars)
                {
                    CurrentEntity.Data.AddField(memvar.Id.GetText(), "M", false);
                }

            }
        }


        public override void ExitBreakStmt([NotNull] XP.BreakStmtContext context)
        {
            if (_options.VulcanRTIncluded)
            {
                ArgumentListSyntax args;
                if (context.Expr != null)
                    args = MakeArgumentList(MakeArgument(context.Expr.Get<ExpressionSyntax>()));
                else
                    args = MakeArgumentList(MakeArgument(GenerateNIL()));
                var expr = CreateObject(GenerateQualifiedName("global::Vulcan.Internal.VulcanWrappedException"), args, null);
                context.Put(_syntaxFactory.ThrowStatement(SyntaxFactory.MakeToken(SyntaxKind.ThrowKeyword),
                    expr,
                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
            else
            {
                context.Put(NoRt("BREAK statement"));
            }
        }



        public override void ExitSeqStmt([NotNull] XP.SeqStmtContext context)
        {
            // This generates 2 try statements:
            // 1) an inner try that controls the calls to CompilerServices.EnterBeginSequence();
            //    and CompilerServices.ExitBeginSequence();
            // 2) an outer try that has the try - catch - finally from the seq statement itself
            if (!_options.VulcanRTIncluded)
            {
                context.Put(NoRt("BEGIN SEQUENCE statement"));
                return;
            }

            var stmts = _pool.Allocate<StatementSyntax>();
            stmts.Add(GenerateExpressionStatement(GenerateMethodCall("global::Vulcan.Internal.CompilerServices.EnterBeginSequence",
                                    EmptyArgumentList())));
            stmts.Add(MakeBlock(context.StmtBlk.Get<BlockSyntax>()));
            var tryBlock = MakeBlock(stmts);
            stmts.Clear();
            stmts.Add(GenerateExpressionStatement(GenerateMethodCall("global::Vulcan.Internal.CompilerServices.ExitBeginSequence",
                                    EmptyArgumentList())));
            var innerTry = _syntaxFactory.TryStatement(SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                 tryBlock,
                 null,
                 _syntaxFactory.FinallyClause(SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                     MakeBlock(stmts)));
            stmts.Clear();
            stmts.Add(innerTry);
            tryBlock = MakeBlock(stmts);
            stmts.Clear();
            CatchClauseSyntax catchClause = null;
            FinallyClauseSyntax finallyClause = null;
            if (context.RecoverBlock != null)
            {
                catchClause = context.RecoverBlock.Get<CatchClauseSyntax>();
            }
            else
            {
                // generate a default catch block if there is no finally block
                if (context.FinBlock == null)
                {
                    var cb = FixPosition(new XP.CatchBlockContext(context, 0), context.Stop);
                    cb.StmtBlk = FixPosition(new XP.StatementBlockContext(cb, 0), context.Stop);
                    this.ExitStatementBlock(cb.StmtBlk);
                    this.ExitCatchBlock(cb);
                    catchClause = cb.Get<CatchClauseSyntax>();

                }
            }
            if (context.FinBlock != null)
            {
                finallyClause = _syntaxFactory.FinallyClause(
                 SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                 context.FinBlock.Get<BlockSyntax>());
            }
            var outerTry = _syntaxFactory.TryStatement(SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                  tryBlock,
                  catchClause,
                  finallyClause);
            context.Put(outerTry);
            _pool.Free(stmts);

        }

        public override void ExitRecoverBlock([NotNull] XP.RecoverBlockContext context)
        {
            // The recover block has source code:
            //RECOVER USING uValue
            //  statements
            //
            // and gets converted to
            // 
            // Catch obj as Exception
            // if obj is VulcanWrappedException                                        // condition 1
            //   uValue := ((VulcanWrappedException) obj).Value                        // assign 1
            //
            // else if obj is Vulcan.Error                                             // condition 2
            //   uValue :=  obj1                                                       // assign 2
            //
            // else if obj is Exception                                                // condition 3
            //   // wraps Exception in Vulcan.Error Object                             // Always true unless obj = NULL
            //   uValue := (USUAL)  Error._WrapRawException((Exception) obj1))         // assign 3
            //
            // else
            //   uValue := obj1                                                        // assign 4
            // endif
            var objName = GenerateSimpleName(RecoverVarName);
            var idName = GenerateSimpleName(context.Id.GetText());

            var condition1 = _syntaxFactory.BinaryExpression(
                  SyntaxKind.IsExpression,
                  objName,
                  SyntaxFactory.MakeToken(SyntaxKind.IsKeyword),
                  GenerateQualifiedName("global::Vulcan.Internal.VulcanWrappedException"));

            var condition2 = _syntaxFactory.BinaryExpression(
                SyntaxKind.IsExpression,
                objName,
                SyntaxFactory.MakeToken(SyntaxKind.IsKeyword),
                GenerateQualifiedName("global::Vulcan.Error"));

            var condition3 = _syntaxFactory.BinaryExpression(
                SyntaxKind.IsExpression,
                objName,
                SyntaxFactory.MakeToken(SyntaxKind.IsKeyword),
                GenerateQualifiedName("global::System.Exception"));

            var assign1 = GenerateExpressionStatement(
                _syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                idName,
                SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                _syntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                   _syntaxFactory.CastExpression(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    GenerateQualifiedName("global::Vulcan.Internal.VulcanWrappedException"),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                    objName),
                   SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                     GenerateSimpleName("Value"))));

            var assign2 = GenerateExpressionStatement(_syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                 idName,
                 SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                 objName));

            var assign3 = GenerateExpressionStatement(_syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                idName,
                SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                _syntaxFactory.CastExpression(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                _usualType,
                SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                GenerateMethodCall("global::Vulcan.Error._WrapRawException",
                MakeArgumentList(MakeArgument(objName))))));

            var assign4= GenerateExpressionStatement(_syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                 idName,
                 SyntaxFactory.MakeToken(SyntaxKind.EqualsToken),
                 objName));

             var elseClause = _syntaxFactory.ElseClause(
                SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword),
                MakeBlock(assign4));

            // if 3
            var ifstmt = _syntaxFactory.IfStatement(
                        SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                        SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                        condition3,
                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                        MakeBlock(assign3), elseClause);

            // if 3 is assigned to the else block of if 2
            elseClause = _syntaxFactory.ElseClause(
                SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword),
                MakeBlock(ifstmt));

            // if 2
            ifstmt = _syntaxFactory.IfStatement(
                        SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                        SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                        condition2,
                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                        MakeBlock(assign2), elseClause);
            
            // if 2 is assigned to the else block of if 1
            elseClause = _syntaxFactory.ElseClause(
                SyntaxFactory.MakeToken(SyntaxKind.ElseKeyword),
                MakeBlock(ifstmt));
            // if 1
            ifstmt = _syntaxFactory.IfStatement(
                        SyntaxFactory.MakeToken(SyntaxKind.IfKeyword),
                        SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                        condition1,
                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken),
                        MakeBlock(assign1), elseClause);

            var stmts = _pool.Allocate<StatementSyntax>();
            stmts.Add(ifstmt);
            stmts.Add(context.StmtBlock.Get<BlockSyntax>());
            var catchClause = _syntaxFactory.CatchClause(
                SyntaxFactory.MakeToken(SyntaxKind.CatchKeyword),
                _syntaxFactory.CatchDeclaration(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    GenerateQualifiedName("global::System.Exception"),
                    SyntaxFactory.Identifier(RecoverVarName),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)),
                    null,
                    MakeBlock(stmts));

            context.Put(catchClause);
            _pool.Free(stmts);
        }


        public override void ExitQoutStmt([NotNull] XP.QoutStmtContext context) {
            if(!_options.VulcanRTFuncsIncluded) {
                base.ExitQoutStmt(context);
                return;
            }
            ArgumentSyntax arg;
            ExpressionSyntax expr;
            // todo: If dialect VO and VulcanRTFuncs included
            // Simply generate call to VulcanRTFuncs.Functions.QOut or QQOut
            // and pass list of expressions as argument
            string methodName;
            if(context.Q.Type == XP.QQMARK)
                methodName = "global::VulcanRTFuncs.Functions.QQOut";
            else
                methodName = "global::VulcanRTFuncs.Functions.QOut";

            ArgumentListSyntax args;
            if(context._Exprs != null && context._Exprs.Count > 0) {
                var arglist = _pool.AllocateSeparated<ArgumentSyntax>();
                foreach(var eCtx in context._Exprs) {
                    arg = MakeArgument(eCtx.Get<ExpressionSyntax>());
                    arglist.Add(arg);
                }
                args = _syntaxFactory.ArgumentList(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                        arglist,
                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
                _pool.Free(arglist);
            } else {
                args = EmptyArgumentList();
            }
            expr = GenerateMethodCall(methodName, args);
            context.Put(_syntaxFactory.ExpressionStatement(expr, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            return;
        }


        public override void ExitAccessMember([NotNull] XP.AccessMemberContext context)
         {
            if (context.Op.Type == XP.DOT)
            {
                if (context.Expr.Get<ExpressionSyntax>() is NameSyntax)
                {
                    context.Put(_syntaxFactory.QualifiedName(
                        context.Expr.Get<NameSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                        context.Name.Get<SimpleNameSyntax>()));
                    return;
                }
                else
                {
                    context.Put(_syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                        context.Expr.Get<ExpressionSyntax>(),
                        SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                        (SimpleNameSyntax) NotInDialect(context.Name.Get<SimpleNameSyntax>(),"equivalency of : and . member access operators"))
                        );
                    return;
                }
            }
            context.Put(_syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                context.Expr.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                context.Name.Get<SimpleNameSyntax>()));
        }


        public override void ExitAssignmentExpression([NotNull] XP.AssignmentExpressionContext context)
        {
            // when /vo12 is used then for the types .ASSIGN_DIV add conversion for the LHS and RHS to Double
            // Check for Field or MemVar assignments
            ExpressionSyntax left = context.Left.Get<ExpressionSyntax>();
            ExpressionSyntax right = context.Right.Get<ExpressionSyntax>();
            if (left.XNode is XP.PrimaryExpressionContext && ((XP.PrimaryExpressionContext)left.XNode).Expr is XP.AliasedFieldContext)
            {
                XP.AliasedFieldContext fieldNode = ((XP.PrimaryExpressionContext)left.XNode).Expr as XP.AliasedFieldContext;
                //ToDo
                // Convert _FIELD->NAME += 1 to _FIELD->NAME := _FIELD->NAME + 1

                ExpressionSyntax expr;
                if (context.Op.Type == XP.ASSIGN_OP)
                {
                    expr = GenerateFieldSet(fieldNode.Alias?.GetText(), fieldNode.Field.GetText(), right);
                }
                else
                {
                    // AREA->NAME+= 1 gets converted to
                    // __FieldSet("Area", "Name", __FieldGet("Area","Name") + 1) 
                    // left already has the FieldGet
                    var op = context.Op.ComplexToSimpleBinaryOp();
                    var token = context.Op.ComplexToSimpleToken();
                    if (op == SyntaxKind.EmptyStatement ) 
                    {
                        expr = GenerateFieldSet(fieldNode.Alias?.GetText(), fieldNode.Field.GetText(), right);
                        expr = (ExpressionSyntax)NotInDialect(expr, "Complex operation: " + context.Op.Text);
                    }
                    else
                    {
                        expr = _syntaxFactory.BinaryExpression(op,left,token,right);
                        expr = GenerateFieldSet(fieldNode.Alias?.GetText(), fieldNode.Field.GetText(), expr);
                    }

                }
                context.Put(expr);
                return;

            }
            else if (left.XNode is XP.PrimaryExpressionContext && ((XP.PrimaryExpressionContext)left.XNode).Expr is XP.NameExpressionContext)
            {
                XP.NameExpressionContext namecontext = ((XP.PrimaryExpressionContext)left.XNode).Expr as XP.NameExpressionContext;
                string name = namecontext.Name.GetText();
                var fieldInfo = CurrentEntity.Data.GetField(name);
                if (fieldInfo != null)
                {
                    ExpressionSyntax expr;
                    if (fieldInfo.IsField)
                    {
                        if (context.Op.Type == XP.ASSIGN_OP)
                        {
                            expr = GenerateFieldSet(fieldInfo.Alias, fieldInfo.Name, right);
                        }
                        else
                        {
                            var op = context.Op.ComplexToSimpleBinaryOp();
                            var token = context.Op.ComplexToSimpleToken();
                            if (op == SyntaxKind.EmptyStatement  )
                            {
                                expr = GenerateFieldSet(fieldInfo.Alias, fieldInfo.Name, right);
                                expr = (ExpressionSyntax)NotInDialect(expr, "Complex operation: " + context.Op.Text);
                            }
                            else
                            {

                                // AREA->NAME+= 1 gets converted to
                                // __FieldSet("Area", "Name", __FieldGet("Area","Name") + 1) 
                                // left already has the FieldGet
                                expr = _syntaxFactory.BinaryExpression(op, left, token, right);
                                expr = GenerateFieldSet(fieldInfo.Alias, fieldInfo.Name, expr);
                            }
                        }
                    }
                    else
                        if (context.Op.Type == XP.ASSIGN_OP)
                    {
                        expr = GenerateMemVarPut(fieldInfo.Name, right);
                    }
                    else
                    {
                        var op = context.Op.ComplexToSimpleBinaryOp();
                        var token = context.Op.ComplexToSimpleToken();
                        if (op == SyntaxKind.EmptyStatement)
                        {
                            expr = GenerateMemVarPut(fieldInfo.Name, right);
                            expr = (ExpressionSyntax)NotInDialect(expr, "Complex operation: " + context.Op.Text);
                        }
                        else
                        {

                            // NAME+= 1 gets converted to MemVarPut("Name", MemVarGet("Name") + 1)
                            // left already has the MemVarGet
                            // MemVarPut( "Name", __MemVarGet("Name") + 1) 
                            expr = _syntaxFactory.BinaryExpression(op, left, token, right);
                            expr = GenerateMemVarPut(fieldInfo.Name, expr);
                        }
                    }
                     context.Put(expr);
                    return;
                }
            }
            base.ExitAssignmentExpression(context);
        }



        public override void ExitMethodCall([NotNull] XP.MethodCallContext context) {
            var expr = context.Expr.Get<ExpressionSyntax>();
            bool bPszConvert = false;
            bool bClipCalFunc = false;
            string name = null;
            if(expr is IdentifierNameSyntax) {
                IdentifierNameSyntax ins = expr as IdentifierNameSyntax;
                name = ins.Identifier.Text.ToUpper();
                switch(name) {
                    case "STRING2PSZ":
                    case "CAST2PSZ":
                        bPszConvert = true;
                        break;
                    case "PCOUNT":
                    case "_GETMPARAM":
                    case "_GETFPARAM":
                        if(CurrentEntity.Data.HasClipperCallingConvention) {
                            bClipCalFunc = true;
                        } else {
                            expr = GenerateLiteral("", 0);
                            expr = expr.WithAdditionalDiagnostics(
                                new SyntaxDiagnosticInfo(ErrorCode.ERR_OnlySupportedForClipperCallingConvention, ins.Identifier.Text));
                            context.Put(expr);
                            return;

                        }

                        break;
                    default:
                        break;
                }
            } else if(expr is ThisExpressionSyntax || expr is BaseExpressionSyntax) {
                expr = _syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expr, SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                    _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(".ctor")));
            }
            if(!bPszConvert && !bClipCalFunc) {
                base.ExitMethodCall(context);
                return;
            }
            ArgumentListSyntax argList;
            if(context.ArgList != null) {
                argList = context.ArgList.Get<ArgumentListSyntax>();
            } else {
                argList = EmptyArgumentList();
            }
            if(bPszConvert) {
                // this will only happen when the VO or Vulcan dialect is selected, so we can use the psz type here
                // and the reference to the String2Psz() in the Vulcan Runtime.
                if(_options.VulcanRTIncluded) {
                    if(CurrentEntity != null && argList.Arguments.Count > 0) {
                        CurrentEntity.Data.UsesPSZ = true;
                        // Add reference to compiler generated List<IntPtr> to the argList
                        NameSyntax pszlist = GenerateSimpleName(VoPszList);
                        expr = argList.Arguments[0].Expression;
                        argList = MakeArgumentList(MakeArgument(expr), MakeArgument(pszlist));
                        expr = GenerateMethodCall("global::Vulcan.Internal.CompilerServices.String2Psz", argList);
                        var args = MakeArgumentList(MakeArgument(expr));
                        expr = CreateObject(this._pszType, args, null);
                        context.Put(expr);
                        return;
                    }
                } else {
                    context.Put(NoRt(GenerateLiteral(""), name));
                    return;
                }
            } else if(bClipCalFunc) {
                if(!_options.VulcanRTFuncsIncluded) {
                    expr = (ExpressionSyntax)NoRtFuncs(GenerateLiteral(0), name);
                    context.Put(expr);
                    return;
                }
                if(name == "PCOUNT") {
                    expr = GenerateSimpleName(ClipperPCount);
                    context.Put(expr);
                    CurrentEntity.Data.UsesPCount = true;
                    return;
                } else {
                    // _GETMPARAM or _GETFPARAM
                    if(argList.Arguments.Count > 0) {
                        var indices = _pool.AllocateSeparated<ArgumentSyntax>();
                        indices.Add(MakeArgument(argList.Arguments[0].Expression));

                        expr = _syntaxFactory.ElementAccessExpression(
                        GenerateSimpleName(ClipperArgs),
                        _syntaxFactory.BracketedArgumentList(
                            SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                            indices,
                            SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
                        context.Put(expr);
                        return;
                    } else {
                        // generate an error: missing argument
                        expr = GenerateLiteral(0).
                        WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_MissingArgument));
                        context.Put(expr);
                        return;

                    }

                }


            }
        }



        public override void ExitNameExpression([NotNull] XP.NameExpressionContext context)
        {
            // Check to see if the name is a field or Memvar, registered with the FIELD or MemVar statement
            string Name = context.Name.GetText();
            ExpressionSyntax expr = context.Name.Get<NameSyntax>();
            var fieldInfo = CurrentEntity.Data.GetField(Name);
            if (fieldInfo != null )
            {
                if (fieldInfo.IsField)
                {
                    expr = GenerateFieldGet(fieldInfo.Alias, fieldInfo.Name);
                    
                }
                else
                {
                    expr = GenerateMemVarGet(fieldInfo.Name);
                }
            }
            context.Put(expr);
        }



        public override void ExitIif([NotNull] XP.IifContext context)
        {
            // if /vo10 is used then cast the LHS and RHS to USUAL or OBJECT depending on the dialect
            ExpressionSyntax left = context.TrueExpr.Get<ExpressionSyntax>();
            ExpressionSyntax right = context.FalseExpr.Get<ExpressionSyntax>();
            if (_options.VOCompatibleIIF)
            {
                var type = (_options.IsDialectVO && _options.VulcanRTFuncsIncluded)
                    ? (TypeSyntax) _usualType : (TypeSyntax) _objectType;
                left = _syntaxFactory.CastExpression(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    type, SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken), left);
                right = _syntaxFactory.CastExpression(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    type, SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken), right);

            }
            context.Put(_syntaxFactory.ConditionalExpression(
                context.Cond.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                left,
                SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                right));
        }

        public override void ExitLiteralArray([NotNull] XP.LiteralArrayContext context)
        {
            TypeSyntax type = null;
            ExpressionSyntax expr = null;
            bool bVOArray = false;
            // detect typed arrays.
            // <LONG> {...} indicates an array of type LONG
            // when no type is specified and the dialect VO or Vulcan the type is USUAL
            if (context.Type != null)
            {
                type = context.Type.Get<TypeSyntax>();
            }
            type = _usualType;
            bVOArray = true;
            if (!_options.VulcanRTFuncsIncluded)
            {
                type = (TypeSyntax) NoUsual(type);
            }


            var initializer = _syntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression, 
                SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken), 
                (context._Exprs?.Count ?? 0) == 0 ? default(SeparatedSyntaxList<ExpressionSyntax>)
                    : MakeSeparatedList<ExpressionSyntax>(context._Exprs), 
                SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
            if (type != null)
            {
                expr = _syntaxFactory.ArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                    _syntaxFactory.ArrayType(type,
                    MakeList(_syntaxFactory.ArrayRankSpecifier(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                        MakeSeparatedList<ExpressionSyntax>(
                            _syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MakeToken(SyntaxKind.OmittedArraySizeExpressionToken))),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)))),
                    initializer);
            }
            else
            {
                expr = _syntaxFactory.ImplicitArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                    EmptyList(),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken),
                    initializer)
                    .WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_UntypedArrayNotAvailableInDialect, _options.Dialect.ToString()));
            }
            if (bVOArray)
            {
                context.Put<ExpressionSyntax>(CreateObject(_arrayType, MakeArgumentList(MakeArgument(expr)), null));
            }
            else
                context.Put<ExpressionSyntax>(expr);
        }

        public override void ExitLiteralValue([NotNull] XP.LiteralValueContext context)
        {
            string[] args;
            
            // Map some literals to static member access or static method calls
            ArgumentSyntax arg0, arg1, arg2;
            ExpressionSyntax expr = null;
            switch (context.Token.Type)
            {
                case XP.NIL:
                    expr = GenerateNIL();
                    break;
                case XP.NULL_PTR:
                    expr = _syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                            _ptrType,
                        SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                        GenerateSimpleName("Zero"));
                    break;
                case XP.NULL_PSZ:
                    arg0 = MakeArgument(GenerateLiteral(""));
                    expr = CreateObject(_pszType, MakeArgumentList(arg0), null);
                    break;
                case XP.NULL_DATE:
                    expr = GenerateMethodCall("global::Vulcan.__VODate.NullDate",EmptyArgumentList());
                    break;
                case XP.DATE_CONST:
                    args = context.Token.Text.Split('.');
                    if (args.Length == 3)
                    {
                        int year, month, day;
                        if (Int32.TryParse(args[0], out year) &&
                            Int32.TryParse(args[1], out month) &&
                            Int32.TryParse(args[2], out day))
                        {
                            arg0 = MakeArgument(GenerateLiteral(args[0], year));
                            arg1 = MakeArgument(GenerateLiteral(args[1], month));
                            arg2 = MakeArgument(GenerateLiteral(args[2], day));
                            expr = CreateObject(_dateType, MakeArgumentList(arg0, arg1, arg2), null);
                        }
                    }
                    break;
                case XP.SYMBOL_CONST:
                    arg0 = MakeArgument(SyntaxFactory.LiteralExpression(context.Token.ExpressionKindLiteral(), context.Token.SyntaxLiteralValue(_options)));
                    expr = CreateObject(_symbolType, MakeArgumentList(arg0), null);
                    break;
                case XP.REAL_CONST:
                    if (_options.VOFloatConstants)
                    {
                        args = context.Token.Text.Split('.');
                        if (args.Length == 2)
                        {
                            int len = context.Token.Text.Length;
                            int dec = args[1].Length;
                            arg0 = MakeArgument(SyntaxFactory.LiteralExpression(context.Token.ExpressionKindLiteral(), context.Token.SyntaxLiteralValue(_options)));
                            arg1 = MakeArgument(GenerateLiteral(len.ToString(), len));
                            arg2 = MakeArgument(GenerateLiteral(dec.ToString(), dec));
                            expr = CreateObject(_floatType, MakeArgumentList(arg0, arg1, arg2), null);
                        }
                    }
                    break;
            }
            if (expr != null)
            {
                context.Put(expr);
                return;
            }
            base.ExitLiteralValue(context);
        }

        public override void ExitXbaseType([NotNull] XP.XbaseTypeContext context)
        {
            TypeSyntax type = null;
            switch (context.Token.Type)
            {
                case XP.ARRAY:
                    type = _arrayType;
                    break;
                case XP.CODEBLOCK:
                    type = _codeblockType;
                    break;
                case XP.DATE:
                    type = _dateType;
                    break;
                case XP.FLOAT:
                    type = _floatType;
                    break;
                case XP.PSZ:
                    type = _pszType;
                    break;
                case XP.USUAL:
                    type = _usualType ;
                    break;
                case XP.SYMBOL:
                    type = _symbolType;
                    break;
                default:
                    type = null;
                    break;
            }
            if (type == null) {
                type = (TypeSyntax)NotInDialect(_objectType, context.Token.Text);
            }
            else if (!_options.VulcanRTFuncsIncluded )
            {
                type = (TypeSyntax) NoRtFuncs(type,context.Token.Text);
            }
            context.Put(type);
        }
        public override void ExitFielddecl([NotNull] XP.FielddeclContext context)
        {
            var stmt = _syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            if (!_options.VulcanRTFuncsIncluded)
            {
                stmt = (EmptyStatementSyntax) NoRtFuncs(stmt, "FIELD");
            }
            context.Put(stmt);
            return;
        }
        public override void EnterFielddecl([NotNull] XP.FielddeclContext context)
        {
            // register field names with current entity
            // so we can check for the field name in the ExitNameExpr method
            string Alias = "";
            if (context.Alias != null)
                Alias = context.Alias.GetText();
            foreach (var field in context._Fields)
            {
                CurrentEntity.Data.AddField(field.Id.GetText(), Alias, true);
            }
        }

        public override void ExitAliasedExpr([NotNull] XP.AliasedExprContext context)
        {
            // CUSTOMER->(<Expression>)
            // translate to 
            // pushWorkarea(CUSTOMER)
            // expr2
            // popWorkarea()
            var exprs = _pool.Allocate<ExpressionSyntax>();
            ExpressionSyntax expr;
            expr = GenerateLiteral(context.Id.GetText());
            var args = MakeArgumentList(MakeArgument(expr));
            expr = GenerateMethodCall("global::VulcanRTFuncs.Functions.pushWorkarea", args);
            exprs.Add(expr);
            exprs.Add(context.Expr.Get<ExpressionSyntax>());
            expr = GenerateMethodCall("global::VulcanRTFuncs.Functions.popWorkarea", EmptyArgumentList());
            exprs.Add(expr);
            context.Put(exprs.ToListNode());
            _pool.Free(exprs);

        }

        public override void ExitAliasedField([NotNull] XP.AliasedFieldContext context)
        {
            //_FIELD->NAME, CUSTOMER-NAME, _FIELD->CUSTOMER->NAME
            // translate to either __FieldGetWa(cArea,cField)
            // or __FieldGet(cField)
            if (!_options.VulcanRTFuncsIncluded)
            {
                context.Put(NoRtFuncs(GenerateLiteral("alias"), "ALIAS(->) operator"));
                return;
            }
            ExpressionSyntax expr;
            string alias = context.Alias?.GetText();
            string field = context.Field.GetText();
            if (!String.IsNullOrEmpty(alias) && alias.ToUpper() == "M") {
                // M->FIELD
                expr = GenerateMemVarGet(field);
            } else {
                expr = GenerateFieldGet(alias, field);
            }
            context.Put(expr);
            return;
        }
        public override void ExitExtendedaliasExpr([NotNull] XP.ExtendedaliasExprContext context)
        {
            // (e1) -> ID
            // or
            // (e1) -> (e2)
            // first is translated to 
            // __FieldGetWa(expr, ID)
            // second is translated to 
            // pushWorkarea(expr1)
            // expr2
            // popWorkarea()
            if (!_options.VulcanRTFuncsIncluded) {
                context.Put(NoRtFuncs(GenerateLiteral("alias"), "ALIAS(->) operator"));
                return;
            }
            if (context.Id != null) {
                string method = "global::VulcanRTFuncs.Functions.__FieldGetWa";
                var arg1 = MakeArgument(context.Alias.Get<ExpressionSyntax>());
                var arg2 = MakeArgument(GenerateLiteral(context.Id.GetText()));
                var args = MakeArgumentList(arg1, arg2);
                context.Put(GenerateMethodCall(method, args));
            } else {
                var exprs = _pool.Allocate<ExpressionSyntax>();
                ExpressionSyntax expr;
                expr = context.Alias.Get<ExpressionSyntax>();
                var args = MakeArgumentList(MakeArgument(expr));
                expr = GenerateMethodCall("global::VulcanRTFuncs.Functions.pushWorkarea", args);
                exprs.Add(expr);
                exprs.Add(context.Expr.Get<ExpressionSyntax>());
                expr = GenerateMethodCall("global::VulcanRTFuncs.Functions.popWorkarea", EmptyArgumentList());
                exprs.Add(expr);
                context.Put(exprs.ToListNode());
                _pool.Free(exprs);
            }
        }
        public override void ExitMacro([NotNull] XP.MacroContext context)
        {
            if (!_options.VulcanRTFuncsIncluded) {
                context.Put(NoRtFuncs(GenerateLiteral("macro"), "MACRO Compiler"));
                return;
            }
            ExpressionSyntax expr;
            if (context.Id != null) {
                expr = context.Id.Get<IdentifierNameSyntax>();
            } else { 
                expr = context.Expr.Get<ExpressionSyntax>();
            }
            var args = MakeArgumentList(MakeArgument(expr));
            expr = GenerateMethodCall("global::VulcanRTFuncs.Functions.Evaluate", args);
            context.Put(expr);
            return;
        }
    }
}