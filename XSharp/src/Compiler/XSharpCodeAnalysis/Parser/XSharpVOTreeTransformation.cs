/*
   Copyright 2016 XSharp B.V.

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
        private TypeSyntax _stringType;

        private ArrayTypeSyntax arrayOfUsual = null;
        private ArrayTypeSyntax arrayOfString = null;

        public XSharpVOTreeTransformation(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool,
            ContextAwareSyntax syntaxFactory, string fileName) :
            base(parser, options, pool, syntaxFactory, fileName)
        {
            _usualType = GenerateQualifiedName("global::Vulcan.__Usual");
            _floatType = GenerateQualifiedName("global::Vulcan.__VOFloat");
            _dateType = GenerateQualifiedName("global::Vulcan.__VODate");
            _arrayType = GenerateQualifiedName("global::Vulcan.__Array");
            _symbolType = GenerateQualifiedName("global::Vulcan.__Symbol");
            _pszType = GenerateQualifiedName("global::Vulcan.__Psz");
            _codeblockType = GenerateQualifiedName("global::Vulcan.Codeblock");
            _stringType = _syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.StringKeyword));

            // calculate the global class name;
            string name = options.CommandLineArguments.CompilationOptions.ModuleName;
            string firstSource = options.CommandLineArguments.SourceFiles.FirstOrDefault().Path;
            if (String.IsNullOrEmpty(name))
            {
                name = firstSource;
            }

            if (!String.IsNullOrEmpty(name))
            {
                string filename = PathUtilities.GetFileName(name);
                filename = PathUtilities.RemoveExtension(filename);
                filename = filename.Replace('.', '_');
                OutputKind kind = options.CommandLineArguments.CompilationOptions.OutputKind;
                if (kind.GetDefaultExtension().ToLower() == ".exe")
                    GlobalClassName = filename + ".Exe.Functions";
                else
                    GlobalClassName = filename + ".Functions";
            }
            else
            {
                GlobalClassName = XSharpGlobalClassName;
            }
            // regenerate default tree with new name
            //CurrentGlobalClassName = GlobalClassName;
            //DefaultXSharpSyntaxTree = GetDefaultTree();
        }

        private void InitializeArrayTypes()
        {
            if (arrayOfUsual == null || arrayOfString == null)
            {
                var emptysizes = _pool.AllocateSeparated<ExpressionSyntax>();
                emptysizes.Add(_syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MakeToken(SyntaxKind.OmittedArraySizeExpressionToken)));
                var emptyrank = _syntaxFactory.ArrayRankSpecifier(
                                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                                emptysizes,
                                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken));
                _pool.Free(emptysizes);
                arrayOfUsual = _syntaxFactory.ArrayType(_usualType, emptyrank);
                arrayOfString = _syntaxFactory.ArrayType(_stringType, emptyrank);
            }
        }
        private SyntaxTree GenerateDefaultSyntaxTree()
        {
            GlobalEntities.Members.Add(GenerateGlobalClass(GlobalClassName, false));


            // Add global attributes

            var arguments = _pool.AllocateSeparated<AttributeArgumentSyntax>();
            var attributes = _pool.AllocateSeparated<AttributeSyntax>();
            // VulcanClassLibrary
            arguments.Add(_syntaxFactory.AttributeArgument(null, null, GenerateLiteral(GlobalClassName)));
            arguments.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
            arguments.Add(_syntaxFactory.AttributeArgument(null, null, GenerateLiteral(_options.DefaultNamespace)));

            attributes.Add(_syntaxFactory.Attribute(
                name: GenerateQualifiedName("global::Vulcan.Internal.VulcanClassLibraryAttribute"),
                argumentList: _syntaxFactory.AttributeArgumentList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    arguments,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))));
            arguments.Clear();
            // VulcanVersion
            arguments.Add(_syntaxFactory.AttributeArgument(null, null, GenerateLiteral("X# " + global::XSharp.Constants.Version)));
            attributes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
            attributes.Add(_syntaxFactory.Attribute(
                name: GenerateQualifiedName("global::Vulcan.Internal.VulcanCompilerVersion"),
                argumentList: _syntaxFactory.AttributeArgumentList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    arguments,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))));
            var target = _syntaxFactory.AttributeTargetSpecifier(SyntaxFactory.Identifier("assembly"), SyntaxFactory.MakeToken(SyntaxKind.ColonToken));
            var attrlist = _syntaxFactory.AttributeList(
                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                target,
                attributes,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken));
            GlobalEntities.Attributes.Add(attrlist);
            _pool.Free(arguments);
            _pool.Free(attributes);

            var eof = SyntaxFactory.Token(SyntaxKind.EndOfFileToken);
            return CSharpSyntaxTree.Create(
                (Syntax.CompilationUnitSyntax)_syntaxFactory.CompilationUnit(
                    GlobalEntities.Externs, GlobalEntities.Usings, GlobalEntities.Attributes, GlobalEntities.Members, eof).CreateRed());
        }
        public static SyntaxTree DefaultVOSyntaxTree(CSharpParseOptions options)
        {
            var t = new XSharpVOTreeTransformation(null, options, new SyntaxListPool(), new ContextAwareSyntax(new SyntaxFactoryContext()), "");
            return t.GenerateDefaultSyntaxTree();
        }

        public static string VOGlobalClassName(CSharpParseOptions options)
        {
            var t = new XSharpVOTreeTransformation(null, options, new SyntaxListPool(), new ContextAwareSyntax(new SyntaxFactoryContext()), "");
            return t.GlobalClassName;
        }

        internal InitializerExpressionSyntax MakeArrayInitializer(SeparatedSyntaxList<ExpressionSyntax> exprs) {
            return _syntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression,
                                              SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                                              exprs,
                                              SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
         }

        internal InitializerExpressionSyntax MakeArrayInitializer(ExpressionSyntax expr) {
            return MakeArrayInitializer(MakeSeparatedList<ExpressionSyntax>(expr));
         }


        internal ExpressionSyntax GenerateMemVarPut(string memvar, ExpressionSyntax right)
        {
            string method = "global::VulcanRTFuncs.Functions.__MemVarPut";
            var arg1 = MakeArgument(GenerateLiteral(memvar));
            var arg2 = MakeArgument(right);
            var args = MakeArgumentList(arg1, arg2);
            var expr = GenerateMethodCall(method, args);
            expr = (ExpressionSyntax)NotInDialect(expr, "MEMVAR");
            //todo: Implement MemVarPut
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
            return expr;
        }

        private ExpressionSyntax GenerateNIL()
        {
            return MakeDefault(_usualType);
        }

        private void Check4ClipperCC(XP.IEntityContext context,
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

        private bool NeedsReturn(IList<XP.StatementContext> stmts)
        {
            // This code checks only the last statement. When there is a return or throw
            // on another line then the system will report 'Unreachable code' anyway.
            if (stmts.Count == 0)
                    return true;
            var stmt = stmts.Last();
            if (stmt is XP.ReturnStmtContext )
            {
                return false;
            }
            if (stmt is XP.JumpStmtContext )
            {
                var jmpstmt = stmt as XP.JumpStmtContext;
                if (jmpstmt.Key.Type == XP.THROW)
                    return false;
            }
            if (stmt is XP.IfStmtContext)
            {
                var ifstmt = stmt as XP.IfStmtContext;
                var ifelsestmt = ifstmt.IfStmt;
                var elsestmt = ifelsestmt?.ElseBlock;
                // The first ifelsestmt should always have a value, but better safe than sorry
                // process to the end of the list
                // when there is no else, then we need a break
                // otherwise process every statement list
                while (ifelsestmt != null)                     //
                {
                    if (NeedsReturn(ifelsestmt.StmtBlk._Stmts))
                    {
                        return true;
                    }
                    elsestmt = ifelsestmt.ElseBlock;
                    ifelsestmt = ifelsestmt.ElseIfBlock;
                }
                // No Else, so there is at least one block that does not end with a RETURN etc
                if (elsestmt == null || elsestmt._Stmts?.Count == 0)
                {
                    return true;
                }
                else
                {
                    return NeedsReturn(elsestmt._Stmts);
                }
            }
            if (stmt is XP.CaseStmtContext)
            {
                var docasestmt = stmt as XP.CaseStmtContext;
                var casestmt = docasestmt.CaseStmt;     // CaseBlock, there may be no blocks at all.
                int lastkey = XP.CASE;
                while (casestmt != null)                // otherwise is also a CaseBlock stored in NextCase
                {
                    if (NeedsReturn(casestmt.StmtBlk._Stmts))
                        return true;
                    lastkey = casestmt.Key.Type;
                    casestmt = casestmt.NextCase;
                }
                if (lastkey == XP.CASE) // There is no otherwise
                    return true;
                return false;           // all branches end with a return  statement
            }
            if (stmt is XP.SwitchStmtContext)
            {
                var swstmt = stmt as XP.SwitchStmtContext;
                bool hasdefault = false;
                foreach (var swBlock in swstmt._SwitchBlock)
                {
                    if (swBlock.StmtBlk._Stmts.Count > 0 && NeedsReturn(swBlock.StmtBlk._Stmts))
                       return true;
                    if (swBlock.Key.Type != XP.CASE)
                        hasdefault = true;
                }
                if (! hasdefault)
                    return true;
                return false;           // all branches end with a return statement
            }

            if (stmt is XP.BlockStmtContext)
            {
                var blockstmt = stmt as XP.BlockStmtContext;
                return NeedsReturn(blockstmt.StmtBlk._Stmts);
            }
            if (stmt is XP.ILoopStmtContext)        // For, Foreach, While, Repeat
            {
                var blockstmt = stmt as XP.ILoopStmtContext;
                return NeedsReturn(blockstmt.Statements._Stmts);
            }

            return true;
        }

        private ExpressionSyntax GetReturnExpression(TypeSyntax returnType)
        {
            ExpressionSyntax result = null;
            if (returnType is PredefinedTypeSyntax)
            {
                var pretype = returnType as PredefinedTypeSyntax;
                switch (pretype.keyword.Kind)
                {
                    case SyntaxKind.VoidKeyword:
                        return null;
                    case SyntaxKind.SByteKeyword:
                    case SyntaxKind.ShortKeyword:
                    case SyntaxKind.IntKeyword:
                    case SyntaxKind.LongKeyword:
                    case SyntaxKind.ByteKeyword:
                    case SyntaxKind.UShortKeyword:
                    case SyntaxKind.UIntKeyword:
                    case SyntaxKind.ULongKeyword:
                    case SyntaxKind.DoubleKeyword:
                    case SyntaxKind.FloatKeyword:
                    case SyntaxKind.DecimalKeyword:
                        result = GenerateLiteral(0);
                        break;
                    case SyntaxKind.ObjectKeyword:
                    case SyntaxKind.StringKeyword:
                    default:
                        result = _syntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression, SyntaxFactory.MakeToken(SyntaxKind.NullKeyword));
                        break;
                }
            }
            else
            {
                if (returnType == _usualType)
                {
                    result = GenerateNIL();
                }
                else if (returnType == _floatType)
                {
                    // Ignore float literals for now.
                    result = GenerateLiteral(0);
                }
                else if (returnType == _dateType)
                {
                    result = GenerateMethodCall("global::Vulcan.__VODate.NullDate", EmptyArgumentList());
                }
                else if (returnType == _pszType || returnType == _symbolType)
                {
                    result = CreateObject(returnType, MakeArgumentList(MakeArgument(GenerateLiteral(""))), null);
                }
                else
                {
                    // _arrayType , _codeblockType
                    // other reference types all use the default null literal
                    result = _syntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression, SyntaxFactory.MakeToken(SyntaxKind.NullKeyword));
                }
            }
            return result;
        }

        protected override BlockSyntax AddMissingReturnStatement(BlockSyntax body, XP.StatementBlockContext stmtBlock, TypeSyntax returnType)
        {
            if (_options.VOAllowMissingReturns && NeedsReturn(stmtBlock._Stmts))
            {
                var result = GetReturnExpression(returnType);
                if (result != null) // this happens for the Void Type
                {
                    var statements = _pool.Allocate<StatementSyntax>();
                    statements.AddRange(body.Statements);
                    statements.Add(_syntaxFactory.ReturnStatement(SyntaxFactory.MakeToken(SyntaxKind.ReturnKeyword),
                        result, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));

                    body = MakeBlock(statements).WithAdditionalDiagnostics(
                                new SyntaxDiagnosticInfo(ErrorCode.WRN_MissingReturnStatement));
                    _pool.Free(statements);

                }
            }
            return body;
        }

        protected StatementSyntax CachedIfBlock = null;
        protected StatementSyntax GetClipperCallingConventionIfStatement()
        {
            if(CachedIfBlock != null)
                return CachedIfBlock;
            ExpressionSyntax assignExpr;
            ExpressionSyntax ifExpr;
            StatementSyntax exprStmt;
            // This method generates the common block that is used for all Clipper calling convention methods
            // to retrieve the PCount
            // the pseudo code for that is

            // IF Xs$Args != NULL
            //      Xs$PCount := Xs$Args.Length
            // ENDIF

            InitializeArrayTypes();
            var blockstmts = _pool.Allocate<StatementSyntax>();
            // Xs$PCount = Xs$Args:Length
            assignExpr = MakeSimpleAssignment(
                    GenerateSimpleName(ClipperPCount),
                                _syntaxFactory.MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    GenerateSimpleName(ClipperArgs),
                                     SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                                    GenerateSimpleName("Length")));

            exprStmt = _syntaxFactory.ExpressionStatement(assignExpr, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            blockstmts.Add(exprStmt);
            // Xs$Args != NULL

            ifExpr = _syntaxFactory.BinaryExpression(
                    SyntaxKind.NotEqualsExpression,
                    GenerateSimpleName(ClipperArgs),
                    SyntaxFactory.MakeToken(SyntaxKind.ExclamationEqualsToken),
                    SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression,
                        SyntaxFactory.MakeToken(SyntaxKind.NullKeyword)));
            // if XsArgs != NULL ......
            CachedIfBlock = GenerateIfStatement(ifExpr, MakeBlock(blockstmts));
            _pool.Free(blockstmts);
            return CachedIfBlock;
        }
        protected override void ImplementClipperAndPSZ(XP.IEntityContext context,
            ref  SyntaxList<AttributeListSyntax> attributes, ref ParameterListSyntax parameters, ref BlockSyntax body,
            ref TypeSyntax dataType )
        {
            InitializeArrayTypes();
            if (context.Data.HasTypedParameter && context.Data.HasClipperCallingConvention)
            {
                parameters = parameters.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_TypedParametersWithClipperCallingConvention));
                return;
            }
            if (body != null && (context.Data.HasClipperCallingConvention || context.Data.UsesPSZ ))
            {
                var stmts = _pool.Allocate<StatementSyntax>();

                if(context.Data.HasClipperCallingConvention)
                {
                    ExpressionSyntax assignExpr;
                    ExpressionSyntax ifExpr;
                    StatementSyntax exprStmt;
                    if(context.Data.UsesPCount || parameters.Parameters.Count > 0  || context.Data.UsesGetMParam)
                    {
                        // When the PCount function is used then we need to introduce a helper variable
                        // and also when there are defined parameters

                        // VAR Xs$PCount  := 0
                        stmts.Add(GenerateLocalDecl(ClipperPCount, _impliedType, GenerateLiteral("0", 0)));
                        // Other common statements are cached
                        stmts.Add(GetClipperCallingConventionIfStatement());
                    }

                    // Assuming the parameters are called oPar1 and oPar2 then the following code is generated
                    // LOCAL oPar1 := NIL as USUAL
                    // LOCAL oPar2 := NIL as USUAL
                    // IF Xs$PCount > 0
                    //   oPar1 := Xs$Args[0]
                    //   IF Xs$PCount > 1
                    //       oPar2 := Xs$Args[1]
                    //    ENDIF
                    // ENDIF
                    var paramNames = new List<String>();
                    if(parameters.Parameters.Count > 0) {
                        var nilExpr = GenerateNIL();
                        for(int i = 0; i < parameters.Parameters.Count; i++) {
                            ParameterSyntax parm = parameters.Parameters[i];
                            string name = parm.Identifier.Text;
                            paramNames.Add(name);
                            stmts.Add(GenerateLocalDecl(name, _usualType, nilExpr));
                        }
                        int iAdd = 1;
                        if(_options.ArrayZero)
                            iAdd = 0;
                        StatementSyntax LastStmt = null;
                        for(int i = paramNames.Count - 1; i >= 0; i--) {
                            var name = paramNames[i];
                            var indices = _pool.AllocateSeparated<ArgumentSyntax>();
                            indices.Add(MakeArgument(GenerateLiteral("", i + iAdd)));
                            assignExpr = MakeSimpleAssignment(GenerateSimpleName(name),
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
                            if(LastStmt != null) {
                                var ifbodystmts = _pool.Allocate<StatementSyntax>();
                                ifbodystmts.Add(exprStmt);
                                ifbodystmts.Add(LastStmt);
                                ifBody = MakeBlock(ifbodystmts);
                                _pool.Free(ifbodystmts);
                            } else {
                                ifBody = exprStmt;
                            }
                            LastStmt = GenerateIfStatement(ifExpr, ifBody);
                        }
                        stmts.Add(LastStmt);
                    }
                    // Now Change argument to X$Args PARAMS USUAL[]
                    SyntaxListBuilder modifiers = _pool.Allocate();
                    modifiers.Add(SyntaxFactory.MakeToken(SyntaxKind.ParamsKeyword));
                    var attrs = _pool.Allocate<AttributeListSyntax>();
                    GenerateAttributeList(attrs, CompilerGenerated);
                    var par = _syntaxFactory.Parameter(
                                    attrs,
                                    modifiers.ToList(),
                                    type: arrayOfUsual,
                                    identifier: SyntaxFactory.Identifier(ClipperArgs),
                                    @default: null );
                    parameters = _syntaxFactory.ParameterList(SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                        MakeSeparatedList<ParameterSyntax>(par),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
                    // Finally add ClipperCallingConventionAttribute to the method
                    // using the names from the paramNames list
                    // [Vulcan.Internal.ClipperCallingConvention(new string[] { "a", "b" })]
                    // make sure that existing attributes are not removed!
                    attrs.Clear();
                    attrs.AddRange(attributes); // Copy existing attributes
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
                                            arrayOfString,
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
            if(context.Data.HasMissingReturnType && context.Data.MustHaveReturnType) {
                dataType = _getMissingType();
            }
        }

        public override void ExitClsctor([NotNull] XP.ClsctorContext context)
        {
            // This method does NOT call the parent. Code is duplicated here.

            if(context.Modifiers?._EXTERN != null) {
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
                if (context.Chain != null )
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
                if(CurrentEntity.Data.HasTypedParameter && _options.VOUntypedAllowed)
                    type = _usualType;
                 else
                    type = _getMissingType();
            }
            return type;
        }


        protected override TypeSyntax _getMissingType()
        {
            TypeSyntax varType;
            if (_options.VOUntypedAllowed)
                varType = _usualType;
            else
                varType = MissingType();
            return varType;
        }

        public override void EnterMethod([NotNull] XP.MethodContext context) {
            base.EnterMethod(context);
            if(context.T.Token.Type != XP.METHOD ) {
                context.Data.HasClipperCallingConvention = false;
                context.Data.HasTypedParameter = true;          // this will set all missing types to USUAL
            }
            else
                Check4ClipperCC(context, context.ParamList, context.CallingConvention?.Convention, context.Type, context.T.Token.Type != XP.ASSIGN);
        }


        public override void EnterXbasedecl([NotNull] XP.XbasedeclContext context)
        {
            // declare memvars
            context.SetSequencePoint(context.end);
            if (context.T.Type == XP.MEMVAR)
            {
                foreach (var memvar in context._Vars)
                {
                    CurrentEntity.Data.AddField(memvar.Id.GetText(), "M", false);
                }

            }
        }


        public override void ExitBracketedargument([NotNull] XP.BracketedargumentContext context)
        {
            if (context.Expr == null)
            {
                context.Put(MakeArgument(GenerateNIL()));
                return;
            }
            base.ExitBracketedargument(context);
        }


        public override void ExitArgument([NotNull] XP.ArgumentContext context) {
            if (context.Expr == null) {
                context.Put(MakeArgument(GenerateNIL()));
                return;
            }
            base.ExitArgument(context);
        }

        public override void ExitBinaryExpression([NotNull] XP.BinaryExpressionContext context) {
            if (context.Op.Type == XP.SUBSTR) {
                string method = "global::VulcanRTFuncs.Functions.Instr";
                var argLeft = context.Left.Get<ExpressionSyntax>();
                var argRight = context.Right.Get<ExpressionSyntax>();
                var args = MakeArgumentList(MakeArgument(argLeft), MakeArgument(argRight));
                context.Put(GenerateMethodCall(method, args));
                return;
            }
            else if (context.Op.Type == XP.DIV && _options.VOClipperIntegerDivisions)
            {
                var lhs = MakeCastTo(_usualType, context.Left.Get<ExpressionSyntax>());
                var rhs = MakeCastTo(_usualType, context.Right.Get<ExpressionSyntax>());
                context.Put(_syntaxFactory.BinaryExpression(
                    context.Op.ExpressionKindBinaryOp(),
                    lhs,
                    context.Op.SyntaxOp(),
                    rhs));
                return;
            }
            base.ExitBinaryExpression(context);
        }

        public override void ExitJumpStmt([NotNull] XP.JumpStmtContext context)
        {
			if (context.Key.Type == XP.BREAK)
			{
	            ArgumentListSyntax args;
	            context.SetSequencePoint(context.end);
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
				base.ExitJumpStmt(context);
			}
        }


        public override void ExitSeqStmt([NotNull] XP.SeqStmtContext context)
        {
            // This generates 2 try statements:
            // 1) an inner try that controls the calls to CompilerServices.EnterBeginSequence();
            //    and CompilerServices.ExitBeginSequence();
            // 2) an outer try that has the try - catch - finally from the seq statement itself

            context.SetSequencePoint(context.end);
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
            outerTry = (TryStatementSyntax)CheckForMissingKeyword(context.e, context, outerTry, "END SEQUENCE");

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
            var stmts = _pool.Allocate<StatementSyntax>();
            var catchVar = SyntaxFactory.Identifier(RecoverVarName);
            context.SetSequencePoint(context.end);
            if(context.Id != null) {
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
                    MakeSimpleAssignment(idName,
                       _syntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                       MakeCastTo(GenerateQualifiedName("global::Vulcan.Internal.VulcanWrappedException"),objName),
                       SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                         GenerateSimpleName("Value"))));

                var assign2 = GenerateExpressionStatement(MakeSimpleAssignment(idName, objName));

                var assign3 = GenerateExpressionStatement(MakeSimpleAssignment(
                    idName,MakeCastTo(_usualType,GenerateMethodCall("global::Vulcan.Error._WrapRawException",
                    MakeArgumentList(MakeArgument(objName))))));

                var assign4 = GenerateExpressionStatement(MakeSimpleAssignment(idName, objName));

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

                stmts.Add(ifstmt);
            }
            else {
                catchVar = null;
            }
            stmts.Add(context.StmtBlock.Get<BlockSyntax>());
            var catchClause = _syntaxFactory.CatchClause(
                SyntaxFactory.MakeToken(SyntaxKind.CatchKeyword),
                _syntaxFactory.CatchDeclaration(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    GenerateQualifiedName("global::System.Exception"),
                    catchVar,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken)),
                    null,
                    MakeBlock(stmts));

            context.Put(catchClause);
            _pool.Free(stmts);
        }
        public override void ExitReturnStmt([NotNull] XP.ReturnStmtContext context)
        {
            context.SetSequencePoint(context.end);
            var expr = context.Expr?.Get<ExpressionSyntax>();
            // when / vo9 is enabled then add missing Expression
            if (expr == null && _options.VOAllowMissingReturns)
            {
                var ent = CurrentEntity;
                if (ent is XP.MethodContext || ent is XP.FunctionContext)
                {
                    ent.Data.HasReturnStatementWithoutValue = true;
                    TypeSyntax dataType;
                    if (ent.Data.HasMissingReturnType)
                    {
                        dataType = _getMissingType();
                    }
                    else
                    {
                        dataType = ent is XP.MethodContext ? ((XP.MethodContext)ent).Type.Get<TypeSyntax>() :
                            dataType = ((XP.FunctionContext)ent).Type.Get<TypeSyntax>();
                    }
                    if (ent is XP.MethodContext && ((XP.MethodContext) ent).T.Token.Type != XP.ASSIGN)
                    {
                        expr = GetReturnExpression(dataType);
                        if (expr != null)   // happens for a void method
                        {
                            expr = expr.WithAdditionalDiagnostics(
                                             new SyntaxDiagnosticInfo(ErrorCode.WRN_MissingReturnValue));
                        }
                    }
                }
            }
            var result = _syntaxFactory.ReturnStatement(SyntaxFactory.MakeToken(SyntaxKind.ReturnKeyword),
                expr,
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            context.Put(result);
        }

        public override void ExitQoutStmt([NotNull] XP.QoutStmtContext context) {
            // Simply generate call to VulcanRTFuncs.Functions.QOut or QQOut
            // and pass list of expressions as argument
            ArgumentSyntax arg;
            ExpressionSyntax expr;
            string methodName;
            context.SetSequencePoint(context.end);
            if(context.Q.Type == XP.QQMARK)
                methodName = "global::VulcanRTFuncs.Functions.QQOut";
            else
                methodName = "global::VulcanRTFuncs.Functions.QOut";
            ArgumentListSyntax args;
            if(context._Exprs != null && context._Exprs.Count > 0) {
                var al = new List<ArgumentSyntax>();
                foreach(var eCtx in context._Exprs) {
                    arg = MakeArgument(eCtx.Get<ExpressionSyntax>());
                    al.Add(arg);
                }
                args = _syntaxFactory.ArgumentList(
                        SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                        MakeSeparatedList<ArgumentSyntax>(al.ToArray()),
                        SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
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



    private bool GenerateSLen(XP.MethodCallContext context) {
        // Pseudo function SLen
        ArgumentListSyntax argList;
        ExpressionSyntax expr;
        if(context.ArgList != null) {
            argList = context.ArgList.Get<ArgumentListSyntax>();
        } else {
            return false;
        }

        expr = MakeCastTo(_stringType, argList.Arguments[0].Expression);

        expr = _syntaxFactory.ConditionalAccessExpression( expr,
                                SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                                _syntaxFactory.MemberBindingExpression(
                                    SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                                    GenerateSimpleName("Length")
                                    ));
        expr = MakeCastTo(_syntaxFactory.PredefinedType(SyntaxFactory.MakeToken(SyntaxKind.UIntKeyword)), expr);

        context.Put(expr);
        return true;
    }
    private bool GenerateString2Psz(XP.MethodCallContext context, string name) {
        // this will only happen when the VO or Vulcan dialect is selected, so we can use the psz type here
        // and the reference to the String2Psz() in the Vulcan Runtime.
        ArgumentListSyntax argList;
        ExpressionSyntax expr;
        if(context.ArgList != null) {
            argList = context.ArgList.Get<ArgumentListSyntax>();
        } else {
            argList = EmptyArgumentList();
        }
        if(CurrentEntity != null ) {
            // Add reference to compiler generated List<IntPtr> to the argList
            if (argList.Arguments.Count != 1) {
                expr = GenerateNIL().WithAdditionalDiagnostics(
                    new SyntaxDiagnosticInfo(ErrorCode.ERR_BadArgCount, name, argList.Arguments.Count));
            }
            else {
                CurrentEntity.Data.UsesPSZ = true;
                NameSyntax pszlist = GenerateSimpleName(VoPszList);
                expr = argList.Arguments[0].Expression;
                argList = MakeArgumentList(MakeArgument(expr), MakeArgument(pszlist));
                expr = GenerateMethodCall("global::Vulcan.Internal.CompilerServices.String2Psz", argList);
                var args = MakeArgumentList(MakeArgument(expr));
                expr = CreateObject(this._pszType, args, null);
            }
            context.Put(expr);
            return true;
        }
        else {
            return false;
        }
    }

    private bool GenerateChr(XP.MethodCallContext context) {
        // this will only happen when the VO or Vulcan dialect is selected, so we can use the psz type here
        // and the reference to the String2Psz() in the Vulcan Runtime.
        ArgumentListSyntax argList;
        if(context.ArgList != null) {
            argList = context.ArgList.Get<ArgumentListSyntax>();
        } else {
            argList = EmptyArgumentList();
        }
        context.Put(GenerateMethodCall("global::VulcanRTFuncs.Functions.Chr", argList));
        return true;
    }

    private bool GenerateClipCallFunc(XP.MethodCallContext context, string name) {
        ArgumentListSyntax argList;
        ExpressionSyntax expr;
        if(context.ArgList != null) {
            argList = context.ArgList.Get<ArgumentListSyntax>();
        } else {
            argList = EmptyArgumentList();
        }
        if(name == "PCOUNT") {
            expr = GenerateSimpleName(ClipperPCount);
            if(argList.Arguments.Count != 0) {
                expr = expr.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_BadArgCount,name, argList.Arguments.Count));
            }
            context.Put(expr);
            CurrentEntity.Data.UsesPCount = true;
            return true;
        } else {
            if(argList.Arguments.Count != 1) {
                expr = GenerateNIL().WithAdditionalDiagnostics(
                    new SyntaxDiagnosticInfo(ErrorCode.ERR_BadArgCount, name, argList.Arguments.Count));
                context.Put(expr);
                return true;
            }

            // _GETMPARAM or _GETFPARAM
            CurrentEntity.Data.UsesGetMParam = true;
            var indices = _pool.AllocateSeparated<ArgumentSyntax>();
            indices.Add(MakeArgument(argList.Arguments[0].Expression));

            expr = _syntaxFactory.ElementAccessExpression(
            GenerateSimpleName(ClipperArgs),
            _syntaxFactory.BracketedArgumentList(
                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                indices,
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)));
            context.Put(expr);
            return true;
        }
    }
    public override void ExitMethodCall([NotNull] XP.MethodCallContext context) {
        var expr = context.Expr.Get<ExpressionSyntax>();
        string name = null;
        if(expr is IdentifierNameSyntax) {
            // Intrinsic functions that depend on Vulcan types
            IdentifierNameSyntax ins = expr as IdentifierNameSyntax;
            name = ins.Identifier.Text.ToUpper();
            switch(name) {
                case "SLEN":
                    if (GenerateSLen(context))
                        return;
                    break;
                case "STRING2PSZ":
                case "CAST2PSZ":
                    if (GenerateString2Psz(context,name))
                        return;
                    break;
                case "PCOUNT":
                case "_GETMPARAM":
                case "_GETFPARAM":
                    if(CurrentEntity.Data.HasClipperCallingConvention) {
                        if(GenerateClipCallFunc(context, name))
                            return;
                    }
                    expr = GenerateLiteral("", 0).WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(ErrorCode.ERR_OnlySupportedForClipperCallingConvention, ins.Identifier.Text));
                    context.Put(expr);
                    return;
                case "_CHR":
                    if(GenerateChr(context))
                        return;
                    break;

                default:
                    break;
            }
        } else if(expr is ThisExpressionSyntax || expr is BaseExpressionSyntax) {
            // SUPER(..) and SELF(..)
            expr = _syntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expr, SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                _syntaxFactory.IdentifierName(SyntaxFactory.Identifier(".ctor")));
            ArgumentListSyntax argList;
            if(context.ArgList != null) {
                argList = context.ArgList.Get<ArgumentListSyntax>();
            } else {
                argList = EmptyArgumentList();
            }
            context.Put(_syntaxFactory.InvocationExpression(expr, argList));
            return;
        }
        // all other method names or syntaxes
        base.ExitMethodCall(context);
        return;
    }


    public override void EnterFunction([NotNull] XP.FunctionContext context) {
        base.EnterFunction(context);
        Check4ClipperCC(context, context.ParamList, context.CallingConvention?.Convention, context.Type, true);
    }

    public override void EnterProcedure([NotNull] XP.ProcedureContext context) {
        base.EnterProcedure(context);
        Check4ClipperCC(context, context.ParamList, context.CallingConvention?.Convention, null, false);
    }

    public override void EnterClsctor([NotNull] XP.ClsctorContext context) {
        base.EnterClsctor(context);
        Check4ClipperCC(context, context.ParamList, context.CallingConvention?.Convention, null, false);
    }

    public override void EnterVodll([NotNull] XP.VodllContext context) {
        base.EnterVodll(context);
       Check4ClipperCC(context, context.ParamList, context.CallingConvention?.Cc, context.Type, true);
    }

    public override void ExitNameExpression([NotNull] XP.NameExpressionContext context)
    {
        // Check to see if the name is a field or Memvar, registered with the FIELD or MemVar statement
        string Name = context.Name.GetText();
        ExpressionSyntax expr = context.Name.Get<NameSyntax>();
        MemVarFieldInfo fieldInfo= null;
        if (CurrentEntity != null)
            fieldInfo = CurrentEntity.Data.GetField(Name);
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
        if(_options.VOCompatibleIIF) {
            ExpressionSyntax left = MakeCastTo(_usualType, context.TrueExpr.Get<ExpressionSyntax>());
            ExpressionSyntax right = MakeCastTo(_usualType, context.FalseExpr.Get<ExpressionSyntax>());

            context.Put(_syntaxFactory.ConditionalExpression(
                context.Cond.Get<ExpressionSyntax>(),
                SyntaxFactory.MakeToken(SyntaxKind.QuestionToken),
                left,
                SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                right));
        } else
            base.ExitIif(context);
    }

    public override void ExitLiteralArray([NotNull] XP.LiteralArrayContext context)
    {
        ExpressionSyntax expr = null;
        // detect typed arrays.
        // <LONG> {...} indicates an array of type LONG -> Handled by base class
        if(context.Type != null) {
            base.ExitLiteralArray(context);
            return;
        }
        // when no type is specified and the dialect VO or Vulcan the type is USUAL
        TypeSyntax type = _usualType;
        SeparatedSyntaxList<ExpressionSyntax> exprs ;
        if((context._Elements?.Count ?? 0) > 0) {
            // Literal array with optional elements.
            // ExitArrayElement has left the CsNode empty for missing Expressions
            var l = _pool.AllocateSeparated<ExpressionSyntax>();
            foreach(var item in context._Elements){
                if(l.Count > 0)
                    l.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                if(item.Expr != null)
                    l.Add(item.Expr.Get<ExpressionSyntax>());
                else
                    l.Add(GenerateNIL());

            }
            exprs = l.ToList();
            _pool.Free(l);
        }
        else {
            exprs = default(SeparatedSyntaxList<ExpressionSyntax>);
        }
        var initializer = MakeArrayInitializer(exprs);
        expr = _syntaxFactory.ArrayCreationExpression(SyntaxFactory.MakeToken(SyntaxKind.NewKeyword),
            _syntaxFactory.ArrayType(type,
            MakeList(_syntaxFactory.ArrayRankSpecifier(
                SyntaxFactory.MakeToken(SyntaxKind.OpenBracketToken),
                MakeSeparatedList<ExpressionSyntax>(
                    _syntaxFactory.OmittedArraySizeExpression(SyntaxFactory.MakeToken(SyntaxKind.OmittedArraySizeExpressionToken))),
                SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken)))),
            initializer);
        context.Put<ExpressionSyntax>(CreateObject(_arrayType, MakeArgumentList(MakeArgument(expr)), null));

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
                    // check to see if the token contains an 'S', 'D' or 'M'. In that case leave as is, since the user has specified
                    // single, double or decimal
                    var text = context.Token.Text;
                    if (text.IndexOfAny("sdmSDM".ToCharArray()) == -1)
                    {
                        args = text.Split('.');
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
        context.Put(type);
    }
    public override void ExitFielddecl([NotNull] XP.FielddeclContext context)
    {
        var stmt = _syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
        context.SetSequencePoint();
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


    public ExpressionSyntax GenerateAliasedExpression( ExpressionSyntax wa, ExpressionSyntax expr)
    {
            // CUSTOMER->(<Expression>)
            //
            // translate to :
            //
            // {||
            //   __pushWorkarea(CUSTOMER)
            //   try
            //     return expr2
            //   finally
            //     __popWorkarea()
            //   end
            // }:Eval()

            return _syntaxFactory.InvocationExpression(
            _syntaxFactory.MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                MakeCastTo(_codeblockType,
                    _syntaxFactory.ParenthesizedLambdaExpression(
                        asyncKeyword: null,
                        parameterList: EmptyParameterList(),
                        arrowToken: SyntaxFactory.MakeToken(SyntaxKind.EqualsGreaterThanToken),
                        body: MakeBlock(MakeList<StatementSyntax>(
                            GenerateExpressionStatement(GenerateMethodCall("global::VulcanRTFuncs.Functions.__pushWorkarea", MakeArgumentList(MakeArgument(wa)))),
                            _syntaxFactory.TryStatement(SyntaxFactory.MakeToken(SyntaxKind.TryKeyword),
                                MakeBlock(MakeList<StatementSyntax>(
                                    _syntaxFactory.ReturnStatement(
                                        SyntaxFactory.MakeToken(SyntaxKind.ReturnKeyword),
                                        expr,
                                        SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken))
                                    )),
                                EmptyList<CatchClauseSyntax>(),
                                _syntaxFactory.FinallyClause(SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword),
                                    MakeBlock(MakeList<StatementSyntax>(
                                        GenerateExpressionStatement(GenerateMethodCall("global::VulcanRTFuncs.Functions.__popWorkarea", EmptyArgumentList()))
                                        ))
                                    )
                                )
                            ))
                        )
                    ),
                SyntaxFactory.MakeToken(SyntaxKind.DotToken),
                _syntaxFactory.IdentifierName(SyntaxFactory.MakeIdentifier("Eval"))
                ),
            EmptyArgumentList());
    }

        public override void ExitAliasedField([NotNull] XP.AliasedFieldContext context)
        {
            //_FIELD->NAME, CUSTOMER-NAME, _FIELD->CUSTOMER->NAME
            // translate to either __FieldGetWa(cArea,cField)
            // or __FieldGet(cField)
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
        public override void ExitAliasedExpr([NotNull] XP.AliasedExprContext context)
        {
            ExpressionSyntax alias = context.Alias.Get<ExpressionSyntax>();
            if (context.Id != null)
            {
                if (context.Expr is XP.PrimaryExpressionContext &&
                    ((XP.PrimaryExpressionContext) context.Expr).Expr is XP.NameExpressionContext)
                {
                    string field = context.Expr.GetText();
                    context.Put(GenerateFieldGet(context.Id.GetText(), field));
                    return;
                }
                alias = GenerateLiteral(context.Id.GetText());
            }
            ExpressionSyntax expr =
                GenerateAliasedExpression(
                        alias,     // workarea
                        context.Expr.Get<ExpressionSyntax>()        // expression
                    );
            context.Put(expr);
        }
        public override void ExitMacro([NotNull] XP.MacroContext context)
        {
            ExpressionSyntax expr;
            if (context.Id != null) {
                expr = context.Id.Get<IdentifierNameSyntax>();
            } else {
                expr = context.Expr.Get<ExpressionSyntax>();
            }
            var args = MakeArgumentList(MakeArgument(expr));
            context.SetSequencePoint();
            expr = GenerateMethodCall("global::VulcanRTFuncs.Functions.Evaluate", args);
            context.Put(expr);
            return;
        }
        private bool voStructHasDim;

        public override void EnterVostruct([NotNull] XP.VostructContext context)
        {
            voStructHasDim = false;
        }

        public override void ExitVostruct([NotNull] XP.VostructContext context)
        {
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            if (voStructHasDim)
            {
                var modBuilder = _pool.Allocate();
                modBuilder.AddRange(mods);
                modBuilder.Add(SyntaxFactory.MakeToken(SyntaxKind.UnsafeKeyword));
                mods = modBuilder.ToTokenList();
            }
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
                                        _syntaxFactory.AttributeArgument(null, null, GenerateQualifiedName("global::System.Runtime.InteropServices.LayoutKind.Sequential")),
                                        _syntaxFactory.AttributeArgument(GenerateNameEquals("Pack"), null,
                                            context.Alignment == null ?
                                                GenerateLiteral("8", 8)
                                                : _syntaxFactory.LiteralExpression(context.Alignment.ExpressionKindLiteral(), context.Alignment.SyntaxLiteralValue(_options)))
                                    ),
                                    closeParenToken: SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))
                                )
                            ),
                        closeBracketToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken))
                    ),
                modifiers: mods,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: null,
                constraintClauses: null,
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: (context._Members?.Count > 0) ? MakeList<MemberDeclarationSyntax>(context._Members) : EmptyList<MemberDeclarationSyntax>(),
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            m.XVoDecl = true;
            if (context.Namespace != null)
            {
                m = AddNameSpaceToMember(context.Namespace, m);
            }
            context.Put(m);
        }


        public override void ExitVostructmember([NotNull] XP.VostructmemberContext context)
        {
            bool isDim = context.Dim != null;
            var varType = context.DataType?.Get<TypeSyntax>() ?? MissingType();
            varType.XVoDecl = true;
            if (context.As?.Type == XP.IS)
            {
                varType.XVoIsDecl = true;
            }
            if (isDim)
                voStructHasDim = true;
            context.Put(_syntaxFactory.FieldDeclaration(
                EmptyList<AttributeListSyntax>(),
                TokenList(SyntaxKind.PublicKeyword, isDim ? SyntaxKind.FixedKeyword : SyntaxKind.None),
                _syntaxFactory.VariableDeclaration(varType,
                    MakeSeparatedList(
                        isDim ? GenerateBuffer(context.Id.Get<SyntaxToken>(), MakeBracketedArgumentList(context.ArraySub._ArrayIndex.Select(e => _syntaxFactory.Argument(null, null, e.Get<ExpressionSyntax>())).ToArray()))
                        : GenerateVariable(context.Id.Get<SyntaxToken>()))),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

        public override void EnterVounion([NotNull] XP.VounionContext context)
        {
            voStructHasDim = false;
        }

        public override void ExitVounion([NotNull] XP.VounionContext context)
        {
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            if (voStructHasDim)
            {
                var modBuilder = _pool.Allocate();
                modBuilder.AddRange(mods);
                modBuilder.Add(SyntaxFactory.MakeToken(SyntaxKind.UnsafeKeyword));
                mods = modBuilder.ToTokenList();
            }
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
                                        _syntaxFactory.AttributeArgument(null, null, GenerateQualifiedName("global::System.Runtime.InteropServices.LayoutKind.Explicit")),
                                        _syntaxFactory.AttributeArgument(GenerateNameEquals("Pack"), null, GenerateLiteral("4", 4))

                                    ),
                                    closeParenToken: SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))
                                )
                            ),
                        closeBracketToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken))
                    ),
                modifiers: mods,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: null,
                constraintClauses: null,
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: (context._Members?.Count > 0) ? MakeList<MemberDeclarationSyntax>(context._Members) : EmptyList<MemberDeclarationSyntax>(),
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            m.XVoDecl = true;
            if (context.Namespace != null)
            {
                m = AddNameSpaceToMember(context.Namespace, m);
            }
            context.Put(m);
        }


        public override void ExitVounionmember([NotNull] XP.VounionmemberContext context)
        {
            bool isDim = context.Dim != null;
            var varType = context.DataType?.Get<TypeSyntax>() ?? MissingType();
            varType.XVoDecl = true;
            if (context.As?.Type == XP.IS)
            {
                varType.XVoIsDecl = true;
            }
            if (isDim)
                voStructHasDim = true;
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
                                        _syntaxFactory.AttributeArgument(null, null,
                                            GenerateLiteral("0", 0)
                                        )
                                    ),
                                    closeParenToken: SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken))
                                )
                            ),
                        closeBracketToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBracketToken))
                    ),
                TokenList(SyntaxKind.PublicKeyword, isDim ? SyntaxKind.FixedKeyword : SyntaxKind.None),
                _syntaxFactory.VariableDeclaration(varType,
                    MakeSeparatedList(
                        isDim ? GenerateBuffer(context.Id.Get<SyntaxToken>(), MakeBracketedArgumentList(context.ArraySub._ArrayIndex.Select(e => _syntaxFactory.Argument(null, null, e.Get<ExpressionSyntax>())).ToArray()))
                        : GenerateVariable(context.Id.Get<SyntaxToken>()))),
                SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }

    }
}