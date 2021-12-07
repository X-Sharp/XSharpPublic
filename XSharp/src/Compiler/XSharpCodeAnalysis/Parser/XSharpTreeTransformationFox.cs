//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

using System;
using System.Linq;
using System.Collections.Generic;
using Roslyn.Utilities; 
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using System.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    using Microsoft.CodeAnalysis.Syntax.InternalSyntax;
    using System.Diagnostics;
    using System.Reflection.Metadata.Ecma335;

    internal class XSharpTreeTransformationFox : XSharpTreeTransformationRT
    {
        protected readonly TypeSyntax _foxarrayType;

        protected override XSharpTreeTransformationCore CreateWalker(XSharpParser parser)
        {
            return new XSharpTreeTransformationFox(parser, _options, _pool, _syntaxFactory, _fileName);
        }

        public XSharpTreeTransformationFox(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool,
                    ContextAwareSyntax syntaxFactory, string fileName) :
                    base(parser, options, pool, syntaxFactory, fileName)
        {
            _foxarrayType = GenerateQualifiedName(XSharpQualifiedTypeNames.FoxArray);

        }
        XP.FuncprocContext _defaultEntity = null;
        XP.FuncprocContext CreateDefaultEntity([NotNull] XP.FoxsourceContext context)
        {
            // Generate leading code for the file
            // Function needs at least an Id and a Statement Block
            // The rest is default
            var name = System.IO.Path.GetFileNameWithoutExtension(_fileName);
            var entity = new XP.EntityContext(context, 0);
            var func = new XP.FuncprocContext(entity, 0);
            var id = new XP.IdentifierContext(func, 0);
            var token = new XSharpToken(XP.FUNCTION, "FUNCTION");
            var sig = new XP.SignatureContext(func, 0);
            sig.Id = id;
            func.Sig = sig;
            token.line = 1;
            token.charPositionInLine = 1;
            func.T = new XP.FuncproctypeContext(func, 0);
            func.T.Token = token;
            token = new XSharpToken(XP.ID, name);
            token.line = 1;
            token.charPositionInLine = 1;
            id.Start = id.Stop = token;
            sig.AddChild(sig.Id);
            ExitIdentifier(id);    // Generate SyntaxToken 
            if (string.Equals(name, _entryPoint, XSharpString.Comparison))
            {
                sig.Type = new XP.DatatypeContext(func, 0);
                sig.Type.Start = new XSharpToken(XP.AS, "AS");
                sig.Type.Stop = new XSharpToken(XP.VOID, "VOID");
                sig.Type.Put(voidType);
                sig.AddChild(sig.Type);
            }
            func.Attributes = new XP.AttributesContext(func, 0);
            func.Attributes.PutList(MakeCompilerGeneratedAttribute());
            func.StmtBlk = context.StmtBlk;
            context.StmtBlk.parent = func;
            func.Start = func.StmtBlk.Start;
            func.Stop = func.StmtBlk.Stop;
            func.AddChild(func.Sig);
            func.AddChild(func.StmtBlk);
            _defaultEntity = func;
            return func;
        }
        public override void EnterFoxsource([NotNull] XP.FoxsourceContext context)
        {
            if (context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0)
            {
                var func = CreateDefaultEntity(context);
                Entities.Push(func);
            }
            _enterSource(context);
        }
        public override void ExitFoxsource([NotNull] XP.FoxsourceContext context)
        {
            if (context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0)
            {
                var func = _defaultEntity;
                ExitFuncproc(func);     // Generate function
                Entities.Pop();
                var entity = func.Parent as XP.EntityContext;
                entity.Start = func.Start;
                entity.Stop = func.Stop;
                entity.AddChild(func);
                ExitEntity(entity);
                context._Entities.Insert(0, entity);
            }

            _exitSource(context);
        }
        public override void ExitAccessMember([NotNull] XP.AccessMemberContext context)
        {
            if (context.Expr != null && context.Op.Type == XP.DOT)  // do not assume an area when no Expr (inside WITH Block)
            {
                context.foxFlags |= XP.FoxFlags.MemberAccess;
            }
            base.ExitAccessMember(context);
            // FoxPro uses M. for Locals and memvars
            // We assume it is a local and then will later correct this inside Binder_Expressions.cs if we can't find the local
            if (context.foxFlags.HasFlag(XP.FoxFlags.MemberAccess) && context.AreaName == "M")
            {
                context.foxFlags |= XP.FoxFlags.MPrefix;
                context.Put(context.Name.Get<ExpressionSyntax>());
                var ent = CurrentMember;
                if (ent != null && _options.HasOption(CompilerOption.MemVars, context, PragmaOptions))
                {
                    ent.Data.HasMemVars = true;
                }
            }
        }

        public override void ExitFoxexpressionStmt([NotNull] XP.FoxexpressionStmtContext context)
        {
            context.SetSequencePoint(context._Exprs[0].Start, context._Exprs[0].Stop);
            var stmt = HandleExpressionStmt(context._Exprs);
            context.Put(stmt);
        }

        private void AddLocalName(string name, XSharpParserRuleContext context, bool local)
        {
            var fieldInfo = findVar(name);
            if (fieldInfo == null)
            {
                var alias = local ? XSharpSpecialNames.LocalPrefix : XSharpSpecialNames.MemVarPrefix;
                var field = addFieldOrMemvar(name, alias, context, false);
                field.IsCreated = local;
            }
        }

        public override void EnterLocalvar([NotNull] XP.LocalvarContext context)
        {
            base.EnterLocalvar(context);
            var name = context.Id.GetText();
            AddLocalName(name, context, true);
        }

        public override void EnterImpliedvar([NotNull] XP.ImpliedvarContext context)
        {
            base.EnterImpliedvar(context);
            var name = context.Id.GetText();
            AddLocalName(name, context, true);
        }

        public override void EnterDimensionVar([NotNull] XP.DimensionVarContext context)
        {
            base.EnterDimensionVar(context);
            var name = CleanVarName(context.Id.GetText());
            var local = false;
            if (context.Parent is XP.FoxdeclContext foxdecl)
            {
                // should always be the case now.
                local = foxdecl.T.Type == XP.LOCAL;
            }
            var alias = local ? XSharpSpecialNames.LocalPrefix : XSharpSpecialNames.MemVarPrefix;
            var field = findVar(name);
            if (field == null)
            {
                AddAmpbasedMemvar(context, name, alias, context.Amp);
            }
        }

        public override void EnterFoxdecl([NotNull] XP.FoxdeclContext context)
        {
            // use the Enter call to declare the memory variables
            // The locals are generated in the ExitFoxdecl()
            context.SetSequencePoint(context.end);
            if (CurrentMember == null)
            {
                return;
            }
            if (context.T.Type == XP.MEMVAR || context.T.Type == XP.PARAMETERS || context.T.Type == XP.LPARAMETERS)
            {
                var prefix = XSharpSpecialNames.ClipperParamPrefix;
                if (context.T.Type != XP.LPARAMETERS)
                {
                    CurrentMember.Data.HasMemVars = true;
                    prefix = "M";
                }
                // List inside _Vars. 
                foreach (var memvar in context._Vars)
                {
                    var name = CleanVarName(memvar.Id.GetText());
                    addFieldOrMemvar(name, prefix, memvar, context.T.Type == XP.PARAMETERS || context.T.Type == XP.LPARAMETERS);
                }
            }
            else if (context.T.Type == XP.PUBLIC || context.T.Type == XP.PRIVATE)
            {
                CurrentMember.Data.HasMemVars = true;
                // List inside _XVars. 
                foreach (var memvar in context._XVars)
                {
                    var name = memvar.Id.GetText();
                    AddAmpbasedMemvar(memvar, name, "M", memvar.Amp);
                }
            }
            if (context.T.Type == XP.PARAMETERS || context.T.Type == XP.LPARAMETERS)
            {
                // Function must be clipper calling convention.
                var member = CurrentMember;
                member.Data.HasClipperCallingConvention = true;
                if (member.Data.HasParametersStmt || member.Data.HasLParametersStmt || member.Data.HasFormalParameters)
                {
                    // trigger error message by setting both
                    // that way 2 x PARAMETERS or 2x LPARAMETERS will also trigger an error
                    member.Data.HasParametersStmt = true;
                    member.Data.HasLParametersStmt = true;
                }
                else
                {
                    member.Data.HasParametersStmt = (context.T.Type == XP.PARAMETERS);
                    member.Data.HasLParametersStmt = (context.T.Type == XP.LPARAMETERS);
                }
            }
        }

        public override void ExitFoxdeclStmt([NotNull] XP.FoxdeclStmtContext context)
        {
            context.PutList(context.Decl.GetList<StatementSyntax>());
        }

        /*
         *                  // This includes array indices and optional type per name
        foxdecl             : T=(DIMENSION|DECLARE) DimVars += dimensionVar
                                (COMMA DimVars+=dimensionVar)*    end=eos
                            // This only has names and optional types
                            | T=(LPARAMETERS|PARAMETERS) Vars+=varidentifierName XT=xbasedecltype?
                                (COMMA Vars+=varidentifierName XT=xbasedecltype? )* end=eos
                            // This only has a list of names
                            | T=(MEMVAR|PRIVATE|PUBLIC) XVars+=foxbasevar
                                (COMMA XVars+=foxbasevar)*  end=eos      
                            // Variations of LOCAL and PUBLIC with the ARRAY keyword
                            | T=(LOCAL|PUBLIC) ARRAY DimVars += dimensionVar
                                (COMMA DimVars+=dimensionVar)*    end=eos  

         */
        public override void ExitFoxdecl([NotNull] XP.FoxdeclContext context)
        {
            context.SetSequencePoint(context.end);
            var stmts = _pool.Allocate<StatementSyntax>();
            bool hasError = false;
            switch (context.T.Type)
            {
                case XP.DIMENSION:
                case XP.DECLARE:
                case XP.LOCAL:          // LOCAL ARRAY only !
                    foreach (var dimvar in context._DimVars)
                    {
                        // call the runtime function to allocate the array
                        // when the array is a LOCAL then the local declaration will also be added to the stmts list returned
                        // all the statements will be 'anchored' to the foxdeclContext.
                        var dimstmts = processDimensionVar(context, dimvar, ref hasError);
                        foreach (var stmt in dimstmts)
                        {
                            stmts.Add(stmt);
                        }
                    }
                    break;
                case XP.MEMVAR:
                    // Handled in the Enter method
                    break;
                case XP.PARAMETERS:
                    int i = 0;
                    foreach (var memvar in context._Vars)
                    {
                        // declare the memvar and fill it with the contents from the clipper params array
                        var name = memvar.GetText();
                        ++i;
                        var exp = GenerateMemVarDecl(memvar, GenerateLiteral(name), true);
                        exp.XNode = memvar;
                        stmts.Add(GenerateExpressionStatement(exp, memvar, true));

                        var val = GenerateGetClipperParam(GenerateLiteral(i), context);
                        exp = GenerateMemVarPut(context, GenerateLiteral(name), val);
                        var stmt = GenerateExpressionStatement(exp, memvar);
                        memvar.Put(stmt);
                        stmts.Add(stmt);
                    }
                    break;
                case XP.PRIVATE:
                    foreach (var memvar in context._XVars)
                    {
                        // declare the private. FoxPro has no initializers 
                        var varname = GetAmpBasedName(memvar.Amp, memvar.Id.Id);
                        var exp = GenerateMemVarDecl(memvar, varname, true);
                        exp.XNode = memvar;
                        stmts.Add(GenerateExpressionStatement(exp, memvar));
                    }
                    // no need to assign a default. The runtime takes care of that
                    break;
                case XP.PUBLIC:
                    if (context._DimVars.Count > 0)
                    {
                        // PUBLIC ARRAY
                        foreach (var dimvar in context._DimVars)
                        {
                            // declare the public and initialize it with a FoxPro array
                            var varname = GetAmpBasedName(dimvar.Amp, dimvar.Id.Id);
                            var exp = GenerateMemVarDecl(dimvar, varname, false);
                            exp.XNode = dimvar;
                            stmts.Add(GenerateExpressionStatement(exp, dimvar));
                            var dimstmts = processDimensionVar(context, dimvar, ref hasError);
                            foreach (var stmt in dimstmts)
                            {
                                stmts.Add(stmt);
                            }
                        }
                    }
                    else // context._XVars.Count > 0
                    {
                        foreach (var memvar in context._XVars)
                        {
                            // declare the public. FoxPro has no initializers
                            var varname = GetAmpBasedName(memvar.Amp, memvar.Id.Id);
                            var exp = GenerateMemVarDecl(memvar, varname, false);
                            exp.XNode = memvar;
                            stmts.Add(GenerateExpressionStatement(exp, memvar));
                        }
                    }
                    break;

                case XP.LPARAMETERS:
                    // list inside Vars
                    if (CurrentEntity?.isScript() == true)
                    {
                        // Inside scripts we will create local variables for the LPARAMETERS like this
                        var prc = (XSharpParserRuleContext)context;
                        int p_i = 1;
                        foreach (var p in context._Vars)
                        {
                            var name = p.Id.GetText();
                            var decl = GenerateLocalDecl(name, _usualType, GenerateGetClipperParam(GenerateLiteral(p_i), prc));
                            decl.XGenerated = true;
                            var variable = decl.Declaration.Variables[0];
                            variable.XGenerated = true;
                            stmts.Add(decl);
                            p_i++;
                        }
                    }
                    break;
                default:
                    Debug.Assert(false, "Unknown type in Foxdecl", "Type = " + context.T.Text);
                    break;
            }
            // do not make a block, otherwise locals will be scoped to that block!
            context.PutList<StatementSyntax>(stmts);
            _pool.Free(stmts);

        }
        private IList<StatementSyntax> processDimensionVar(XSharpParserRuleContext context, XP.DimensionVarContext dimVar, ref bool hasError)
        {
            var name = CleanVarName(dimVar.Id.GetText());
            var stmts = new List<StatementSyntax>();
            MemVarFieldInfo fieldInfo = findVar(name);

            ArgumentListSyntax args;
            ArgumentSyntax arg1;
            ExpressionSyntax mcall;
            if (fieldInfo.IsLocal)
            {
                if (!fieldInfo.IsCreated)
                {
                    var decl = GenerateLocalDecl(name, _foxarrayType, GenerateLiteralNull());
                    decl.XNode = context;
                    stmts.Add(decl);
                    fieldInfo.IsCreated = true;
                }
                if (fieldInfo.Context.Parent == context)
                {
                    arg1 = MakeArgument(GenerateNIL());
                }
                else
                {
                    arg1 = MakeArgument(GenerateSimpleName(name));
                }
            }
            else
            {
                arg1 = MakeArgument(GenerateLiteral(name));
                args = MakeArgumentList(arg1);
                mcall = GenerateMethodCall(XSharpQualifiedFunctionNames.MemVarGetSafe, args, true);
                arg1 = MakeArgument(mcall);
            }
            var arg2 = MakeArgument(dimVar._Dims[0].Get<ExpressionSyntax>());
            if (dimVar._Dims.Count == 2)
            {
                var arg3 = MakeArgument(dimVar._Dims[1].Get<ExpressionSyntax>());
                args = MakeArgumentList(arg1, arg2, arg3);
            }
            else
            {
                args = MakeArgumentList(arg1, arg2);
            }
            mcall = GenerateMethodCall(XSharpQualifiedFunctionNames.FoxRedim, args);
            mcall.XNode = dimVar;
            ExpressionSyntax lhs;
            if (fieldInfo != null && !fieldInfo.IsLocal)
            {
                lhs = MakeMemVarField(fieldInfo);
            }
            else
            {
                lhs = GenerateSimpleName(name);
            }
            var ass = MakeSimpleAssignment(lhs, mcall);
            var stmt = GenerateExpressionStatement(ass, context);
            stmt.XNode = dimVar;
            stmts.Add(stmt);
            return stmts;
        }


        public override void ExitMethodCall([NotNull] XP.MethodCallContext context)
        {
            var expr = context.Expr.Get<ExpressionSyntax>();
            string name = null;
            if (expr is IdentifierNameSyntax)
            {
                // Intrinsic functions that depend on Vulcan types
                IdentifierNameSyntax ins = expr as IdentifierNameSyntax;
                name = ins.Identifier.Text.ToUpper();
                if (name == XSharpIntrinsicNames.DoDefault)
                {
                    var entity = CurrentMember;
                    name = entity.ShortName;
                    var super = GenerateSuper();
                    var member = MakeSimpleMemberAccess(super, GenerateSimpleName(name));
                    ArgumentListSyntax argList;
                    if (context.ArgList != null)
                    {
                        argList = context.ArgList.Get<ArgumentListSyntax>();
                    }
                    else 
                    {
                        if (entity.Data.HasClipperCallingConvention)
                            argList = MakeArgumentList(MakeArgument(GenerateSimpleName(XSharpSpecialNames.ClipperArgs)));
                        else
                            argList = EmptyArgumentList();
                    }
                    expr = _syntaxFactory.InvocationExpression(member, argList);
                    context.Put(expr);
                    return;
                }
            }
            // all other method names or syntaxes
            base.ExitMethodCall(context);
            return;
        }
 
        public override void ExitFoxclsvars([NotNull] XP.FoxclsvarsContext context)
        {
            context.SetSequencePoint(context.Member.end);
        }
        public override void ExitFoxclsmethod([NotNull] XP.FoxclsmethodContext context)
        {
            context.SetSequencePoint(context.Member.end);
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }
        public override void ExitFoxclsctor([NotNull] XP.FoxclsctorContext context) 
        {
            context.SetSequencePoint(context.Member.end);
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }
        public override void ExitFoxclsdtor([NotNull] XP.FoxclsdtorContext context)
        {
            context.SetSequencePoint(context.Member.end);
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void ExitFoxclsvarinit([NotNull] XP.FoxclsvarinitContext context)
        {
            context.SetSequencePoint(context.Member.end);
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void EnterFoxclass([NotNull] XP.FoxclassContext context)
        {
            ClassEntities.Push(CreateClassEntities());

        }

        public override void ExitFoxfieldinitializer([NotNull] XP.FoxfieldinitializerContext context)
        {
            var id = context.Name.Get<ExpressionSyntax>();
            var assign = MakeSimpleAssignment(id, context.Expr.Get<ExpressionSyntax>());
            context.Put(assign);
        }

        public override void ExitFoxaddobject([NotNull] XP.FoxaddobjectContext context)
        {
            context.SetSequencePoint(context.Member.end);
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }
        public override void ExitFoxaddobjectclause([NotNull] XP.FoxaddobjectclauseContext context)
        {
            var modifiers = context.Modifiers?.GetList<SyntaxToken>() ?? TokenList(SyntaxKind.PublicKeyword);
            var datatype = context.Type.Get<TypeSyntax>();
            var name = context.Id.GetText();
            var prop = createProperty(name, datatype, context, context.Modifiers);
            context.Put(prop);
          }

        public ExpressionSyntax createAddObject(XP.FoxaddobjectclauseContext context)
        {
            InitializerExpressionSyntax init = null;
            if (context._FieldsInits.Count > 0)
            {
                init = _syntaxFactory.InitializerExpression(
                    SyntaxKind.ObjectInitializerExpression,
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    MakeSeparatedList<ExpressionSyntax>(context._FieldsInits),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));

            }
            return CreateObject(context.Type.Get<TypeSyntax>(), EmptyArgumentList(), init);
        }

        public override void ExitFoxclassvars([NotNull] XP.FoxclassvarsContext context)
        {
            var varType = getDataType(context.DataType);
            varType.XVoDecl = true;
            var list = new List<MemberDeclarationSyntax>();
            foreach (var varCtx in context._Vars)
            {
                if (context.Fld != null)
                {
                    var fielddecl = createField(varCtx.GetText(), varType,context.Modifiers);
                    list.Add(fielddecl);
                }
                else
                {
                    var propdecl = createProperty(varCtx.GetText(), varType, varCtx, context.Modifiers);
                    list.Add(propdecl);
                }
            }
            context.CsNode = list;
        }

        public override void EnterFoxmethod([NotNull] XP.FoxmethodContext context)
        {
            Check4ClipperCC(context, context.Sig.ParamList?._Params, context.Sig.CallingConvention?.Convention, context.Sig.Type);
            var name = context.Id.GetText().ToUpper();
            if (name == "INIT" && (context.Params == null || context.Params._Params.Count == 0))
            {
                context.Data.HasClipperCallingConvention = true;
            }
            else if (name.EndsWith("_ASSIGN"))
            {
                context.Data.HasClipperCallingConvention = false;
                context.Data.HasTypedParameter = true;          // this will set all missing types to USUAL
                context.Data.MustBeVoid = true;
                context.RealType = XP.ASSIGN;
            }
            else if (name.EndsWith("_ACCESS"))
            {
                context.Data.HasClipperCallingConvention = false;
                context.Data.HasTypedParameter = true;          // this will set all missing types to USUAL
                context.RealType = XP.ACCESS;
            }
            else
            {
                context.RealType = XP.METHOD;
            }
            CheckVirtualOverride(context, context.Modifiers?._Tokens);
        }

        public override void ExitFoxmethod([NotNull] XP.FoxmethodContext context)
        {
            context.SetSequencePoint(context.T.Token, context.end.Stop);
            var idName = context.Id.Get<SyntaxToken>();
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? DefaultMethodModifiers(context, context.TypeParameters != null);
            var isExtern = mods.Any((int)SyntaxKind.ExternKeyword);
            var isAbstract = mods.Any((int)SyntaxKind.AbstractKeyword);
            var isStatic = mods.Any((int)SyntaxKind.StaticKeyword);
            var hasNoBody = isExtern || isAbstract || context.Sig.ExpressionBody != null;
            var mName = idName.Text;
            if (mName.EndsWith("_ACCESS", StringComparison.OrdinalIgnoreCase))
            {
                mName = mName.Substring(0, mName.Length - "_ACCESS".Length);
            }
            else if (mName.EndsWith("_ASSIGN", StringComparison.OrdinalIgnoreCase))
            {
                mName = mName.Substring(0, mName.Length - "_ASSIGN".Length);
            }
            idName = SyntaxFactory.MakeIdentifier(mName);
            bool isAccessAssign = this.isAccessAssign(context.RealType);
            var attributes = getAttributes(context.Attributes);
            bool hasExtensionAttribute = false;
            if (isAccessAssign)
            {
                var vomods = _pool.Allocate();
                vomods.Add(SyntaxFactory.MakeToken(SyntaxKind.PrivateKeyword));
                if (mods.Any((int)SyntaxKind.StaticKeyword))
                    vomods.Add(SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword));
                if (mods.Any((int)SyntaxKind.UnsafeKeyword))
                    vomods.Add(SyntaxFactory.MakeToken(SyntaxKind.UnsafeKeyword));
                mods = vomods.ToList<SyntaxToken>();
                _pool.Free(vomods);
            }
           if (!isExtern)
            {
                isExtern = hasDllImport(attributes);
                hasNoBody = hasNoBody || isExtern;
            }
            if (isExtern && !mods.Any((int)SyntaxKind.ExternKeyword))
            {
                // Add Extern Keyword to modifiers
                var m1 = _pool.Allocate();
                m1.AddRange(mods);
                if (!m1.Any((int)SyntaxKind.ExternKeyword))
                    m1.Add(SyntaxFactory.MakeToken(SyntaxKind.ExternKeyword));
                mods = m1.ToList<SyntaxToken>();
                _pool.Free(m1);
            }
            var parameters = getParameters(context.ParamList);
            var body = hasNoBody ? null : processEntityBody(context);
            var expressionBody = GetExpressionBody(context.Sig.ExpressionBody);
            var returntype = context.ReturnType?.Get<TypeSyntax>();
            if (returntype == null)
            {
                if (context.RealType == XP.ASSIGN)
                {
                    returntype = VoidType();
                }
                else  // method and access
                {
                    returntype = _getMissingType();
                    returntype.XNode = context;
                }
            }
            else
            {
                returntype.XVoDecl = true;
            }
            var oldbody = body;
            ImplementClipperAndPSZ(context, ref attributes, ref parameters, ref body, ref returntype);
            if (body != oldbody)
            {
                context.StmtBlk.Put(body);
            }
            if (context.RealType == XP.ASSIGN)
            {
                // Assign does not need a return. 
                // So do not add missing returns
                returntype = VoidType();
            }
            else if (context.StmtBlk != null && !hasNoBody)
            {
                body = AddMissingReturnStatement(body, context.StmtBlk, returntype);
            }
            MemberDeclarationSyntax m = _syntaxFactory.MethodDeclaration(
                attributeLists: attributes,
                modifiers: mods,
                returnType: returntype,
                explicitInterfaceSpecifier: null ,
                identifier: idName,
                typeParameterList: getTypeParameters(context.TypeParameters),
                parameterList: parameters,
                constraintClauses: getTypeConstraints(context._ConstraintsClauses),
                body: body,
                expressionBody: expressionBody, 
                semicolonToken: (!hasNoBody && context.StmtBlk != null) ? null : SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            if (hasExtensionAttribute)
            {
                m = m.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_ExplicitExtension));
            }
            bool separateMethod = false;
            context.Put(m);
            if (isAccessAssign && !separateMethod)
            {
                if (context.Data.HasClipperCallingConvention && context.CallingConvention != null)
                {
                    m = m.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(
                                    ErrorCode.ERR_NoClipperCallingConventionForAccessAssign));
                }
                context.Put(m);
                ClassEntities.Peek().AddVoPropertyAccessor(context, context.RealType, idName,isStatic);
            }
        }
        public override void ExitFoxclass([NotNull] XP.FoxclassContext context)
        {
            var fieldNames = new List<String>();
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            var generated = ClassEntities.Pop();
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            context.Data.Partial = mods.Any((int)SyntaxKind.PartialKeyword);
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            var baseType = context.BaseType?.Get<TypeSyntax>();
            if (baseType != null)
            {
                baseTypes.Add(_syntaxFactory.SimpleBaseType(baseType));
            }
            if  (generated.Members.Count > 0)  
            {
                members.AddRange(generated.Members);
            }
            if (generated.VoProperties != null)
            {
                foreach (var vop in generated.VoProperties.Values)
                {
                    var prop = GenerateVoProperty(vop, context);
                    if (prop != null)
                        members.Add(prop);
                }
            }
            // Collect list of FieldNames from clsvarscontext to prevent generating properties twice
            foreach (var mCtx in context._Members)
            {
                if (mCtx is XP.FoxclsvarsContext fcfc)
                {
                    var mem = fcfc.Member;
                    foreach (var v in mem._Vars)
                    {
                        fieldNames.Add(v.GetText().ToLower());
                    }
                    var list = mem.CsNode as List<MemberDeclarationSyntax>;
                    if (list != null)
                    {
                        foreach (var m1 in list)
                        {
                            members.Add(m1);
                        }
                    }
                }
            }

            // Do this after VOProps generation because GenerateVOProperty sets the members
            // for Access & Assign to NULL
            ConstructorDeclarationSyntax ctor = null;
            foreach (var mCtx in context._Members)
            {
                if (mCtx is XP.FoxclsvarinitContext cvi)
                {
                    var fld = cvi.Member.F.Name.GetText();
                    if (!fieldNames.Contains(fld.ToLower()) )
                    {
                        if (mCtx.CsNode != null)
                        {
                            members.Add(mCtx.Get<MemberDeclarationSyntax>());
                        }
                    }
                    else
                    {
                        // field is declared and initialized. No need to generate a second property or field.
                    }
                }
                else if (mCtx is XP.FoximplementsContext fic)
                {
                    var clause = fic.Member as XP.FoximplementsclauseContext;
                    var type = clause.Type.Get<TypeSyntax>();
                    if (baseTypes.Count > 0)
                        baseTypes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    baseTypes.Add(_syntaxFactory.SimpleBaseType(type));

                }
                else if (mCtx is XP.FoxclsmethodContext cmc)
                { 
                    if (cmc.Member.CsNode is MemberDeclarationSyntax mds)
                    {
                        members.Add(mds);
                    }
                }
                else if (mCtx is XP.FoxaddobjectContext fac)
                {
                    var prop = fac.Get<MemberDeclarationSyntax>();
                    members.Add(prop);
                }
                else
                {
                    if (mCtx.CsNode is MemberDeclarationSyntax mds)
                    {
                        members.Add(mds);
                        if (mds is ConstructorDeclarationSyntax)
                        {
                            ctor = mds as ConstructorDeclarationSyntax;
                        }
                    }
                }
            }
            generated.Free();
            if (ctor != null)
            {
                var newlist = members.ToList();
                members.Clear();
                foreach (var mem in newlist)
                {
                    if (mem != ctor)
                    {
                        members.Add(mem);
                    }
                }

                ctor = createConstructor(context, members, fieldNames, ctor);
                members.Add(ctor);
            }
            else
            {
                ctor = createConstructor(context, members, fieldNames, null);
                members.Add(ctor);
            }
            MemberDeclarationSyntax m = _syntaxFactory.ClassDeclaration(
                attributeLists: getAttributes(context.Attributes),
                modifiers: mods,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: getTypeParameters(context.TypeParameters),
                baseList: _syntaxFactory.BaseList(SyntaxFactory.MakeToken(SyntaxKind.ColonToken), baseTypes),
                constraintClauses: getTypeConstraints(context._ConstraintsClauses),
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: members,
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            _pool.Free(members);
            _pool.Free(baseTypes);
            if (context.Namespace != null)
            {
                m = AddNameSpaceToMember(context.Namespace, m);

            }
            else
            {
                m = (MemberDeclarationSyntax)CheckForConflictBetweenTypeNameAndNamespaceName(context, "CLASS", m);
            }

            context.Put(m);
            if (context.Data.Partial)
            {
                GlobalEntities.NeedsProcessing = true;
            }
        }

        private MemberDeclarationSyntax createField(string fldName, TypeSyntax type, XP.ClassvarModifiersContext modifiers)
        {
            var list = MakeSeparatedList(GenerateVariable(fldName, null));
            var decl = _syntaxFactory.VariableDeclaration(type, list);
            var mods = modifiers?.GetList<SyntaxToken>() ?? DefaultMethodModifiers(modifiers, false);
            var fdecl = _syntaxFactory.FieldDeclaration(
                                    attributeLists: default,
                                    modifiers: mods,
                                    declaration: decl,
                                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            return fdecl;
        }

        private MemberDeclarationSyntax createProperty(string fldName, TypeSyntax type, XSharpParserRuleContext context, XP.ClassvarModifiersContext modifiers)
        {
            var accessors = _pool.Allocate<AccessorDeclarationSyntax>();
            BlockSyntax body = null;
            if (_options.fox1)
            {
                var call = GenerateThisMethodCall(XSharpSpecialNames.GetProperty, MakeArgumentList(MakeArgument(GenerateLiteral(fldName))), true);
                body = MakeBlock(GenerateReturn(call, true));
                body.XGenerated = true;
            }
            var accessor = _syntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration,
                    default, default,
                    SyntaxFactory.MakeToken(SyntaxKind.GetKeyword),
                    body,
                    null,
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            accessor.XNode = context;
            accessor.XGenerated = true;
            accessors.Add(accessor);
            if (_options.fox1)
            {
                var call = GenerateThisMethodCall(XSharpSpecialNames.SetProperty, MakeArgumentList(MakeArgument(GenerateLiteral(fldName)), MakeArgument(GenerateSimpleName("value"))), true);
                body = MakeBlock(GenerateExpressionStatement(call, context));
                body.XGenerated = true;
            }
            accessor = _syntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration,
                    default, default,
                    SyntaxFactory.MakeToken(SyntaxKind.SetKeyword),
                    body,
                    null,
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            accessor.XNode = context;
            accessor.XGenerated = true;
            accessors.Add(accessor);
            var id = SyntaxFactory.MakeIdentifier(fldName);
            var accessorList = _syntaxFactory.AccessorList(SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                        accessors, SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
            _pool.Free(accessors);
            var mods = modifiers?.GetList<SyntaxToken>() ?? DefaultMethodModifiers(context, false);
            var prop = _syntaxFactory.PropertyDeclaration(
                   attributeLists: default,
                   modifiers: mods,
                   type: type,
                   explicitInterfaceSpecifier: null,
                   identifier: id,
                   accessorList: accessorList,
                   expressionBody: null,
                   initializer: null,
                   semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            return prop;
        }

        public override void ExitFoxfield([NotNull] XP.FoxfieldContext context)
        {
            if (context.Fld != null)
            {
                var flddecl = createField(context.F.Name.GetText(), _usualType, context.Modifiers);
                if (flddecl != null)
                {
                    context.Put(flddecl);
                }
            }
            else
            {
                var propdecl = createProperty(context.F.Name.GetText(), _usualType, context,context.Modifiers);
                if (propdecl != null)
                {
                    context.Put(propdecl);
                }
            }
        }

        private ConstructorDeclarationSyntax createConstructor(XP.FoxclassContext context, SyntaxListBuilder<MemberDeclarationSyntax> members, 
            List<String> fieldNames, ConstructorDeclarationSyntax existingctor)
        {
            var stmts = new List<StatementSyntax>();
            bool hasinit = false;
            var attributeLists = _pool.Allocate<AttributeListSyntax>();

            ParameterListSyntax initparams = EmptyParameterList();
            ConstructorDeclarationSyntax ctor = null;
            foreach (var member in context._Members)
            {
                if (member is XP.FoxclsmethodContext fm)
                {
                    // fetch parameters from procedure/function init
                    var method = fm.Member as XP.FoxmethodContext;
                    if (method.Id.GetText().ToLower() == "init")
                    {
                        var syntax = method.Get<MethodDeclarationSyntax>();
                        initparams = syntax.ParameterList;
                        attributeLists.AddRange( syntax.AttributeLists);
                        hasinit = true;
                    }
                }
                else if (member is XP.FoxclsvarinitContext cvi)
                {
                    // Generate code to initialize properties
                    var fldinit = cvi.Member;
                    var assign = fldinit.F.Get<ExpressionSyntax>();
                    var stmt = GenerateExpressionStatement(assign, cvi);
                    stmt.XNode = fldinit.F;
                    stmts.Add(stmt);
                }
                else if (member is XP.FoxaddobjectContext fac)
                {
                    // generate object creation and addobject call
                    // Property:= ClassName{}{.......}
                    var addobject = fac.Member;
                    var create = createAddObject(addobject);
                    var name = addobject.Id.GetText();
                    var prop = MakeSimpleMemberAccess(GenerateSelf(), GenerateSimpleName(name));
                    var assign = MakeSimpleAssignment(prop, create);
                    var stmt = GenerateExpressionStatement(assign, fac);
                    stmt.XNode = addobject;
                    stmts.Add(stmt);
                    // AddObject(SELF:Property)
                    var arg1 = MakeArgument(GenerateLiteral(name));
                    var arg2 = MakeArgument(prop);
                    var mcall = GenerateThisMethodCall(XSharpSpecialNames.AddObject, MakeArgumentList(arg1, arg2));
                    stmt = GenerateExpressionStatement(mcall, fac);
                    stmt.XNode = addobject;
                    stmts.Add(stmt);

                }
            }
            if (_options.fox1)
            {
                if (stmts.Count > 0)
                {
                    // Generate Virtual Protected _InitProperties
                    // SUPER:_InitProperties()
                    // All Statements
                    var mac = MakeSimpleMemberAccess(GenerateSuper(), GenerateSimpleName(XSharpSpecialNames.InitProperties));
                    var superCall = _syntaxFactory.InvocationExpression(mac, EmptyArgumentList());
                    var stmt = GenerateExpressionStatement(superCall, context);
                    stmts.Insert(0, stmt);
                    var body = MakeBlock(stmts);
                    body.XGenerated = true;
                    var mods = TokenList(SyntaxKind.ProtectedKeyword, SyntaxKind.OverrideKeyword);
                    var id = SyntaxFactory.MakeIdentifier(XSharpSpecialNames.InitProperties);
                    var mem = _syntaxFactory.MethodDeclaration(MakeCompilerGeneratedAttribute(), mods, voidType, null, id,
                        null, EmptyParameterList(), null, body, null, SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                    members.Add(mem);
                    stmts.Clear();
                }
            }
            if (stmts.Count > 0 || hasinit || existingctor != null)
            {
                var argList = new List<ArgumentSyntax>();
                for (int i = 0; i < initparams.Parameters.Count; i++)
                {
                    var par = initparams.Parameters[i];
                    argList.Add(MakeArgument(GenerateSimpleName(par.Identifier.Text)));
                }
                ArgumentListSyntax args = MakeArgumentList(argList.ToArray());
                if (existingctor != null)
                {
                    stmts.AddRange(existingctor.Body.Statements.Nodes);
                    var body = MakeBlock(stmts);
                    ctor = existingctor.Update(existingctor.AttributeLists, existingctor.Modifiers, existingctor.Identifier,
                        existingctor.ParameterList, existingctor.Initializer, body, existingctor.ExpressionBody, existingctor.SemicolonToken);
                }
                else
                {
                    var chain = _syntaxFactory.ConstructorInitializer(SyntaxKind.BaseConstructorInitializer,
                                                                        SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                                                                        SyntaxFactory.MakeToken(SyntaxKind.BaseKeyword),
                                                                        args
                                                                        );
                    var mods = TokenList(SyntaxKind.PublicKeyword);
                    var id = context.Id.Get<SyntaxToken>();
                    GenerateAttributeList(attributeLists, SystemQualifiedNames.CompilerGenerated);
                    var body = MakeBlock(stmts);
                    ctor = _syntaxFactory.ConstructorDeclaration(attributeLists, mods, id, initparams, chain, body, null, null);
                }
            }
            _pool.Free(attributeLists);
            return ctor;
        }
        #region TextMergeSupport
        private bool checkTextMergeDelimiters(string sourceText)
        {
            var open = sourceText.Contains("<<");
            var close = sourceText.Contains(">>");
            bool unbalanced = false;
            if (open && close)
            {
                var numOpen = sourceText.Length - sourceText.Replace("<<", "").Length;
                var numClose = sourceText.Length - sourceText.Replace(">>", "").Length;
                if (numOpen == numClose)    // balanced, may still be incorrect, such as >> aaa <<
                {
                    sourceText = sourceText.Replace("<<", "{");
                    sourceText = sourceText.Replace(">>", "}");
                    // check to see if open is before close
                    string worker = sourceText;
                    numOpen = worker.IndexOf("{");
                    numClose = worker.IndexOf("}");
                    while (numClose > numOpen && numClose != -1)
                    {
                        worker = worker.Substring(numClose + 1);
                        numOpen = worker.IndexOf("{");
                        numClose = worker.IndexOf("}");
                    }
                    if (numOpen > numClose)
                        unbalanced = true;
                }
                else
                {
                    unbalanced = true;
                }
            }
            else if (open || close)
            {
                unbalanced = true;
            }
            return !unbalanced;
        }

        private string ReplaceFoxTextDelimiters(string source)
        {
            var sb = new StringBuilder(source.Length);
            bool extended = false;
            char last = '\0';
            for (int i = 0; i < source.Length; i++)
            {
                var c = source[i];
                switch (c)
                {
                    case '<':
                        if (i < source.Length-1 && source[i+1] == '<')
                        {
                            c = '{';
                            i++;
                        }
                        break;
                    case '>':
                        if (i < source.Length - 1 && source[i + 1] == '>')
                        {
                            c = '}';
                            i++;

                        }
                        break;
                    case '"':
                        break;
                    case '\n':
                        extended = true;
                        sb.Append('\\');
                        c = 'n';
                        break;
                    case '\t':
                        extended = true;
                        sb.Append('\\');
                        c = 't';
                        break;
                    case '\r':
                        extended = true;
                        sb.Append('\\');
                        c = 'r';
                        break;
                    default:
                        break;
                }
                sb.Append(c);
                last = c;
            }
            source = sb.ToString();
            if (extended)
                source  = "ei\"" + source + "\"";
            else
                source = "i\"" + source + "\"";
            return source;
        }

        public override void ExitFoxtextStmt([NotNull] XP.FoxtextStmtContext context)
        {
            var sourceText = context.String.Text;
            ExpressionSyntax stringExpr;
            bool delimitersOk = checkTextMergeDelimiters(sourceText);
            bool hasDelim = sourceText.IndexOf("<<") >= 0 && sourceText.IndexOf(">>") >= 2;
            if (delimitersOk && hasDelim)
            {
                sourceText = ReplaceFoxTextDelimiters(sourceText);
                var token = new XSharpToken(XSharpParser.TEXT, sourceText);
                token.Line = context.Start.Line;
                token.Column = context.Start.Column;
                stringExpr = CreateInterPolatedStringExpression(token, context);
            }
            else
            {
                stringExpr = GenerateLiteral(sourceText);
            }
            if (hasDelim && context.Merge == null)
            {
                var txtmerge = GenerateMethodCall(XSharpQualifiedFunctionNames.TextMergeCheck, true);
                stringExpr = MakeConditional(txtmerge, stringExpr, GenerateLiteral(context.String.Text));
            }

            var arg1 = MakeArgument(stringExpr);
            var arg2 = MakeArgument(GenerateLiteral(context.NoShow != null));
            var arg3 = MakeArgument(context.Flags != null ? context.Flags.Get<ExpressionSyntax>() : GenerateLiteral(0));
            var arg4 = MakeArgument(context.Pretext != null ? context.Pretext.Get<ExpressionSyntax>() : GenerateNIL());
            var args = MakeArgumentList(arg1, arg2, arg3, arg4);
            var call = GenerateMethodCall(XSharpQualifiedFunctionNames.TextSupport, args);

            if (context.Id != null)
            {
                // Call TextSupport  Function and generate assign expression
                AssignmentExpressionSyntax assignExpr;
                var id = GenerateSimpleName(context.Id.GetText());
                if (context.Add != null)
                {
                    assignExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.AddAssignmentExpression, id, SyntaxFactory.MakeToken(SyntaxKind.PlusEqualsToken), call);
                }
                else
                {
                    assignExpr = _syntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, id, SyntaxFactory.MakeToken(SyntaxKind.EqualsToken), call);
                }
                context.Put(GenerateExpressionStatement(assignExpr, context));
            }
            else
            {
                // no assignment, simply call the TextSupport function
                context.Put(GenerateExpressionStatement(call, context));
            }
            if (!delimitersOk)
            {
                var stmt = context.Get<StatementSyntax>();
                stmt = stmt.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.WRN_UnbalancedTextMergeOperators));
                context.Put(stmt);
            }
            return;
        }

        public override void EnterFoxdll([NotNull] XP.FoxdllContext context)
        {
            if (context._Params?.Count > 0)
            {
                CurrentMember.Data.HasFormalParameters = true;
            }
        }

        TypeSyntax foxdllGetType(XSharpParser.DatatypeContext type)
        {
            TypeSyntax result;
            if (type != null)
            {
                switch (type.GetText().ToLower())
                {
                    case "integer":
                        result = intType;
                        break;
                    case "single":
                        result = floatType;
                        break;
                    case "double":
                        result = doubleType;
                        break;
                    case "string":
                        result = stringType;
                        break;
                    default:
                        result = type.Get<TypeSyntax>();
                        break;
                }
            }
            else
            {
                result = voidType;
            }
            result.XVoDecl = true;
            return result;
        }

        public override void ExitFoxdllparam([NotNull] XP.FoxdllparamContext context)
        {

            var attributeList = getAttributes(context.Attributes);
            TypeSyntax type = foxdllGetType(context.Type);  // handle integer, single, double etc.
            SyntaxList<SyntaxToken> modifiers = default;

            if (context.Address != null)
            {
                SyntaxListBuilder list = _pool.Allocate();
                list.Add(SyntaxFactory.MakeToken(SyntaxKind.RefKeyword, context.Address.Text));
                modifiers = list.ToList();
                _pool.Free(list);
            }
            var foxdll = context.Parent as XP.FoxdllContext;
            var pos = foxdll._Params.IndexOf(context)+1;
            SyntaxToken id;
            if (context.Name != null)
                id = context.Name.Get<SyntaxToken>();
            else
                id = SyntaxFactory.MakeIdentifier("$param" + pos.ToString());

            var par = _syntaxFactory.Parameter(
                attributeLists: attributeList,
                modifiers: modifiers,
                type: type,
                identifier: id,
                @default: null);

            if (type == stringType && context.Address != null)
            {
                par = par.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_FoxDeclareDLLStringByReference));
            }

            context.Put(par);
        }

        private AttributeSyntax _foxdllImportAttribute(XP.FoxdllContext context, ExpressionSyntax dllExpr, ExpressionSyntax entrypointExpr)
        {
            AttributeArgumentSyntax charset;
            SyntaxToken id;
            id = SyntaxFactory.Identifier("Ansi");
            charset = _syntaxFactory.AttributeArgument(GenerateNameEquals("CharSet"), null,
                             MakeSimpleMemberAccess(GenerateQualifiedName(SystemQualifiedNames.CharSet),
                                  _syntaxFactory.IdentifierName(id)));
            var attribs = new List<AttributeArgumentSyntax>() { _syntaxFactory.AttributeArgument(null, null, dllExpr), charset };
            if (entrypointExpr != null)
            {
                attribs.Add(_syntaxFactory.AttributeArgument(GenerateNameEquals("EntryPoint"), null, entrypointExpr));
                attribs.Add(_syntaxFactory.AttributeArgument(GenerateNameEquals("ExactSpelling"), null, GenerateLiteral(true)));
            }
            else
            {
                attribs.Add(_syntaxFactory.AttributeArgument(GenerateNameEquals("ExactSpelling"), null, GenerateLiteral(false)));
            }
            attribs.Add(_syntaxFactory.AttributeArgument(GenerateNameEquals("SetLastError"), null, GenerateLiteral(true)));
            return _syntaxFactory.Attribute(
                name: GenerateQualifiedName(SystemQualifiedNames.DllImport),
                argumentList: MakeAttributeArgumentList(MakeSeparatedList(attribs.ToArray())));

        }

        public override void ExitFoxdll([NotNull] XP.FoxdllContext context)
        {
            // todo: declare and process attributes 
            string dllName = context.Dll.GetText();
            if (context.Extension != null)
            {
                dllName += "." + context.Extension.GetText();
            }

            ExpressionSyntax dllExpr = GenerateLiteral(dllName);
            SyntaxToken id = context.Id.Get<SyntaxToken>();
            ExpressionSyntax entrypoint = null;
            // When the AS Alias=identifier clause is used then we want to use that as the Id of the function.
            if (context.Alias != null)
            {
                id = context.Alias.Get<SyntaxToken>();
                entrypoint = GenerateLiteral(context.Id.GetText());
            }
            var returnType = foxdllGetType(context.Type);// handle integer, single, double etc.

            ParameterListSyntax parameters;
            if (context._Params.Count > 0)
            {
                var @params = _pool.AllocateSeparated<ParameterSyntax>();
                foreach (var paramCtx in context._Params)
                {
                    if (@params.Count > 0)
                        @params.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));

                    var par = paramCtx.Get<ParameterSyntax>();
 
                    @params.Add(par);
                    
                }
                parameters = _syntaxFactory.ParameterList(
                    SyntaxFactory.MakeToken(SyntaxKind.OpenParenToken),
                    @params,
                    SyntaxFactory.MakeToken(SyntaxKind.CloseParenToken));
                _pool.Free(@params);
            }
            else
            {
                parameters = EmptyParameterList();
            }
            var modifiers = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility(false, SyntaxKind.StaticKeyword, SyntaxKind.ExternKeyword);
            var attributes = MakeSeparatedList(_unmanagedCodeAttribute(),
                                                _foxdllImportAttribute(context, dllExpr, entrypoint)
                                              );
            var attList = MakeList(MakeAttributeList(target: null, attributes: attributes));

            context.Put(_syntaxFactory.MethodDeclaration(
                attributeLists: attList,
                modifiers: modifiers,
                returnType: returnType,
                explicitInterfaceSpecifier: null,
                identifier: id,
                typeParameterList: null,
                parameterList: parameters,
                constraintClauses: default(SyntaxList<TypeParameterConstraintClauseSyntax>),
                body: null,
                expressionBody: null,
                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
        }
        public override void ExitFoxtextoutStmt([NotNull] XP.FoxtextoutStmtContext context)
        {
            var sourceText = context.String.Text;
            ExpressionSyntax expr;
            bool hasDelim = sourceText.IndexOf("<<") >= 0 && sourceText.IndexOf(">>") >= 2;
            bool delimitersOk = checkTextMergeDelimiters(sourceText);
            if (delimitersOk)
            {
                sourceText = ReplaceFoxTextDelimiters(sourceText);
                var token = new XSharpToken(XSharpParser.TEXT, sourceText);
                token.Line = context.Start.Line;
                token.Column = context.Start.Column;
                expr = CreateInterPolatedStringExpression(token, context);
            }
            else
            {
                expr = GenerateLiteral(sourceText);
            }
            var arg2 = GenerateLiteral(context.B.Type != XP.BACKBACKSLASH);
            var cond = GenerateMethodCall(XSharpQualifiedFunctionNames.TextMergeCheck, true);  
            if (! hasDelim)
            {
                cond = GenerateLiteral(false);
            }
            var arg1 = MakeConditional(cond, expr, GenerateLiteral(context.String.Text));
            var args = MakeArgumentList(MakeArgument(arg1), MakeArgument(arg2));
            var call = GenerateMethodCall(XSharpQualifiedFunctionNames.TextOut, args, true); 
            var stmt = GenerateExpressionStatement(call, context);
            if (!delimitersOk)
                stmt = stmt.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.WRN_UnbalancedTextMergeOperators));
            context.Put(stmt);
            return;
        }
        #endregion

        public override void ExitStatementBlock([NotNull] XP.StatementBlockContext context)
        {
            base.ExitStatementBlock(context);
            if (_options.HasOption(CompilerOption.MemVars, context, PragmaOptions))
            {
                // Make sure we have a privates level in case we want to
                // keep track of locals for the macro compiler or Type() 
                if (CurrentMember != null)
                {
                    CurrentMember.Data.HasMemVars = true;
                }
            }
        }
    }
}

