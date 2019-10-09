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


    
    internal class XSharpTreeTransformationFox : XSharpTreeTransformationRT
    {
        protected override XSharpTreeTransformationCore CreateWalker(XSharpParser parser)
        {
            return new XSharpTreeTransformationFox(parser, _options, _pool, _syntaxFactory, _fileName);
        }

        public XSharpTreeTransformationFox(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool,
                    ContextAwareSyntax syntaxFactory, string fileName) :
                    base(parser, options, pool, syntaxFactory, fileName)
        {
        }

        public override void EnterFoxsource([NotNull] XP.FoxsourceContext context)
        {
            base._enterSource();
        }
        public override void ExitFoxsource([NotNull] XP.FoxsourceContext context)
        {
            
            if (context.StmtBlk != null && context.StmtBlk._Stmts.Count > 0)
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
                func.T = token;
                token = new XSharpToken(XP.ID, name);
                token.line = 1;
                token.charPositionInLine = 1;
                id.Start = id.Stop = token;
                sig.AddChild(sig.Id);
                ExitIdentifier(id);    // Generate SyntaxToken 
                if (string.Equals(name, _entryPoint, StringComparison.OrdinalIgnoreCase))
                {
                    sig.Type = new XP.DatatypeContext(func,0);
                    sig.Type.Start = new XSharpToken(XP.AS, "AS");
                    sig.Type.Stop = new XSharpToken(XP.VOID, "VOID");
                    sig.Type.Put(_voidType);
                    sig.AddChild(sig.Type);
                }
                func.Attributes = new XP.AttributesContext(func,0);
                func.Attributes.PutList(MakeCompilerGeneratedAttribute());
                func.StmtBlk = context.StmtBlk;
                context.StmtBlk.parent = func;
                func.Start = func.StmtBlk.Start;
                func.Stop = func.StmtBlk.Stop;
                func.AddChild(func.Sig);
                func.AddChild(func.StmtBlk); 
                Entities.Push(func);
                ExitFuncproc(func);     // Generate function
                Entities.Pop();
                entity.Start = func.Start;
                entity.Stop = func.Stop;
                entity.AddChild(func);
                ExitEntity(entity);
                context._Entities.Insert(0, entity);
            }
            var entities = new List<XSharpParserRuleContext>();
            entities.AddRange(context._Entities);

            _exitSource(context, entities);
        }
       
        public override void ExitAccessMember([NotNull] XP.AccessMemberContext context)
        {
            CoreAccessMember(context);
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
                    var entity = CurrentEntity;
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
            InitializerExpressionSyntax init = null;
            if (context._FieldsInits.Count > 0)
            {
                init = _syntaxFactory.InitializerExpression(
                    SyntaxKind.ObjectInitializerExpression,
                    SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                    MakeSeparatedList<ExpressionSyntax>(context._FieldsInits),
                    SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken));
                
            }
            var initExpr = CreateObject(datatype, EmptyArgumentList(), init);
            var attributeList = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
            var variable = GenerateVariable(context.Id.GetText(), initExpr);
            var varList = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            varList.Add(variable);
            var decl = _syntaxFactory.VariableDeclaration(type: datatype, variables: varList);
            var fdecl = _syntaxFactory.FieldDeclaration(
                                    attributeLists: attributeList,
                                    modifiers: modifiers,
                                    declaration: decl,
                                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            context.Put(fdecl);
            _pool.Free(varList);
        }



        public override void ExitFoxclassvars([NotNull] XP.FoxclassvarsContext context)
        {
            var varType = context.DataType?.Get<TypeSyntax>() ?? _getMissingType();
            varType.XVoDecl = true;
            var list = new List<MemberDeclarationSyntax>();
            foreach (var varCtx in context._Vars)
            {
                if (context.Fld != null)
                {
                    var fielddecl = createField(varCtx.GetText(), varType);
                    list.Add(fielddecl);
                }
                else
                {
                    var propdecl = createProperty(varCtx.GetText(), varType, varCtx);
                    list.Add(propdecl);
                }
            }
            context.CsNode = list;
        }

        public override void EnterFoxmethod([NotNull] XP.FoxmethodContext context)
        {
            Check4ClipperCC(context, context.Sig.ParamList?._Params, context.Sig.CallingConvention?.Convention, context.Sig.Type);
            var name = context.Id.GetText().ToUpper();
            if (name.EndsWith("_ASSIGN"))
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
        }
 
        public override void ExitFoxmethod([NotNull] XP.FoxmethodContext context)
        {
            context.SetSequencePoint(context.T.Start, context.end.Stop);
            var idName = context.Id.Get<SyntaxToken>();
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? DefaultMethodModifiers(false, false, context.TypeParameters != null);
            var isExtern = mods.Any((int)SyntaxKind.ExternKeyword);
            var isAbstract = mods.Any((int)SyntaxKind.AbstractKeyword);
            var hasNoBody = isExtern || isAbstract;
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
            var attributes = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
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
            var parameters = context.ParamList?.Get<ParameterListSyntax>() ?? EmptyParameterList();
            var body = hasNoBody ? null : context.StmtBlk.Get<BlockSyntax>();
            var returntype = context.Type?.Get<TypeSyntax>();
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
                typeParameterList: context.TypeParameters?.Get<TypeParameterListSyntax>(),
                parameterList: parameters,
                constraintClauses: MakeList<TypeParameterConstraintClauseSyntax>(context._ConstraintsClauses),
                body: body,
                expressionBody: null, // TODO: (grammar) expressionBody methods
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
                ClassEntities.Peek().AddVoPropertyAccessor(context, context.RealType, idName);
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
                attributeLists: context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>(),
                modifiers: mods,
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
            if (context.Namespace != null)
            {
                m = AddNameSpaceToMember(context.Namespace, m);

            }
            else
            {
                m = (MemberDeclarationSyntax)CheckTypeName(context, "CLASS", m);
            }
            
            context.Put(m);
            if (context.Data.Partial)
            {
                GlobalEntities.NeedsProcessing = true;
            }
        }

        private MemberDeclarationSyntax createField(string fldName, TypeSyntax type)
        {
            var list = MakeSeparatedList(GenerateVariable(fldName, null));
            var decl = _syntaxFactory.VariableDeclaration(type, list);
            var mods = TokenList(SyntaxKind.PublicKeyword);
            var fdecl = _syntaxFactory.FieldDeclaration(
                                    attributeLists: EmptyList<AttributeListSyntax>(),
                                    modifiers: mods,
                                    declaration: decl,
                                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            return fdecl;
        }

        private MemberDeclarationSyntax createProperty(string fldName, TypeSyntax type, XSharpParserRuleContext context)
        {
            var accessors = _pool.Allocate<AccessorDeclarationSyntax>();
            BlockSyntax body = null;
            if (_options.fox1)
            {
                var call = GenerateMethodCall("_GetProperty", MakeArgumentList(MakeArgument(GenerateLiteral(fldName))), true);
                body = MakeBlock(GenerateReturn(call, true));
                body.XGenerated = true;
            }
            var accessor = _syntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration,
                    EmptyList<AttributeListSyntax>(), EmptyList(),
                    SyntaxFactory.MakeToken(SyntaxKind.GetKeyword),
                    body,
                    null,
                    SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            accessor.XNode = context;
            accessor.XGenerated = true;
            accessors.Add(accessor);
            if (_options.fox1)
            {
                var call = GenerateMethodCall("_SetProperty", MakeArgumentList(MakeArgument(GenerateLiteral(fldName)), MakeArgument(GenerateSimpleName("value"))), true);
                body = MakeBlock(GenerateExpressionStatement(call, true));
                body.XGenerated = true;
            }
            accessor = _syntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration,
                    EmptyList<AttributeListSyntax>(), EmptyList(),
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
            var mods = TokenList(SyntaxKind.PublicKeyword, SyntaxKind.VirtualKeyword);
            var prop = _syntaxFactory.PropertyDeclaration(
                   attributeLists: EmptyList<AttributeListSyntax>(),
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
                var flddecl = createField(context.F.Name.GetText(), _usualType);
                if (flddecl != null)
                {
                    context.Put(flddecl);
                }
            }
            else
            {
                var propdecl = createProperty(context.F.Name.GetText(), _usualType, context);
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
                    var fldinit = cvi.Member;
                    var assign = fldinit.F.Get<ExpressionSyntax>();
                    var stmt = GenerateExpressionStatement(assign);
                    stmt.XNode = fldinit.F;
                    stmts.Add(stmt);
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
                        extended = true;
                        sb.Append('\\');
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
                stringExpr = CreateInterPolatedStringExpression(sourceText);
            }
            else
            {
                stringExpr = GenerateLiteral(sourceText);
            }
            if (hasDelim && context.Merge == null)
            {
                var txtmerge = GenerateMethodCall(XSharpFunctionNames.TextMergeCheck, true);
                stringExpr = MakeConditional(txtmerge, stringExpr, GenerateLiteral(context.String.Text));
            }

            var arg1 = MakeArgument(stringExpr);
            var arg2 = MakeArgument(GenerateLiteral(context.NoShow != null));
            var arg3 = MakeArgument(context.Flags != null ? context.Flags.Get<ExpressionSyntax>() : GenerateLiteral(0));
            var arg4 = MakeArgument(context.Pretext != null ? context.Pretext.Get<ExpressionSyntax>() : GenerateNIL());
            var args = MakeArgumentList(arg1, arg2, arg3, arg4);
            var call = GenerateMethodCall(XSharpFunctionNames.TextSupport, args);

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
                context.Put(GenerateExpressionStatement(assignExpr));
            }
            else
            {
                // no assignment, simply call the TextSupport function
                context.Put(GenerateExpressionStatement(call));
            }
            if (!delimitersOk)
            {
                var stmt = context.Get<StatementSyntax>();
                stmt = stmt.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.WRN_UnbalancedTextMergeOperators));
                context.Put(stmt);
            }
            return;
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
                expr = CreateInterPolatedStringExpression(sourceText);
            }
            else
            {
                expr = GenerateLiteral(sourceText);
            }
            var arg2 = GenerateLiteral(context.B.Type != XP.BACKBACKSLASH);
            var cond = GenerateMethodCall(XSharpFunctionNames.TextMergeCheck, true);  
            if (! hasDelim)
            {
                cond = GenerateLiteral(false);
            }
            var arg1 = MakeConditional(cond, expr, GenerateLiteral(context.String.Text));
            var args = MakeArgumentList(MakeArgument(arg1), MakeArgument(arg2));
            var call = GenerateMethodCall(XSharpFunctionNames.TextOut, args, true); 
            var stmt = GenerateExpressionStatement(call);
            if (!delimitersOk)
                stmt = stmt.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.WRN_UnbalancedTextMergeOperators));
            context.Put(stmt);
            return;
        }
        #endregion

    }
}
