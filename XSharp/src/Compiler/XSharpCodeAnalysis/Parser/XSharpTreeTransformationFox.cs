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
                token.line = 1;
                token.charPositionInLine = 1;
                func.T = token;
                token = new XSharpToken(XP.ID, name);
                token.line = 1;
                token.charPositionInLine = 1;
                 id.Start = id.Stop = token;
                ExitIdentifier(id);    // Generate SyntaxToken 
                if (string.Equals(name, _entryPoint, StringComparison.OrdinalIgnoreCase))
                {
                    func.Type = new XP.DatatypeContext(func,0);
                    func.Type.Start = new XSharpToken(XP.AS, "AS");
                    func.Type.Stop = new XSharpToken(XP.VOID, "VOID");
                    func.Type.Put(_voidType);
                }
                func.Attributes = new XP.AttributesContext(func,0);
                func.Attributes.PutList(MakeCompilerGeneratedAttribute());
                func.Id = id;
                func.StmtBlk = context.StmtBlk;
                context.StmtBlk.parent = func;
                func.Start = func.StmtBlk.Start;
                func.Stop = func.StmtBlk.Stop;
                func.AddChild(func.Id);
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
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }
        public override void ExitFoxmethod([NotNull] XP.FoxmethodContext context)
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


        public override void ExitFoxclassvar([NotNull] XP.FoxclassvarContext context)
        {
            var classvars = context.Parent as XP.FoxclassvarsContext;
            var dataType = classvars.DataType;
            var varType = dataType?.Get<TypeSyntax>() ?? _getMissingType();
            ExpressionSyntax initExpr = null;
            if (dataType != null)
                initExpr = GenerateInitializer(dataType);
            context.Put(GenerateVariable(context.Id.GetText(), initExpr));
        }

        public override void ExitFoxclassvars([NotNull] XP.FoxclassvarsContext context)
        {
            var varList = _pool.AllocateSeparated<VariableDeclaratorSyntax>();
            var varType = context.DataType?.Get<TypeSyntax>() ?? _getMissingType();
            varType.XVoDecl = true;
            foreach (var varCtx in context._Vars)
            {
                if (varList.Count > 0)
                    varList.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                varList.Add(varCtx.Get<VariableDeclaratorSyntax>());
            }
            if (varList.Count > 0)
            {
                var decl = _syntaxFactory.VariableDeclaration( type: varType, variables: varList); 
                var attributeList = context.Attributes?.GetList<AttributeListSyntax>() ?? EmptyList<AttributeListSyntax>();
                var modifiers = context.Modifiers?.GetList<SyntaxToken>() ?? TokenList(SyntaxKind.PublicKeyword);
                context.Put(_syntaxFactory.FieldDeclaration(
                                    attributeLists: attributeList,
                                    modifiers: modifiers,
                                    declaration: decl,
                                    semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken)));
            }
            _pool.Free(varList);
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
            // Do this after VOProps generation because GenerateVOProperty sets the members
            // for Access & Assign to NULL
            foreach (var mCtx in context._Members)
            {
                if (mCtx.CsNode != null)
                { 
                    members.Add(mCtx.Get<MemberDeclarationSyntax>());
                }
                if (mCtx is XP.FoxclsvarsContext fcfc)
                {
                    var mem = fcfc.Member;
                    foreach (var v in mem._Vars)
                    {
                        fieldNames.Add(v.Id.GetText().ToLower());
                    }
                }
            }
            generated.Free();
            foreach (var mem  in context._Members)
            {
                if (mem is XP.FoximplementsContext fic)
                {
                    var clause = fic.Member as XP.FoximplementsclauseContext;
                    var type = clause.Type.Get<TypeSyntax>();
                    if (baseTypes.Count > 0)
                        baseTypes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                    baseTypes.Add(_syntaxFactory.SimpleBaseType(type));
 
                }
            }
            var ctor = createConstructor(context, members, fieldNames);
            if (ctor != null)
            {
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

        private ConstructorDeclarationSyntax createConstructor(XP.FoxclassContext context, SyntaxListBuilder<MemberDeclarationSyntax> members, List<String> fieldNames)
        {
            var stmts = new List<StatementSyntax>();
            var attributeLists = _pool.Allocate<AttributeListSyntax>();
            bool hasinit = false;
            ParameterListSyntax initparams = EmptyParameterList();
            ConstructorDeclarationSyntax ctor = null;
            foreach (var member in context._Members)
            {
                if (member is XP.FoxclsvarinitContext cvi)
                {
                    var fldinit = cvi.Member.F;
                    // skip PEMName_COMATTRIB  assignments
                    if (fldinit.Name.GetText().ToUpper().EndsWith("_COMATTRIB"))
                    {
                        continue;
                    }
                    var assign = fldinit.Get<ExpressionSyntax>();
                    // check to see if the field exists
                    var fldName = fldinit.Name.GetText().ToLower();
                    if (!fieldNames.Contains(fldName))
                    {
                        // create a field and set the value as default value
                        fieldNames.Add(fldName);
                        var list = MakeSeparatedList(GenerateVariable(fldinit.Name.GetText(), fldinit.Expr.Get<ExpressionSyntax>()));
                        var decl = _syntaxFactory.VariableDeclaration(type: _usualType, list);
                        var mods = TokenList(SyntaxKind.PublicKeyword);
                        var fdecl = _syntaxFactory.FieldDeclaration(
                                                attributeLists: attributeLists,
                                                modifiers: mods,
                                                declaration: decl,
                                                semicolonToken: SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
                        members.Add(fdecl);
                        fldinit.Put(fdecl);
                    }
                    else
                    {
                        var stmt = GenerateExpressionStatement(assign);
                        fldinit.Put(stmt);
                        stmts.Add(stmt);
                    }
                }
                else if (member is XP.FoxmethodContext fm)
                {
                    var method = fm.Member as XP.MethodContext;
                    if (method.Id.GetText().ToLower() == "init")
                    {
                        var syntax = method.Get<MethodDeclarationSyntax>();
                        initparams = syntax.ParameterList;
                        attributeLists.AddRange( syntax.AttributeLists);
                        hasinit = true;
                    }
                }
            }
            if (stmts.Count > 0 || hasinit)
            {
                var body= MakeBlock(stmts);
                var argList = new List<ArgumentSyntax>();
                for (int i = 0; i < initparams.Parameters.Count; i++)
                {
                    var par = initparams.Parameters[i];
                    argList.Add(MakeArgument(GenerateSimpleName(par.Identifier.Text)));
                }
                ArgumentListSyntax args = MakeArgumentList(argList.ToArray());
                var chain = _syntaxFactory.ConstructorInitializer(SyntaxKind.BaseConstructorInitializer,
                                                                    SyntaxFactory.MakeToken(SyntaxKind.ColonToken),
                                                                    SyntaxFactory.MakeToken(SyntaxKind.BaseKeyword),
                                                                    args
                                                                    );
                
                GenerateAttributeList(attributeLists, SystemQualifiedNames.CompilerGenerated);
                var mods = TokenList(SyntaxKind.PublicKeyword);
                ctor = _syntaxFactory.ConstructorDeclaration(attributeLists, mods, context.Id.Get<SyntaxToken>(), initparams, chain, body, null, null);
                ctor.XGenerated = true;
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
