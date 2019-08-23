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

        public override void ExitAccessMember([NotNull] XP.AccessMemberContext context)
        {
            CoreAccessMember(context);
        }

         public override void ExitFoxclsvars([NotNull] XP.FoxclsvarsContext context)
        {
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }
        public override void ExitFoxfmethod([NotNull] XP.FoxfmethodContext context)
        {
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }
        public override void ExitFoxpmethod([NotNull] XP.FoxpmethodContext context)
        {
            context.Put(context.Member.Get<MemberDeclarationSyntax>());
        }

        public override void EnterFoxclass([NotNull] XP.FoxclassContext context)
        {
        }

        public override void ExitFoxclass([NotNull] XP.FoxclassContext context)
        {
            /*
            var members = _pool.Allocate<MemberDeclarationSyntax>();
            var generated = ClassEntities.Pop();
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            context.Data.Partial = mods.Any((int)SyntaxKind.PartialKeyword);
            if (generated.Members.Count > 0)
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
            // check if class has Ctor. 
            foreach (var mem in members.ToList())
            {
                // when an instant constructors then remember this
                if (mem is ConstructorDeclarationSyntax && !((ConstructorDeclarationSyntax)mem).IsStatic())
                {
                    context.Data.HasInstanceCtor = true;
                    break;
                }
            }

            // Do this after VOProps generation because GenerateVOProperty sets the members
            // for Access & Assign to NULL
            foreach (var mCtx in context._Members)
            {
                if (mCtx.CsNode != null)
                    members.Add(mCtx.Get<MemberDeclarationSyntax>());
            }
            generated.Free();
            var baseTypes = _pool.AllocateSeparated<BaseTypeSyntax>();
            var baseType = context.BaseType?.Get<TypeSyntax>();
            if (baseType != null)
            {
                baseTypes.Add(_syntaxFactory.SimpleBaseType(baseType));
            }
            foreach (var iCtx in context._Implements)
            {
                if (baseTypes.Count > 0)
                    baseTypes.AddSeparator(SyntaxFactory.MakeToken(SyntaxKind.CommaToken));
                baseTypes.Add(_syntaxFactory.SimpleBaseType(iCtx.Get<TypeSyntax>()));
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
            m = CheckForGarbage(m, context.Ignored, "Name after END CLASS");
            context.Put(m);
            if (context.Data.Partial)
            {
                GlobalEntities.NeedsProcessing = true;
            }
            */
        }

        public override void EnterFfunction([NotNull] XP.FfunctionContext context)
        {

        }
        public override void ExitFfunction([NotNull] XP.FfunctionContext context)
        {

        }
        public override void EnterFprocedure([NotNull] XP.FprocedureContext context)
        {

        }
        public override void ExitFprocedure([NotNull] XP.FprocedureContext context)
        {

        }

        public override void EnterFoxfieldinitializer([NotNull] XP.FoxfieldinitializerContext context)
        {

        }

        public override void ExitFoxfieldinitializer([NotNull] XP.FoxfieldinitializerContext context)
        {

        }

        public override void EnterFoxaddobject([NotNull] XP.FoxaddobjectContext context)
        {

        }
        public override void ExitFoxaddobject([NotNull] XP.FoxaddobjectContext context)
        {

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

        public override void ExitTextStmt([NotNull] XP.TextStmtContext context)
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

        public override void ExitTextoutStmt([NotNull] XP.TextoutStmtContext context)
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
