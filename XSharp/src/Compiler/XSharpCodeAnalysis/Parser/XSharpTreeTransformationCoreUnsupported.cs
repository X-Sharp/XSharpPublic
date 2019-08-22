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
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    using Antlr4.Runtime.Misc;
    using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
    internal partial class XSharpTreeTransformationCore
    {
        internal CSharpSyntaxNode NotInDialect(string feature)
        {
            CSharpSyntaxNode node = _syntaxFactory.EmptyStatement(SyntaxFactory.MakeToken(SyntaxKind.SemicolonToken));
            return NotInDialect(node, feature);
        }
        internal T NotInDialect<T>(T node, string feature, string additional = "") where T : CSharpSyntaxNode
        {
            return node.WithAdditionalDiagnostics(
                new SyntaxDiagnosticInfo(ErrorCode.ERR_FeatureNotAvailableInDialect, feature, _options.Dialect.ToString() + " " + additional));
        }

        private MemberDeclarationSyntax makeDummyType(XP.IdentifierContext id)
        {
            MemberDeclarationSyntax m = _syntaxFactory.StructDeclaration(
                attributeLists: EmptyList<AttributeListSyntax>(),
                modifiers: null,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: null,
                constraintClauses: null,
                openBraceToken: SyntaxFactory.MakeToken(SyntaxKind.OpenBraceToken),
                members: EmptyList<MemberDeclarationSyntax>(),
                closeBraceToken: SyntaxFactory.MakeToken(SyntaxKind.CloseBraceToken),
                semicolonToken: null);
            return m;
        }

        public override void ExitVostruct([NotNull] XP.VostructContext context)
        {
            var m = makeDummyType(context.Id);
            context.Put(NotInDialect(m, "VOSTRUCT"));
        }

        public override void ExitVounion([NotNull] XP.VounionContext context)
        {
            var m = makeDummyType(context.Id);
            context.Put(NotInDialect(m, "UNION"));
        }
        public override void ExitXbasedeclStmt([NotNull] XP.XbasedeclStmtContext context)
        {
            context.Put(context.xbasedecl().Get<StatementSyntax>());
        }

        public override void ExitXbasedecl([NotNull] XP.XbasedeclContext context)
        {
            context.Put(NotInDialect(context.T.Text + " statement"));
        }

        public override void ExitSeqStmt([NotNull] XP.SeqStmtContext context)
        {
            context.Put(NotInDialect("BEGIN SEQUENCE statement"));
            return;
        }
        public override void ExitXppclass([NotNull] XP.XppclassContext context)
        {
            var m = makeDummyType(context.Id);
            context.Put(NotInDialect(m, "Xbase++ Class Syntax"));
            return;
        }
        public override void ExitXppmethod([NotNull] XP.XppmethodContext context)
        {
            var m = makeDummyType(context.Id);
            context.Put(NotInDialect(m, "Xbase++ Method Syntax"));
        }

        public override void ExitFilewidememvar([NotNull] XP.FilewidememvarContext context)
        {
            MemberDeclarationSyntax m;
            if (context._Vars.Count > 0)
            {
                m = makeDummyType(context._Vars[0].Id);
            }
            else
            {
                m = makeDummyType(context._XVars[0].Id.Id);
            }
            context.Put(NotInDialect(m, "File Dynamic Memory Variables"));

        }
        public override void ExitFoxclass([NotNull] XP.FoxclassContext context)
        {
            var m = makeDummyType(context.Id);
            context.Put(NotInDialect(m, "FoxPro Class Syntax"));
            return;
        }

        public override void ExitTextStmt([NotNull] XP.TextStmtContext context)
        {
            context.Put(NotInDialect("TEXT .. ENDTEXT statement"));
            return;
        }
        public override void ExitTextoutStmt([NotNull] XP.TextoutStmtContext context)
        {
            context.Put(NotInDialect("TextMerge output statement ('\\' or '\\\\')"));
            return;
        }
        public override void ExitRecoverBlock([NotNull] XP.RecoverBlockContext context)
        {
            context.Put(NotInDialect("RECOVER USING block"));
            return;
        }
        public override void ExitArrayOfType([NotNull] XP.ArrayOfTypeContext context)
        {
            context.Put(NotInDialect(_objectType, "ARRAY OF <type>"));
        }

        public override void ExitXbaseType([NotNull] XP.XbaseTypeContext context)
        {
            var type = NotInDialect(_objectType, context.Token.Text);
            context.Put(type);
        }
        public override void ExitFielddecl([NotNull] XP.FielddeclContext context)
        {
            context.Put(NotInDialect("FIELD statement"));
            return;
        }

        private ExpressionSyntax NoAlias()
        {
            return NotInDialect(GenerateLiteral("alias"), "ALIAS(->) operator");
        }
        public override void ExitAliasedMemvar([NotNull] XP.AliasedMemvarContext context)
        {
            context.Put(NoAlias());
            return;
        }
        public override void ExitAliasedField([NotNull] XP.AliasedFieldContext context)
        {
            context.Put(NoAlias());
            return;
        }

        public override void ExitAliasedFieldLate([NotNull] XP.AliasedFieldLateContext context)
        {
            context.Put(NoAlias());
            return;
        }
        public override void ExitMacro([NotNull] XP.MacroContext context)
        {
            context.Put(NotInDialect(GenerateLiteral("macro"), "MACRO compiler"));
            return;
        }
        public override void ExitMacroName([NotNull] XP.MacroNameContext context)
        {
            context.Put(NotInDialect(GenerateLiteral("macro"), "MACRO compiler"));
            return;
        }
        public override void ExitAccessMemberLate([NotNull] XP.AccessMemberLateContext context)
        {
            context.Put(NotInDialect(GenerateLiteral("value"), "Late bound member access"));
            return;
        }
        public override void ExitAccessMemberLateName([NotNull] XP.AccessMemberLateNameContext context)
        {
            context.Put(NotInDialect(GenerateLiteral("value"), "Late bound member access"));
            return;
        }
       

    }
}
