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
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    using Microsoft.CodeAnalysis.Syntax.InternalSyntax;
    using System.Diagnostics;

    [DebuggerDisplay("{Name}")]

    internal class XSharpFoxTreeTransformation : XSharpVOTreeTransformation
    {
        public XSharpFoxTreeTransformation(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool,
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
    }
}
