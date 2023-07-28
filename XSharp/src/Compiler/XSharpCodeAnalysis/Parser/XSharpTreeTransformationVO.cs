//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System.Linq;
using Antlr4.Runtime.Misc;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
using Microsoft.CodeAnalysis.PooledObjects;
namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    using Microsoft.CodeAnalysis.Syntax.InternalSyntax;
    class XSharpTreeTransformationVO : XSharpTreeTransformationRT
    {
        private bool voStructHasDim;
        protected override XSharpTreeTransformationCore CreateWalker(XSharpParser parser)
        {
            return new XSharpTreeTransformationVO(parser, _options, _pool, _syntaxFactory, _fileName);
        }

        public XSharpTreeTransformationVO(XSharpParser parser, CSharpParseOptions options, SyntaxListPool pool,
            ContextAwareSyntax syntaxFactory, string fileName) :
            base(parser, options, pool, syntaxFactory, fileName)
        {
        }
        public override void EnterVostruct([NotNull] XP.VostructContext context)
        {
            voStructHasDim = false;
        }

        public override void ExitVostruct([NotNull] XP.VostructContext context)
        {
            context.SetSequencePoint(context.V, context.e.Stop);
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            if (voStructHasDim)
            {
                var modBuilder = _pool.Allocate();
                modBuilder.AddRange(mods);
                modBuilder.Add(SyntaxFactory.MakeToken(SyntaxKind.UnsafeKeyword));
                mods = modBuilder.ToList<SyntaxToken>();
                _pool.Free(modBuilder);
            }
            var attargs = ArrayBuilder<AttributeArgumentSyntax>.GetInstance();
            attargs.Add(_syntaxFactory.AttributeArgument(null, null, GenerateQualifiedName(SystemQualifiedNames.LayoutSequential)));
            if (context.Alignment != null)
            {
                var lit = GenerateLiteral(context.Alignment);
                attargs.Add(_syntaxFactory.AttributeArgument(GenerateNameEquals("Pack"), null, lit));
            }
            var atts = MakeSeparatedList(
                            _syntaxFactory.Attribute(
                                name: GenerateQualifiedName(SystemQualifiedNames.StructLayout),
                                argumentList: MakeAttributeArgumentList(MakeSeparatedList(attargs.ToArrayAndFree()))
                                )
                            );

            MemberDeclarationSyntax m = _syntaxFactory.StructDeclaration(
                attributeLists: MakeList(MakeAttributeList(null, atts)),
                modifiers: mods,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: null,
                constraintClauses: null,
                openBraceToken: SyntaxFactory.OpenBraceToken,
                members: (context._Members?.Count > 0) ? MakeList<MemberDeclarationSyntax>(context._Members) : default,
                closeBraceToken: SyntaxFactory.CloseBraceToken,
                semicolonToken: null);
            m.XVoStructUnion = true;
            if (context.Namespace != null)
            {
                m = AddNameSpaceToMember(context.Namespace, m);
            }
            else
            {
                m = (MemberDeclarationSyntax)CheckForConflictBetweenTypeNameAndNamespaceName(context, "VOSTRUCT", m);
            }
            context.Put(m);
        }

        private TypeSyntax voStructMemberDataType(XP.VostructmemberContext context)
        {
            var varType = getDataType(context.DataType);
            if (context.DataType is XP.SimpleDatatypeContext)
            {
                var sdt = context.DataType as XP.SimpleDatatypeContext;
                if (sdt.TypeName.NativeType != null)
                {
                    if (sdt.TypeName.NativeType.Token.Type == XP.LOGIC)
                    {
                        var winBoolType = _options.XSharpRuntime ? XSharpQualifiedTypeNames.WinBool : VulcanQualifiedTypeNames.WinBool;
                        varType = GenerateQualifiedName(winBoolType);
                    }
                }
                if (sdt.TypeName.XType != null)
                {
                    var winDateType = _options.XSharpRuntime ? XSharpQualifiedTypeNames.WinDate : VulcanQualifiedTypeNames.WinDate; // UInt32
                    if (sdt.TypeName.XType.Token.Type == XP.DATE)
                    {
                        varType = GenerateQualifiedName(winDateType);
                    }
                }
            }
            return varType;
        }

        public override void ExitVostructmember([NotNull] XP.VostructmemberContext context)
        {
            var isDim = context.Dim != null;
            var isUnionMember = (context.Parent is XP.VounionContext);
            var varType = voStructMemberDataType(context);

            varType.XCanBeVoStruct = varType is not PredefinedTypeSyntax;
            if (context.As?.Type == XP.IS)
            {
                varType.XVoIsDecl = true;
            }
            if (isDim)
            {
                voStructHasDim = true;
            }

            SyntaxList<AttributeListSyntax> atts = default;
            if (isUnionMember)
            {
                var args = MakeSeparatedList(
                                        _syntaxFactory.AttributeArgument(null, null,
                                            GenerateLiteral("0", 0)));
                var arglist = MakeAttributeArgumentList(args);
                var att = MakeSeparatedList(
                            _syntaxFactory.Attribute(
                                name: GenerateQualifiedName(SystemQualifiedNames.FieldOffset),
                                argumentList: arglist));
                atts = MakeAttributeList(null, att);
            }
            context.Put(_syntaxFactory.FieldDeclaration(
                atts,
                TokenList(SyntaxKind.PublicKeyword, isDim ? SyntaxKind.FixedKeyword : SyntaxKind.None),
                _syntaxFactory.VariableDeclaration(varType,
                    MakeSeparatedList(
                        isDim ? GenerateBuffer(context.Id.Get<SyntaxToken>(), MakeBracketedArgumentList(context.ArraySub._ArrayIndex.Select(e => _syntaxFactory.Argument(null, null, e.Get<ExpressionSyntax>())).ToArray()))
                        : GenerateVariable(context.Id.Get<SyntaxToken>()))),
                SyntaxFactory.SemicolonToken));
        }

        public override void EnterVounion([NotNull] XP.VounionContext context)
        {
            voStructHasDim = false;
        }

        public override void ExitVounion([NotNull] XP.VounionContext context)
        {
            context.SetSequencePoint(context.U, context.e.Stop);
            var mods = context.Modifiers?.GetList<SyntaxToken>() ?? TokenListWithDefaultVisibility();
            if (voStructHasDim)
            {
                var modBuilder = _pool.Allocate();
                modBuilder.AddRange(mods);
                modBuilder.Add(SyntaxFactory.MakeToken(SyntaxKind.UnsafeKeyword));
                mods = modBuilder.ToList<SyntaxToken>();
                _pool.Free(modBuilder);
            }

            MemberDeclarationSyntax m = _syntaxFactory.StructDeclaration(
                attributeLists: MakeList(
                    MakeAttributeList(
                        target: null,
                        attributes: MakeSeparatedList(
                            _syntaxFactory.Attribute(
                                name: GenerateQualifiedName(SystemQualifiedNames.StructLayout),
                                argumentList: MakeAttributeArgumentList(
                                    MakeSeparatedList(_syntaxFactory.AttributeArgument(null, null, GenerateQualifiedName(SystemQualifiedNames.LayoutExplicit)))
                                    )
                                )
                            ))
                    ),
                modifiers: mods,
                keyword: SyntaxFactory.MakeToken(SyntaxKind.StructKeyword),
                identifier: context.Id.Get<SyntaxToken>(),
                typeParameterList: null,
                baseList: null,
                constraintClauses: null,
                openBraceToken: SyntaxFactory.OpenBraceToken,
                members: (context._Members?.Count > 0) ? MakeList<MemberDeclarationSyntax>(context._Members) : default,
                closeBraceToken: SyntaxFactory.CloseBraceToken,
                semicolonToken: null);
            m.XVoStructUnion = true;
            if (context.Namespace != null)
            {
                m = AddNameSpaceToMember(context.Namespace, m);
            }
            else
            {
                m = (MemberDeclarationSyntax)CheckForConflictBetweenTypeNameAndNamespaceName(context, "UNION", m);
            }
            context.Put(m);
        }
    }
}

