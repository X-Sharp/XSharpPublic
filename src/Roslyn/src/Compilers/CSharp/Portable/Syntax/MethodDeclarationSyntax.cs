// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.
#nullable disable
using Microsoft.CodeAnalysis.CSharp.Syntax;


namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal partial class MethodDeclarationSyntax
    {
        internal MethodDeclarationSyntax UpdateBody(BlockSyntax body)
        {
            return this.Update(this.AttributeLists, this.Modifiers, this.ReturnType,
                this.ExplicitInterfaceSpecifier, this.Identifier, this.TypeParameterList,
                this.ParameterList, this.ConstraintClauses, body, this.ExpressionBody, this.SemicolonToken);
        }
    }
}

namespace Microsoft.CodeAnalysis.CSharp.Syntax
{
    public partial class MethodDeclarationSyntax
    {
        public int Arity
        {
            get
            {
                return this.TypeParameterList == null ? 0 : this.TypeParameterList.Parameters.Count;
            }
        }
    }
}

namespace Microsoft.CodeAnalysis.CSharp
{
    public partial class SyntaxFactory
    {
        public static MethodDeclarationSyntax MethodDeclaration(
            SyntaxList<AttributeListSyntax> attributeLists,
            SyntaxTokenList modifiers,
            TypeSyntax returnType,
            ExplicitInterfaceSpecifierSyntax explicitInterfaceSpecifier,
            SyntaxToken identifier,
            TypeParameterListSyntax typeParameterList,
            ParameterListSyntax parameterList,
            SyntaxList<TypeParameterConstraintClauseSyntax> constraintClauses,
            BlockSyntax body,
            SyntaxToken semicolonToken)
        {
            return SyntaxFactory.MethodDeclaration(
                attributeLists,
                modifiers,
                returnType,
                explicitInterfaceSpecifier,
                identifier,
                typeParameterList,
                parameterList,
                constraintClauses,
                body,
                null,
                semicolonToken);
        }
    }
}
