// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;
using xs = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;

namespace Microsoft.CodeAnalysis.CSharp
{
    // An invariant of a merged type declaration is that all of its children are also merged
    // declarations.
    internal sealed partial class MergedTypeDeclaration : MergedNamespaceOrTypeDeclaration
    {

        void AddVulcanTypeAttributes(ArrayBuilder<SyntaxList<AttributeListSyntax>> attributeSyntaxListBuilder)
        {
            //if (this.Kind == DeclarationKind.Class )
            //{
            //    bool haslayout = false;
            //    foreach (var atlist in attributeSyntaxListBuilder)
            //    {
            //        // atlist = SyntaxList<AttributeListSyntax>
            //        foreach (var attList in atlist)
            //        {
            //            foreach (AttributeSyntax att in attList.Attributes)
            //            {
            //                if (att.Name.GetText().ToString().ToLower() == "structlayoutattribute")
            //                    haslayout = true;
            //            }
            //        }
            //    }
            //    if (! haslayout && this.Name != "Functions" )
            //    {
            //        var a = xs.XSharpVOTreeTransformation.VOClassAttribs;
            //        var als = new AttributeListSyntax(a[0], null, 0);
            //        var list = new SyntaxList<AttributeListSyntax>(als);
            //        attributeSyntaxListBuilder.Add(list);
            //    }
            //}
        }
    }
}
