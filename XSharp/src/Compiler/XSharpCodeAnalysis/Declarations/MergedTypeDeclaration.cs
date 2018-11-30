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
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;
using xs = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using Microsoft.CodeAnalysis.PooledObjects;
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
