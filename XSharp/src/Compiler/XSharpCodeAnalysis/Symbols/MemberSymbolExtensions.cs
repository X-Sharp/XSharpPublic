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
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.Symbols;
using Microsoft.CodeAnalysis.CSharp.Symbols.Metadata.PE;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal static partial class MemberSymbolExtensions
    {


        internal static MethodSymbol BaseConstructor(this SourceMemberContainerTypeSymbol type)
        {
            var baseType = type.BaseType;
            var members = baseType.GetMembers(".ctor");
            if (members.Length == 1)
            {
                return members[0] as MethodSymbol;
            }
            return null;
        }
        internal static bool suppressGeneratedConstructorParams( this ConstructorDeclarationSyntax syntax, SourceMemberContainerTypeSymbol type)
        {
            if (!syntax.XGenerated)
                return false;
            var baseCtor = type.BaseConstructor();
            if (baseCtor == null || !baseCtor.HasClipperCallingConvention())
            {
                return true;
            }
            return false;
        }
        internal static bool CtorWasGenerated(this SourceConstructorSymbol ctor)
        {
            var syntax = ctor.SyntaxNode;
            return ctor.CtorWasGenerated();
        }

        internal static bool CtorWasGenerated(this MethodSymbol  ctor)
        {
            if (ctor is SourceConstructorSymbol)
            {
                var ctorSymbol = ctor as SourceConstructorSymbol;
                var syntax = ctorSymbol.SyntaxNode;
                return syntax.XGenerated;
            }
            return false;
        }
        internal static ImmutableArray<CSharpAttributeData> GetBaseAttributes(this SourceConstructorSymbol ctor)
        {
            var type = ctor.ContainingType as SourceMemberContainerTypeSymbol;
            var baseCtor = type.BaseConstructor();
            if (baseCtor != null && baseCtor.HasClipperCallingConvention())
            {
                if (baseCtor is SourceConstructorSymbol)
                {
                    var scs = baseCtor as SourceConstructorSymbol;
                    if (scs.CtorWasGenerated())
                        return scs.GetBaseAttributes();
                    else
                        return baseCtor.GetAttributes();
                }
                return baseCtor.GetAttributes();
            }
            return ImmutableArray<CSharpAttributeData>.Empty;
        }
        internal static bool HasClipperCallingConvention(this Symbol method)
        {
            if (method is SourceMethodSymbol)
            {
                var sms = method as SourceMethodSymbol;
                var xnode = sms.SyntaxNode?.XNode;
                if (xnode != null)
                {
                    var clsmethod = xnode as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.ClsmethodContext;
                    if (clsmethod != null)
                    {
                        return clsmethod.Member.Data.HasClipperCallingConvention;
                    }
                }
            }
            var pars = method.GetParameters();
            if (pars.Length != 1)
                return false;
            var par = pars[0];
            if (par.Name == XSharpSpecialNames.ClipperArgs)
                return true;
            if (par.Name == VulcanSpecialNames.ClipperArgs)
                return true;
            return false;
        }
    }

}