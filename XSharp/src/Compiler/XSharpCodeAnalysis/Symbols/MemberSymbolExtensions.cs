//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.Cci;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal static partial class MemberSymbolExtensions
    {

        internal static IEnumerable<ICustomAttribute> GetConstructorAttributes(this SourceConstructorSymbol ctor, IEnumerable<ICustomAttribute> attribs)
        {
            if (!ctor.IsGeneratedConstructor())
                return attribs;
            var list = new List<ICustomAttribute>();
            foreach (var att in attribs)
            {
                if (!att.ToString().Contains(OurTypeNames.ClipperCallingConventionAttribute))
                    list.Add(att);
            }
            if (ctor.ParameterCount != 0)
            {
                var baseCtor = ctor.BaseConstructor() as MethodSymbol;
                if (baseCtor != null)
                {
                    foreach (var att in baseCtor.GetAttributes())
                    {
                        if (att.ToString().Contains("Clipper"))
                            list.Add(att);
                    }
                }
            }
            return list;

        }
        internal static MethodSymbol BaseConstructor(this SourceConstructorSymbol ctor)
        {
            var container = ctor.ContainingSymbol as SourceMemberContainerTypeSymbol;
            return container.BaseConstructor();
        }

        internal static MethodSymbol BaseConstructor(this SourceMemberContainerTypeSymbol type)
        {
            var baseType = type.BaseTypeNoUseSiteDiagnostics;
            var members = baseType.GetMembers(".ctor");
            if (members.Length == 1)
            {
                var member = members[0] as MethodSymbol;
                if (member is SourceConstructorSymbol)
                {
                    var ctor = member as SourceConstructorSymbol;
                    if (!ctor.IsGeneratedConstructor())
                        return member;
                    return ctor.BaseConstructor();
                }
                return member;
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

        internal static bool IsGeneratedConstructor(this MethodSymbol  ctor)
        {
            if (ctor is SourceConstructorSymbol)
            {
                var ctorSymbol = ctor as SourceConstructorSymbol;
                var syntax = ctorSymbol.SyntaxNode;
                return syntax.XGenerated;
            }
            return false;
        }
        internal static bool HasClipperCallingConvention(this Symbol method)
        {
            if (method is SourceMemberMethodSymbol)
            {
                var sms = method as SourceMemberMethodSymbol;
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
