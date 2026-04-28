//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.Cci;
using static LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

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
            var members = baseType.GetMembers(WellKnownMemberNames.InstanceConstructorName);
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
        internal static bool SuppressGeneratedConstructorParams( this ConstructorDeclarationSyntax syntax, SourceMemberContainerTypeSymbol type)
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
            if (ctor is SourceConstructorSymbol ctorSymbol)
            {
                var syntax = ctorSymbol.SyntaxNode;
                return syntax.XGenerated;
            }
            return false;
        }

        internal static bool HasFoxArrayParameter(this Symbol method, out CSharpAttributeData attr)
        {
            attr = null;
            if (method is MethodSymbol)
            {
                foreach (var att in method.GetAttributes())
                {
                    var cls = att.AttributeClass;
                    if (cls.Name == OurTypeNames.FoxArrayInputParameterAttribute &&
                        string.Equals(cls.ConstructedFrom.ContainingAssembly.Name,XSharpAssemblyNames.XSharpCore,System.StringComparison.OrdinalIgnoreCase))
                    {
                        attr = att;
                        return true;
                    }
                }
            }
            return false;
        }
        internal static bool HasClipperCallingConvention(this Symbol method)
        {
            if (method is SourceMemberMethodSymbol sms)
            {
                var xnode = sms.SyntaxNode?.XNode;
                if (xnode is ClsmethodContext clsmethod)
                {
                    xnode = clsmethod.Member;
                }
                if (xnode is IMemberContext member)
                {
                    return member.Data.HasClipperCallingConvention;
                }
                if (xnode is IXPPMemberContext xmember)
                {
                    return xmember.Data.HasClipperCallingConvention;
                }
            }
            if (method is MethodSymbol)
            {
                var pars = method.GetParameters();
                if (pars.Length != 1)
                    return false;
                var par = pars[0];
                if (par.IsParams)
                { 
                    if (par.Name == XSharpSpecialNames.ClipperArgs)
                        return true;
                    //if (par.Type is ArrayTypeSymbol ats && ats.ElementType.IsUsualType())
                    //{ 
                    //  return true;
                    //}
                }
            }

            return false;
        }
        internal static bool EndsWithUsualParams(this Symbol method)
        {
            if (method is MethodSymbol ms && ms.IsParams())
            {
                var parType = ms.Parameters[ms.ParameterCount - 1].Type;
                return parType is ArrayTypeSymbol at && at.ElementType.IsUsualType();
            }
            return false;
        }
    }

}
