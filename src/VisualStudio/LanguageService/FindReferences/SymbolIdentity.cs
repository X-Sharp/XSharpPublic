//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using XSharpModel;

namespace XSharp.LanguageService.FindReferences
{
    /// <summary>
    /// Decides whether two symbols returned by the lookup are the same declaration.
    /// </summary>
    internal static class SymbolIdentity
    {
        internal static bool IsSame(IXSymbol target, IXSymbol candidate)
        {
            if (target == null || candidate == null)
                return false;
            if (ReferenceEquals(target, candidate))
                return true;
            // partial classes are declared in several places but are one type
            if (target is IXTypeSymbol targetType && candidate is IXTypeSymbol candidateType)
                return NameEquals(targetType.FullName, candidateType.FullName);
            if (target is IXTypeSymbol || candidate is IXTypeSymbol)
                return false;
            // ACCESS, ASSIGN and PROPERTY with the same name form one property
            if (target is IXMemberSymbol targetMember && candidate is IXMemberSymbol candidateMember &&
                IsPropertyLike(target.Kind) && IsPropertyLike(candidate.Kind))
            {
                return NameEquals(target.Name, candidate.Name) &&
                    NameEquals(targetMember.ParentType?.FullName, candidateMember.ParentType?.FullName);
            }
            if (target is XSourceSymbol targetSource && candidate is XSourceSymbol candidateSource)
            {
                // members read from the database have no file: compare type, name and signature
                if ((targetSource.File == null || candidateSource.File == null) &&
                    target is IXMemberSymbol targetMember2 && candidate is IXMemberSymbol candidateMember2)
                {
                    return target.Kind == candidate.Kind &&
                        NameEquals(target.Name, candidate.Name) &&
                        NameEquals(targetMember2.ParentType?.FullName, candidateMember2.ParentType?.FullName) &&
                        string.Equals(targetMember2.ParameterList ?? "", candidateMember2.ParameterList ?? "", StringComparison.OrdinalIgnoreCase);
                }
                return target.Kind == candidate.Kind &&
                    NameEquals(target.Name, candidate.Name) &&
                    targetSource.Interval.Start == candidateSource.Interval.Start &&
                    NameEquals(targetSource.File?.FullPath, candidateSource.File?.FullPath);
            }
            if (target is XSourceSymbol || candidate is XSourceSymbol)
                return false;
            return target.Kind == candidate.Kind && NameEquals(target.FullName, candidate.FullName);
        }

        /// <summary>
        /// Does a symbol resolved at a candidate token refer to the target? <c>Foo{...}</c> resolves to a
        /// constructor, and that is also a reference to the class.
        /// </summary>
        internal static bool IsReferenceTo(IXSymbol target, IXSymbol candidate)
        {
            if (IsSame(target, candidate))
                return true;
            if (target is IXTypeSymbol type && candidate is IXMemberSymbol member && candidate.Kind == Kind.Constructor)
            {
                return member.ParentType != null
                    ? IsSame(type, member.ParentType)
                    : NameEquals(type.FullName, member.DeclaringType);
            }
            return false;
        }

        internal static bool IsPropertyLike(Kind kind)
        {
            return kind == Kind.Property || kind == Kind.Access || kind == Kind.Assign;
        }

        private static bool NameEquals(string lhs, string rhs)
        {
            return lhs != null && rhs != null && string.Equals(lhs, rhs, StringComparison.OrdinalIgnoreCase);
        }
    }
}
