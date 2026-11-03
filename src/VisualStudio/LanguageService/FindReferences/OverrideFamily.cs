//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using XSharpModel;

namespace XSharp.LanguageService.FindReferences
{
    /// <summary>
    /// A method or property, the member it overrides and every override of that member form one family: a call on
    /// any of them can reach the others at runtime. VIRTUAL and OVERRIDE are not checked (with /vo3 every method is
    /// virtual); the same name, kind and parameter list in a derived class is an override. Only source types count.
    /// </summary>
    internal static class OverrideFamily
    {
        /// <summary>The topmost member up the base class chain that the member overrides, or the member itself.</summary>
        internal static IXMemberSymbol Root(IXMemberSymbol member, Func<IXTypeSymbol, IXTypeSymbol> baseOf = null)
        {
            if (member == null || !CanOverride(member.Kind) || member.ParentType == null)
                return member;
            baseOf = baseOf ?? (t => t.BaseType);
            var root = member;
            var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase) { member.ParentType.FullName ?? "" };
            for (var type = baseOf(member.ParentType); type is XSourceTypeSymbol && seen.Add(type.FullName ?? ""); type = baseOf(type))
            {
                var match = type.Members?.FirstOrDefault(m => SameSlot(member, m));
                if (match != null)
                    root = match;
            }
            return root;
        }

        /// <summary>Is the candidate the target, a member it overrides, or another override of the same member?</summary>
        internal static bool Contains(IXSymbol target, IXSymbol candidate, Func<IXTypeSymbol, IXTypeSymbol> baseOf = null)
        {
            if (!(target is IXMemberSymbol targetMember) || !(candidate is IXMemberSymbol candidateMember) ||
                !CanOverride(target.Kind) || !SameSlot(targetMember, candidateMember))
                return false;
            return SymbolIdentity.IsSame(Root(targetMember, baseOf), Root(candidateMember, baseOf));
        }

        private static bool CanOverride(Kind kind)
        {
            return kind == Kind.Method || SymbolIdentity.IsPropertyLike(kind);
        }

        /// <summary>Same name and, for methods, the same kind and parameter list.</summary>
        private static bool SameSlot(IXMemberSymbol member, IXMemberSymbol other)
        {
            if (!string.Equals(member.Name, other.Name, StringComparison.OrdinalIgnoreCase))
                return false;
            if (SymbolIdentity.IsPropertyLike(member.Kind) && SymbolIdentity.IsPropertyLike(other.Kind))
                return true;
            return member.Kind == other.Kind &&
                string.Equals(member.ParameterList ?? "", other.ParameterList ?? "", StringComparison.OrdinalIgnoreCase);
        }
    }
}
