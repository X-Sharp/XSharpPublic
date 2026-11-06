//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using XSharpModel;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>The members of the base classes that a class can override and does not override yet.</summary>
    internal static class OverrideCandidates
    {
        /// <summary>
        /// Methods, ACCESS, ASSIGN and properties of the base types of <paramref name="type"/> (nearest base first,
        /// then by name) that are overridable – every one of a class for which <paramref name="allVirtual"/> is true
        /// (VO dialects), otherwise VIRTUAL / ABSTRACT / OVERRIDE ones – and not static, sealed or private. A member
        /// that the class or a nearer base already declares with the same kind, name and parameter types is skipped.
        /// </summary>
        internal static IReadOnlyList<IXMemberSymbol> For(IXTypeSymbol type, Func<IXTypeSymbol, IXTypeSymbol> baseOf,
            Func<IXTypeSymbol, bool> allVirtual, bool caseSensitive)
        {
            var result = new List<IXMemberSymbol>();
            if (type == null)
                return result;
            var declared = new HashSet<string>(StringComparer.Ordinal);
            bool first = true;
            foreach (var current in GenerateMethodRule.SelfAndBases(type, baseOf))
            {
                var members = current.Members.Where(m => Category(m.Kind) != null).ToList();
                if (!first)
                {
                    bool virtualClass = allVirtual(current);
                    foreach (var member in members.OrderBy(m => m.Name, StringComparer.OrdinalIgnoreCase))
                    {
                        if (!declared.Contains(Signature(member, caseSensitive)) && IsOverridable(member, virtualClass))
                            result.Add(member);
                    }
                }
                foreach (var member in members)
                    declared.Add(Signature(member, caseSensitive));
                first = false;
            }
            return result;
        }

        private static bool IsOverridable(IXMemberSymbol member, bool virtualClass)
        {
            var modifiers = member.Modifiers;
            if ((modifiers & (Modifiers.Static | Modifiers.Sealed)) != 0 || member.IsStatic)
                return false;
            if ((modifiers & Modifiers.VisibilityMask) == Modifiers.Private)
                return false;
            return virtualClass || (modifiers & (Modifiers.Virtual | Modifiers.Abstract | Modifiers.Override)) != 0;
        }

        private static string Category(Kind kind)
        {
            switch (kind)
            {
                case Kind.Method: return "M";
                case Kind.Access: return "A";
                case Kind.Assign: return "S";
                case Kind.Property: return "P";
                default: return null;
            }
        }

        private static string Signature(IXMemberSymbol member, bool caseSensitive)
        {
            string Norm(string s) => caseSensitive ? (s ?? "") : (s ?? "").ToLowerInvariant();
            var types = member.Parameters?.Select(p => Norm(p.TypeName)) ?? Enumerable.Empty<string>();
            return Category(member.Kind) + "|" + Norm(member.Name) + "(" + string.Join(",", types) + ")";
        }
    }
}
