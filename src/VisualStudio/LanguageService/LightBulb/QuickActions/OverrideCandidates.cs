//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
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
                        if (!IsFinalize(member) && !Keys(member, caseSensitive).Any(declared.Contains) && IsOverridable(member, virtualClass))
                            result.Add(member);
                    }
                }
                foreach (var member in members)
                    declared.UnionWith(Keys(member, caseSensitive));
                first = false;
            }
            return result;
        }

        /// <summary>
        /// False when the only candidates are the virtual members of System.Object (ToString, Equals, GetHashCode):
        /// every class has them, so the light bulb would be lit everywhere for them alone.
        /// </summary>
        internal static bool WorthOffering(IReadOnlyList<IXMemberSymbol> candidates)
        {
            return candidates.Any(m => !string.Equals((m.Parent as IXTypeSymbol)?.FullName ?? m.ParentType?.FullName,
                "System.Object", StringComparison.OrdinalIgnoreCase));
        }

        /// <summary>Overriding Finalize is an error (a destructor is written instead).</summary>
        private static bool IsFinalize(IXMemberSymbol member)
        {
            return member.Kind == Kind.Method && string.Equals(member.Name, "Finalize", StringComparison.OrdinalIgnoreCase) &&
                   (member.Parameters == null || member.Parameters.Count == 0);
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

        /// <summary>
        /// What a member declares, as keys: a method its name and parameter types (simple names – a DLL reports
        /// System.Windows.Forms.PaintEventArgs where the source says PaintEventArgs); an ACCESS the getter, an ASSIGN
        /// the setter, a PROPERTY both (a VO ACCESS compiled into a DLL is a property).
        /// </summary>
        private static IEnumerable<string> Keys(IXMemberSymbol member, bool caseSensitive)
        {
            var name = caseSensitive ? member.Name ?? "" : (member.Name ?? "").ToLowerInvariant();
            switch (member.Kind)
            {
                case Kind.Access:
                    return new[] { "get|" + name };
                case Kind.Assign:
                    return new[] { "set|" + name };
                case Kind.Property:
                    return new[] { "get|" + name, "set|" + name };
                default:
                    var types = member.Parameters?.Select(p => SimpleType(p.TypeName)) ?? Enumerable.Empty<string>();
                    return new[] { "method|" + name + "(" + string.Join(",", types) + ")" };
            }
        }

        private static string SimpleType(string type)
        {
            return Regex.Replace(type ?? "", @"(\w+\.)+", "").ToLowerInvariant();
        }
    }
}
