//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using XSharpModel;

namespace XSharp.LanguageService.Rename
{
    /// <summary>Targets that a rename would silently break, and must therefore refuse.</summary>
    internal static class RenameGuard
    {
        /// <summary>
        /// Why the target cannot be renamed, or null. Interface members and their implementations: Find All
        /// References does not know the relation, renaming one side breaks the build. Overrides of a member declared
        /// in a referenced assembly: the renamed method would silently stop overriding.
        /// </summary>
        internal static string Refusal(IXSymbol target, Func<IXTypeSymbol, IXTypeSymbol> baseOf, Func<string, IXTypeSymbol> findType,
            Func<IXTypeSymbol, bool> isSource = null)
        {
            if (!(target is IXMemberSymbol member) || member.ParentType == null || !CanOverride(member.Kind))
                return null;
            isSource = isSource ?? (t => t is XSourceTypeSymbol);
            var parent = member.ParentType;
            if (parent.Kind == Kind.Interface)
                return $"'{member.Name}' belongs to an interface. Renaming interface members is not supported.";
            foreach (var name in parent.Interfaces ?? Array.Empty<string>())
            {
                var iface = findType(name);
                if (iface != null && iface.Members.Any(m => FindReferences.OverrideFamily.SameSlot(member, m)))
                    return $"'{member.Name}' implements a member of interface '{iface.Name}'. Renaming it is not supported.";
            }
            var seen = new HashSet<IXTypeSymbol>();
            for (var type = baseOf(parent); type != null && seen.Add(type); type = baseOf(type))
            {
                if (isSource(type))
                    continue;
                // a type of an assembly lists its inherited members too; its parameter lists are written differently
                if (type.GetMembers(member.Name, true).Any(m => SameKind(member, m)))
                    return $"'{member.Name}' overrides a member of '{type.Name}' from a referenced assembly and cannot be renamed.";
                break;
            }
            return null;
        }

        private static bool CanOverride(Kind kind)
        {
            return kind == Kind.Method || FindReferences.SymbolIdentity.IsPropertyLike(kind);
        }

        private static bool SameKind(IXMemberSymbol member, IXMemberSymbol other)
        {
            if (FindReferences.SymbolIdentity.IsPropertyLike(member.Kind))
                return FindReferences.SymbolIdentity.IsPropertyLike(other.Kind);
            return other.Kind == member.Kind;
        }
    }
}
