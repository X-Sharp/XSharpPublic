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
    /// <summary>Where the conflict check looks: base types, types and functions of the project, locals of a member.</summary>
    internal sealed class RenameScope
    {
        internal Func<IXTypeSymbol, IXTypeSymbol> BaseOf { get; set; }
        /// <summary>A type, function or procedure with this name that the project sees, otherwise null</summary>
        internal Func<string, IXSymbol> FindTypeOrFunction { get; set; }
        /// <summary>The locals of a member (its parameters are read from the member itself)</summary>
        internal Func<IXMemberSymbol, IEnumerable<IXSymbol>> LocalsOf { get; set; }
    }

    /// <summary>Symbols that already have the new name where the renamed symbol lives.</summary>
    internal static class RenameConflicts
    {
        /// <summary>
        /// The symbols named <paramref name="newName"/> in the target's scope: for a member its type and base types,
        /// for a local or parameter its member, for a type or function the project. A rename that only changes the
        /// case has none.
        /// </summary>
        internal static IList<IXSymbol> Find(IXSymbol target, string newName, RenameScope scope)
        {
            var result = new List<IXSymbol>();
            newName = (newName ?? "").Trim();
            if (newName.StartsWith("@@", StringComparison.Ordinal))
                newName = newName.Substring(2);
            if (string.Equals(newName, target.Name, StringComparison.OrdinalIgnoreCase))
                return result;
            bool Named(IXSymbol s) => string.Equals(s.Name, newName, StringComparison.OrdinalIgnoreCase);
            if (target is XSourceVariableSymbol variable)
            {
                if (variable.Parent is IXMemberSymbol owner)
                {
                    var candidates = (scope.LocalsOf?.Invoke(owner) ?? Enumerable.Empty<IXSymbol>())
                        .Concat(owner.Parameters.Cast<IXSymbol>());
                    result.AddRange(candidates.Where(c => !ReferenceEquals(c, target) && Named(c)));
                }
            }
            else if (target is IXMemberSymbol member && member.ParentType != null && !IsGlobal(member))
            {
                var seen = new HashSet<IXTypeSymbol>();
                for (var type = member.ParentType; type != null && seen.Add(type); type = scope.BaseOf?.Invoke(type))
                    result.AddRange(type.GetMembers(newName, true).Where(m => !ReferenceEquals(m, target)));
            }
            else
            {
                var existing = scope.FindTypeOrFunction?.Invoke(newName);
                if (existing != null && !FindReferences.SymbolIdentity.IsSame(target, existing))
                    result.Add(existing);
            }
            return result.Distinct().ToList();
        }

        private static bool IsGlobal(IXMemberSymbol member)
        {
            return member.Kind == Kind.Function || member.Kind == Kind.Procedure || member.ParentType.IsGlobalType();
        }
    }
}
