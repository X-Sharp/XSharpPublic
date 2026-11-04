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
    /// One instance per search: base classes and roots are looked up once.
    /// </summary>
    internal sealed class OverrideFamily
    {
        private readonly Func<IXTypeSymbol, IXTypeSymbol> _baseOf;
        private readonly Dictionary<string, IXTypeSymbol> _bases = new Dictionary<string, IXTypeSymbol>(StringComparer.OrdinalIgnoreCase);
        private readonly Dictionary<string, IXMemberSymbol> _roots = new Dictionary<string, IXMemberSymbol>(StringComparer.OrdinalIgnoreCase);

        /// <summary>Create it on the UI thread: the target's types belong to the editor.</summary>
        internal OverrideFamily(IXSymbol target, Func<IXTypeSymbol, IXTypeSymbol> baseOf = null)
        {
            _baseOf = baseOf ?? ResolveBase;
            if (target is IXMemberSymbol member && CanOverride(member.Kind))
            {
                Target = member;
                TargetRoot = Root(member);
            }
        }

        /// <summary>The target when it is a method or property, otherwise null.</summary>
        internal IXMemberSymbol Target { get; }

        /// <summary>The topmost member of the target's family, null when the target is no method or property.</summary>
        internal IXMemberSymbol TargetRoot { get; }

        /// <summary>The topmost member up the base class chain that the member overrides, or the member itself.</summary>
        internal IXMemberSymbol Root(IXMemberSymbol member)
        {
            if (member == null || !CanOverride(member.Kind) || member.ParentType == null)
                return member;
            var key = $"{member.ParentType.FullName}|{member.Name}|{(SymbolIdentity.IsPropertyLike(member.Kind) ? "property" : member.Kind + "|" + member.ParameterList)}";
            if (_roots.TryGetValue(key, out var cached))
                return cached;
            var root = member;
            var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase) { member.ParentType.FullName ?? "" };
            for (var type = BaseOf(member.ParentType); type is XSourceTypeSymbol && seen.Add(type.FullName ?? ""); type = BaseOf(type))
            {
                var match = type.Members?.FirstOrDefault(m => SameSlot(member, m));
                if (match != null)
                    root = match;
            }
            _roots[key] = root;
            return root;
        }

        /// <summary>Is the candidate the target, a member it overrides, or another override of the same member?</summary>
        internal bool Contains(IXSymbol candidate)
        {
            if (Target == null || !(candidate is IXMemberSymbol member) || !SameSlot(Target, member))
                return false;
            return SymbolIdentity.IsSame(TargetRoot, Root(member));
        }

        /// <summary>The base class of a type, resolved within the type's namespace and cached for this search.</summary>
        internal IXTypeSymbol BaseTypeOf(IXTypeSymbol type) => type == null ? null : BaseOf(type);

        private IXTypeSymbol BaseOf(IXTypeSymbol type)
        {
            var key = type.FullName ?? "";
            if (!_bases.TryGetValue(key, out var result))
            {
                result = _baseOf(type);
                _bases[key] = result;
            }
            return result;
        }

        /// <summary>
        /// BaseType is only set by ForceComplete, which changes the type (not safe off the UI thread) and looks the
        /// base class up with the USINGs of the file alone: INHERIT Animal inside BEGIN NAMESPACE FarTest is not
        /// found. Look it up here, with the type's namespace too.
        /// </summary>
        private static IXTypeSymbol ResolveBase(IXTypeSymbol type)
        {
            if (!(type is XSourceTypeSymbol source) || source.BaseType != null)
                return type.BaseType;
            var baseName = source.BaseTypeName;
            // a part of a partial class without INHERIT: the merged class knows the base class
            if (string.IsNullOrEmpty(baseName) && source.IsPartial)
                baseName = source.Clone?.BaseTypeName;
            var project = source.File?.Project;
            if (string.IsNullOrEmpty(baseName) || project == null)
                return null;
            var usings = new List<string>(source.File.Usings);
            if (!string.IsNullOrEmpty(source.Namespace))
                usings.Add(source.Namespace);
            return project.FindType(baseName, usings);
        }

        private static bool CanOverride(Kind kind)
        {
            return kind == Kind.Method || SymbolIdentity.IsPropertyLike(kind);
        }

        /// <summary>Same name and, for methods, the same kind and parameter list.</summary>
        internal static bool SameSlot(IXMemberSymbol member, IXMemberSymbol other)
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
