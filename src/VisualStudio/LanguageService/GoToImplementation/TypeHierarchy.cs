//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using XSharp.LanguageService.FindReferences;
using XSharpModel;

namespace XSharp.LanguageService.GoToImplementation
{
    /// <summary>
    /// Up and down the type hierarchy for Go To Base and Go To Implementation. The lookups are passed in: the
    /// candidates of a short name come from the code model database and are only a prefilter, every candidate is
    /// kept only when its resolved base class or interface is the type searched for (compared by full name).
    /// </summary>
    internal static class TypeHierarchy
    {
        private static readonly Regex _implements = new Regex(@"\bIMPLEMENTS\b", RegexOptions.IgnoreCase);
        private static readonly Regex _interfaceInherit = new Regex(@"\bINTERFACE\b.*?\bINHERIT\b", RegexOptions.IgnoreCase);
        private static readonly Regex _where = new Regex(@"\bWHERE\b", RegexOptions.IgnoreCase);

        /// <summary>"Ns.List&lt;Dog&gt;" → "List": the name a base class is stored under, without namespace and type arguments.</summary>
        internal static string ShortName(string typeName)
        {
            if (string.IsNullOrEmpty(typeName))
                return "";
            var name = typeName;
            var generic = name.IndexOfAny(new[] { '<', '`' });
            if (generic >= 0)
                name = name.Substring(0, generic);
            var dot = name.LastIndexOf('.');
            return dot >= 0 ? name.Substring(dot + 1) : name;
        }

        /// <summary>
        /// The interfaces named in a declaration line: after IMPLEMENTS for classes and structures, after INHERIT for
        /// interfaces (the parser keeps only the first base interface of INTERFACE IFoo INHERIT IA, IB).
        /// </summary>
        internal static IReadOnlyList<string> DeclaredInterfaceNames(string declarationLine)
        {
            var result = new List<string>();
            if (string.IsNullOrEmpty(declarationLine))
                return result;
            var line = declarationLine;
            var comment = line.IndexOf("//", StringComparison.Ordinal);
            if (comment >= 0)
                line = line.Substring(0, comment);
            Match start = _implements.Match(line);
            if (!start.Success)
                start = _interfaceInherit.Match(line);
            if (!start.Success)
                return result;
            var list = line.Substring(start.Index + start.Length);
            var where = _where.Match(list);
            if (where.Success)
                list = list.Substring(0, where.Index);
            int depth = 0, begin = 0;
            for (int i = 0; i <= list.Length; i++)
            {
                if (i == list.Length || (list[i] == ',' && depth == 0))
                {
                    var name = list.Substring(begin, i - begin).Trim();
                    if (name.Length > 0)
                        result.Add(name);
                    begin = i + 1;
                }
                else if (list[i] == '<')
                    depth++;
                else if (list[i] == '>')
                    depth--;
            }
            return result;
        }

        /// <summary>
        /// Identity of a type in the solution: project and full name. In VO solutions many classes have no namespace,
        /// so two projects can each declare a class with the same name.
        /// </summary>
        internal static string KeyOf(IXTypeSymbol type)
        {
            var project = (type as XSourceTypeSymbol)?.File?.Project?.FileName;
            return string.IsNullOrEmpty(project) ? type.FullName ?? "" : project + "|" + type.FullName;
        }

        /// <summary>All classes deriving from the target, all levels, breadth-first; the target itself never.</summary>
        internal static IReadOnlyList<IXTypeSymbol> DerivedTypes(IXTypeSymbol target, Func<string, IEnumerable<IXTypeSymbol>> byBaseShortName,
            Func<IXTypeSymbol, IXTypeSymbol> baseOf, Func<IXTypeSymbol, string> keyOf = null)
        {
            keyOf = keyOf ?? KeyOf;
            var result = new List<IXTypeSymbol>();
            var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase) { keyOf(target) };
            var queue = new Queue<IXTypeSymbol>();
            queue.Enqueue(target);
            while (queue.Count > 0)
            {
                var current = queue.Dequeue();
                foreach (var candidate in byBaseShortName(ShortName(current.Name)) ?? Enumerable.Empty<IXTypeSymbol>())
                {
                    if (candidate == null || !SameType(baseOf(candidate), current) || !seen.Add(keyOf(candidate)))
                        continue;
                    result.Add(candidate);
                    queue.Enqueue(candidate);
                }
            }
            return result;
        }

        /// <summary>
        /// The classes and structures that implement the interface, directly or through an interface derived from it.
        /// Derived interfaces are searched, not listed.
        /// </summary>
        internal static IReadOnlyList<IXTypeSymbol> Implementers(IXTypeSymbol iface, Func<string, IEnumerable<IXTypeSymbol>> byInterfaceShortName,
            Func<IXTypeSymbol, IEnumerable<IXTypeSymbol>> interfacesOf, Func<IXTypeSymbol, string> keyOf = null)
        {
            keyOf = keyOf ?? KeyOf;
            var result = new List<IXTypeSymbol>();
            var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase) { keyOf(iface) };
            var queue = new Queue<IXTypeSymbol>();
            queue.Enqueue(iface);
            while (queue.Count > 0)
            {
                var current = queue.Dequeue();
                foreach (var candidate in byInterfaceShortName(ShortName(current.Name)) ?? Enumerable.Empty<IXTypeSymbol>())
                {
                    if (candidate == null || !(interfacesOf(candidate) ?? Enumerable.Empty<IXTypeSymbol>()).Any(i => SameType(i, current)))
                        continue;
                    if (!seen.Add(keyOf(candidate)))
                        continue;
                    if (candidate.Kind == Kind.Interface)
                        queue.Enqueue(candidate);
                    else
                        result.Add(candidate);
                }
            }
            return result;
        }

        /// <summary>The base class (if any) first, then the interfaces.</summary>
        internal static IReadOnlyList<IXTypeSymbol> BaseTypes(IXTypeSymbol type, Func<IXTypeSymbol, IXTypeSymbol> baseOf,
            Func<IXTypeSymbol, IEnumerable<IXTypeSymbol>> interfacesOf)
        {
            var result = new List<IXTypeSymbol>();
            var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            var baseType = baseOf(type);
            if (baseType != null && seen.Add(baseType.FullName ?? ""))
                result.Add(baseType);
            foreach (var iface in interfacesOf(type) ?? Enumerable.Empty<IXTypeSymbol>())
            {
                if (iface != null && seen.Add(iface.FullName ?? ""))
                    result.Add(iface);
            }
            return result;
        }

        /// <summary>The members of the types that occupy the target's slot.</summary>
        internal static IReadOnlyList<IXMemberSymbol> Overrides(IXMemberSymbol target, IEnumerable<IXTypeSymbol> types)
        {
            return MembersInSlot(target, types);
        }

        /// <summary>The nearest member up the base class chain with the member's slot, null if there is none.</summary>
        internal static IXMemberSymbol OverriddenMember(IXMemberSymbol member, Func<IXTypeSymbol, IXTypeSymbol> baseOf)
        {
            if (member?.ParentType == null)
                return null;
            var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase) { member.ParentType.FullName ?? "" };
            for (var type = baseOf(member.ParentType); type != null && seen.Add(type.FullName ?? ""); type = baseOf(type))
            {
                var match = type.Members?.FirstOrDefault(m => OverrideFamily.SameSlot(member, m));
                if (match != null)
                    return match;
            }
            return null;
        }

        /// <summary>
        /// Go To Base on a member: the overridden member, then the interface members it implements (interfaces of its
        /// class and of the base classes); every declaration once.
        /// </summary>
        internal static IReadOnlyList<IXMemberSymbol> BaseMembers(IXMemberSymbol member, Func<IXTypeSymbol, IXTypeSymbol> baseOf,
            Func<IXTypeSymbol, IEnumerable<IXTypeSymbol>> interfacesOf)
        {
            var result = new List<IXMemberSymbol>();
            var overridden = OverriddenMember(member, baseOf);
            if (overridden != null)
                result.Add(overridden);
            var interfaces = new List<IXTypeSymbol>();
            var seenTypes = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            var seenInterfaces = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            for (var parent = member.ParentType; parent != null && seenTypes.Add(KeyOf(parent)); parent = baseOf(parent))
            {
                foreach (var iface in interfacesOf(parent) ?? Enumerable.Empty<IXTypeSymbol>())
                {
                    if (iface != null && seenInterfaces.Add(KeyOf(iface)))
                        interfaces.Add(iface);
                }
            }
            foreach (var implemented in ImplementedMembers(member, interfaces))
            {
                if (!result.Any(r => ReferenceEquals(r, implemented)))
                    result.Add(implemented);
            }
            return result;
        }

        /// <summary>The interface members the member implements.</summary>
        internal static IReadOnlyList<IXMemberSymbol> ImplementedMembers(IXMemberSymbol member, IEnumerable<IXTypeSymbol> interfaces)
        {
            return MembersInSlot(member, interfaces);
        }

        private static IReadOnlyList<IXMemberSymbol> MembersInSlot(IXMemberSymbol target, IEnumerable<IXTypeSymbol> types)
        {
            var result = new List<IXMemberSymbol>();
            foreach (var type in types ?? Enumerable.Empty<IXTypeSymbol>())
            {
                foreach (var member in type?.Members ?? Enumerable.Empty<IXMemberSymbol>())
                {
                    if (!ReferenceEquals(member, target) && OverrideFamily.SameSlot(target, member))
                        result.Add(member);
                }
            }
            return result;
        }

        private static bool SameType(IXTypeSymbol one, IXTypeSymbol other)
        {
            return one != null && other != null && string.Equals(one.FullName, other.FullName, StringComparison.OrdinalIgnoreCase);
        }
    }
}
