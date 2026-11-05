//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageService.FindReferences;
using XSharpModel;

namespace XSharp.LanguageService.GoToImplementation
{
    /// <summary>
    /// The lookups TypeHierarchy needs, served from the code model of the open solution: candidates from the
    /// database, every type and interface resolved the way OverrideFamily resolves a base class (namespace + USINGs,
    /// partial classes). One instance per command run: lookups are cached.
    /// </summary>
    internal sealed class SourceHierarchy
    {
        private readonly Dictionary<string, IXTypeSymbol> _types = new Dictionary<string, IXTypeSymbol>(StringComparer.OrdinalIgnoreCase);
        private readonly Dictionary<string, IXTypeSymbol> _bases = new Dictionary<string, IXTypeSymbol>(StringComparer.OrdinalIgnoreCase);
        private readonly Dictionary<string, IReadOnlyList<IXTypeSymbol>> _interfaces = new Dictionary<string, IReadOnlyList<IXTypeSymbol>>(StringComparer.OrdinalIgnoreCase);

        /// <summary>Database rows looked at (before resolving), for the log line.</summary>
        internal int Candidates { get; private set; }

        /// <summary>The types whose base class is written with this short name.</summary>
        internal IEnumerable<IXTypeSymbol> ByBaseShortName(string shortName)
        {
            return ToTypes(XDatabase.GetProjectTypesByBaseName(shortName));
        }

        /// <summary>The types whose IMPLEMENTS list (or, for interfaces, INHERIT list) names an interface with this short name.</summary>
        internal IEnumerable<IXTypeSymbol> ByInterfaceShortName(string shortName)
        {
            var rows = XDatabase.GetProjectTypesMentioning(shortName)
                .Where(row => TypeHierarchy.DeclaredInterfaceNames(row.SourceCode)
                    .Any(name => string.Equals(TypeHierarchy.ShortName(name), shortName, StringComparison.OrdinalIgnoreCase)));
            return ToTypes(rows.ToList());
        }

        internal IXTypeSymbol BaseOf(IXTypeSymbol type)
        {
            if (type == null)
                return null;
            var key = type.FullName ?? "";
            if (!_bases.TryGetValue(key, out var result))
            {
                result = OverrideFamily.ResolveBase(type);
                _bases[key] = result;
            }
            return result;
        }

        /// <summary>
        /// The resolved interfaces of a source type: its Interfaces list plus the names in its declaration line (the
        /// parser keeps only the first base interface of INTERFACE IFoo INHERIT IA, IB). DLL types: none.
        /// </summary>
        internal IEnumerable<IXTypeSymbol> InterfacesOf(IXTypeSymbol type)
        {
            if (!(type is XSourceTypeSymbol source))
                return Array.Empty<IXTypeSymbol>();
            var key = type.FullName ?? "";
            if (_interfaces.TryGetValue(key, out var cached))
                return cached;
            var names = new List<string>();
            names.AddRange(source.Interfaces ?? Array.Empty<string>());
            names.AddRange(TypeHierarchy.DeclaredInterfaceNames(source.SourceCode));
            if (source.IsPartial && source.Clone != null)
                names.AddRange(source.Clone.Interfaces ?? Array.Empty<string>());
            var result = new List<IXTypeSymbol>();
            var project = source.File?.Project;
            if (project != null)
            {
                var usings = new List<string>(source.File.Usings);
                if (!string.IsNullOrEmpty(source.Namespace))
                    usings.Add(source.Namespace);
                foreach (var name in names.Distinct(StringComparer.OrdinalIgnoreCase))
                {
                    var resolved = project.FindType(name, usings);
                    if (resolved != null && !result.Any(r => string.Equals(r.FullName, resolved.FullName, StringComparison.OrdinalIgnoreCase)))
                        result.Add(resolved);
                }
            }
            _interfaces[key] = result;
            return result;
        }

        /// <summary>The part a partial class is shown at: the one with INHERIT or IMPLEMENTS, otherwise the first.</summary>
        internal static XDbResult DeclaringPart(IEnumerable<XDbResult> parts)
        {
            var list = parts.ToList();
            return list.FirstOrDefault(p => !string.IsNullOrEmpty(p.BaseTypeName) || TypeHierarchy.DeclaredInterfaceNames(p.SourceCode).Count > 0)
                ?? list.FirstOrDefault();
        }

        /// <summary>The full type name of a type row; XDbResult.FullName is the file name for these rows.</summary>
        internal static string TypeFullName(XDbResult row)
        {
            return string.IsNullOrEmpty(row.Namespace) ? row.TypeName : row.Namespace + "." + row.TypeName;
        }

        private IEnumerable<IXTypeSymbol> ToTypes(IList<XDbResult> rows)
        {
            Candidates += rows.Count;
            var result = new List<IXTypeSymbol>();
            foreach (var group in rows.GroupBy(TypeFullName, StringComparer.OrdinalIgnoreCase))
            {
                var type = ToType(group.Key, DeclaringPart(group));
                if (type != null)
                    result.Add(type);
            }
            return result;
        }

        private IXTypeSymbol ToType(string fullName, XDbResult row)
        {
            if (_types.TryGetValue(fullName, out var cached))
                return cached;
            var project = XSolution.FindFile(row.FileName)?.Project;
            var type = project?.Lookup(fullName);
            _types[fullName] = type;
            return type;
        }
    }
}
