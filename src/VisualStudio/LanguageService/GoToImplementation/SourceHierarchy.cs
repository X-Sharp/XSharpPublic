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
    /// <summary>A type in the hierarchy: a database row (candidates) or a code model symbol (the target).</summary>
    internal sealed class TypeNode
    {
        internal TypeNode(string name, string fullName, Kind kind, string project, XDbResult row, IXTypeSymbol symbol)
        {
            Name = name;
            FullName = fullName;
            Kind = kind;
            Project = project ?? "";
            Row = row;
            Symbol = symbol;
        }

        internal string Name { get; }
        internal string FullName { get; }
        internal Kind Kind { get; }
        internal string Project { get; }
        /// <summary>The declaring part (partial classes: the one with INHERIT / IMPLEMENTS); null for the target.</summary>
        internal XDbResult Row { get; }
        internal IXTypeSymbol Symbol { get; set; }
        internal string Key => Project + "|" + FullName;
    }

    /// <summary>
    /// The code model side of Go To Implementation / Go To Base. Down: the derived-class tree is built in memory from
    /// one query of all types with a base class; each INHERIT / IMPLEMENTS name is resolved with the USINGs of its
    /// file (cached per project, USINGs and name), so candidates are never loaded as symbols. Only the types that
    /// declare a member of the searched name are loaded, for the slot check. Up: resolved as OverrideFamily does.
    /// One instance per command run.
    /// </summary>
    internal sealed class SourceHierarchy
    {
        private Dictionary<string, List<XDbResult>> _byBase;
        private readonly Dictionary<string, string> _resolved = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        private readonly Dictionary<string, (XProject Project, List<string> Usings, string UsingsKey)> _files =
            new Dictionary<string, (XProject, List<string>, string)>(StringComparer.OrdinalIgnoreCase);
        private readonly Dictionary<string, IXTypeSymbol> _bases = new Dictionary<string, IXTypeSymbol>(StringComparer.OrdinalIgnoreCase);
        private readonly Dictionary<string, IReadOnlyList<IXTypeSymbol>> _interfaces = new Dictionary<string, IReadOnlyList<IXTypeSymbol>>(StringComparer.OrdinalIgnoreCase);

        /// <summary>Database rows looked at, for the log line.</summary>
        internal int Candidates { get; private set; }

        /// <summary>The node of a code model type (the starting symbol).</summary>
        internal static TypeNode NodeOf(IXTypeSymbol type)
        {
            var project = (type as XSourceTypeSymbol)?.File?.Project?.FileName;
            return new TypeNode(type.Name, type.FullName, type.Kind, project, null, type);
        }

        /// <summary>The rows indexed by the short name of their base class (Ns.List&lt;Dog&gt; → List), case-insensitive.</summary>
        internal static Dictionary<string, List<XDbResult>> BaseIndex(IEnumerable<XDbResult> rows)
        {
            var index = new Dictionary<string, List<XDbResult>>(StringComparer.OrdinalIgnoreCase);
            foreach (var row in rows)
            {
                var key = TypeHierarchy.ShortName(row.BaseTypeName);
                if (key.Length == 0)
                    continue;
                if (!index.TryGetValue(key, out var list))
                    index[key] = list = new List<XDbResult>();
                list.Add(row);
            }
            return index;
        }

        /// <summary>All classes deriving from the type, all levels.</summary>
        internal IReadOnlyList<TypeNode> DerivedTypes(TypeNode target)
        {
            if (_byBase == null)
            {
                var rows = XDatabase.GetProjectTypesWithBase();
                Candidates += rows.Count;
                _byBase = BaseIndex(rows);
            }
            return TypeHierarchy.Descendants(target, ChildrenByBase, n => n.Key);
        }

        /// <summary>The classes and structures implementing the interface, also through derived interfaces.</summary>
        internal IReadOnlyList<TypeNode> Implementers(TypeNode iface)
        {
            return TypeHierarchy.Descendants(iface, n => n.Kind == Kind.Interface ? ChildrenByInterface(n) : Enumerable.Empty<TypeNode>(), n => n.Key)
                .Where(n => n.Kind != Kind.Interface).ToList();
        }

        /// <summary>The keys (project|full name) of the types that declare a member with this name.</summary>
        internal HashSet<string> TypesWithMember(string name)
        {
            var rows = XDatabase.GetProjectTypesWithMember(name);
            Candidates += rows.Count;
            return new HashSet<string>(rows.Select(r => r.Project + "|" + TypeFullName(r)), StringComparer.OrdinalIgnoreCase);
        }

        /// <summary>The code model type of a node, loaded on demand.</summary>
        internal IXTypeSymbol SymbolOf(TypeNode node)
        {
            if (node.Symbol == null && node.Row != null)
                node.Symbol = FileInfo(node.Row.FileName).Project?.Lookup(node.FullName);
            return node.Symbol;
        }

        private IEnumerable<TypeNode> ChildrenByBase(TypeNode node)
        {
            if (!_byBase.TryGetValue(TypeHierarchy.ShortName(node.Name), out var rows))
                yield break;
            foreach (var group in rows.GroupBy(row => row.Project + "|" + TypeFullName(row), StringComparer.OrdinalIgnoreCase))
            {
                var part = DeclaringPart(group);
                if (string.Equals(Resolve(part, part.BaseTypeName), node.FullName, StringComparison.OrdinalIgnoreCase))
                    yield return new TypeNode(part.TypeName, TypeFullName(part), part.Kind, part.Project, part, null);
            }
        }

        private IEnumerable<TypeNode> ChildrenByInterface(TypeNode node)
        {
            var shortName = TypeHierarchy.ShortName(node.Name);
            var rows = XDatabase.GetProjectTypesMentioning(shortName);
            Candidates += rows.Count;
            foreach (var group in rows.GroupBy(row => row.Project + "|" + TypeFullName(row), StringComparer.OrdinalIgnoreCase))
            {
                var part = DeclaringPart(group);
                var names = TypeHierarchy.DeclaredInterfaceNames(part.SourceCode)
                    .Where(n => string.Equals(TypeHierarchy.ShortName(n), shortName, StringComparison.OrdinalIgnoreCase));
                if (names.Any(n => string.Equals(Resolve(part, n), node.FullName, StringComparison.OrdinalIgnoreCase)))
                    yield return new TypeNode(part.TypeName, TypeFullName(part), part.Kind, part.Project, part, null);
            }
        }

        /// <summary>The full name a type name written in the row's file resolves to ("" if not found).</summary>
        private string Resolve(XDbResult row, string typeName)
        {
            if (string.IsNullOrEmpty(typeName))
                return "";
            var file = FileInfo(row.FileName);
            if (file.Project == null)
                return "";
            var key = file.Project.FileName + "|" + row.Namespace + "|" + file.UsingsKey + "|" + typeName;
            if (!_resolved.TryGetValue(key, out var result))
            {
                var usings = new List<string>(file.Usings);
                if (!string.IsNullOrEmpty(row.Namespace))
                    usings.Add(row.Namespace);
                result = ResolveName(file.Project, typeName, usings);
                _resolved[key] = result;
            }
            return result;
        }

        /// <summary>
        /// The full name of a type name as XProject.FindType resolves it (source types of this and the referenced
        /// projects first, then DLL types), without building the type with its members: only the name is compared.
        /// </summary>
        private static string ResolveName(XProject project, string typeName, List<string> usings)
        {
            var name = typeName.Trim();
            var generic = name.IndexOf('<');
            if (generic > 0)
                name = name.Substring(0, generic);
            var rows = XDatabase.GetProjectTypes(TypeHierarchy.ShortName(name), project.DependentProjectList);
            var match = project.FilterUsings(rows, usings, name, false)
                .FirstOrDefault(r => !name.Contains(".") || string.Equals(TypeFullName(r), name, StringComparison.OrdinalIgnoreCase));
            if (match != null)
                return TypeFullName(match);
            return project.FindSystemType(typeName, usings)?.FullName ?? "";
        }

        private (XProject Project, List<string> Usings, string UsingsKey) FileInfo(string fileName)
        {
            if (!_files.TryGetValue(fileName ?? "", out var info))
            {
                var file = XSolution.FindFile(fileName);
                var usings = file == null ? new List<string>() : new List<string>(file.Usings);
                info = (file?.Project, usings, string.Join(",", usings.OrderBy(u => u, StringComparer.OrdinalIgnoreCase)));
                _files[fileName ?? ""] = info;
            }
            return info;
        }

        /// <summary>The resolved base class (Go To Base), as OverrideFamily resolves it.</summary>
        internal IXTypeSymbol BaseOf(IXTypeSymbol type)
        {
            if (type == null)
                return null;
            var key = TypeHierarchy.KeyOf(type);
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
            var key = TypeHierarchy.KeyOf(type);
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
    }
}
