//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;

namespace XSharp.LanguageService.ForeignSource
{
    /// <summary>
    /// Maps a symbol that X# read from the output DLL of a C# or VB project back to its declarations in that project's
    /// source. The doc ID decides; when it does not resolve (generics), the member name and parameter count do, and
    /// when the member has no source (implicit constructor) the type does.
    /// </summary>
    internal static class ForeignSymbolResolver
    {
        /// <param name="isOnDisk">
        /// Whether a source file can be opened. Source generators produce trees with virtual paths; their locations are
        /// skipped, and the search goes on as if the symbol had no source there (member → type → nothing). Null: all files.
        /// </param>
        internal static IReadOnlyList<ForeignLocation> Resolve(IEnumerable<Compilation> compilations, ForeignSymbolKey key, Func<string, bool> isOnDisk = null)
        {
            var result = new List<ForeignLocation>();
            var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            foreach (var compilation in compilations)
            {
                foreach (var symbol in Find(compilation, key, isOnDisk))
                {
                    foreach (var location in symbol.Locations.Where(l => Usable(l, isOnDisk)))
                    {
                        var span = location.GetLineSpan();
                        if (!seen.Add(span.Path + "|" + span.StartLinePosition.Line))
                            continue;
                        result.Add(new ForeignLocation(span.Path, span.StartLinePosition.Line, span.StartLinePosition.Character,
                            span.EndLinePosition.Line, span.EndLinePosition.Character, symbol.Name, Container(symbol)));
                    }
                }
            }
            return result;
        }

        /// <summary>Doc IDs separate nested types with '.'; the metadata names X# reads use '/' (Cecil) or '+' (reflection).</summary>
        private static string DocId(string id)
        {
            return id?.Replace('/', '.').Replace('+', '.');
        }

        private static bool Usable(Location location, Func<string, bool> isOnDisk)
        {
            return location.IsInSource && (isOnDisk == null || isOnDisk(location.SourceTree.FilePath));
        }

        private static IEnumerable<ISymbol> Find(Compilation compilation, ForeignSymbolKey key, Func<string, bool> isOnDisk)
        {
            List<ISymbol> InSource(IEnumerable<ISymbol> symbols) => symbols.Where(s => s.Locations.Any(l => Usable(l, isOnDisk))).ToList();
            if (key.MemberDocId != null)
            {
                var members = InSource(DocumentationCommentId.GetSymbolsForDeclarationId(DocId(key.MemberDocId), compilation));
                if (members.Count > 0)
                    return members;
            }
            var type = DocumentationCommentId.GetFirstSymbolForDeclarationId(DocId(key.TypeDocId), compilation) as INamedTypeSymbol;
            if (type == null)
                return Enumerable.Empty<ISymbol>();
            if (key.MemberDocId != null)
            {
                var named = key.MemberName == ".ctor" ? type.InstanceConstructors.Cast<ISymbol>().ToList() : type.GetMembers(key.MemberName).ToList();
                var counted = named.Where(m => ParameterCount(m) == key.ParameterCount).ToList();
                var members = InSource(counted.Count > 0 ? counted : named);
                if (members.Count > 0)
                    return members;
            }
            return new[] { type };
        }

        private static int ParameterCount(ISymbol symbol)
        {
            switch (symbol)
            {
                case IMethodSymbol method:
                    return method.Parameters.Length;
                case IPropertySymbol property:
                    return property.Parameters.Length;
                default:
                    return 0;
            }
        }

        private static string Container(ISymbol symbol)
        {
            if (symbol.ContainingType != null)
                return symbol.ContainingType.ToDisplayString();
            return symbol.ContainingNamespace?.ToDisplayString() ?? "";
        }
    }
}
