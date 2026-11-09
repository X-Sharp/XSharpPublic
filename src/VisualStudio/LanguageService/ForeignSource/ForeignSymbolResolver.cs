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
        internal static IReadOnlyList<ForeignLocation> Resolve(IEnumerable<Compilation> compilations, ForeignSymbolKey key)
        {
            var result = new List<ForeignLocation>();
            var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            foreach (var compilation in compilations)
            {
                foreach (var symbol in Find(compilation, key))
                {
                    foreach (var location in symbol.Locations.Where(l => l.IsInSource))
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

        private static IEnumerable<ISymbol> Find(Compilation compilation, ForeignSymbolKey key)
        {
            if (key.MemberDocId != null)
            {
                var members = InSource(DocumentationCommentId.GetSymbolsForDeclarationId(key.MemberDocId, compilation));
                if (members.Count > 0)
                    return members;
            }
            var type = DocumentationCommentId.GetFirstSymbolForDeclarationId(key.TypeDocId, compilation) as INamedTypeSymbol;
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

        private static List<ISymbol> InSource(IEnumerable<ISymbol> symbols)
        {
            return symbols.Where(s => s.Locations.Any(l => l.IsInSource)).ToList();
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
