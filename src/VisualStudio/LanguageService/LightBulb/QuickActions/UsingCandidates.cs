//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>The namespaces to offer in "USING ..." for a type name that does not resolve.</summary>
    internal static class UsingCandidates
    {
        /// <summary>
        /// The namespaces of the <paramref name="types"/> named <paramref name="name"/>, without global types, without
        /// the namespaces the file already uses and without <paramref name="currentNamespace"/> and its parents (their
        /// types are visible anyway). Distinct and sorted.
        /// </summary>
        internal static IReadOnlyList<string> Namespaces(IEnumerable<(string Namespace, string Name)> types, string name,
            IEnumerable<string> fileUsings, string currentNamespace, bool caseSensitive)
        {
            var comparer = caseSensitive ? StringComparer.Ordinal : StringComparer.OrdinalIgnoreCase;
            var skip = new HashSet<string>(fileUsings ?? Enumerable.Empty<string>(), comparer);
            var current = currentNamespace ?? "";
            while (current.Length > 0)
            {
                skip.Add(current);
                var dot = current.LastIndexOf('.');
                current = dot < 0 ? "" : current.Substring(0, dot);
            }
            return types
                .Where(t => comparer.Equals(t.Name, name) && !string.IsNullOrEmpty(t.Namespace) && !skip.Contains(t.Namespace))
                .Select(t => t.Namespace)
                .Distinct(comparer)
                .OrderBy(n => n, StringComparer.Ordinal)
                .ToList();
        }
    }
}
