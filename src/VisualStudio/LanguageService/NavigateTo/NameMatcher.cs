//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;

namespace XSharp.LanguageService.NavigateTo
{
    /// <summary>How a search text matches a name; the order is the rank, best first after None.</summary>
    internal enum NameMatch
    {
        None,
        Exact,
        Prefix,
        CamelCase,
        Substring
    }

    /// <summary>Matches a search text against a name the way Go To All does for C#, ignoring case.</summary>
    internal static class NameMatcher
    {
        /// <summary>
        /// Exact, prefix, the beginnings of two or more parts of the name in order (<c>WT</c>, <c>WrTe</c> for
        /// <c>WriteText</c>), or anywhere in the name.
        /// </summary>
        internal static NameMatch Match(string pattern, string name)
        {
            pattern = pattern?.Trim();
            if (string.IsNullOrEmpty(pattern) || string.IsNullOrEmpty(name))
                return NameMatch.None;
            if (string.Equals(pattern, name, StringComparison.OrdinalIgnoreCase))
                return NameMatch.Exact;
            if (name.StartsWith(pattern, StringComparison.OrdinalIgnoreCase))
                return NameMatch.Prefix;
            if (MatchesParts(pattern, 0, Parts(name), 0, 0))
                return NameMatch.CamelCase;
            if (name.IndexOf(pattern, StringComparison.OrdinalIgnoreCase) >= 0)
                return NameMatch.Substring;
            return NameMatch.None;
        }

        /// <summary>The parts of a name: they start at its beginning, at an upper-case letter after a lower-case
        /// letter or digit, and after an underscore.</summary>
        private static List<string> Parts(string name)
        {
            var parts = new List<string>();
            int start = -1;
            for (int i = 0; i < name.Length; i++)
            {
                var c = name[i];
                if (c == '_')
                {
                    if (start >= 0)
                        parts.Add(name.Substring(start, i - start));
                    start = -1;
                    continue;
                }
                bool begins = start < 0 || (char.IsUpper(c) && !char.IsUpper(name[i - 1]));
                if (begins)
                {
                    if (start >= 0)
                        parts.Add(name.Substring(start, i - start));
                    start = i;
                }
            }
            if (start >= 0)
                parts.Add(name.Substring(start));
            return parts;
        }

        /// <summary>Can the rest of the pattern be split into beginnings of parts, in order, using two parts or more?</summary>
        private static bool MatchesParts(string pattern, int position, List<string> parts, int firstPart, int used)
        {
            if (position == pattern.Length)
                return used >= 2;
            for (int p = firstPart; p < parts.Count; p++)
            {
                var part = parts[p];
                for (int length = Math.Min(part.Length, pattern.Length - position); length >= 1; length--)
                {
                    if (string.Compare(pattern, position, part, 0, length, StringComparison.OrdinalIgnoreCase) == 0 &&
                        MatchesParts(pattern, position + length, parts, p + 1, used + 1))
                        return true;
                }
            }
            return false;
        }
    }
}
