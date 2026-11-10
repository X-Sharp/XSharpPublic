//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Generic;
using System.Linq;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>Where a new USING line goes and what it looks like.</summary>
    internal static class UsingInserter
    {
        /// <summary>
        /// After the last existing USING (<paramref name="usingEndLines"/>, 0-based); without USINGs before the first
        /// line that is not blank, not a comment and not a preprocessor directive.
        /// </summary>
        internal static TextInsertion Insert(IList<string> lines, IEnumerable<int> usingEndLines, string ns, CodeStyle style)
        {
            var text = style.Kw("USING") + " " + ns + style.NewLine;
            var ends = usingEndLines?.ToList() ?? new List<int>();
            if (ends.Count > 0)
                return new TextInsertion(ends.Max() + 1, text);
            return new TextInsertion(FirstCodeLine(lines), text);
        }

        private static int FirstCodeLine(IList<string> lines)
        {
            bool inBlock = false;
            for (int i = 0; i < lines.Count; i++)
            {
                var line = lines[i].Trim();
                if (inBlock)
                {
                    if (line.Contains("*/"))
                        inBlock = false;
                    continue;
                }
                if (line.StartsWith("/*"))
                {
                    inBlock = !line.Contains("*/");
                    continue;
                }
                if (line.Length == 0 || line.StartsWith("//") || line.StartsWith("*") || line.StartsWith("&&") || line.StartsWith("#"))
                    continue;
                return i;
            }
            return lines.Count;
        }
    }
}
