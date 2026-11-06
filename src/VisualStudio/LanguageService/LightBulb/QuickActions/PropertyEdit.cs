//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>Lines <see cref="StartLine"/>..<see cref="EndLine"/> (0-based, inclusive) are replaced by <see cref="Text"/> (empty: removed).</summary>
    internal sealed class LineReplacement
    {
        internal LineReplacement(int startLine, int endLine, string text)
        {
            StartLine = startLine;
            EndLine = endLine;
            Text = text ?? "";
        }

        internal int StartLine { get; }
        internal int EndLine { get; }
        internal string Text { get; }
    }

    /// <summary>The lines of a member: the comment block above it, its header and its body.</summary>
    internal struct MemberLines
    {
        internal MemberLines(int startLine, int headerLine, int endLine)
        {
            StartLine = startLine;
            HeaderLine = headerLine;
            EndLine = endLine;
        }

        internal int StartLine { get; }
        internal int HeaderLine { get; }
        internal int EndLine { get; }
    }

    /// <summary>Where the text of ACCESS/ASSIGN members is replaced by the property.</summary>
    internal static class PropertyEdit
    {
        private static readonly HashSet<string> VisibilityWords = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "PUBLIC", "EXPORT", "PROTECTED", "INTERNAL", "PRIVATE", "HIDDEN",
        };

        /// <summary>
        /// A member from the comment block directly above its header to the end of its body: X# members have no END,
        /// so the body runs to <paramref name="nextEntityLine"/> (the next member, type or END CLASS), without the
        /// comment block directly above that and without trailing blank lines.
        /// </summary>
        internal static MemberLines MemberLines(Func<int, string> lineText, int lineCount, int headerLine, int nextEntityLine)
        {
            var start = headerLine;
            while (start > 0 && IsComment(lineText(start - 1)))
                start--;
            var end = Math.Min(nextEntityLine, lineCount) - 1;
            while (end > headerLine && IsComment(lineText(end)))
                end--;
            while (end > headerLine && lineText(end).Trim().Length == 0)
                end--;
            return new MemberLines(start, headerLine, Math.Max(end, headerLine));
        }

        /// <summary>
        /// The property replaces <paramref name="place"/> (comments of both members above it, those of
        /// <paramref name="place"/> first); <paramref name="removed"/> is deleted with the blank lines directly above it.
        /// </summary>
        internal static IList<LineReplacement> Replacements(Func<int, string> lineText, MemberLines place, MemberLines? removed, string propertyText)
        {
            var newLine = propertyText.EndsWith("\r\n") ? "\r\n" : "\n";
            var text = new StringBuilder();
            for (int line = place.StartLine; line < place.HeaderLine; line++)
                text.Append(lineText(line)).Append(newLine);
            if (removed.HasValue)
            {
                for (int line = removed.Value.StartLine; line < removed.Value.HeaderLine; line++)
                    text.Append(lineText(line)).Append(newLine);
            }
            text.Append(propertyText);
            var result = new List<LineReplacement> { new LineReplacement(place.StartLine, place.EndLine, text.ToString()) };
            if (removed.HasValue)
            {
                var start = removed.Value.StartLine;
                var limit = removed.Value.StartLine > place.EndLine ? place.EndLine : -1;
                while (start - 1 > limit && lineText(start - 1).Trim().Length == 0)
                    start--;
                result.Add(new LineReplacement(start, removed.Value.EndLine, ""));
            }
            return result;
        }

        /// <summary>No two replacements touch the same line.</summary>
        internal static bool NonOverlapping(IList<LineReplacement> replacements)
        {
            var sorted = replacements.OrderBy(r => r.StartLine).ToList();
            for (int i = 1; i < sorted.Count; i++)
            {
                if (sorted[i].StartLine <= sorted[i - 1].EndLine)
                    return false;
            }
            return true;
        }

        /// <summary>
        /// The visibility written before ACCESS/ASSIGN in <paramref name="header"/> (empty for the default); false when
        /// the header has other modifiers (STATIC ...), which the property would have to repeat.
        /// </summary>
        internal static bool HeaderVisibility(string header, out string visibility)
        {
            visibility = "";
            var words = (header ?? "").Trim().Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
            var found = new List<string>();
            foreach (var word in words)
            {
                if (string.Equals(word, "ACCESS", StringComparison.OrdinalIgnoreCase) || string.Equals(word, "ASSIGN", StringComparison.OrdinalIgnoreCase))
                {
                    visibility = string.Join(" ", found).ToUpperInvariant();
                    return true;
                }
                if (!VisibilityWords.Contains(word))
                    return false;
                found.Add(word);
            }
            return false;
        }

        private static bool IsComment(string line)
        {
            var text = line.Trim();
            return text.StartsWith("//") || text.StartsWith("*") || text.StartsWith("&&") || text.StartsWith("/*");
        }
    }
}
