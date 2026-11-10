//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;

namespace XSharp.LanguageService.InlayHints
{
    /// <summary>
    /// The part of a document the hints are computed for: the visible lines and one screen above and below,
    /// so that a large file shows its hints quickly and scrolling adds the rest.
    /// </summary>
    internal static class InlayHintRange
    {
        internal static (int First, int Last) Lines(int firstVisible, int lastVisible, int lineCount)
        {
            var screen = lastVisible - firstVisible + 1;
            return (Math.Max(0, firstVisible - screen), Math.Min(lineCount - 1, lastVisible + screen));
        }

        /// <summary>True when [start, end) lies inside one of the covered spans.</summary>
        internal static bool IsCovered(IReadOnlyList<(int Start, int End)> covered, int start, int end)
        {
            foreach (var span in covered)
            {
                if (span.Start <= start && end <= span.End)
                    return true;
            }
            return false;
        }

        /// <summary>The covered spans with [start, end) added, sorted, overlapping and adjacent spans joined.</summary>
        internal static IReadOnlyList<(int Start, int End)> AddCovered(IReadOnlyList<(int Start, int End)> covered, int start, int end)
        {
            var all = covered.Concat(new[] { (Start: start, End: end) }).OrderBy(s => s.Start);
            var result = new List<(int Start, int End)>();
            foreach (var span in all)
            {
                if (result.Count > 0 && span.Start <= result[result.Count - 1].End)
                {
                    var last = result[result.Count - 1];
                    result[result.Count - 1] = (last.Start, Math.Max(last.End, span.End));
                }
                else
                    result.Add(span);
            }
            return result;
        }

        /// <summary>
        /// The old hints and the fresh ones of the same snapshot. Inside the range the fresh computation covered only its
        /// hints count (a call that became ambiguous loses its hint); outside it the old ones stay.
        /// </summary>
        internal static IReadOnlyList<InlayHint> Merge(IReadOnlyList<InlayHint> old, IReadOnlyList<InlayHint> fresh, IReadOnlyList<(int Start, int End)> freshCovered)
        {
            var taken = new HashSet<(InlayHintKind, int)>(fresh.Select(h => (h.Kind, h.Position)));
            return old.Where(h => !taken.Contains((h.Kind, h.Position)) && !IsCovered(freshCovered, h.Position, h.Position + 1))
                .Concat(fresh)
                .OrderBy(h => h.Position)
                .ThenBy(h => h.Kind)
                .ToList();
        }
    }
}
