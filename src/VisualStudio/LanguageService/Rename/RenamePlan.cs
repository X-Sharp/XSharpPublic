//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageService.FindReferences;

namespace XSharp.LanguageService.Rename
{
    /// <summary>The places a rename changes, and the places it reports but never changes.</summary>
    internal sealed class RenamePlan
    {
        internal RenamePlan(IReadOnlyList<ReferenceHit> changes, IReadOnlyList<ReferenceHit> notChanged, IReadOnlyList<string> files)
        {
            Changes = changes;
            NotChanged = notChanged;
            Files = files;
        }
        /// <summary>Sure places, per file from the end of the file to its start, each once</summary>
        internal IReadOnlyList<ReferenceHit> Changes { get; }
        /// <summary>Every place whose uncertainty is not None</summary>
        internal IReadOnlyList<ReferenceHit> NotChanged { get; }
        /// <summary>The files of the changes, each once</summary>
        internal IReadOnlyList<string> Files { get; }
    }

    internal static class RenamePlanner
    {
        /// <summary>
        /// Splits the hits of a search: sure places become changes (a file that two projects share yields the same
        /// place twice: changed once), everything uncertain is only reported. With <paramref name="includeLateBound"/>
        /// late-bound sends (<c>o:Name</c>) and symbols (<c>#Name</c>) are changed too, as Find All References lists them.
        /// </summary>
        internal static RenamePlan Build(IReadOnlyList<ReferenceHit> hits, bool includeLateBound = false)
        {
            var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            var changes = new List<ReferenceHit>();
            var notChanged = new List<ReferenceHit>();
            foreach (var found in hits)
            {
                var hit = found;
                bool lateBound = hit.Uncertainty == ReferenceUncertainty.LateBound || hit.Uncertainty == ReferenceUncertainty.Symbol;
                if (hit.Uncertainty != ReferenceUncertainty.None && !(includeLateBound && lateBound))
                {
                    notChanged.Add(hit);
                    continue;
                }
                if (hit.Uncertainty == ReferenceUncertainty.Symbol)
                    hit = WithoutHash(hit);
                if (seen.Add(hit.FilePath + "|" + hit.Start))
                    changes.Add(hit);
            }
            return Create(changes, notChanged);
        }

        /// <summary>A #Name hit covers the hash: the change is the name behind it.</summary>
        private static ReferenceHit WithoutHash(ReferenceHit hit)
        {
            if (hit.Length < 2)
                return hit;
            return new ReferenceHit
            {
                FilePath = hit.FilePath,
                ProjectName = hit.ProjectName,
                Line = hit.Line,
                Column = hit.Column + 1,
                Length = hit.Length - 1,
                LineText = hit.LineText,
                Classifications = hit.Classifications,
                IsDefinition = hit.IsDefinition,
                Group = hit.Group,
                Start = hit.Start + 1,
                Uncertainty = hit.Uncertainty,
                InText = hit.InText
            };
        }

        /// <summary>The plan without the changes the user unchecked in the preview.</summary>
        internal static RenamePlan Without(RenamePlan plan, ISet<ReferenceHit> dropped)
        {
            return Create(plan.Changes.Where(h => !dropped.Contains(h)).ToList(), plan.NotChanged);
        }

        /// <summary>Is the old name still at the place (case-insensitive)? The file may have changed since the search.</summary>
        internal static bool StillMatches(string fileText, ReferenceHit hit, string oldName)
        {
            if (fileText == null || hit.Start < 0 || hit.Length != oldName.Length || hit.Start + hit.Length > fileText.Length)
                return false;
            return string.Compare(fileText, hit.Start, oldName, 0, oldName.Length, StringComparison.OrdinalIgnoreCase) == 0;
        }

        private static RenamePlan Create(IList<ReferenceHit> changes, IReadOnlyList<ReferenceHit> notChanged)
        {
            var files = new List<string>();
            var ordered = new List<ReferenceHit>();
            foreach (var file in changes.GroupBy(h => h.FilePath, StringComparer.OrdinalIgnoreCase))
            {
                files.Add(file.First().FilePath);
                ordered.AddRange(file.OrderByDescending(h => h.Start));
            }
            return new RenamePlan(ordered, notChanged, files);
        }
    }
}
