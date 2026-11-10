//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;

namespace XSharp.ProjectSystem.ShadowDesigner
{
    /// <summary>
    /// Copies a form's .resx file(s) -- the neutral Form1.resx and any culture-specific
    /// siblings the Designer creates when a form's Language property is changed, e.g.
    /// Form1.nl.resx -- from the companion project's folder back to the real project's
    /// folder, next to the real .prg. Unlike Form1.Designer.cs, nothing else in the bridge
    /// ever copies these over: EventHandlerSync/DesignerChangesSync only look at C# source,
    /// so a resx the Designer writes into the companion project is otherwise silently
    /// stranded there.
    ///
    /// The companion project is a fresh, standard-convention project, so the Designer only
    /// ever reads/writes the neutral resx as "{ClassName}.resx" there. The real project may
    /// have its neutral resx under a different, non-canonical name (e.g. "Form1.Designer.resx",
    /// via a <DependentUpon> association) -- matching is therefore done by culture (neutral
    /// vs. a specific CultureInfo), not by filename equality, so a pre-existing real-side
    /// filename is preserved across syncs instead of spawning a parallel canonically-named
    /// duplicate. Culture-specific siblings use the same "{ClassName}.{culture}.resx" name on
    /// both sides, so those are matched/copied by filename directly.
    ///
    /// Assumes the real .xsproj's SDK-style default item globbing already picks up a .resx
    /// file dropped into its folder as an EmbeddedResource (the same implicit-globbing
    /// behavior already relied on for .prg Compile items elsewhere in this bridge) -- no
    /// project-file edit is made here, only a file copy.
    ///
    /// What about other external files (Images, ??? ); Time will tell.
    ///
    /// </summary>
    internal static class CompanionResourceSync
    {
        public sealed class SyncResult
        {
            public List<string> CopiedFileNames { get; } = new List<string>();
        }

        public static SyncResult Sync(ShadowDesignerBridge.CompanionLocation location)
        {
            var result = new SyncResult();

            string companionDir = Path.GetDirectoryName(location.CompanionDesignerCsPath);
            string realDir = Path.GetDirectoryName(location.MainPrgPath);
            string className = Path.GetFileNameWithoutExtension(location.MainPrgPath);
            if (string.IsNullOrEmpty(companionDir) || string.IsNullOrEmpty(realDir) || !Directory.Exists(companionDir))
            {
                return result;
            }

            foreach (string companionResxPath in Directory.EnumerateFiles(companionDir, className + "*.resx"))
            {
                string companionFileName = Path.GetFileName(companionResxPath);
                if (!TryGetCulture(companionFileName, className, out string culture))
                {
                    continue;
                }

                string realResxPath = culture == null
                    ? FindRealNeutralResxPath(realDir, className) ?? Path.Combine(realDir, companionFileName)
                    : Path.Combine(realDir, companionFileName);

                if (FilesDiffer(companionResxPath, realResxPath))
                {
                    File.Copy(companionResxPath, realResxPath, overwrite: true);
                    result.CopiedFileNames.Add(Path.GetFileName(realResxPath));
                }
            }

            return result;
        }

        /// <summary>
        /// Bootstraps the companion folder from any .resx already sitting next to the real
        /// .prg (e.g. an icon on a form carried over before the bridge ever ran) -- called
        /// once, right after the companion project/files are (re)written, so the Designer
        /// doesn't open against a blank resource set the first time. Deliberately one-way and
        /// non-destructive: only copies a file the companion side doesn't have yet, never
        /// overwrites an existing companion .resx -- the real project is otherwise always the
        /// source of truth once <see cref="Sync"/> has run at least once (companion -> real).
        /// The real project's neutral resx is renamed to the companion's canonical
        /// "{ClassName}.resx" regardless of what it's actually called on the real side (see
        /// class remarks), so the Designer correctly associates it with the form's class.
        /// </summary>
        public static void PrimeFromReal(ShadowDesignerBridge.CompanionLocation location)
        {
            string companionDir = Path.GetDirectoryName(location.CompanionDesignerCsPath);
            string realDir = Path.GetDirectoryName(location.MainPrgPath);
            string className = Path.GetFileNameWithoutExtension(location.MainPrgPath);
            if (string.IsNullOrEmpty(companionDir) || string.IsNullOrEmpty(realDir) || !Directory.Exists(realDir))
            {
                return;
            }
            Directory.CreateDirectory(companionDir);

            foreach (string realResxPath in Directory.EnumerateFiles(realDir, className + "*.resx"))
            {
                string realFileName = Path.GetFileName(realResxPath);
                if (!TryGetCulture(realFileName, className, out string culture))
                {
                    continue;
                }

                string companionFileName = culture == null ? className + ".resx" : realFileName;
                string companionResxPath = Path.Combine(companionDir, companionFileName);
                if (!File.Exists(companionResxPath))
                {
                    File.Copy(realResxPath, companionResxPath);
                }
            }
        }

        /// <summary>
        /// Finds the real project's existing neutral resx for this class, whatever it's
        /// actually named (e.g. "Form1.Designer.resx"), so <see cref="Sync"/> keeps updating
        /// that same file instead of spawning a parallel canonically-named one. Prefers the
        /// canonical "{ClassName}.resx" name if more than one candidate somehow matches.
        /// </summary>
        private static string FindRealNeutralResxPath(string realDir, string className)
        {
            string canonicalPath = Path.Combine(realDir, className + ".resx");
            if (File.Exists(canonicalPath))
            {
                return canonicalPath;
            }

            return Directory.EnumerateFiles(realDir, className + "*.resx")
                .FirstOrDefault(path => TryGetCulture(Path.GetFileName(path), className, out string culture) && culture == null);
        }

        /// <summary>
        /// Matches "Form1.resx" and culture-qualified siblings like "Form1.nl.resx" /
        /// "Form1.de-DE.resx", but not an unrelated form sharing the same companion folder
        /// whose name merely starts with the same characters (e.g. "Form10.resx" must NOT
        /// match className "Form1"). On a match, <paramref name="culture"/> is null for the
        /// neutral resx (including a non-standard middle segment like "Designer" that isn't a
        /// real culture name) or the culture segment (e.g. "nl") for a culture-specific one.
        /// </summary>
        private static bool TryGetCulture(string fileName, string className, out string culture)
        {
            culture = null;
            if (!fileName.EndsWith(".resx", StringComparison.OrdinalIgnoreCase))
            {
                return false;
            }
            string stem = fileName.Substring(0, fileName.Length - ".resx".Length);
            if (stem.Equals(className, StringComparison.OrdinalIgnoreCase))
            {
                return true;
            }
            if (!stem.StartsWith(className + ".", StringComparison.OrdinalIgnoreCase))
            {
                return false;
            }

            string segment = stem.Substring(className.Length + 1);
            if (IsValidCulture(segment))
            {
                culture = segment;
            }
            return true;
        }

        private static bool IsValidCulture(string segment)
        {
            try
            {
                return !string.IsNullOrEmpty(CultureInfo.GetCultureInfo(segment).Name);
            }
            catch (CultureNotFoundException)
            {
                return false;
            }
        }

        private static bool FilesDiffer(string companionPath, string realPath)
        {
            if (!File.Exists(realPath))
            {
                return true;
            }
            return !File.ReadAllBytes(companionPath).SequenceEqual(File.ReadAllBytes(realPath));
        }
    }
}
