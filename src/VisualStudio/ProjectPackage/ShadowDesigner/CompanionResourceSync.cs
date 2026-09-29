//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace XSharp.Project.ShadowDesigner
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
                string fileName = Path.GetFileName(companionResxPath);
                if (!IsResxForClass(fileName, className))
                {
                    continue;
                }

                string realResxPath = Path.Combine(realDir, fileName);
                if (FilesDiffer(companionResxPath, realResxPath))
                {
                    File.Copy(companionResxPath, realResxPath, overwrite: true);
                    result.CopiedFileNames.Add(fileName);
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
                string fileName = Path.GetFileName(realResxPath);
                if (!IsResxForClass(fileName, className))
                {
                    continue;
                }

                string companionResxPath = Path.Combine(companionDir, fileName);
                if (!File.Exists(companionResxPath))
                {
                    File.Copy(realResxPath, companionResxPath);
                }
            }
        }

        /// <summary>
        /// Matches "Form1.resx" and culture-qualified siblings like "Form1.nl.resx" /
        /// "Form1.de-DE.resx", but not an unrelated form sharing the same companion folder
        /// whose name merely starts with the same characters (e.g. "Form10.resx" must NOT
        /// match className "Form1").
        /// </summary>
        private static bool IsResxForClass(string fileName, string className)
        {
            if (!fileName.EndsWith(".resx", StringComparison.OrdinalIgnoreCase))
            {
                return false;
            }
            string stem = fileName.Substring(0, fileName.Length - ".resx".Length);
            return stem.Equals(className, StringComparison.OrdinalIgnoreCase)
                || stem.StartsWith(className + ".", StringComparison.OrdinalIgnoreCase);
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
