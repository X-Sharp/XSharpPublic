//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Linq;

namespace XSharp.ProjectSystem.ShadowDesigner
{
    /// <summary>
    /// Removes the companion projects, and the "Shadow Designer (generated)" Solution Folder once it is empty, from a
    /// solution file (.slnx or .sln) after VS closed the solution. Only needed when the user answered No to VS's
    /// save prompt while the file still listed a companion (see ShadowDesignerCleanup). Only these entries are
    /// removed; the rest of the file, its encoding and its line endings are kept. Pure file I/O, no VS API.
    /// </summary>
    internal static class SolutionFileCleanup
    {
        private const string SolutionFolderTypeGuid = "{2150E333-8FDC-42A3-9474-1A3956D46DE8}";

        public static void RemoveCompanionProjects(string solutionFile, ICollection<string> companionCsprojPaths)
        {
            if (string.IsNullOrEmpty(solutionFile) || companionCsprojPaths == null || companionCsprojPaths.Count == 0 ||
                !File.Exists(solutionFile))
            {
                return;
            }
            try
            {
                byte[] bytes = File.ReadAllBytes(solutionFile);
                bool bom = bytes.Length >= 3 && bytes[0] == 0xEF && bytes[1] == 0xBB && bytes[2] == 0xBF;
                if (!bom && bytes.Length >= 2 && (bytes[0] == 0xFF || bytes[0] == 0xFE))
                {
                    // UTF-16: not written by VS for .sln/.slnx, left alone
                    return;
                }
                var encoding = new UTF8Encoding(bom);
                string text = encoding.GetString(bytes, bom ? 3 : 0, bytes.Length - (bom ? 3 : 0));
                string solutionDir = Path.GetDirectoryName(solutionFile);
                bool isCompanion(string relativePath) => IsCompanionPath(solutionDir, relativePath, companionCsprojPaths);

                string updated = solutionFile.EndsWith(".slnx", StringComparison.OrdinalIgnoreCase)
                    ? RemoveFromSlnx(text, isCompanion)
                    : RemoveFromSln(text, isCompanion);
                if (updated == null || updated == text)
                {
                    return;
                }
                File.WriteAllBytes(solutionFile, encoding.GetPreamble().Concat(encoding.GetBytes(updated)).ToArray());
                Logger.Information($"SolutionFileCleanup: removed the companion projects from {solutionFile}");
            }
            catch (Exception ex)
            {
                // Best-effort: at worst VS reports the project as not found on the next open, and
                // SolutionWiring.EnsureProjectInSolution removes the stale entry on the next View Designer
                Logger.Exception(ex, $"SolutionFileCleanup: could not clean {solutionFile}");
            }
        }

        private static bool IsCompanionPath(string solutionDir, string relativePath, ICollection<string> companionCsprojPaths)
        {
            if (string.IsNullOrEmpty(relativePath))
            {
                return false;
            }
            try
            {
                string fullPath = Path.GetFullPath(Path.Combine(solutionDir, relativePath.Replace('/', '\\')));
                return companionCsprojPaths.Any(p => string.Equals(p, fullPath, StringComparison.OrdinalIgnoreCase));
            }
            catch (Exception)
            {
                // Not a valid path, so not a companion
                return false;
            }
        }

        /// <summary>
        /// .slnx: removes the &lt;Project Path="..."&gt; elements of the companions, then the Solution Folder
        /// (&lt;Folder Name="/Shadow Designer (generated)/"&gt;) when it has no other content.
        /// </summary>
        private static string RemoveFromSlnx(string text, Func<string, bool> isCompanion)
        {
            var doc = XDocument.Parse(text, LoadOptions.PreserveWhitespace);
            var projects = doc.Descendants("Project").Where(p => isCompanion((string)p.Attribute("Path"))).ToList();
            if (projects.Count == 0)
            {
                return null;
            }
            foreach (var project in projects)
            {
                RemoveWithIndentation(project);
            }
            string folderName = "/" + SolutionWiring.ShadowDesignerFolderName + "/";
            foreach (var folder in doc.Descendants("Folder").Where(f => (string)f.Attribute("Name") == folderName && !f.HasElements).ToList())
            {
                RemoveWithIndentation(folder);
            }

            var settings = new XmlWriterSettings
            {
                OmitXmlDeclaration = doc.Declaration == null,
                Encoding = new UTF8Encoding(false),
                // XML parsing turned CRLF into LF in the whitespace nodes
                NewLineChars = text.Contains("\r\n") ? "\r\n" : "\n",
                NewLineHandling = NewLineHandling.Replace,
            };
            var builder = new StringBuilder();
            using (var writer = XmlWriter.Create(new StringWriter(builder), settings))
            {
                doc.Save(writer);
            }
            string result = builder.ToString();
            if (doc.Declaration != null)
            {
                // A StringWriter makes XmlWriter write encoding="utf-16": keep the original declaration
                int end = result.IndexOf("?>", StringComparison.Ordinal);
                result = doc.Declaration.ToString() + result.Substring(end + 2);
            }
            // Keep a final line break the original had
            if (text.EndsWith("\n") && !result.EndsWith("\n"))
            {
                result += settings.NewLineChars;
            }
            return result;
        }

        /// <summary>Removes the element and the whitespace before it, so no empty line is left.</summary>
        private static void RemoveWithIndentation(XElement element)
        {
            if (element.PreviousNode is XText indentation && string.IsNullOrWhiteSpace(indentation.Value))
            {
                indentation.Remove();
            }
            element.Remove();
        }

        private static readonly Regex SlnProjectLine = new Regex(
            @"^Project\(""(?<type>\{[^}]+\})""\)\s*=\s*""(?<name>[^""]*)""\s*,\s*""(?<path>[^""]*)""\s*,\s*""(?<guid>\{[^}]+\})""",
            RegexOptions.IgnoreCase);

        /// <summary>
        /// .sln: removes the Project...EndProject blocks of the companions and every line that names their GUID
        /// (configurations, NestedProjects), then the Solution Folder when no project is nested in it anymore.
        /// </summary>
        private static string RemoveFromSln(string text, Func<string, bool> isCompanion)
        {
            string newLine = text.Contains("\r\n") ? "\r\n" : "\n";
            var lines = text.Split(new[] { newLine }, StringSplitOptions.None).ToList();

            var companionGuids = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            string folderGuid = null;
            foreach (string line in lines)
            {
                var match = SlnProjectLine.Match(line);
                if (!match.Success)
                {
                    continue;
                }
                if (isCompanion(match.Groups["path"].Value))
                {
                    companionGuids.Add(match.Groups["guid"].Value);
                }
                else if (string.Equals(match.Groups["type"].Value, SolutionFolderTypeGuid, StringComparison.OrdinalIgnoreCase) &&
                    match.Groups["name"].Value == SolutionWiring.ShadowDesignerFolderName)
                {
                    folderGuid = match.Groups["guid"].Value;
                }
            }
            if (companionGuids.Count == 0)
            {
                return null;
            }
            RemoveSlnProjects(lines, companionGuids);

            // NestedProjects lines read "{child} = {parent}": the folder is empty when it is nobody's parent anymore
            if (folderGuid != null && !lines.Any(l => l.Trim().EndsWith("= " + folderGuid, StringComparison.OrdinalIgnoreCase)))
            {
                RemoveSlnProjects(lines, new HashSet<string>(StringComparer.OrdinalIgnoreCase) { folderGuid });
            }
            RemoveEmptyNestedProjectsSection(lines);
            return string.Join(newLine, lines);
        }

        private static void RemoveSlnProjects(List<string> lines, HashSet<string> guids)
        {
            for (int i = 0; i < lines.Count; i++)
            {
                var match = SlnProjectLine.Match(lines[i]);
                if (match.Success && guids.Contains(match.Groups["guid"].Value))
                {
                    int end = i;
                    while (end < lines.Count && lines[end].Trim() != "EndProject")
                    {
                        end++;
                    }
                    lines.RemoveRange(i, Math.Min(end, lines.Count - 1) - i + 1);
                    i--;
                }
                else if (!match.Success && guids.Any(g => lines[i].TrimStart().StartsWith(g, StringComparison.OrdinalIgnoreCase)))
                {
                    // "{guid}.Debug|Any CPU.ActiveCfg = ..." and "{guid} = {parent}"
                    lines.RemoveAt(i);
                    i--;
                }
            }
        }

        private static void RemoveEmptyNestedProjectsSection(List<string> lines)
        {
            for (int i = 0; i < lines.Count - 1; i++)
            {
                if (lines[i].Trim().StartsWith("GlobalSection(NestedProjects)", StringComparison.OrdinalIgnoreCase) &&
                    lines[i + 1].Trim() == "EndGlobalSection")
                {
                    lines.RemoveRange(i, 2);
                    return;
                }
            }
        }
    }
}
