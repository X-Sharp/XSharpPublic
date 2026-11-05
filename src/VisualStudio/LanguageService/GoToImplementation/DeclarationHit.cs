//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Text.RegularExpressions;
using XSharp.LanguageService.FindReferences;

namespace XSharp.LanguageService.GoToImplementation
{
    /// <summary>A row of the Find All References window for a declaration found by Go To Implementation / Go To Base.</summary>
    internal static class DeclarationHit
    {
        /// <summary>
        /// The declaration line, indented by its start column so that Column points into the text, with the name (as a
        /// whole word, not a keyword that starts with it) as the hit.
        /// </summary>
        internal static ReferenceHit Create(string filePath, string projectName, int line, int startColumn, string declaration, string name, string group)
        {
            startColumn = Math.Max(startColumn, 0);
            var text = (declaration ?? name ?? "").Split('\r', '\n')[0];
            var match = string.IsNullOrEmpty(name) ? Match.Empty
                : Regex.Match(text, @"(?<![\w@])" + Regex.Escape(name) + @"(?![\w])", RegexOptions.IgnoreCase);
            return new ReferenceHit
            {
                FilePath = filePath,
                ProjectName = projectName,
                Line = line,
                Column = match.Success ? startColumn + match.Index : startColumn,
                Length = match.Success ? name.Length : 0,
                LineText = new string(' ', startColumn) + text,
                IsDefinition = true,
                Group = group,
            };
        }
    }
}
