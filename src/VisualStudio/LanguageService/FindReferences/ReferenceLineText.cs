//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;

namespace XSharp.LanguageService.FindReferences
{
    /// <summary>
    /// The Text column of a hit: the trimmed source line with the hit in bold, like the C# entries.
    /// </summary>
    internal static class ReferenceLineText
    {
        /// <summary>Splits the trimmed line around the hit; a hit outside the line gives the whole line in Before.</summary>
        internal static (string Before, string Hit, string After) Split(string lineText, int column, int length)
        {
            lineText = lineText ?? "";
            var trimmed = lineText.Trim();
            var start = column - (lineText.Length - lineText.TrimStart().Length);
            if (start < 0 || length <= 0 || start + length > trimmed.Length)
                return (trimmed, "", "");
            return (trimmed.Substring(0, start), trimmed.Substring(start, length), trimmed.Substring(start + length));
        }

        internal static FrameworkElement CreateTextBlock(string lineText, int column, int length)
        {
            var parts = Split(lineText, column, length);
            var block = new TextBlock { TextWrapping = TextWrapping.NoWrap };
            block.Inlines.Add(new Run(parts.Before));
            if (parts.Hit.Length > 0)
            {
                block.Inlines.Add(new Run(parts.Hit) { FontWeight = FontWeights.Bold });
                block.Inlines.Add(new Run(parts.After));
            }
            return block;
        }
    }
}
