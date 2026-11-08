//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.ComponentModel.Composition;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace XSharp.LanguageService.InlayHints
{
    /// <summary>
    /// The colours of the hints when the C# "inline hints" item is missing (no Roslyn): the C# defaults.
    /// Not shown in Fonts and Colors: with C# present the item would change nothing.
    /// </summary>
    [Export(typeof(EditorFormatDefinition))]
    [Name(FormatName)]
    [UserVisible(false)]
    internal sealed class InlayHintFormat : EditorFormatDefinition
    {
        internal const string FormatName = "X# Inlay Hint";

        public InlayHintFormat()
        {
            DisplayName = FormatName;
            ForegroundColor = Color.FromRgb(0x68, 0x68, 0x68);
            BackgroundColor = Color.FromRgb(0xE6, 0xE6, 0xE6);
        }
    }

    internal static class InlayHintColors
    {
        /// <summary>The Fonts and Colors item of the C# inline hints (Roslyn), which the Visual Studio themes colour.</summary>
        internal const string CSharpFormatName = "inline hints";

        /// <summary>
        /// The hints look like the C# inline hints: their colours follow the theme and the user's choice for C#.
        /// Without the C# item the X# item is used.
        /// </summary>
        internal static (Brush Foreground, Brush Background) Pick(Brush csharpText, Brush csharpBack, Brush xsharpText, Brush xsharpBack)
        {
            if (csharpText != null)
                return (csharpText, csharpBack ?? Brushes.Transparent);
            return (xsharpText ?? Brushes.Gray, xsharpBack ?? Brushes.Transparent);
        }
    }
}
