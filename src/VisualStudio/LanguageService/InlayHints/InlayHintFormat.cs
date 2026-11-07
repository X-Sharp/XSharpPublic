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
    /// <summary>The colours of the hints; changeable in Tools - Options - Fonts and Colors.</summary>
    [Export(typeof(EditorFormatDefinition))]
    [Name(FormatName)]
    [UserVisible(true)]
    internal sealed class InlayHintFormat : EditorFormatDefinition
    {
        internal const string FormatName = "X# Inlay Hint";

        public InlayHintFormat()
        {
            DisplayName = FormatName;
            ForegroundColor = Color.FromRgb(0x80, 0x80, 0x80);
            BackgroundColor = Color.FromArgb(InlayHintColors.BackgroundAlpha, 0x80, 0x80, 0x80);
        }
    }

    /// <summary>
    /// Fonts and Colors stores colours without alpha, so the background comes back opaque, as grey as the text.
    /// An opaque background is drawn translucent; the colour itself stays the user's choice.
    /// </summary>
    internal static class InlayHintColors
    {
        internal const byte BackgroundAlpha = 0x28;

        internal static byte DrawnAlpha(byte storedAlpha) => storedAlpha == 0xFF ? BackgroundAlpha : storedAlpha;

        internal static Brush Background(Brush stored)
        {
            if (!(stored is SolidColorBrush solid))
                return stored ?? Brushes.Transparent;
            var color = solid.Color;
            color.A = DrawnAlpha(color.A);
            var brush = new SolidColorBrush(color);
            brush.Freeze();
            return brush;
        }
    }
}
