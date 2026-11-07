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
            BackgroundColor = Color.FromArgb(0x28, 0x80, 0x80, 0x80);
        }
    }
}
