//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.ComponentModel.Composition;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace XSharp.LanguageService
{

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ColorizerConstants.XSharpRegionStartFormat)]
    [Name(ColorizerConstants.XSharpRegionStartFormat)]
    [UserVisible(false)]
    [Order(After = Priority.Default)]

    internal sealed class XSharpRegionStartFormat : ClassificationFormatDefinition
    {
        public XSharpRegionStartFormat()
        {
            this.DisplayName = "XSharp Region Start";
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ColorizerConstants.XSharpRegionStopFormat)]
    [Name(ColorizerConstants.XSharpRegionStopFormat)]
    [UserVisible(false)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpRegionStopFormat : ClassificationFormatDefinition
    {
        public XSharpRegionStopFormat()
        {
            this.DisplayName = "XSharp Region Stop";
        }
    }


    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ColorizerConstants.XSharpTextEndTextFormat)]
    [Name(ColorizerConstants.XSharpTextEndTextFormat)]
    [UserVisible(true)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpTextEndTextFormat : ClassificationFormatDefinition
    {
        public XSharpTextEndTextFormat()
        {
            this.DisplayName = "X# Text .. EndText";
            this.ForegroundColor = Colors.DarkSalmon;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [Name(ColorizerConstants.BraceFormatDefinition)]
    [UserVisible(true)]
    internal class BraceFormatDefinition : MarkerFormatDefinition
    {
        /// <summary>
        /// Color of matching braces
        /// </summary>
        public BraceFormatDefinition()
        {
            this.BackgroundColor = Colors.Tan;
            this.DisplayName = "X# Brace Matching";
            this.ZOrder = 5;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [Name(ColorizerConstants.KeyWordFormatDefinition)]
    [UserVisible(true)]
    internal class KeywordFormatDefinition : MarkerFormatDefinition
    {
        /// <summary>
        /// The color of highlighted keyword (pairs)
        /// </summary>
        public KeywordFormatDefinition()
        {
            this.BackgroundColor = Colors.LightSalmon;
            this.DisplayName = "X# Highlight Keyword";
            this.ZOrder = 5;
        }
    }
    [Export(typeof(EditorFormatDefinition))]
    [Name(ColorizerConstants.HighLightIdentifierFormatDefinition)]
    [UserVisible(true)]
    internal class HighLightIdentifierFormatDefinition : MarkerFormatDefinition
    {
        /// <summary>
        /// Color of highlighted identifiers
        /// </summary>
        public HighLightIdentifierFormatDefinition()
        {
            this.BackgroundColor = Colors.Bisque;
            this.DisplayName = "X# Highlight Identifier";
            this.ZOrder = 5;
        }
    }
}
