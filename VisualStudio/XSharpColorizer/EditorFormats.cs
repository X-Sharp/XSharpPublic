//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.ComponentModel.Composition;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;
using System.Windows;

namespace XSharpColorizer
{
    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ColorizerConstants.XSharpKeywordFormat)]
    [Name(ColorizerConstants.XSharpKeywordFormat)]
    [UserVisible(true)]
    [Order(After = Priority.Low)]
    internal sealed class XSharpKeywordFormat : ClassificationFormatDefinition
    {
        public XSharpKeywordFormat()
        {
            this.DisplayName = "XSharp Keyword";
            this.ForegroundColor = Colors.Blue;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ColorizerConstants.XSharpConstantFormat)]
    [Name(ColorizerConstants.XSharpConstantFormat)]
    [UserVisible(true)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpValueFormat : ClassificationFormatDefinition
    {
        public XSharpValueFormat()
        {
            this.DisplayName = "XSharp Constant";
            this.ForegroundColor = Colors.Brown;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ColorizerConstants.XSharpIdentifierFormat)]
    [Name(ColorizerConstants.XSharpIdentifierFormat)]
    [UserVisible(true)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpIdentifierFormat : ClassificationFormatDefinition
    {
        public XSharpIdentifierFormat()
        {
            this.DisplayName = "XSharp Identifier";
            this.ForegroundColor = Colors.Black;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ColorizerConstants.XSharpOperatorFormat)]
    [Name(ColorizerConstants.XSharpOperatorFormat)]
    [UserVisible(true)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpOperatorFormat : ClassificationFormatDefinition
    {
        public XSharpOperatorFormat()
        {
            this.DisplayName = "XSharp Operator";
            this.ForegroundColor = Colors.DarkCyan;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ColorizerConstants.XSharpCommentFormat)]
    [Name(ColorizerConstants.XSharpCommentFormat)]
    [UserVisible(true)]
    [Order(After = Priority.High)]
    internal sealed class XSharpCommentFormat : ClassificationFormatDefinition
    {
        public XSharpCommentFormat()
        {
            this.DisplayName = "XSharp Comment";
            this.ForegroundColor = Colors.Green;
            this.IsItalic = true;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ColorizerConstants.XSharpBraceOpenFormat)]
    [Name(ColorizerConstants.XSharpBraceOpenFormat)]
    [UserVisible(false)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpBraceOpenFormat : ClassificationFormatDefinition
    {
        public XSharpBraceOpenFormat()
        {
            this.DisplayName = "XSharp Brace Open";
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ColorizerConstants.XSharpBraceCloseFormat)]
    [Name(ColorizerConstants.XSharpBraceCloseFormat)]
    [UserVisible(false)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpBraceCloseFormat : ClassificationFormatDefinition
    {
        public XSharpBraceCloseFormat()
        {
            this.DisplayName = "XSharp Brace Close";
        }
    }

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

}
