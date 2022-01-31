﻿//
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
    [ClassificationType(ClassificationTypeNames = ColorizerConstants.XSharpKwOpenFormat)]
    [Name(ColorizerConstants.XSharpKwOpenFormat)]
    [UserVisible(false)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpBraceOpenFormat : ClassificationFormatDefinition
    {
        public XSharpBraceOpenFormat()
        {
            this.DisplayName = "XSharp Keyword Open";
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ColorizerConstants.XSharpKwCloseFormat)]
    [Name(ColorizerConstants.XSharpKwCloseFormat)]
    [UserVisible(false)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpBraceCloseFormat : ClassificationFormatDefinition
    {
        public XSharpBraceCloseFormat()
        {
            this.DisplayName = "XSharp Keyword Close";
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
            this.DisplayName = "XSharp Text .. EndText";
            this.ForegroundColor = Colors.DarkSalmon;
        }
    }

}
