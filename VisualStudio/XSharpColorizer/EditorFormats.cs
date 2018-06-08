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
using XSharpModel;

namespace XSharpColorizer
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

}
