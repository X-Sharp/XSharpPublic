using System.ComponentModel.Composition;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;
using System.Windows;

namespace XSharpColorizer
{
    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = Constants.XSharpKeywordFormat)]
    [Name(Constants.XSharpKeywordFormat)]
    [UserVisible(true)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpKeywordFormat : ClassificationFormatDefinition
    {
        public XSharpKeywordFormat()
        {
            this.DisplayName = "XSharp Keyword";
            this.ForegroundColor = Colors.Blue;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = Constants.XSharpValueFormat)]
    [Name(Constants.XSharpValueFormat)]
    [UserVisible(true)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpValueFormat : ClassificationFormatDefinition
    {
        public XSharpValueFormat()
        {
            this.DisplayName = "XSharp Value";
            this.ForegroundColor = Colors.Firebrick;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = Constants.XSharpBraceOpenFormat)]
    [Name(Constants.XSharpBraceOpenFormat)]
    [UserVisible(true)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpBraceOpenFormat : ClassificationFormatDefinition
    {
        public XSharpBraceOpenFormat()
        {
            this.DisplayName = "XSharp Brace Open";
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = Constants.XSharpBraceCloseFormat)]
    [Name(Constants.XSharpBraceCloseFormat)]
    [UserVisible(true)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpBraceCloseFormat : ClassificationFormatDefinition
    {
        public XSharpBraceCloseFormat()
        {
            this.DisplayName = "XSharp Brace Close";
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = Constants.XSharpRegionFormat)]
    [Name(Constants.XSharpRegionFormat)]
    [UserVisible(true)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpRegionFormat : ClassificationFormatDefinition
    {
        public XSharpRegionFormat()
        {
            this.DisplayName = "XSharp Region";
        }
    }

}
