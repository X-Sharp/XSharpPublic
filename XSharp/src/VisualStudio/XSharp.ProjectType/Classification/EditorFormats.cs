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
    [ClassificationType(ClassificationTypeNames = Constants.XSharpConstantFormat)]
    [Name(Constants.XSharpConstantFormat)]
    [UserVisible(true)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpValueFormat : ClassificationFormatDefinition
    {
        public XSharpValueFormat()
        {
            this.DisplayName = "XSharp Constant";
            this.ForegroundColor = Colors.Firebrick;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = Constants.XSharpIdentifierFormat)]
    [Name(Constants.XSharpIdentifierFormat)]
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
    [ClassificationType(ClassificationTypeNames = Constants.XSharpOperatorFormat)]
    [Name(Constants.XSharpOperatorFormat)]
    [UserVisible(true)]
    [Order(After = Priority.Default)]
    internal sealed class XSharpOperatorFormat : ClassificationFormatDefinition
    {
        public XSharpOperatorFormat()
        {
            this.DisplayName = "XSharp Operator";
            this.ForegroundColor = Colors.DarkGray;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = Constants.XSharpCommentFormat)]
    [Name(Constants.XSharpCommentFormat)]
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
    [ClassificationType(ClassificationTypeNames = Constants.XSharpBraceOpenFormat)]
    [Name(Constants.XSharpBraceOpenFormat)]
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
    [ClassificationType(ClassificationTypeNames = Constants.XSharpBraceCloseFormat)]
    [Name(Constants.XSharpBraceCloseFormat)]
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
    [ClassificationType(ClassificationTypeNames = Constants.XSharpRegionStartFormat)]
    [Name(Constants.XSharpRegionStartFormat)]
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
    [ClassificationType(ClassificationTypeNames = Constants.XSharpRegionStopFormat)]
    [Name(Constants.XSharpRegionStopFormat)]
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
