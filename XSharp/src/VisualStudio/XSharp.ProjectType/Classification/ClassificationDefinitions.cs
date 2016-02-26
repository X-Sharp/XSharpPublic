using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace XSharpColorizer
{
    internal static class ClassificationTypes
    {
        [Export(typeof(ClassificationTypeDefinition))]
        [Name(Constants.XSharpKeywordFormat)]
        internal static ClassificationTypeDefinition XSharpKeyword = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(Constants.XSharpConstantFormat)]
        internal static ClassificationTypeDefinition XSharpConstant = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(Constants.XSharpIdentifierFormat)]
        internal static ClassificationTypeDefinition XSharpIdentifier = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(Constants.XSharpCommentFormat)]
        internal static ClassificationTypeDefinition XSharpComment = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(Constants.XSharpOperatorFormat)]
        internal static ClassificationTypeDefinition XSharpOperator = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(Constants.XSharpBraceOpenFormat)]
        internal static ClassificationTypeDefinition XSharpBraceOpenFormat = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(Constants.XSharpBraceCloseFormat)]
        internal static ClassificationTypeDefinition XSharpBraceCloseFormat = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(Constants.XSharpRegionStartFormat)]
        internal static ClassificationTypeDefinition XSharpRegionStartFormat = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(Constants.XSharpRegionStopFormat)]
        internal static ClassificationTypeDefinition XSharpRegionStopFormat = null;

    }
}
