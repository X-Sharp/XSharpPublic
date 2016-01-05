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
        [Name(Constants.XSharpValueFormat)]
        internal static ClassificationTypeDefinition XSharpValue = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(Constants.XSharpBraceOpenFormat)]
        internal static ClassificationTypeDefinition XSharpBraceOpenFormat = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(Constants.XSharpBraceCloseFormat)]
        internal static ClassificationTypeDefinition XSharpBraceCloseFormat = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(Constants.XSharpRegionFormat)]
        internal static ClassificationTypeDefinition XSharpRegionFormat = null;

    }
}
