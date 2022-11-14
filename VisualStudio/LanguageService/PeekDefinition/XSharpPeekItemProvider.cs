using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Utilities;
using static XSharp.XSharpConstants;
using XSharpModel;
namespace XSharp.LanguageService
{
    [Export(typeof(IPeekableItemSourceProvider))]
    [ContentType( LanguageName)]
    [Name("XSharp Class Peekable Item Provider")]
    [SupportsStandaloneFiles(true)]
    class XSharpPeekItemProvider : IPeekableItemSourceProvider
    {

#pragma warning disable 649 // "field never assigned to" -- field is set by MEF.
        [Import]
        private IPeekResultFactory _peekResultFactory;
#pragma warning restore 649

        public IPeekableItemSource TryCreatePeekableItemSource(ITextBuffer textBuffer)
        {
            var file = textBuffer.GetFile();
            if (file == null || file.XFileType != XFileType.SourceCode)
                return null;
            return textBuffer.Properties.GetOrCreateSingletonProperty(() => new XSharpPeekItemSource(textBuffer, _peekResultFactory, file));
        }
    }
}
