using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Utilities;
using static XSharp.LanguageService.XSharpConstants;

namespace XSharp.LanguageService
{
    [Export(typeof(IPeekableItemSourceProvider))]
    [ContentType( LanguageName)]
    [Name("XSharp Class Peekable Item Provider")]
    [SupportsStandaloneFiles(false)]
    class XSharpPeekItemProvider : IPeekableItemSourceProvider
    {

#pragma warning disable 649 // "field never assigned to" -- field is set by MEF.
        [Import]
        private IPeekResultFactory _peekResultFactory;
#pragma warning restore 649

        public IPeekableItemSource TryCreatePeekableItemSource(ITextBuffer textBuffer)
        {

            var package = XSharpLanguageService.Instance;
            var optionsPage = package.GetIntellisenseOptionsPage();
            if (optionsPage.DisablePeekDefinition)
                return null;

            return textBuffer.Properties.GetOrCreateSingletonProperty(() => new XSharpPeekItemSource(textBuffer, _peekResultFactory));
        }
    }
}
