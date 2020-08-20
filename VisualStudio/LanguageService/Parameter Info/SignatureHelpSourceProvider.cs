using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Utilities;

namespace XSharp.LanguageService
{

    [Export(typeof(ISignatureHelpSourceProvider))]
    [Name("Signature Help source")]
    [Order(Before = "default")]
    [ContentType(XSharpConstants.LanguageName)]
    internal class XSharpSignatureHelpSourceProvider : ISignatureHelpSourceProvider
    {
        public ISignatureHelpSource TryCreateSignatureHelpSource(ITextBuffer textBuffer)
        {
            var package = XSharpLanguageService.Instance;
            var optionsPage = package.GetIntellisenseOptionsPage();
            if (optionsPage.DisableParameterInfo)
                return null;

            return new XSharpSignatureHelpSource(textBuffer);
        }
    }
}
