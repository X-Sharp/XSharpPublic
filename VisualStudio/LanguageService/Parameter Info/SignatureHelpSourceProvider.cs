using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;
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
            if (XEditorSettings.DisableParameterInfo)
                return null;
            var file = textBuffer.GetFile();
            if (file == null || file.XFileType != XFileType.SourceCode)
                return null;

            return new XSharpSignatureHelpSource(textBuffer, file);
        }
    }
}
