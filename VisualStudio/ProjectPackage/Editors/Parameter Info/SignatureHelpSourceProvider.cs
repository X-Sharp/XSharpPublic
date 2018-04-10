using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel.Composition;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.OLE.Interop;

namespace XSharp.Project
{

    [Export(typeof(ISignatureHelpSourceProvider))]
    [Name("Signature Help source")]
    [Order(Before = "default")]
    [ContentType("XSharp")]
    internal class XSharpSignatureHelpSourceProvider : ISignatureHelpSourceProvider
    {
        public ISignatureHelpSource TryCreateSignatureHelpSource(ITextBuffer textBuffer)
        {
            var package = XSharp.Project.XSharpProjectPackage.Instance;
            var optionsPage = package.GetIntellisenseOptionsPage();
            if (optionsPage.DisableParameterInfo)
                return null;

            return new XSharpSignatureHelpSource(textBuffer);
        }
    }
}
