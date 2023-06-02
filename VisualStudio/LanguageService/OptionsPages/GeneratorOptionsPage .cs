using Microsoft.VisualStudio.Shell;
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.GeneratorOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp",false)]
    [ComVisible(true)]
    class GeneratorOptionsPage : XSDialogPage<GeneratorOptionsControl, GeneratorOptions>
    {
        public bool ShowXmlComments { get; set; }
        public int PublicStyle { get; set; }
        public int PrivateStyle { get; set; }


    }
}
