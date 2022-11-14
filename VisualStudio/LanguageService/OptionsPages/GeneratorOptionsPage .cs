using Microsoft.VisualStudio.Shell;
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.GeneratorOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp",false)]
    [ComVisible(true)]
    class GeneratorOptionsPage : XSDialogPage<GeneratorOptionsControl>
    {
        [DefaultValue(true)]
        public bool ShowXmlComments { get; set; }
        [DefaultValue(0)]
        public int PublicStyle { get; set; }
        [DefaultValue(0)]
        public int PrivateStyle { get; set; }
    
    }
}
