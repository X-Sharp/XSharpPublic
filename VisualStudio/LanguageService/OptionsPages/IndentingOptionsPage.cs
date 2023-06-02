using Microsoft.VisualStudio.Shell;
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using XSharpModel;

namespace XSharp.LanguageService.OptionsPages
{

    [SharedSettings("TextEditor.XSharp",false)]
    [Guid(XSharpConstants.IndentingOptionsPageGuidString)]
    [ComVisible(true)]
    public class IndentingOptionsPage : XSDialogPage<IndentingOptionsControl, IndentingOptions>
    {
        public bool IndentInitialized { get; set; }
        public bool IndentEntityContent { get; set; }
        public bool IndentFieldContent { get; set; }
        public bool IndentBlockContent { get; set; }
        public bool IndentCaseContent { get; set; }
        public bool IndentMultiLines { get; set; }
        public bool IndentCaseLabel { get; set; }
        public bool IndentPreprocessorLines { get; set; }
        public bool IndentNamespace { get; set; }

        public void ValidateSettings()
        {
            this.SaveSettingsToStorage();
        }
    }
}
