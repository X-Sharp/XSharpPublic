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
    public class IndentingOptionsPage : XSDialogPage<IndentingOptionsControl>
    {
        [DefaultValue(false)]
        public bool IndentInitialized { get; set; }
        [DefaultValue(true)]
        public bool IndentEntityContent { get; set; }
        [DefaultValue(true)]
        public bool IndentFieldContent { get; set; }
        [DefaultValue(true)]
        public bool IndentBlockContent { get; set; }
        [DefaultValue(true)]
        public bool IndentCaseContent { get; set; }
        [DefaultValue(true)]
        public bool IndentMultiLines { get; set; }
        [DefaultValue(false)]

        public bool IndentCaseLabel { get; set; }
        [DefaultValue(false)]
        public bool IndentPreprocessorLines { get; set; }
        [DefaultValue(false)]
        public bool IndentNamespace { get; set; }

        public void ValidateSettings()
        {
            if (!IndentInitialized)
            {
                IndentEntityContent = true; // class members
                IndentFieldContent = true;  // class fields
                IndentBlockContent = true;  // statements
                IndentCaseContent = true;   // statement block inside case
                IndentMultiLines = true;    // Multi line statements
                IndentInitialized = true;
                this.SaveSettingsToStorage();
            }
        }
    }
}
