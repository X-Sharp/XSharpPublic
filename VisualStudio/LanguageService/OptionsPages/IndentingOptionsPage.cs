using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;
using XSharpModel;

namespace XSharp.LanguageService.OptionsPages
{

    [SharedSettings("TextEditor.XSharp",false)]
    [Guid(XSharpConstants.IndentingOptionsPageGuidString)]
    public class IndentingOptionsPage : XSDialogPage<IndentingOptionsControl>
    {
        public bool IndentEntityContent { get; set; }

        public bool IndentBlockContent { get; set; }
        public bool IndentCaseContent { get; set; }
        public bool IndentCaseLabel { get; set; }
        public bool IndentMultiLines { get; set; }

    }
}
