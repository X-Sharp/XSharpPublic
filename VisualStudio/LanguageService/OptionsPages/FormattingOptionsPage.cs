using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;
using XSharpModel;

namespace XSharp.LanguageService.OptionsPages
{

    [SharedSettings("TextEditor.XSharp",false)]
    [Guid(XSharpConstants.FormattingOptionsPageGuidString)]
    public class FormattingOptionsPage : XSDialogPage<FormattingOptionsControl>
    {
        // 0 : none; 1 : UPPER; 2 : lower; 3 : TitleCase
        public KeywordCase KeywordCase { get; set; }
        public bool IdentifierCase => false;
        public bool UdcCase  { get; set; }
        public bool AlignDoCase { get; set; }
        public bool AlignMethod { get; set; }
        public bool TrimTrailingWhiteSpace { get; set; }
        public bool InsertFinalNewLine { get; set; }
        public int MultiFactor { get; set; }
    }
}
