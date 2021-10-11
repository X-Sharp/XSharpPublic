using Microsoft.VisualStudio.Shell;
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.OtherOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp",false)]
    class OtherOptionsPage : XSDialogPage<OtherOptionsControl>
    {
        [DefaultValue(true)]
        public bool ShowDividers { get; set; }
        [DefaultValue(false)]
        public bool ShowSingleLineDividers { get; set; }
        [DefaultValue(true)]
        public bool ShowXmlComments { get; set; }
        [DefaultValue(0)]
        public int PublicStyle { get; set; }
        [DefaultValue(0)]
        public int PrivateStyle { get; set; }
        [DefaultValue(true)]
        public bool FormEditorMakeBackupFiles { get; set; }


    }
}
