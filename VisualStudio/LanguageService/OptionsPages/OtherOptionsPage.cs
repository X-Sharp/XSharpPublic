using Microsoft.VisualStudio.Shell;
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.OtherOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp", false)]
    [ComVisible(true)]
    class OtherOptionsPage : XSDialogPage<OtherOptionsControl, OtherOptions>
    {
        public bool AutoPairs { get; set; }
        public bool AutoOpen { get; set; }
        public bool ShowDividers { get; set; }
        public bool ShowSingleLineDividers { get; set; }
        public bool ShowXmlComments { get; set; }
        public bool LanguageServiceLogging { get; set; }
        public bool FormEditorMakeBackupFiles { get; set; }
        public bool EnableHighlightWord { get; set; }
        public bool EnableBraceMatching { get; set; }
        public bool EnableKeywordmatching { get; set; }
        public bool EnableLightBulbs { get; set; }
        public bool EnableQuickInfo { get; set; }
        public bool EnableParameterInfo { get; set; }
        public bool EnableRegions { get; set; }
        public bool EnableCodeCompletion { get; set; }

        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
           
        }
    }
}
