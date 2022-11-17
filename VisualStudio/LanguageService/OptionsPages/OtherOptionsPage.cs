using Microsoft.VisualStudio.Shell;
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.OtherOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp", false)]
    [ComVisible(true)]
    class OtherOptionsPage : XSDialogPage<OtherOptionsControl>
    {

        [DefaultValue(true)]
        public bool AutoPairs { get; set; }
        [DefaultValue(true)]
        public bool AutoOpen { get; set; }
        [DefaultValue(true)]
        public bool ShowDividers { get; set; }
        [DefaultValue(false)]
        public bool ShowSingleLineDividers { get; set; }
        [DefaultValue(true)]
        public bool ShowXmlComments { get; set; }
        [DefaultValue(false)]
        public bool LanguageServiceLogging { get; set; }
        [DefaultValue(true)]
        public bool FormEditorMakeBackupFiles { get; set; }

        [DefaultValue(true)]
        public bool EnableHighlightWord { get; set; }
        [DefaultValue(true)]
        public bool EnableBraceMatching { get; set; }

        [DefaultValue(true)]
        public bool EnableKeywordmatching { get; set; }

        [DefaultValue(true)]
        public bool EnableLightBulbs { get; set; }

        [DefaultValue(true)]
        public bool EnableQuickInfo { get; set; }

        [DefaultValue(true)]
        public bool EnableParameterInfo { get; set; }

        [DefaultValue(true)]
        public bool EnableRegions { get; set; }
        [DefaultValue(true)]
        public bool EnableCodeCompletion { get; set; }

        [DefaultValue(1)]
        public int OptionsInitialized { get; set; }

        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
            if (OptionsInitialized == 0)
            {
                EnableHighlightWord = true;
                EnableBraceMatching = true;
                EnableKeywordmatching = true;
                EnableLightBulbs = true;
                EnableQuickInfo = true;
                EnableParameterInfo = true;
                EnableCodeCompletion = true;
                EnableRegions = true;
                AutoPairs = true;
                AutoOpen = true;
                OptionsInitialized = 1;
            }
        }
    }
}
