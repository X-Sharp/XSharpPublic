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
        public bool AutoPairs
        {
            get => Options.AutoPairs;
            set => Options.AutoPairs = value;
        }
        public bool AutoOpen
        {
            get => Options.AutoOpen;
            set => Options.AutoOpen = value;
        }
        public bool ShowDividers
        {
            get => Options.ShowDividers;
            set => Options.ShowDividers = value;
        }
        public bool ShowSingleLineDividers
        {
            get => Options.ShowSingleLineDividers;
            set => Options.ShowSingleLineDividers = value;
        }
        
      
        public bool FormEditorMakeBackupFiles
        {
            get => Options.FormEditorMakeBackupFiles;
            set => Options.FormEditorMakeBackupFiles = value;
        }
        public bool EnableHighlightWord
        {
            get => Options.EnableHighlightWord;
            set => Options.EnableHighlightWord = value;
        }
        public bool EnableBraceMatching
        {
            get => Options.EnableBraceMatching;
            set => Options.EnableBraceMatching = value;
        }
        public bool EnableKeywordmatching
        {
            get => Options.EnableKeywordmatching;
            set => Options.EnableKeywordmatching = value;
        }
        public bool EnableLightBulbs
        {
            get => Options.EnableLightBulbs;
            set => Options.EnableLightBulbs = value;
        }
        public bool EnableQuickInfo
        {
            get => Options.EnableQuickInfo;
            set => Options.EnableQuickInfo = value;
        }
        public bool EnableParameterInfo
        {
            get => Options.EnableParameterInfo;
            set => Options.EnableParameterInfo = value;
        }
        public bool EnableRegions
        {
            get => Options.EnableRegions;
            set => Options.EnableRegions = value;
        }
        public bool EnableCodeCompletion
        {
            get => Options.EnableCodeCompletion;
            set => Options.EnableCodeCompletion = value;
        }
        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
           
        }
    }
}
