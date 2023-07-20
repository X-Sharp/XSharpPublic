using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.IntellisenseOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp", false)]
    [ComVisible(true)]
    public class IntellisenseOptionsPage : XSDialogPage<IntellisenseOptionsControl, IntellisenseOptions>
    {
        // The base class exposes the AutomationObject that contains the values

        private void SetDefaultCommitChars()
        {
            if (Options.CommitChars == null || string.IsNullOrEmpty(Options.CommitChars))
            {
                Options.CommitChars = IntellisenseOptions.DefaultCommitChars;
            }
        }

        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
            if (Options.CommitChars != "<Empty>")
            {
                SetDefaultCommitChars();
            }
            else
            {
                Options.CommitChars = "";
            }
        }
        public override void SaveSettingsToStorage()
        {
            if (Options.CommitChars != null && Options.CommitChars.Length == 0)
                Options.CommitChars = "<Empty>";
            SetDefaultCommitChars();
            base.SaveSettingsToStorage();
        }

    }
 
}
