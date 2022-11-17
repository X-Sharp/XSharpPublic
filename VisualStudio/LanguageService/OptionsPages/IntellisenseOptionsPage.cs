using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;

namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.IntellisenseOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp",false)]
    [ComVisible(true)]
    public class IntellisenseOptionsPage : XSDialogPage<IntellisenseOptionsControl>
    {
        public bool CompletionListTabs { get; set; }
        public String CommitChars { get; set; }
        public bool ExcludeMembersFromOtherFiles { get; set; }
        public bool KeywordsInAll { get; set; }

        public bool ShowAfterChar { get; set; }

        public XSharpModel.KeywordCase KeywordCase { get; set; }
        public bool IncludeFieldsInNavigationBars { get; set; }
        public bool SortNavigationBars { get; set; }
        public bool ShowMembersOfCurrentTypeOnly { get; set; }
        public bool DisableSyntaxColorization { get; set; }
        public bool DisableEntityParsing { get; set; }
        public bool DisableEditorDropdowns { get; set; }
        public bool DisableClassViewObjectView { get; set; }
        public bool DisableAssemblyReferences { get; set; }
        public bool DisableForeignProjectReferences { get; set; }
        public bool DisableXSharpProjectReferences { get; set; }
        public bool EnableOutputPane { get; set; }
        public bool EnableDatabaseLog { get; set; }
        public bool EnableParserLog { get; set; }
        public bool EnableCodeCompletionLog { get; set; }
        public bool EnableParameterLog{ get; set; }
        public bool EnableBraceMatchLog{ get; set; }
        public bool EnableQuickInfoLog { get; set; }
        public bool EnableTypelookupLog { get; set; }
        public bool EnableReferenceInfoLog { get; set; }

        public bool HideAdvancemembers { get; set; } // not on control. Is already on another page.
        
        private void SetDefaultCommitChars()
        {
            if (this.CommitChars == null || string.IsNullOrEmpty(this.CommitChars) )
            {
                this.CommitChars = "{}[]();+-*/%&|^!~=<>?@#\'\"\\";
            }
        }

        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
            if (this.CommitChars != "<Empty>")
            {
                SetDefaultCommitChars();
            }
            else
            {
                this.CommitChars = "";
            }
        }
        public override void SaveSettingsToStorage()
        {
            if (this.CommitChars != null && this.CommitChars.Length == 0)
                this.CommitChars = "<Empty>";
            SetDefaultCommitChars();
            base.SaveSettingsToStorage();
        }
        public IntellisenseOptionsPage() 
        {

        }

    }
}
