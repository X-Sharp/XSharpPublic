using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using XSharpModel;

namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.IntellisenseOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp",false)]
    class IntellisenseOptionsPage : DialogPage
    {
        public bool SettingsChanged { get; set; }
        public bool CompletionListTabs { get; set; }
        public String CommitChars { get; set; }
        public bool AutoPairs { get; set; }
        public bool ExcludeMembersFromOtherFiles { get; set; }
        public bool KeywordsInAll { get; set; }

        public bool UseDotAsUniversalSelector { get; set; }

        public bool ShowAfterChar { get; set; }

        public XSharpModel.KeywordCase KeywordCase { get; set; }
        public bool IncludeFieldsInNavigationBars { get; set; }
        public bool SortNavigationBars { get; set; }
        public bool ShowMembersOfCurrentTypeOnly { get; set; }
        public bool DisableBraceMatching { get; set; }
        public bool DisableLightBulb { get; set; }
        public bool DisableHighLightWord { get; set; }
        public bool DisableParameterInfo { get; set; }
        public bool DisablePeekDefinition { get; set; }
        public bool DisableQuickInfo { get; set; }
        public bool DisableSyntaxColorization { get; set; }
        public bool DisableEntityParsing { get; set; }
        public bool DisableRegions { get; set; }
        public bool DisableCaseSynchronization { get; set; }
        public bool DisableEditorDropdowns { get; set; }
        public bool DisableClassViewObjectView { get; set; }
        public bool DisableCodeCompletion { get; set; }
        public bool DisableGotoDefinition { get; set; }
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
        protected override IWin32Window Window
        {
            get
            {
                IntellisenseOptionsControl page = new IntellisenseOptionsControl();
                page.optionsPage = this;
                page.Initialize();
                return page;
            }
        }

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
            SettingsChanged = true;
        }
        public override void SaveSettingsToStorage()
        {
            if (this.CommitChars != null && this.CommitChars.Length == 0)
                this.CommitChars = "<Empty>";
            SetDefaultCommitChars();
            base.SaveSettingsToStorage();
            SettingsChanged = true;
        }
        public IntellisenseOptionsPage() 
        {

        }

    }
}
