using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace XSharp.Project.OptionsPages
{

    delegate string caseSync(string original);

    [Guid(XSharpConstants.IntellisenseOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp",false)]
    class IntellisenseOptionsPage : DialogPage
    {
        public bool CompletionListTabs { get; set; }
        public String CommitChars { get; set; }
        public bool AutoPairs { get; set; }

        public bool KeywordsInAll { get; set; }

        public bool UseDotAsUniversalSelector { get; set; }

        public bool ShowAfterChar { get; set; }

        // 0 : none; 1 : UPPER; 2 : lower; 3 : TitleCase
        public KeywordCase KeywordCase { get; set; }
        public bool IdentifierCase => false;
        public bool AlignDoCase { get; set; }
        public bool AlignMethod { get; set; }
        public int MultiFactor { get; set; }
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
            SetDefaultCommitChars();
            SetCaseSync();
        }
        public override void SaveSettingsToStorage()
        {
            SetDefaultCommitChars();
            base.SaveSettingsToStorage();
            SetCaseSync();
        }
        private caseSync CaseSync;
        public IntellisenseOptionsPage() : base()
        {
            CaseSync = (x) => x;

        }


        void SetCaseSync()
        {
            switch (KeywordCase)
            {
                case KeywordCase.Upper:
                    CaseSync = (x) => x.ToUpper();
                    break;
                case KeywordCase.Lower:
                    CaseSync = (x) => x.ToLower();
                    break;
                case KeywordCase.Title:
                    CaseSync = (x) => x.Length > 1 ? x.Substring(0, 1).ToUpper() + x.Substring(1).ToLower() : x.ToUpper();
                    break;
                case KeywordCase.None:
                    CaseSync = (x) => x;
                    break;
            }
            try
            {
                var key = Microsoft.Win32.Registry.CurrentUser;
                var subkey = key.OpenSubKey(Constants.RegistryKey, true);
                if (subkey == null)
                {
                    subkey = key.CreateSubKey(Constants.RegistryKey);
                }
                var kwcase = subkey.GetValue("KeywordCase");
                if (kwcase == null)
                {
                    subkey.SetValue("KeywordCase", 1);
                }
                subkey.SetValue("KeywordCase", (int)KeywordCase);
            }
            catch
            {
                ;
            }


        }

        public string SyncKeyword(string original)
        {
            if (string.IsNullOrEmpty(original))
                return original;
            return CaseSync(original);
        }
    }
}
