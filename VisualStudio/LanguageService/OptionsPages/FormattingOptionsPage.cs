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

    delegate string caseSync(string original);

    [SharedSettings("TextEditor.XSharp",false)]
    [Guid(XSharpConstants.FormattingOptionsPageGuidString)]
    class FormattingOptionsPage : DialogPage
    {
        public bool SettingsChanged { get; set; }
        // 0 : none; 1 : UPPER; 2 : lower; 3 : TitleCase
        public XSharpModel.KeywordCase KeywordCase { get; set; }
        public bool IdentifierCase => false;
        public bool UdcCase  { get; set; }
        public bool AlignDoCase { get; set; }
        public bool AlignMethod { get; set; }
        public bool TrimTrailingWhiteSpace { get; set; }
        public bool InsertFinalNewLine { get; set; }
        public int MultiFactor { get; set; }
 
        protected override IWin32Window Window
        {
            get
            {
                FormattingOptionsControl page = new FormattingOptionsControl
                {
                    formattingPage = this
                };
                page.Initialize();
                return page;
            }
        }


        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
            SetCaseSync();
            SettingsChanged = true;
        }
        public override void SaveSettingsToStorage()
        {
            base.SaveSettingsToStorage();
            SetCaseSync();
            SettingsChanged = true;
        }
        private caseSync CaseSync;
        public FormattingOptionsPage() 
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
            if (string.IsNullOrEmpty(original) || string.Equals(original, "value", StringComparison.OrdinalIgnoreCase))
                return original;
            return CaseSync(original);
        }
    }
}
