using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using XSharpModel;

namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.CompletionOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp", false)]
    class CompletionOptionsPage : DialogPage
    {
        public bool SettingsChanged { get; set; }

        [DefaultValue(4)]
        public int CompleteNumChars{ get; set; }
        [DefaultValue(true)]
        public bool CompleteLocals { get; set; }
        [DefaultValue(true)]
        public bool CompleteSelf { get; set; }
        [DefaultValue(true)]
        public bool CompleteParent { get; set; }
        [DefaultValue(true)]
        public bool CompleteKeywords { get; set; }
        [DefaultValue(true)]
        public bool CompleteSnippets { get; set; }
        [DefaultValue(true)]
        public bool CompleteGlobals{ get; set; }
        [DefaultValue(true)]
        public bool CompleteGlobalsP { get; set; }
        [DefaultValue(true)]
        public bool CompleteGlobalsA { get; set; }
        [DefaultValue(true)]
        public bool CompleteFunctions { get; set; }
        [DefaultValue(true)]
        public bool CompleteFunctionsP { get; set; }
        [DefaultValue(true)]
        public bool CompleteFunctionsA { get; set; }

        protected override IWin32Window Window
        {
            get
            {
                CompletionOptionsControl page = new CompletionOptionsControl();
                page.optionsPage = this;
                page.Initialize();
                return page;
            }
        }


        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
            SettingsChanged = true;
        }
        public override void SaveSettingsToStorage()
        {
            base.SaveSettingsToStorage();
            SettingsChanged = true;
        }
        public CompletionOptionsPage() 
        {

        }

    }
}
