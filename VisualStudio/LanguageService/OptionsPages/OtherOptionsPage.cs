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

    [Guid(XSharpConstants.OtherOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp",false)]
    class OtherOptionsPage : DialogPage
    {
        public bool SettingsChanged { get; set; }
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
        protected override IWin32Window Window
        {
            get
            {
                OtherOptionsControl page = new OtherOptionsControl();
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
        public OtherOptionsPage() 
        {

        }

    }
}
