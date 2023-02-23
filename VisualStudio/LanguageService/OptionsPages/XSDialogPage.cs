using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Windows.Forms;
#pragma warning disable VSTHRD012 
namespace XSharp.LanguageService.OptionsPages
{
    [ComVisible(true)]
    public class XSDialogPage<T> : DialogPage where T: XSUserControl, new()
    {
        internal XSDialogPage() : base()
        {
           
        }
        protected override void OnActivate(CancelEventArgs e)
        {
            base.OnActivate(e);
            
        }

        protected override void OnApply(PageApplyEventArgs e)
        {
            base.OnApply(e);

        }
        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
            if (control != null)
            {
                control.ReadValues();
            }
        }
        public override void SaveSettingsToStorage()
        {
            initControl();
            control.SaveValues();
            base.SaveSettingsToStorage();
            XSharpLanguagePackage.Instance.optionWasChanged = true;
        }
        public override void ResetSettings()
        {
            base.ResetSettings();
        }
        protected override void SaveSetting(PropertyDescriptor property)
        {
            base.SaveSetting(property);
        }
        protected override void LoadSettingFromStorage(PropertyDescriptor prop)
        {
            base.LoadSettingFromStorage(prop);
        }

        private void initControl()
        {
            if (control == null)
            {
                control = new T
                {
                    optionPage = this
                };
                control.ReadValues();
            }
        }

        XSUserControl control = null;
        protected override IWin32Window Window
        {
            get
            {
                initControl();
                return control;
            }
        }
    }
}
