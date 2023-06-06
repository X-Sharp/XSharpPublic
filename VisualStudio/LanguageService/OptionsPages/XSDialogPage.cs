using Microsoft.VisualStudio.Shell;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Windows.Forms;
#pragma warning disable VSTHRD012 
namespace XSharp.LanguageService.OptionsPages
{
    [ComVisible(true)]
    public class XSDialogPage<T, U> : DialogPage where T: XSUserControl, new() where U : OptionsBase, new()
    {
        internal XSDialogPage() : base(ThreadHelper.JoinableTaskContext)
        {
            Options = new U();
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
                control.ReadValues(Options);
            }
        }
        public override void SaveSettingsToStorage()
        {
            initControl();
            control.SaveValues(Options);
            SetOptions(Options);
            Options.WriteToSettings();
        }
        public override void ResetSettings()
        {
            base.ResetSettings();
        }
        protected override void SaveSetting(PropertyDescriptor property)
        {
            // We no longer save here. The controls save directly to the options object
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
                control.ReadValues(Options);
            }
        }

        XSUserControl control = null;
        public U Options { get; private set; }
        /// <summary>
        /// Set the properties of the page from the Options object
        /// </summary>
        /// <param name="options"></param>
        public void SetOptions(U options)
        {
            Options = options;
            foreach (var prop in this.GetType().GetProperties())
            {
                var name = prop.Name;
                try
                {
                    var optprop = options.GetType().GetProperty(name);
                    prop.SetValue(this, optprop.GetValue(options));
                }
                catch
                {

                }
            }
        }
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
