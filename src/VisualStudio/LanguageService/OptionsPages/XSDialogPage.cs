// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
using XSharpModel;
using Microsoft.VisualStudio.Shell;
using System.Runtime.InteropServices;
using System.Windows;
using XSharp.Settings;
#pragma warning disable VSTHRD012
namespace XSharp.LanguageService.OptionsPages
{
    [ComVisible(true)]
    public class XSDialogPage<T, U> : UIElementDialogPage where T : XSUserControl, new() where U : OptionsBase, new()
    {
        #region Properties
        public override object AutomationObject => Options;
        public U Options { get; private set; } = null;
        #endregion

        internal XSDialogPage()
        {
            Options = new U();
        }

        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
            if (_control != null)
                _control.ReadValues(Options);
        }

        public override void SaveSettingsToStorage()
        {
            EnsureControl();
            if (_control != null)
                _control.SaveValues(Options);
            base.SaveSettingsToStorage();
            Options.WriteToSettings();
        }

        private void EnsureControl()
        {
            if (_control == null)
            {
                _control = new T { optionPage = this };
                _control.ReadValues(Options);
            }
        }

        private T _control;

        internal void SetOptions(U options)
        {
            if (Options == null)
            {
                Options = options;
            }
            else
            {
                foreach (var prop in typeof(U).GetProperties())
                {
                    var value = prop.GetValue(options, null);
                    prop.SetValue(Options, value, null);
                }
            }
        }

        protected override UIElement Child
        {
            get
            {
                EnsureControl();
                return _control;
            }
        }
    }
}
