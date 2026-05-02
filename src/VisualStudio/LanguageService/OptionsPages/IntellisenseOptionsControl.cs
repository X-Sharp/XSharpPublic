// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
using Community.VisualStudio.Toolkit;
using System;
using System.Windows;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.LanguageService.OptionsPages
{
    public partial class IntellisenseOptionsControl : XSUserControl
    {
        private const string DefaultCommitChars = "{}[]().,:;+-*/%&|^!~<>?@#'\"\\";
        private bool _isx86;

        public IntellisenseOptionsControl()
        {
            InitializeComponent();
            commitChars.Text = DefaultCommitChars;

            if (string.IsNullOrEmpty(Environment.GetEnvironmentVariable(Constants.EnvironmentXSharpDev)))
                btnShowMeTheMagic.Visibility = Visibility.Collapsed;

            _isx86 = IntPtr.Size == 4;
            if (_isx86)
            {
                chkUseMicrosoftSQLite.IsChecked = false;
                chkUseMicrosoftSQLite.Visibility = Visibility.Collapsed;
            }
            else if (XSettings.IsArm)
            {
                chkUseMicrosoftSQLite.IsChecked = true;
                chkUseMicrosoftSQLite.Visibility = Visibility.Collapsed;
            }
        }

        internal override void ReadValues(object options)
        {
            base.ReadValues(options);
            if (_isx86)
                chkUseMicrosoftSQLite.IsChecked = false;
            else if (XSettings.IsArm)
                chkUseMicrosoftSQLite.IsChecked = true;
        }

        internal override void SaveValues(object options)
        {
            base.SaveValues(options);
            if (!_isx86 && chkUseMicrosoftSQLite.IsChecked != XSettings.UseMicrosoftSQLite && !XSettings.IsArm)
            {
                VS.MessageBox.ShowWarning("You have changed the setting for the SQLite provider. This change will only take effect after you restart Visual Studio");
                XDatabase.DeleteOnClose = true;
                XSettings.UseMicrosoftSQLite = chkUseMicrosoftSQLite.IsChecked == true;
            }
        }

        private void OnShowMeTheMagic(object sender, RoutedEventArgs e)
        {
            var form = new XSharpSpecialOptions((IntellisenseOptionsPage)optionPage);
            form.ShowDialog();
        }

        private void OnReset(object sender, RoutedEventArgs e)
        {
            commitChars.Text = DefaultCommitChars;
        }
    }
}
