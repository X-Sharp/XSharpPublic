// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
using System.Windows;
using XSharpModel;
using XSharp.Settings;

namespace XSharp.LanguageService.OptionsPages
{
    public partial class FormattingOptionsControl : XSUserControl
    {
        public FormattingOptionsControl()
        {
            InitializeComponent();
        }

        internal override void ReadValues(object options)
        {
            base.ReadValues(options);
            switch (((FormattingOptions)options).KeywordCase)
            {
                case KeywordCase.Upper: rbUpper.IsChecked = true; break;
                case KeywordCase.Lower: rbLower.IsChecked = true; break;
                case KeywordCase.Title: rbTitle.IsChecked = true; break;
                default:               rbNone.IsChecked  = true; break;
            }
            ShowExample();
        }

        internal override void SaveValues(object options)
        {
            base.SaveValues(options);
            var opts = (FormattingOptions)options;
            if      (rbUpper.IsChecked == true) opts.KeywordCase = KeywordCase.Upper;
            else if (rbLower.IsChecked == true) opts.KeywordCase = KeywordCase.Lower;
            else if (rbTitle.IsChecked == true) opts.KeywordCase = KeywordCase.Title;
            else                               opts.KeywordCase = KeywordCase.None;
        }

        private void OnCaseChanged(object sender, RoutedEventArgs e)
        {
            ShowExample();
        }

        private void ShowExample()
        {
            if      (rbUpper.IsChecked == true) tbExample.Text = "FUNCTION";
            else if (rbLower.IsChecked == true) tbExample.Text = "function";
            else if (rbNone.IsChecked  == true) tbExample.Text = "FuNcTiOn";
            else if (rbTitle.IsChecked == true) tbExample.Text = "Function";
        }
    }
}
