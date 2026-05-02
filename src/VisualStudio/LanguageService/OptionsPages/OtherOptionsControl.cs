// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
using System.Windows;

namespace XSharp.LanguageService.OptionsPages
{
    public partial class OtherOptionsControl : XSUserControl
    {
        public OtherOptionsControl()
        {
            InitializeComponent();
        }

        internal override void ReadValues(object options)
        {
            base.ReadValues(options);
            chkSingleLineDividers.IsEnabled = chkShowDividers.IsChecked == true;
        }

        private void OnShowDividersChanged(object sender, RoutedEventArgs e)
        {
            if (chkSingleLineDividers != null)
            {
                chkSingleLineDividers.IsEnabled = chkShowDividers.IsChecked == true;
                if (!chkSingleLineDividers.IsEnabled)
                    chkSingleLineDividers.IsChecked = false;
            }
        }
    }
}
