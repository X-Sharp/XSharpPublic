//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Windows;
using System.Windows.Controls;
using Microsoft.VisualStudio.PlatformUI;

namespace XSharp.ProjectSystem.Properties
{
    /// <summary>
    /// Modal dialog that manages the <c>&lt;Using&gt;</c> items of an X# project.
    /// </summary>
    public partial class GlobalUsingsDialog : DialogWindow
    {
        public GlobalUsingsDialog()
        {
            InitializeComponent();
        }

        private GlobalUsingsDialogViewModel ViewModel => DataContext as GlobalUsingsDialogViewModel;

        private void OnAddUsingClick(object sender, RoutedEventArgs e)
        {
            ViewModel?.AddUsing();
        }

        private void OnDeleteUsingClick(object sender, RoutedEventArgs e)
        {
            if (sender is Button button && button.Tag is GlobalUsingItem item)
                ViewModel?.DeleteUsing(item);
        }

        private void OnOkClick(object sender, RoutedEventArgs e)
        {
            DialogResult = true;
            Close();
        }
    }
}
