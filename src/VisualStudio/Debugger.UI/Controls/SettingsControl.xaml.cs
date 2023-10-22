//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Debugger.Support;
using Microsoft.VisualStudio.Shell;
using System;
using System.Linq;
using System.Windows;
using System.Windows.Controls;

namespace XSharp.Debugger.UI
{
    /// <summary>
    /// Interaction logic for dbgShowGlobalsControl.xaml
    /// </summary>
    public partial class SettingsControl : UserControl
    {
        public SettingsControl()
        {
            InitializeComponent();
            ResizeColumns();
            this.SizeChanged += lvSettings_SizeChanged;
            this.IsVisibleChanged += LvSettings_IsVisibleChanged;
        }


        private void LvSettings_IsVisibleChanged(object sender, DependencyPropertyChangedEventArgs e)
        {
            if (this.Visibility == Visibility.Visible)
            {
                Refresh();
            }
        }

        SettingsView View => DataContext as SettingsView;
        private void lvSettings_SizeChanged(object sender, SizeChangedEventArgs e)
        {
            ResizeColumns();
        }

        void ResizeColumns()
        {
            int lastColumn = (lvSettings.View as GridView).Columns.Count - 1;
            if (lvSettings.ActualWidth == Double.NaN)
                lvSettings.Measure(new Size(Double.PositiveInfinity, Double.PositiveInfinity));
            double remainingSpace = lvSettings.ActualWidth * 0.8;
            for (int i = 0; i < (lvSettings.View as GridView).Columns.Count; i++)
            {
                var column = (lvSettings.View as GridView).Columns[i];
                if (i != lastColumn)
                    column.Width = remainingSpace / 4;
                else
                    column.Width = remainingSpace / 2;

            }

        }
        internal void Clear()
        {
            if (View.Items != null)
                View.Items.Clear();

        }
        internal void Refresh()
        {
            if (this.Visibility != Visibility.Visible)
            {
                return;
            }
            this.SetTheme(lvSettings, tbNotLoaded);
            if (View.IsRTLoaded)
            {
                lvSettings.Visibility = Visibility.Visible;
                tbNotLoaded.Visibility = Visibility.Hidden;
                ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var str = await Support.GetSettingsAsync();
                var items = SettingItems.Deserialize(str);
                View.Items = items.Items.ToList();
            }
            );
            }
            else
            {
                lvSettings.Visibility = Visibility.Hidden;
                tbNotLoaded.Visibility = Visibility.Visible;

            }
        }
        
    }
}
