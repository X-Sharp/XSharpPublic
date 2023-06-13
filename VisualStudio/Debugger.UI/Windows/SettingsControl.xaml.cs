using Debugger.Support;
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using XSharp.Debugger.Support;

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
            lvSettings.SizeChanged += lvSettings_SizeChanged;
        }

        private void lvSettings_SizeChanged(object sender, SizeChangedEventArgs e)
        {
            ResizeColumns();
        }

        void ResizeColumns()
        {
            int autoFillColumnIndex = (lvSettings.View as GridView).Columns.Count - 1;
            if (lvSettings.ActualWidth == Double.NaN)
                lvSettings.Measure(new Size(Double.PositiveInfinity, Double.PositiveInfinity));
            double remainingSpace = lvSettings.ActualWidth;
            for (int i = 0; i < (lvSettings.View as GridView).Columns.Count; i++)
            {
                var column = (lvSettings.View as GridView).Columns[i];
                if (i != autoFillColumnIndex)
                    column.Width = lvSettings.ActualWidth / 3;
                else
                    column.Width = 2 * lvSettings.ActualWidth / 3;

            }

        }
        internal void Clear()
        {
            this.lvSettings.ItemsSource = null;
        }
        internal void Refresh()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var str = await Support.ExecExpressionAsync("XSharp.Debugger.Support.RtLink.GetSettings()");
                if (str != null && str.IndexOf(RtLink.ErrorPrefix) == -1)
                {
                    if (str.StartsWith("\"") && str.EndsWith("\"") )
                    {
                        str = str.Substring(1, str.Length - 2);
                    }
                    var items = SettingItems.Deserialize(str);
                    this.lvSettings.ItemsSource = items.Items;
                }
            }
            );
        }
        
    }
}
