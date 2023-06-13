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
    public partial class GlobalsControl : UserControl
    {
        public GlobalsControl()
        {
            InitializeComponent();
            ResizeColumns();
            lvGlobals.SizeChanged += lvGlobals_SizeChanged;
        }

        private void lvGlobals_SizeChanged(object sender, SizeChangedEventArgs e)
        {
            ResizeColumns();
        }

        void ResizeColumns()
        {
            int autoFillColumnIndex = (lvGlobals.View as GridView).Columns.Count - 1;
            if (lvGlobals.ActualWidth == Double.NaN)
                lvGlobals.Measure(new Size(Double.PositiveInfinity, Double.PositiveInfinity));
            double remainingSpace = lvGlobals.ActualWidth;
            for (int i = 0; i < (lvGlobals.View as GridView).Columns.Count; i++)
            {
                var column = (lvGlobals.View as GridView).Columns[i];
                if (i != autoFillColumnIndex)
                    column.Width = lvGlobals.ActualWidth / 4;
                else
                    column.Width = lvGlobals.ActualWidth / 2;

            }

        }
        internal void Clear()
        {
            this.lvGlobals.ItemsSource = null;
        }
        internal void Refresh()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var str = await Support.ExecExpressionAsync("XSharp.Debugger.Support.RtLink.GetGlobals()");
                if (str != null && str.IndexOf(RtLink.ErrorPrefix) == -1)
                {
                    if (str.StartsWith("\"") && str.EndsWith("\""))
                    {
                        str = str.Substring(1, str.Length - 2);
                    }
                    var items = GlobalItems.Deserialize(str);
                    this.lvGlobals.ItemsSource = items.Items;
                }
            }

            );
        }
       
    }
}
