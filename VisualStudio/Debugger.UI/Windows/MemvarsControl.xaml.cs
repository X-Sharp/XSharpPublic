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
    public partial class MemvarsControl : UserControl
    {
        public MemvarsControl()
        {
            InitializeComponent();
            ResizeColumns();
            lvMemVars.SizeChanged += LvMemVars_SizeChanged;
        }

        private void LvMemVars_SizeChanged(object sender, SizeChangedEventArgs e)
        {
            ResizeColumns();
        }

        void ResizeColumns()
        {
            int autoFillColumnIndex = (lvMemVars.View as GridView).Columns.Count - 1;
            if (lvMemVars.ActualWidth == Double.NaN)
                lvMemVars.Measure(new Size(Double.PositiveInfinity, Double.PositiveInfinity));
            double remainingSpace = lvMemVars.ActualWidth;
            for (int i = 0; i < (lvMemVars.View as GridView).Columns.Count; i++)
            {
                var column = (lvMemVars.View as GridView).Columns[i];
                if (i != autoFillColumnIndex)
                    column.Width = lvMemVars.ActualWidth / 4;
                else
                    column.Width = lvMemVars.ActualWidth / 2;

            }

        }
        internal void Clear()
        {
            this.lvMemVars.ItemsSource = null;
        }

        internal void Refresh()
        {


            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var str = await Support.ExecExpressionAsync("XSharp.Debugger.Support.RtLink.GetMemVars()");
                if (str != null && str.IndexOf(RtLink.ErrorPrefix) == -1)
                {
                    if (str.StartsWith("\"") && str.EndsWith("\"") )
                    {
                        str = str.Substring(1, str.Length - 2);
                    }
                    var items = MemvarItems.Deserialize(str);
                    this.lvMemVars.ItemsSource = items.Items;
                }
            }

            );
        }
       
    }
}
