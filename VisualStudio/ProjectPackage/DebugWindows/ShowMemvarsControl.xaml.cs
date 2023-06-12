using Microsoft.VisualStudio.Shell;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using static XSharp.Project.DebugWindows.ShowMemvarsControl;

namespace XSharp.Project.DebugWindows
{
    /// <summary>
    /// Interaction logic for dbgShowGlobalsControl.xaml
    /// </summary>
    public partial class ShowMemvarsControl : UserControl
    {
        public ShowMemvarsControl()
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

        internal void Refresh()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var memvarItems = new List<MemvarItem>();
                var memvarName = string.Empty;
                memvarName = await Support.ExecExpressionAsync("XSharp.Memvar.DbgPublicsFirst()");
                while (memvarName != "null")
                {
                    var value = await Support.ExecExpressionAsync("XSharp.Memvar.DbgGetVar("+memvarName+")");
                    var item = new MemvarItem() {  Name = memvarName.Substring(1, memvarName.Length-2), Type = "Public", Value = value };
                    memvarItems.Add(item);
                    memvarName = await Support.ExecExpressionAsync("XSharp.Memvar.DbgPublicsNext()");
                }
                memvarName = await Support.ExecExpressionAsync("XSharp.Memvar.DbgPrivatesFirst()");
                while (memvarName != "null")
                {
                    var value = await Support.ExecExpressionAsync("XSharp.Memvar.DbgGetVar(" + memvarName + ")");
                    var item = new MemvarItem() { Name = memvarName.Substring(1, memvarName.Length - 2), Type = "Private", Value = value };
                    memvarItems.Add(item);
                    memvarName = await Support.ExecExpressionAsync("XSharp.Memvar.DbgPrivatesNext()");
                }
                this.lvMemVars.ItemsSource = memvarItems;
            }

            );
        }
        public sealed class MemvarItem
        {
            public string Name { get; set; }
            public string Type { get; set; }
            public string Value { get; set; }
        }
    }
}
