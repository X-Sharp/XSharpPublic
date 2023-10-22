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
    public partial class MemvarsControl : UserControl
    {
        public MemvarsControl()
        {
            InitializeComponent();
            ResizeColumns();
            this.SizeChanged += LvMemVars_SizeChanged;
            this.IsVisibleChanged += MemvarsControl_IsVisibleChanged;
        }

         private void MemvarsControl_IsVisibleChanged(object sender, DependencyPropertyChangedEventArgs e)
        {
            if (this.Visibility == Visibility.Visible)
            {
                Refresh();
            }
        }

        MemvarsView View => DataContext as MemvarsView;

        private void LvMemVars_SizeChanged(object sender, SizeChangedEventArgs e)
        {
            ResizeColumns();
        }

        void ResizeColumns()
        {
            int lastColumn = (lvMemVars.View as GridView).Columns.Count - 1;
            if (lvMemVars.ActualWidth == Double.NaN)
                lvMemVars.Measure(new Size(Double.PositiveInfinity, Double.PositiveInfinity));
            double remainingSpace = lvMemVars.ActualWidth * 0.8;
            for (int i = 0; i < (lvMemVars.View as GridView).Columns.Count; i++)
            {
                var column = (lvMemVars.View as GridView).Columns[i];
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
            this.SetTheme(lvMemVars, tbNotLoaded);

            if (View.IsRTLoaded)
            {
                lvMemVars.Visibility = Visibility.Visible;
                tbNotLoaded.Visibility = Visibility.Hidden;

                ThreadHelper.JoinableTaskFactory.Run(async delegate
                {

                    var str = await Support.GetMemVarsAsync();
                    var items = MemvarItems.Deserialize(str);
                    View.Items = items.Items.ToList();
                });
            }
            else
            {
                lvMemVars.Visibility = Visibility.Hidden;
                tbNotLoaded.Visibility = Visibility.Visible;

            }
        }

    }
}
