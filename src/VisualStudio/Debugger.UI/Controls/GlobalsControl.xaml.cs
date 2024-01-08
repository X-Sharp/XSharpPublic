//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Debugger.Support;
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;

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
            this.SizeChanged += lvGlobals_SizeChanged;
            this.IsVisibleChanged += GlobalsControl_IsVisibleChanged;
        }

 
        private void GlobalsControl_IsVisibleChanged(object sender, DependencyPropertyChangedEventArgs e)
        {
            if (this.Visibility == Visibility.Visible)
            {
                Refresh();
            }
        }

        private void lvGlobals_SizeChanged(object sender, SizeChangedEventArgs e)
        {
            ResizeColumns();
        }
        GlobalsView View => DataContext as GlobalsView;
        void ResizeColumns()
        {
            int lastColumn = lvGlobals.Columns.Count - 1;
            if (lvGlobals.ActualWidth == Double.NaN)
                lvGlobals.Measure(new Size(Double.PositiveInfinity, Double.PositiveInfinity));
            double remainingSpace = lvGlobals.ActualWidth * 0.9;
            for (int i = 0; i < lvGlobals.Columns.Count; i++)
            {
                var column = lvGlobals.Columns[i];
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
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                if (View.IsRTLoaded)
                {
                    lvGlobals.Visibility = Visibility.Visible;
                    tbNotLoaded.Visibility = Visibility.Hidden;
                    var str = await Support.GetGlobalsAsync();
                    var items = GlobalItems.Deserialize(str);
                    View.Items = items.Items;
                    var assemblies = new List<String>();
                    assemblies.Add(GlobalsView.AllAssemblies);
                    foreach (var item in items.Items)
                    {
                        if (!assemblies.Contains(item.Assembly))
                            assemblies.Add(item.Assembly);
                    }
                    View.Assemblies = assemblies;
                    View.Items = items.Items.ToList();
                    if (View.SelectedAssembly == null ||
                        !assemblies.Contains(View.SelectedAssembly))
                        View.SelectedAssembly = assemblies.FirstOrDefault();
                }
                else
                {
                    lvGlobals.Visibility = Visibility.Hidden ;
                    tbNotLoaded.Visibility = Visibility.Visible;
                }
            }

            );
        }

    }
}
