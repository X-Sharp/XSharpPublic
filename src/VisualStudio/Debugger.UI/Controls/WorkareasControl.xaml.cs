//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Debugger.Support;
using Microsoft.VisualStudio.Shell;
using System.Collections.Generic;
using System.Linq;
using System;
using System.Windows.Controls;
using System.Windows;

namespace XSharp.Debugger.UI
{
    /// <summary>
    /// Interaction logic for WorkareasControl.xaml
    /// </summary>
    public partial class WorkareasControl : UserControl
    {
        public WorkareasControl()
        {
            InitializeComponent();
            this.IsVisibleChanged += WorkareasControl_IsVisibleChanged;
        }

        private void WorkareasControl_IsVisibleChanged(object sender, DependencyPropertyChangedEventArgs e)
        {
            if (this.Visibility == Visibility.Visible)
            {
                Refresh();
            }
        }

        internal void Clear()
        {
            if (View.Items != null)
                View.Items.Clear();

        }
        WorkareasView View => DataContext as WorkareasView;
        internal void Refresh()
        {
            if (this.Visibility != Visibility.Visible)
            {
                return;
            }
            this.SetTheme(lvAreas, tbNotLoaded);
            if (View.IsRTLoaded)
            {
                lvAreas.Visibility = Visibility.Visible;
                tbNotLoaded.Visibility = Visibility.Hidden;
                ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var str = await Support.GetWorkareasAsync();
                var items = WorkareaItems.Deserialize(str);
                View.Items = items.Items;
                var rdds = new List<String>();
                rdds.Add(WorkareasView.AllRDDs);
                foreach (var item in items.Items)
                {
                    if (!rdds.Contains(item.RDD))
                        rdds.Add(item.RDD);
                }
                View.RDDs = rdds;
                View.Items = items.Items.ToList();
                if (View.SelectedRDD == null ||
                    !rdds.Contains(View.SelectedRDD))
                    View.SelectedRDD = rdds.FirstOrDefault();
            }

            );
            }
            else
            {
                lvAreas.Visibility = Visibility.Hidden;
                tbNotLoaded.Visibility = Visibility.Visible;

            }
        }
    }
}
