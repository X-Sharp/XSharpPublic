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
using System.Windows.Media;
using XSharp.Debugger.Support;
using XSharp.Settings;

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
                ShowDetails();
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
            if (View.IsRTLoaded)
            {
                lvAreas.Visibility = Visibility.Visible;
                lvInfo.Visibility = Visibility.Visible;
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
                if (View.SelectedRDD == null || !rdds.Contains(View.SelectedRDD))
                    View.SelectedRDD = rdds.FirstOrDefault();
                if (lvAreas.SelectedIndex == -1)
                    lvAreas.SelectedIndex = 0;
            }

            );
            }
            else
            {
                lvAreas.Visibility = Visibility.Hidden;
                lvInfo.Visibility = Visibility.Hidden;
                tbNotLoaded.Visibility = Visibility.Visible;

            }
        }

        private NameValueItems GetStatus(int area)
        {
            return ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var str = await Support.GetAreaInfoAsync(area);
                var status = NameValueItems.Deserialize(str);
                return status;
            });
        }


        private NameValueItems GetFields(int area)
        {
            return ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var str = await Support.GetFieldValuesAsync((int)area);
                var fields = NameValueItems.Deserialize(str);
                return fields;
            });
        }

        private void ShowDetails()
        {
            if (XDebuggerSettings.DebuggerMode == DebuggerMode.Break)
            {
                WorkareaItem item = this.lvAreas.SelectedItem as WorkareaItem;
                if (item != null)
                {
                    var areaNum = item.Area;
                    View.Alias = item.Alias;
                    View.Status = GetStatus(areaNum).Items;
                    View.Fields = GetFields(areaNum).Items;
                }
                else if (this.lvAreas.Items.Count > 0)
                {
                    this.lvAreas.SelectedIndex = 0;
                }
            }
            else
            {
                WorkareaItem item = this.lvAreas.SelectedItem as WorkareaItem;
                if (item != null)
                {
                    View.Alias = item.Alias;
                }
                if (stubs == null)
                {
                    var items = new NameValueItems();
                    var nvitem = new NameValueItem
                    {
                        Name = "",
                        Value = "Debugger is not in break mode"
                    };
                    items.Add(nvitem);
                    stubs = items;
                }
                View.Status = stubs.Items; ;
                View.Fields = stubs.Items;
            }
        }

        NameValueItems stubs = null;

        private void lvAreas_SelectionChanged(object sender, System.Windows.Controls.SelectionChangedEventArgs e)
        {
            ShowDetails();
        }
    }
}
