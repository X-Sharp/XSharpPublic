//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Imaging;
using System;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;

namespace XSharp.Debugger.UI
{
    public class WorkareasWindow: BaseToolWindow<WorkareasWindow>
    {
        public override string GetTitle(int toolWindowId) => "X# Workareas";

        public override Type PaneType => typeof(Pane);
        public WorkareasControl Control = null;


        public override async Task<FrameworkElement> CreateAsync(int toolWindowId, CancellationToken cancellationToken)
        {
            Support.RegisterWindow(this);
            Version _ = await VS.Shell.GetVsVersionAsync();
            Control = new WorkareasControl { DataContext = new WorkareasView() };
            Control.Refresh();
            return Control;
        }


        internal void Refresh()
        {
            Control.Refresh();
        }
        internal void Clear()
        {
            Control.Clear();
        }


        [Guid("B5B41BAB-62F9-48E0-80D8-947F2F14D1C5")]
        internal class Pane : ToolkitToolWindowPane
        {
            public Pane()
            {
                BitmapImageMoniker = KnownMonikers.DatabaseGroup;
            }
            public override void OnToolWindowCreated()
            {
                base.OnToolWindowCreated();
                Support.RefreshWindows();
            }
            protected override void OnCreate()
            {
                base.OnCreate();
                Support.RefreshWindows();
            }

        }

    }
}
