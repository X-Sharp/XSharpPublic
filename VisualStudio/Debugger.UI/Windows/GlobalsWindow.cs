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
    public class GlobalsWindow : BaseToolWindow<GlobalsWindow>
    {
        public override string GetTitle(int toolWindowId) => "X# Globals";

        public override Type PaneType => typeof(Pane);
        public GlobalsControl Control = null;


        public override async Task<FrameworkElement> CreateAsync(int toolWindowId, CancellationToken cancellationToken)
        {
            Support.RegisterWindow(this);
            Version _ = await VS.Shell.GetVsVersionAsync();
            Control = new GlobalsControl { DataContext = new GlobalsView() };
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


        [Guid("53B7968B-251B-44E0-BDF5-A225BF0DBC77")]
        internal class Pane : ToolkitToolWindowPane
        {
            public Pane()
            {
                BitmapImageMoniker = KnownMonikers.AutosWindow;
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
