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
using XSharp.Settings;

namespace XSharp.Debugger.UI
{
    public class WorkareasWindow: BaseToolWindow<WorkareasWindow>, IDebuggerToolWindow
    {
        public override string GetTitle(int toolWindowId) => "X# Workareas";

        public override Type PaneType => typeof(Pane);
        public WorkareasControl Control = null;

        public override async Task<FrameworkElement> CreateAsync(int toolWindowId, CancellationToken cancellationToken)
        {
            if (XSettings.IsVs15)
            {
                return null;
            }

            Support.RegisterWindow(this);
            Version _ = await VS.Shell.GetVsVersionAsync();
            Control = new WorkareasControl { DataContext = new WorkareasView() };
            Control.Refresh();
            return Control;
        }

        public void Refresh() => Control.Refresh();
        public void Clear() => Control.Clear();


        [Guid(XSharpConstants.DebuggerWorkareasPane)]
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

        }

    }
}
