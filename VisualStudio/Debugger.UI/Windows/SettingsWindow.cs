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
    public class SettingsWindow: BaseToolWindow<SettingsWindow>, IDebuggerToolWindow
    {
        public override string GetTitle(int toolWindowId) => "X# Settings";

        public override Type PaneType => typeof(Pane);
        public SettingsControl Control = null;
        

        public override async Task<FrameworkElement> CreateAsync(int toolWindowId, CancellationToken cancellationToken)
        {
            Support.RegisterWindow(this);
            Version _ = await VS.Shell.GetVsVersionAsync();
            Control = new SettingsControl() { DataContext = new SettingsView() };
            Control.Refresh();
            return Control;
        }

        public void Refresh() => Control.Refresh();
        public void Clear() => Control.Clear();


        [Guid(XSharpConstants.DebuggerSettingsPane)]
        internal class Pane : ToolkitToolWindowPane
        {
            public Pane()
            {
                BitmapImageMoniker = KnownMonikers.Settings;
            }
            public override void OnToolWindowCreated()
            {
                base.OnToolWindowCreated();
                Support.RefreshWindows();
            }
        }

    }
}
