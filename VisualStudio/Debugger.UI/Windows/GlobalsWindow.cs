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
    public class GlobalsWindow : BaseToolWindow<GlobalsWindow>, IDebuggerToolWindow
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

        public void Refresh() => Control.Refresh();
        public void Clear() => Control.Clear();

        [Guid(XSharpConstants.DebuggerGlobalsPane)]
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
        }

    }
}
