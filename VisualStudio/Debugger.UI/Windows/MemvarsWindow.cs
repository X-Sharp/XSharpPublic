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
    public class MemvarsWindow : BaseToolWindow<MemvarsWindow>
    {
        public override string GetTitle(int toolWindowId) => "X# Public and Privates";

        public override Type PaneType => typeof(Pane);
        public MemvarsControl Control = null;


        public override async Task<FrameworkElement> CreateAsync(int toolWindowId, CancellationToken cancellationToken)
        {
            Support.RegisterWindow(this);
            Version _ = await VS.Shell.GetVsVersionAsync();
            Control = new MemvarsControl { DataContext = new MemvarsView() };
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


        [Guid("7C2FC14E-4BBE-4B95-B0CB-B3B7E0658A23")]
        internal class Pane : ToolkitToolWindowPane
        {
            public Pane()
            {
                BitmapImageMoniker = KnownMonikers.LocalsWindow;
                //ToolBar = new CommandID(PackageGuids.guidProjectPackage, PackageIds.idDbgGlobalsWindow);
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
