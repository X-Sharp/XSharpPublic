using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.ComponentModel.Design;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;

namespace XSharp.Project.DebugWindows
{
    public class ShowMemvarsWindow : BaseToolWindow<ShowMemvarsWindow>
    {
        public override string GetTitle(int toolWindowId) => "Dynamic Memory Variables";

        public override Type PaneType => typeof(Pane);
        public ShowMemvarsControl Control = null;


        public override async Task<FrameworkElement> CreateAsync(int toolWindowId, CancellationToken cancellationToken)
        {
            Support.RegisterWindow(this);
            Version version = await VS.Shell.GetVsVersionAsync();
            Control = new ShowMemvarsControl();
            return Control;
        }

        internal void Refresh()
        {
            Control.Refresh();
        }


        [Guid("7C2FC14E-4BBE-4B95-B0CB-B3B7E0658A23")]
        internal class Pane : ToolkitToolWindowPane
        {
            public Pane()
            {
                BitmapImageMoniker = KnownMonikers.LocalsWindow;
                //ToolBar = new CommandID(PackageGuids.guidProjectPackage, PackageIds.idDbgGlobalsWindow);
            }
        }

    }
}
