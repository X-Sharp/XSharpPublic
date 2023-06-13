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
        public override string GetTitle(int toolWindowId) => "Dynamic Memory Variables";

        public override Type PaneType => typeof(Pane);
        public MemvarsControl Control = null;


        public override async Task<FrameworkElement> CreateAsync(int toolWindowId, CancellationToken cancellationToken)
        {
            Support.RegisterWindow(this);
            Version version = await VS.Shell.GetVsVersionAsync();
            Control = new MemvarsControl();
            Control.Initialized += Control_Initialized;
            return Control;
        }

        private void Control_Initialized(object sender, EventArgs e)
        {
            this.Refresh();
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
        }

    }
}
