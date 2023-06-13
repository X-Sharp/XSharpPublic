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
        public override string GetTitle(int toolWindowId) => "Global Variables";

        public override Type PaneType => typeof(Pane);
        public GlobalsControl Control = null;


        public override async Task<FrameworkElement> CreateAsync(int toolWindowId, CancellationToken cancellationToken)
        {
            Support.RegisterWindow(this);
            Version version = await VS.Shell.GetVsVersionAsync();
            Control = new GlobalsControl();
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


        [Guid("53B7968B-251B-44E0-BDF5-A225BF0DBC77")]
        internal class Pane : ToolkitToolWindowPane
        {
            public Pane()
            {
                BitmapImageMoniker = KnownMonikers.AutosWindow;
            }
        }

    }
}
