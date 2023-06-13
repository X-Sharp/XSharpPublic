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

namespace XSharp.Debugger.UI
{
    public class SettingsWindow: BaseToolWindow<SettingsWindow>
    {
        public override string GetTitle(int toolWindowId) => "Settings";

        public override Type PaneType => typeof(Pane);
        public SettingsControl Control = null;


        public override async Task<FrameworkElement> CreateAsync(int toolWindowId, CancellationToken cancellationToken)
        {
            Support.RegisterWindow(this);
            Version version = await VS.Shell.GetVsVersionAsync();
            Control = new SettingsControl();
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


        [Guid("F7ED7826-137A-462D-8757-37A02BEF4DCF")]
        internal class Pane : ToolkitToolWindowPane
        {
            public Pane()
            {
                BitmapImageMoniker = KnownMonikers.Settings;
            }
        }

    }
}
