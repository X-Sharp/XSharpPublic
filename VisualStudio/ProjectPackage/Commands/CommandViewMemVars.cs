using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idDbgMemvarsWindow)]
    internal sealed class CommandViewMemvars : BaseCommand<CommandViewMemvars>
    {
        protected override void BeforeQueryStatus(EventArgs e)
        {
            Command.Enabled = XSettings.DebuggerMode == DebuggerMode.Break;
            base.BeforeQueryStatus(e);
        }

        protected override Task InitializeCompletedAsync()
        {
            Command.Supported = false;
            return base.InitializeCompletedAsync();
        }
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await VS.MessageBox.ShowAsync("Not ready yet");
        }
    }
}
