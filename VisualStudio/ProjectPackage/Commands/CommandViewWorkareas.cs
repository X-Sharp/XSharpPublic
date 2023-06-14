using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idDbgWorkareaWindow)]
    internal sealed class CommandViewWorkareas : BaseCommand<CommandViewWorkareas>
    {
        protected override void BeforeQueryStatus(EventArgs e)
        {
            Command.Enabled = XDebuggerSettings.DebuggerMode == DebuggerMode.Break;
            

            base.BeforeQueryStatus(e);
        }
        protected override Task InitializeCompletedAsync()
        {
            Command.Enabled = false;
            Command.Supported = true;
            return base.InitializeCompletedAsync();
        }
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
           
                await XSharp.Debugger.UI.WorkareasWindow.ShowAsync();
            
        }
    }
}
