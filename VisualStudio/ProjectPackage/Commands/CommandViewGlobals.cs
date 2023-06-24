using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idDbgGlobalsWindow)]
    internal sealed class CommandViewGlobals : XSharpDebuggerBaseCommand<CommandViewGlobals>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await XSharp.Debugger.UI.GlobalsWindow.ShowAsync();
        }
    }
}
