using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idDbgMemvarsWindow)]
    internal sealed class CommandViewMemvars : XSharpDebuggerBaseCommand<CommandViewMemvars>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await XSharp.Debugger.UI.MemvarsWindow.ShowAsync();
        }
    }
}
