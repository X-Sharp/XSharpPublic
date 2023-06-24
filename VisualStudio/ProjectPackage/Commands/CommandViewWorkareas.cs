using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idDbgWorkareaWindow)]
    internal sealed class CommandViewWorkareas : XSharpDebuggerBaseCommand<CommandViewWorkareas>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await XSharp.Debugger.UI.WorkareasWindow.ShowAsync();
        }
    }
}
