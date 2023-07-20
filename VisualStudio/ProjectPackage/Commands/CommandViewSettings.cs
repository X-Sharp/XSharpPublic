using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idDbgSettingsWindow)]
    internal sealed class CommandViewSettings : XSharpDebuggerBaseCommand<CommandViewSettings>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await XSharp.Debugger.UI.SettingsWindow.ShowAsync();
        }
    }
}
