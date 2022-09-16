using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using XSharpModel;
using Task = System.Threading.Tasks.Task;
using System.IO;
namespace XSharp.Project
{
    [Command(PackageIds.idRebuildIntellisense)]
    internal sealed class CommandRebuildIntellisense : BaseCommand<CommandRebuildIntellisense>
    {
        protected override Task InitializeCompletedAsync()
        {
            Command.Supported = true;
            return base.InitializeCompletedAsync();
        }
        protected override void BeforeQueryStatus(EventArgs e)
        {
            Command.Enabled = XSolution.HasProject;
            base.BeforeQueryStatus(e);
        }
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            if (await VS.MessageBox.ShowConfirmAsync("Rebuild Intellisense Database", "This will close the current solution, shutdown Visual Studio and reopen the current solution.\r\nOK to restart Visual Studio?"))
            {
                await VS.StatusBar.ShowMessageAsync("Restarting Visual Studio to rebuild the XSharp intellisense database");
                XDatabase.DeleteOnClose = true;
                await VS.Shell.RestartAsync(false);
            }
        }
    }
}
