using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

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
            await VS.StatusBar.ShowMessageAsync("Reloading the project to rebuild the XSharp intellisense database");
            var fileName = XDatabase.FileName;
            XDatabase.DeleteOnClose = true;
            var sol = await VS.Solutions.GetCurrentSolutionAsync();
            var path = sol.FullPath;
            await VS.Commands.ExecuteAsync(KnownCommands.File_CloseProject, path);
            await VS.Commands.ExecuteAsync(KnownCommands.File_OpenProject, path);
            await VS.StatusBar.ClearAsync();
        }
    }
}
