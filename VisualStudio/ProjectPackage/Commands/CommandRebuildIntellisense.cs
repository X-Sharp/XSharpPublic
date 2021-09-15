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
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var fileName = XDatabase.FileName;
            XDatabase.CloseDatabase(fileName);
            System.IO.File.Delete(fileName);
            XDatabase.CreateOrOpenDatabase(fileName);
            XSolution.RebuildIntellisense();
        }
    }
}
