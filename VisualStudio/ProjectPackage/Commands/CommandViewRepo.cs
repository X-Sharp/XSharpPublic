using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idViewRepo)]
    internal sealed class CommandViewRepo : BaseCommand<CommandViewRepo>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await RepositoryWindow.ShowAsync();
        }

        protected override Task InitializeCompletedAsync()
        {
            Command.Supported = false;
            return base.InitializeCompletedAsync();
        }

        protected override void BeforeQueryStatus(EventArgs e)
        {
            Command.Enabled = XSolution.HasProject;
            base.BeforeQueryStatus(e);
        }
    }
}
