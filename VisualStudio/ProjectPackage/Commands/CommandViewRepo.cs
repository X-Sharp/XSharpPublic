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
#if REPOWINDOW
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await RepositoryWindow.ShowAsync();
        }
#endif
        protected override Task InitializeCompletedAsync()
        {
#if !REPOWINDOW

            Command.Supported = false;
            Command.Visible = false;
#else
            Command.Visible = true;

#endif
            return base.InitializeCompletedAsync();
        }

        protected override void BeforeQueryStatus(EventArgs e)
        {
#if REPOWINDOW
            Command.Enabled = XSolution.HasProject;
            Command.Visible = true;
#else
            Command.Enabled = false;
            Command.Visible = false;
#endif
            base.BeforeQueryStatus(e);

        }
    }
}
