using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
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
    }
}
