using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idFoxXporter)]
    internal sealed class CommandFoxXPorter : BaseCommand<CommandFoxXPorter>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            string xporterPath = Commands.GetXsPath(@"Bin\FoxXporter.exe");
            await Commands.StartProcessAsync(xporterPath);
        }
        protected override Task InitializeCompletedAsync()
        {
            Command.Supported = false;
            return base.InitializeCompletedAsync();
        }
        protected override void BeforeQueryStatus(EventArgs e)
        {
            //Command.Enabled = XSolution.HasProject;
            //base.BeforeQueryStatus(e);
            Command.Visible = false;
        }
    }
}
