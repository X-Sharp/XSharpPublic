using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idVOXporter)]
    internal sealed class CommandVoXPorter : BaseCommand<CommandVoXPorter>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            var xporterPath = Commands.GetXsPath(@"VOXPorter\VOXporter.exe");
            await Commands.StartProcessAsync(xporterPath);
        }
    }
}
