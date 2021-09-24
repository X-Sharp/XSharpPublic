using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idXporter)]
    internal sealed class CommandXPorter : BaseCommand<CommandXPorter>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            string xporterPath = Commands.GetXsPath(@"Bin\Xporter.exe");
            await Commands.StartProcessAsync(xporterPath);
        }
    }
}
