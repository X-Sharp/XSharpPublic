using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idFoxXporter)]
    internal sealed class CommandFoxXPorter : BaseCommand<CommandFoxXPorter>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            string xporterPath = Commands.GetXsPath(@"VFPXPorter\VFPXPorter.exe ");
            await Commands.StartProcessAsync(xporterPath);
        }
    }
}
