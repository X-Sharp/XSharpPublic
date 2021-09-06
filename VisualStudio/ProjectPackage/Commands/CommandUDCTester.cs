using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idUDCTester)]
    internal sealed class CommandUDCTester : BaseCommand<CommandUDCTester>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            string xporterPath = Commands.GetXsPath(@"Bin\XUDCTester.exe");
            await Commands.StartProcessAsync(xporterPath);
        }
    }
}
