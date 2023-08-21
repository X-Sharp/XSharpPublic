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
            string udcPath = System.IO.Path.GetDirectoryName(typeof(CommandUDCTester).Assembly.Location);
            udcPath = System.IO.Path.Combine(udcPath, "XUDCTester.exe");
            await Commands.StartProcessAsync(udcPath);
        }
    }
}
