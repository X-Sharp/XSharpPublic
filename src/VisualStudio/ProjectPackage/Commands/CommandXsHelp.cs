using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idHelpOffLine)]
    internal sealed class CommandXsHelp : BaseCommand<CommandXsHelp>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            string docPath = Commands.GetXsPath(@"Help\XSharp.chm");
            await Commands.StartProcessAsync(docPath);
        }
    }
}
