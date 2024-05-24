using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idHelpOffLineChinese)]
    internal sealed class CommandXsHelpChinese : BaseCommand<CommandXsHelpChinese>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            string docPath = Commands.GetXsPath(@"Help\XSharp_ZH-CN.chm");
            await Commands.StartProcessAsync(docPath);
        }
    }
}
