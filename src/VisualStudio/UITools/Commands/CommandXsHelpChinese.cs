using Community.VisualStudio.Toolkit;
using EnvDTE;
using Microsoft.VisualStudio.Shell;
using System;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idHelpOffLineChinese)]
    internal sealed class CommandXsHelpChinese : BaseCommand<CommandXsHelpChinese>
    {
        protected override Task InitializeCompletedAsync()
        {
            EnableDisable();
            return Task.CompletedTask;
        }
        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
            EnableDisable();
        }
        private void EnableDisable()
        {
            Command.Enabled = System.IO.File.Exists(DocPath);
        }

        string DocPath = Commands.GetXsPath(@"Help\XSharp_ZH-CN.chm");

        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            if (System.IO.File.Exists(DocPath))
            {
                await Commands.StartProcessAsync(DocPath);
            }
        }
    }
}
