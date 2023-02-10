using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Design.Serialization;
using Microsoft.VisualStudio.Shell.Interop;
using System;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idWebsite)]
    internal sealed class CommandXsWebsite : BaseCommand<CommandXsWebsite>
    {
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await Commands.StartProcessAsync("https://www.xsharp.eu");
        }
    }
}
