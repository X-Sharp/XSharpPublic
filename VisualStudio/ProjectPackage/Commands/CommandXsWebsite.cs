using Community.VisualStudio.Toolkit;
using System;
using System.Diagnostics;

namespace XSharp.Project
{
    [Command(PackageIds.idWebsite)]
    internal sealed class CommandXsWebsite : BaseCommand<CommandXsWebsite>
    {
        protected override void Execute(object sender, EventArgs e)
        {
            var pi = new ProcessStartInfo
            {
                FileName = "https://www.xsharp.eu",
                UseShellExecute = true
            };
            Process.Start(pi);
        }
    }
}
