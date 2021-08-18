using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Threading;
using System;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idConvertXSharpRuntime)]
    internal sealed class CommandConvertXsRuntime : BaseCommand<CommandConvertXsRuntime>
    {
        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
            ThreadHelper.JoinableTaskFactory.Run(CheckAvailabilityAsync);
        }

        private async Task CheckAvailabilityAsync()
        {
            var project = await VS.Solutions.GetActiveProjectAsync();
            var path = project.FullPath;
            var xsproject = XSolution.FindProject(path);
            var isVulcan = false;
            if (xsproject != null)
            {
                foreach (var asmref in xsproject.AssemblyReferences)
                {
                    if (asmref.FullName.Contains("Vulcan"))
                    {
                        isVulcan = true;
                        break;
                    }
                }
            }
            Command.Visible = isVulcan;
        }

        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await VS.MessageBox.ShowAsync("Not ready yet");
        }
    }
}
