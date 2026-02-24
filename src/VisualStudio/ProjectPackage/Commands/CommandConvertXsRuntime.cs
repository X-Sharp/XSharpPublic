using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using Task = System.Threading.Tasks.Task;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.Shell.Interop;
namespace XSharp.Project
{
    [Command(PackageIds.idConvertSDKProject)]
    internal sealed class CommandConvertSDKProject: BaseCommand<CommandConvertSDKProject>
    {
        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
            ThreadHelper.JoinableTaskFactory.Run(CheckAvailabilityAsync);
        }

        private async Task CheckAvailabilityAsync()
        {
            Command.Visible = await Commands.ProjectIsXSharpProjectAsync();
            if (Command.Visible)
            {
                var project = await VS.Solutions.GetActiveProjectAsync();
                var path = project.FullPath;
                var prj = XSharpProjectNode.FindProject(path);
                Command.Visible = prj != null && !prj.IsSdkProject;
            }
        }

        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            await VS.MessageBox.ShowWarningAsync("Converting a project to SDK style is not supported yet.");

        }
    }
}
