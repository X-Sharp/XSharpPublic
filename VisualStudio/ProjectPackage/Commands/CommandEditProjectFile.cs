using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Threading;
using System;
using System.Runtime.InteropServices;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idEditProjectFile)]
    internal sealed class CommandEditProjectFile : BaseCommand<CommandEditProjectFile>
    {
        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
            ThreadHelper.JoinableTaskFactory.Run(CheckAvailabilityAsync);
        }
        private async Task CheckAvailabilityAsync()
        {
            Command.Visible = await Commands.ProjectIsXSharpProjectAsync();
        }
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            var project = await VS.Solutions.GetActiveProjectAsync();
            try
            {
                await VS.Documents.OpenAsync(project.FullPath);
            }
            catch (COMException ex) when (ex.ErrorCode == unchecked((int) 0x80004005))
            {
                // The project needs to be unloaded before it
                // can be opened. Get the GUID of the project.
                project.GetItemInfo(out IVsHierarchy hierarchy, out _, out _);
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                IVsSolution solution = await VS.Services.GetSolutionAsync();
                ErrorHandler.ThrowOnFailure(solution.GetGuidOfProject(hierarchy, out Guid guid));

                // Unload the project.
                ((IVsSolution4)solution).UnloadProject(guid, (uint)_VSProjectUnloadStatus.UNLOADSTATUS_UnloadedByUser);

                // Now try to open the project file again.
                await VS.Documents.OpenAsync(project.FullPath);
            }

        }
    }
}
