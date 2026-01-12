using Community.VisualStudio.Toolkit;

using EnvDTE;

using EnvDTE100;

using Microsoft.Build.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

using System;
using System.Threading.Tasks;

using XSharp.Settings;

using static XSharp.Project.XSharpSdkProjectNode;

using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.DebugTargetMenuDebugFrameworkMenu)]
    internal sealed class DebuggerFrameworkSubMenu : XSharpSdkCommand<DebuggerFrameworkSubMenu>
    {
        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
            ThreadHelper.JoinableTaskFactory.Run(CheckAvailabilityAsync);
        }
        private async Task CheckAvailabilityAsync()
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            Command.Visible = false;
            Command.Enabled = false;
            var node = await GetStartupProjectAsync();
            if (node is XSharpSdkProjectNode sdkNode)
            {
                if (sdkNode.SubProjects.Count <= 1)
                    return;
                Command.Visible = true;
                Command.Enabled = true;
                var subProject = sdkNode.ActiveSubProject;
                if (subProject != null)
                {
                    Command.Text = $"Framework ({subProject.TargetFramework})";
                }
            }
        }
    }

    internal abstract class XSharpSdkCommand<T> : BaseCommand<T>
           where T : class, new()
    {
        internal async Task<SdkSubProjectInfo> GetActiveSubProjectAsync()
        {
            var node = await GetStartupProjectAsync();
            if (node is XSharpSdkProjectNode sdkNode)
            {
                return sdkNode.ActiveSubProject;
            }
            return null;
        }

        internal async Task<ProjectNode> GetStartupProjectAsync()
        {
            var dte = (DTE) await VS.GetRequiredServiceAsync<DTE,DTE>();
            var sol4 = dte.Solution as Solution4;

            if (sol4 == null)
                return null;
            var build = sol4.SolutionBuild;
            if (build != null)
            {
                var startupprojects = build.StartupProjects;
                if (startupprojects != null)
                {
                    var projects = await VS.Solutions.GetAllProjectsAsync();
                    var projectList = startupprojects as Array;
                    foreach (string prjName in projectList)
                    {
                        foreach (var prj in projects)
                        {
                            var fileName = System.IO.Path.GetFileName(prj.FullPath);
                            if (fileName.Equals(prjName, StringComparison.OrdinalIgnoreCase))
                            {
                                var node = XSharpProjectNode.FindProject(prj.FullPath);
                                if (node != null)
                                    return node;
                            }
                        }
                    }
                }
            }
            return null;

        }
    }
        internal abstract class XSharpFrameworkCommand<T> : XSharpSdkCommand<T>
           where T : class, new()
    {
        protected override void BeforeQueryStatus(EventArgs e)
        {
            ThreadHelper.JoinableTaskFactory.Run(CheckAvailabilityAsync);
            base.BeforeQueryStatus(e);
        }
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var activeSubProject = await GetActiveSubProjectAsync();
            if (activeSubProject != null)
            {
                var sdkNode = activeSubProject.ParentProject;
                if (Id <= sdkNode.SubProjects.Count)
                {
                    var thisSubProjectNode = sdkNode.SubProjects[Id - 1];
                    if (thisSubProjectNode != activeSubProject)
                    {
                        sdkNode.SelectSubProject(thisSubProjectNode);
                    }
                }
            }
            return;
        }


        private async Task CheckAvailabilityAsync()
        {
            Command.Visible = await Commands.ProjectIsXSharpProjectAsync();
            if (Command.Visible)
            {
                var activeSubProject = await GetActiveSubProjectAsync();
                if ( activeSubProject != null)
                {
                    var sdkNode = activeSubProject.ParentProject;
                    if (sdkNode.SubProjects.Count == 1)
                        Command.Visible = false;
                    else
                    {
                        Command.Visible = sdkNode.SubProjects.Count >= Id;
                    }
                    if (Command.Visible)
                    {
                        var subProject = sdkNode.SubProjects[Id - 1];
                        Command.Text = subProject.TargetFramework;
                        Command.Checked = sdkNode.ActiveSubProject == subProject;
                    }

                }
            }
        }
        protected abstract int Id { get; }
        protected override Task InitializeCompletedAsync()
        {
            Command.Enabled = true;
            Command.Supported = true;
            return base.InitializeCompletedAsync();
        }
    }

    [Command(PackageIds.idFramework1)]
    internal sealed class DebuggerFramework1 : XSharpFrameworkCommand<DebuggerFramework1>
    {
        protected override int Id => 1;
    }
    [Command(PackageIds.idFramework2)]
    internal sealed class DebuggerFramework2 : XSharpFrameworkCommand<DebuggerFramework2>
    {
        protected override int Id => 2;
    }
    [Command(PackageIds.idFramework3)]
    internal sealed class DebuggerFramework3 : XSharpFrameworkCommand<DebuggerFramework3>
    {
        protected override int Id => 3;
    }
    [Command(PackageIds.idFramework4)]
    internal sealed class DebuggerFramework4 : XSharpFrameworkCommand<DebuggerFramework4>
    {
        protected override int Id => 4;
    }

}
