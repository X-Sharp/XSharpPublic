using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

using System;
using System.ComponentModel.Design;
using System.Diagnostics;
using System.IO;
using System.Threading.Tasks;

namespace XSharp.Project
{
    internal abstract class CommandBuild<T> :  BaseCommand<T> where T : class, new()
    {
        protected abstract string CommandName { get; }
        protected abstract string CommandDescription { get; }
        protected abstract int CommandID { get; }
        protected abstract Guid CommandGroup { get; }
        protected CommandProgression DoCmd()
        {
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                await DoCmdAsync();
            });
            return CommandProgression.Stop;
        }


        protected override async Task InitializeCompletedAsync()
        {
            await base.InitializeCompletedAsync();
            await VS.Commands.InterceptAsync(CommandName, () => DoCmd());
        }
        protected string projectPath;
        protected async Task<bool> VerifySdkProjectAsync()
        {
            await VS.Commands.ExecuteAsync(KnownCommands.File_SaveAll);
            var project = await VS.Solutions.GetActiveProjectAsync();
            if (project == null)
            {
                await VS.MessageBox.ShowErrorAsync(CommandDescription, "No active project selected.");
                return false;
            }
            projectPath = project.FullPath;
            var prj = XSharpProjectNode.FindProject(projectPath);
            if (prj == null || !prj.IsSdkProject)
            {
                await VS.MessageBox.ShowErrorAsync(CommandDescription, $"The {CommandDescription} command is only available for SDK-style projects.");
                return false;
            }
            return true;
        }

        protected async Task<int> CreateProcessAsync(string arguments, string output)
        {
            var psi = new ProcessStartInfo
            {
                FileName = "dotnet",
                Arguments = arguments,
                WorkingDirectory = Path.GetDirectoryName(projectPath),
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true
            };
            var process = new Process { StartInfo = psi };
            process.OutputDataReceived += Process_OutputDataReceived;
            process.ErrorDataReceived += Process_ErrorDataReceived; ;

            await EnsureOutputPaneAsync();
            await outputPane.ActivateAsync();
            await outputPane.WriteLineAsync(output);
            await outputPane.WriteLineAsync($"Command: dotnet {arguments}");
            await outputPane.WriteLineAsync("");
            process.Start();
            process.BeginOutputReadLine();
            process.BeginErrorReadLine();

            await Task.Run(() => process.WaitForExit());

            if (process.ExitCode == 0)
            {
                await outputPane.WriteLineAsync("");
                var msg = CommandDescription + " succeeded.";
                await outputPane.WriteLineAsync(msg);
                await VS.StatusBar.ShowMessageAsync(msg);
             }
            else
            {
                await outputPane.WriteLineAsync("");
                await outputPane.WriteLineAsync($"{CommandDescription} failed with exit code {process.ExitCode}.");
                await VS.StatusBar.ShowMessageAsync(CommandDescription+" failed.");
                await VS.MessageBox.ShowErrorAsync(CommandDescription, CommandDescription + " failed. See Output window for details.");
            }

            return process.ExitCode;
        }

        private void Process_ErrorDataReceived(object sender, DataReceivedEventArgs ea)
        {
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                if (!string.IsNullOrEmpty(ea.Data))
                {
                    await outputPane.WriteLineAsync($"ERROR: {ea.Data}");
                }
            });
        }

        private void Process_OutputDataReceived(object sender, DataReceivedEventArgs ea)
        {
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                try
                {
                    if (!string.IsNullOrEmpty(ea.Data))
                    {
                        await outputPane.WriteLineAsync(ea.Data);
                    }
                }
                catch { }
            });

        }

        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            await DoCmdAsync();
        }
        protected abstract Task DoCmdAsync();
        protected static OutputWindowPane outputPane = null;
        protected async Task EnsureOutputPaneAsync()
        {

            if (outputPane == null)
            {
                var guid = VSConstants.GUID_BuildOutputWindowPane;
                outputPane = await VS.Windows.GetOutputWindowPaneAsync(guid);
            }
        }
        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
            ThreadHelper.JoinableTaskFactory.Run(CheckAvailabilityAsync);
        }
        protected async Task CheckAvailabilityAsync()
        {
            Command.Visible = await Commands.ProjectIsXSharpProjectAsync();
            if (Command.Visible)
            {
                var project = await VS.Solutions.GetActiveProjectAsync();
                if (project == null)
                {
                    Command.Visible = false;
                    Command.Enabled = false;
                    return;
                }
                var path = project.FullPath;
                var prj = XSharpProjectNode.FindProject(path);
                // Only show for SDK-style projects as they support dotnet pack/publish
                Command.Visible = prj != null && prj.IsSdkProject;
                Command.Enabled = Command.Visible;
            }
        }


    }
}
