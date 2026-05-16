using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using System.Diagnostics;
using System.IO;
using System.Threading.Tasks;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idPublishProject)]
    internal sealed class CommandPublish : BaseCommand<CommandPublish>
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
                // Only show for SDK-style projects as they support dotnet publish
                Command.Visible = prj != null && prj.IsSdkProject;
            }
        }

        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

            var project = await VS.Solutions.GetActiveProjectAsync();
            if (project == null)
            {
                await VS.MessageBox.ShowErrorAsync("Publish", "No active project selected.");
                return;
            }

            var projectPath = project.FullPath;
            var prj = XSharpProjectNode.FindProject(projectPath);

            if (prj == null || !prj.IsSdkProject)
            {
                await VS.MessageBox.ShowErrorAsync("Publish", "Publish is only available for SDK-style projects.");
                return;
            }

            // Show publish dialog to get options
            var dialog = new PublishDialog(projectPath);
            var result = dialog.ShowModal();

            if (result == true)
            {
                await PublishProjectAsync(projectPath, dialog.PublishOptions);
            }
        }

        private async Task PublishProjectAsync(string projectPath, PublishOptions options)
        {
            try
            {
                await VS.StatusBar.ShowMessageAsync("Publishing project...");

                var arguments = BuildPublishArguments(projectPath, options);

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

                var outputPane = await VS.Windows.CreateOutputWindowPaneAsync("X# Publish");
                await outputPane.ActivateAsync();
                await outputPane.ClearAsync();
                await outputPane.WriteLineAsync($"Publishing project: {Path.GetFileName(projectPath)}");
                await outputPane.WriteLineAsync($"Command: dotnet {arguments}");
                await outputPane.WriteLineAsync("");

                var process = new Process { StartInfo = psi };

                process.OutputDataReceived += async (s, ea) =>
                {
                    if (!string.IsNullOrEmpty(ea.Data))
                    {
                        await outputPane.WriteLineAsync(ea.Data);
                    }
                };

                process.ErrorDataReceived += async (s, ea) =>
                {
                    if (!string.IsNullOrEmpty(ea.Data))
                    {
                        await outputPane.WriteLineAsync($"ERROR: {ea.Data}");
                    }
                };

                process.Start();
                process.BeginOutputReadLine();
                process.BeginErrorReadLine();

                await Task.Run(() => process.WaitForExit());

                if (process.ExitCode == 0)
                {
                    await outputPane.WriteLineAsync("");
                    await outputPane.WriteLineAsync("Publish succeeded.");
                    await VS.StatusBar.ShowMessageAsync("Publish succeeded.");
                    await VS.MessageBox.ShowAsync("Publish",
                        $"Project published successfully to:\n{options.OutputPath}",
                        Microsoft.VisualStudio.Shell.Interop.OLEMSGICON.OLEMSGICON_INFO,
                        Microsoft.VisualStudio.Shell.Interop.OLEMSGBUTTON.OLEMSGBUTTON_OK);
                }
                else
                {
                    await outputPane.WriteLineAsync("");
                    await outputPane.WriteLineAsync($"Publish failed with exit code {process.ExitCode}.");
                    await VS.StatusBar.ShowMessageAsync("Publish failed.");
                    await VS.MessageBox.ShowErrorAsync("Publish", "Publish failed. See Output window for details.");
                }
            }
            catch (Exception ex)
            {
                await VS.MessageBox.ShowErrorAsync("Publish Error", $"Failed to publish project:\n{ex.Message}");
            }
        }

        private string BuildPublishArguments(string projectPath, PublishOptions options)
        {
            var args = $"publish \"{projectPath}\"";

            if (!string.IsNullOrEmpty(options.Configuration))
            {
                args += $" -c {options.Configuration}";
            }

            if (!string.IsNullOrEmpty(options.TargetFramework))
            {
                args += $" -f {options.TargetFramework}";
            }

            if (!string.IsNullOrEmpty(options.Runtime))
            {
                args += $" -r {options.Runtime}";
            }

            if (!string.IsNullOrEmpty(options.OutputPath))
            {
                args += $" -o \"{options.OutputPath}\"";
            }

            if (options.SelfContained.HasValue)
            {
                args += options.SelfContained.Value ? " --self-contained" : " --no-self-contained";
            }

            if (options.SingleFile)
            {
                args += " -p:PublishSingleFile=true";
            }

            if (options.ReadyToRun)
            {
                args += " -p:PublishReadyToRun=true";
            }

            if (options.Trimmed)
            {
                args += " -p:PublishTrimmed=true";
            }

            return args;
        }
    }

    public class PublishOptions
    {
        public string Configuration { get; set; } = "Release";
        public string TargetFramework { get; set; }
        public string Runtime { get; set; }
        public string OutputPath { get; set; }
        public bool? SelfContained { get; set; }
        public bool SingleFile { get; set; }
        public bool ReadyToRun { get; set; }
        public bool Trimmed { get; set; }
    }
}
