using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using System.Diagnostics;
using System.IO;
using System.Threading.Tasks;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idPackProject)]
    internal sealed class CommandPack : BaseCommand<CommandPack>
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
                // Only show for SDK-style projects as they support dotnet pack
                Command.Visible = prj != null && prj.IsSdkProject;
            }
        }

        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

            var project = await VS.Solutions.GetActiveProjectAsync();
            if (project == null)
            {
                await VS.MessageBox.ShowErrorAsync("Pack", "No active project selected.");
                return;
            }

            var projectPath = project.FullPath;
            var prj = XSharpProjectNode.FindProject(projectPath);

            if (prj == null || !prj.IsSdkProject)
            {
                await VS.MessageBox.ShowErrorAsync("Pack", "Pack is only available for SDK-style projects.");
                return;
            }

            // Show confirmation dialog
            var result = await VS.MessageBox.ShowAsync(
                "Create NuGet Package",
                "This will create a NuGet package from the project using 'dotnet pack'.\n\n" +
                "The package will be created in the project's bin folder.\n\n" +
                "Continue?",
                Microsoft.VisualStudio.Shell.Interop.OLEMSGICON.OLEMSGICON_QUERY,
                Microsoft.VisualStudio.Shell.Interop.OLEMSGBUTTON.OLEMSGBUTTON_OKCANCEL);

            if (result == Microsoft.VisualStudio.VSConstants.MessageBoxResult.IDOK)
            {
                await PackProjectAsync(projectPath);
            }
        }

        private async Task PackProjectAsync(string projectPath)
        {
            try
            {
                await VS.StatusBar.ShowMessageAsync("Creating NuGet package...");

                // Build dotnet pack command
                var arguments = $"pack \"{projectPath}\" -c Release";

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

                var outputPane = await VS.Windows.CreateOutputWindowPaneAsync("X# Pack");
                await outputPane.ActivateAsync();
                await outputPane.ClearAsync();
                await outputPane.WriteLineAsync($"Creating NuGet package for: {Path.GetFileName(projectPath)}");
                await outputPane.WriteLineAsync($"Command: dotnet {arguments}");
                await outputPane.WriteLineAsync("");

                var process = new Process { StartInfo = psi };

                string packagePath = null;

                process.OutputDataReceived += async (s, ea) =>
                {
                    if (!string.IsNullOrEmpty(ea.Data))
                    {
                        await outputPane.WriteLineAsync(ea.Data);

                        // Try to extract the package path from output
                        if (ea.Data.Contains(".nupkg"))
                        {
                            var parts = ea.Data.Split(new[] { '\'' }, StringSplitOptions.RemoveEmptyEntries);
                            if (parts.Length >= 2)
                            {
                                packagePath = parts[1];
                            }
                        }
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
                    await outputPane.WriteLineAsync("Pack succeeded.");
                    await VS.StatusBar.ShowMessageAsync("NuGet package created successfully.");

                    var message = "NuGet package created successfully.";
                    if (!string.IsNullOrEmpty(packagePath))
                    {
                        message += $"\n\nPackage location:\n{packagePath}";
                    }

                    await VS.MessageBox.ShowAsync("Pack",
                        message,
                        Microsoft.VisualStudio.Shell.Interop.OLEMSGICON.OLEMSGICON_INFO,
                        Microsoft.VisualStudio.Shell.Interop.OLEMSGBUTTON.OLEMSGBUTTON_OK);
                }
                else
                {
                    await outputPane.WriteLineAsync("");
                    await outputPane.WriteLineAsync($"Pack failed with exit code {process.ExitCode}.");
                    await VS.StatusBar.ShowMessageAsync("Pack failed.");
                    await VS.MessageBox.ShowErrorAsync("Pack", "Pack failed. See Output window for details.");
                }
            }
            catch (Exception ex)
            {
                await VS.MessageBox.ShowErrorAsync("Pack Error", $"Failed to create NuGet package:\n{ex.Message}");
            }
        }
    }
}
