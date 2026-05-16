using Community.VisualStudio.Toolkit;

using System;
using System.IO;

using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idPublishProject)]
    internal sealed class CommandPublish : CommandBuild<CommandPublish>
    {
        protected override string CommandName => "Build.PublishSelection";

        protected override async Task DoCmdAsync()
        {
            if (!await VerifySdkProjectAsync("Publish"))
            {
                return;
            }

            // Show publish dialog to get options
            var dialog = new PublishDialog(projectPath);
            var result = dialog.ShowDialog();

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
                var process = await CreateProcessAsync(arguments,
                    $"Publishing project: {Path.GetFileName(projectPath)}");

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
