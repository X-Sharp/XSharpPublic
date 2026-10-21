using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Task = System.Threading.Tasks.Task;
namespace XSharp.Project
{
    [Command(PackageIds.idConvertSDKProject)]
    internal sealed class CommandConvertSDKProject: BaseCommand<CommandConvertSDKProject>
    {
        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
#if DEV17
            ThreadHelper.JoinableTaskFactory.Run(CheckAvailabilityAsync);
#else
            Command.Visible = false;
#endif
        }
#if DEV17
        private async Task CheckAvailabilityAsync()
        {
            Command.Visible = await Commands.ProjectIsXSharpProjectAsync();

            if (Command.Visible)
            {
                var project = await VS.Solutions.GetActiveProjectAsync();
                var path = project.FullPath;
                var prj = XSharpProjectNode.FindProject(path);
                Command.Visible = prj != null;
            }
        }
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            string converterpath = Commands.GetXsPath(@"Converter\Converter.Exe");
            if (!File.Exists(converterpath))
            {
                await VS.MessageBox.ShowErrorAsync("Convert to SDK style project", "Could not find " + converterpath);
                return;
            }
            var project = await VS.Solutions.GetActiveProjectAsync();
            var path = project.FullPath;

            var info = new ProcessStartInfo(converterpath)
            {
                // Quoting is handled here, so a path with spaces arrives as one argument
                Arguments = "\"" + path + "\"",
                WorkingDirectory = Path.GetDirectoryName(path),
                UseShellExecute = false,
                CreateNoWindow = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
            };

            var output = new StringBuilder();
            var errors = new StringBuilder();
            int exitCode;
            try
            {
                exitCode = await RunAsync(info, output, errors);
            }
            catch (Exception ex)
            {
                await VS.MessageBox.ShowErrorAsync("Convert to SDK style project", ex.Message);
                return;
            }


            var pane = await VS.Windows.GetOutputWindowPaneAsync(Community.VisualStudio.Toolkit.Windows.VSOutputWindowPane.Build);
            await pane.ActivateAsync();
            await pane.WriteLineAsync("Converting project to SDK format");
            await pane.WriteLineAsync(converterpath + " " + info.Arguments);
            if (output.Length > 0)
                await pane.WriteLineAsync(output.ToString().TrimEnd());
            if (errors.Length > 0)
                await pane.WriteLineAsync(errors.ToString().TrimEnd());
            await pane.WriteLineAsync($"Converter ended with exit code {exitCode}");

            if (exitCode != 0)
            {
                var message = errors.Length > 0 ? errors.ToString().Trim() : output.ToString().Trim();
                if (message.Length == 0)
                    message = $"The converter ended with exit code {exitCode}.";
                await VS.MessageBox.ShowErrorAsync("Convert to SDK style project", message);
            }
        }

        /// <summary>
        /// Starts the converter and collects its console output and error output.
        /// </summary>
        /// <remarks>
        /// Both streams are read with the asynchronous events: reading one stream to the end while the other one
        /// fills its buffer would block the converter. The task completes when the process has exited AND both
        /// streams are closed (the Exited event can come before the last output has been delivered).
        /// </remarks>
        private static Task<int> RunAsync(ProcessStartInfo info, StringBuilder output, StringBuilder errors)
        {
            var exited = new TaskCompletionSource<int>();
            var outputDone = new TaskCompletionSource<bool>();
            var errorsDone = new TaskCompletionSource<bool>();

            var process = new Process { StartInfo = info, EnableRaisingEvents = true };
            process.OutputDataReceived += (s, args) =>
            {
                if (args.Data == null)
                    outputDone.TrySetResult(true);
                else
                    output.AppendLine(args.Data);
            };
            process.ErrorDataReceived += (s, args) =>
            {
                if (args.Data == null)
                    errorsDone.TrySetResult(true);
                else
                    errors.AppendLine(args.Data);
            };
            process.Exited += (s, args) => exited.TrySetResult(process.ExitCode);

            process.Start();
            process.BeginOutputReadLine();
            process.BeginErrorReadLine();

            return Task.WhenAll(exited.Task, outputDone.Task, errorsDone.Task)
                .ContinueWith(_ =>
                {
                    var exitCode = exited.Task.Result;
                    process.Dispose();
                    return exitCode;
                }, TaskScheduler.Default);
        }
#endif
    }
}
