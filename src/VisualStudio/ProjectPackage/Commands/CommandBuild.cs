using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio;
using OLE=Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;

using System;
using System.Diagnostics;
using System.IO;
using System.Threading.Tasks;
using System.Reflection.Metadata.Ecma335;

namespace XSharp.Project
{
    internal abstract class CommandBuild<T> :  BaseCommand<T> where T : class, new()
    {
        protected abstract string CommandName { get; }
        protected abstract string CommandDescription { get; }
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
    class BuildCommandFilter : OLE.IOleCommandTarget
    {
        static readonly Guid PackGuid = new Guid("{568ABDF7-D522-474D-9EED-34B5E5095BA5}");

        public int QueryStatus(ref Guid pguidCmdGroup, uint cCmds,
            OLE.OLECMD[] prgCmds, IntPtr pCmdText)
        {
            bool isXSharp = false;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {

                isXSharp  = await Commands.ProjectIsXSharpProjectAsync();
            });
            if (!isXSharp)
            {
                return VSConstants.S_OK;
            }
            var mustCheck = false;
            var cmd = prgCmds[0].cmdID;
            if (pguidCmdGroup == CommandPack.CommandGroup && cmd == CommandPack.CommandID)
                mustCheck = true;
            else if (pguidCmdGroup == CommandPublish.CommandGroup && cmd == CommandPublish.CommandID)
                mustCheck = true;
            if (mustCheck)
            {
                bool show = false;
                ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    show = await Commands.ProjectIsXSharpSdkProjectAsync();
                });
                prgCmds[0].cmdf = show
                    ? (uint)(OLE.OLECMDF.OLECMDF_SUPPORTED | OLE.OLECMDF.OLECMDF_ENABLED)
                    : (uint)(OLE.OLECMDF.OLECMDF_SUPPORTED | OLE.OLECMDF.OLECMDF_INVISIBLE);
                return VSConstants.S_OK;
            }
            return (int) OLE.Constants.OLECMDERR_E_NOTSUPPORTED;
        }

        public int Exec(ref Guid pguidCmdGroup, uint nCmdID, uint nCmdexecopt,
            IntPtr pvaIn, IntPtr pvaOut)
            => (int)OLE.Constants.OLECMDERR_E_NOTSUPPORTED;
    }
}
