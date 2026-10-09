using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using XSharpModel;
using Task = System.Threading.Tasks.Task;
using XSharp.Settings;
namespace XSharp.Project
{
    [Command(PackageIds.idViewInDisassembler)]
    internal sealed class CommandViewInDisassembler : BaseCommand<CommandViewInDisassembler>
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
            var isCPS = project.IsCapabilityMatch("CPS");
            string path = "";
            if (isCPS)
            {
                var outputPath = await project.GetAttributeAsync("TargetPath");
                path = outputPath;
            }
            else
            {
                var fullpath = project.FullPath;
                var xsproject = XSolution.FindProjectByFileName(fullpath);
                path = xsproject.ProjectNode.OutputFile;
            }
            if (!String.IsNullOrEmpty(path))
            {
                if (!System.IO.File.Exists(path))
                {
                    await project.BuildAsync();
                }

                if (System.IO.File.Exists(path))
                {
                    var disasm = XSettings.Disassembler;
                    if (string.IsNullOrEmpty(disasm))
                    {
                        await VS.MessageBox.ShowWarningAsync("No disassembler defined.","Please setup the disassembler in Tools/Options/X# Customer Editors/Other Editors");
                    }
                    else if (!System.IO.File.Exists(disasm))
                    {
                        await VS.MessageBox.ShowWarningAsync($"Cannot find disassembler '{disasm}'", "Please setup the disassembler in Tools/Options/X# Customer Editors/Other Editors");
                    }
                    else
                    {
                        await Commands.StartProcessAsync(disasm, path);
                    }
                }
                else
                {
                    await VS.MessageBox.ShowWarningAsync($"Output file '{path}' not found.","Build the project first");
                }
            }
        }
    }
}
