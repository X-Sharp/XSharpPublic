using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System.Collections.Generic;
using System;
using System.Linq;
using XSharpModel;
using Task = System.Threading.Tasks.Task;
using XSharp.CodeDom;
using System.IO;
using System.Text;
using System.CodeDom;
using System.Xml.Serialization;
using System.CodeDom.Compiler;
using static System.Net.Mime.MediaTypeNames;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio.Shell.Interop;
using XSharp.Settings;
namespace XSharp.Project
{
    [Command(PackageIds.idGenerateWinForm)]
    internal sealed class CommandGenerateWindorm : BaseCommand<CommandGenerateWindorm>
    {
        PhysicalFile currentFile = null;
        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
            currentFile = null;
            ThreadHelper.JoinableTaskFactory.Run(CheckAvailabilityAsync);
        }
        private async Task CheckAvailabilityAsync()
        {
            var items = await VS.Solutions.GetActiveItemsAsync();
            currentFile = null;
            bool visible = false;
            foreach (var item in items)
            {
                if (item is PhysicalFile file)
                {
                    var type = XFileTypeHelpers.GetFileType(file.FullPath);
                    visible = type == XFileType.VOForm;
                    if (visible)
                    {
                        currentFile = file;
                    }
                }
            }
            Command.Visible = visible;
            if (items.Count() != 1)
            {
                Command.Enabled = false;
                return;
            }
        }

        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            if (currentFile != null)
            {
                var baseName = Path.GetFileName(currentFile.FullPath);
                await VS.StatusBar.ShowMessageAsync("Creating Windows Form for "+baseName);
                await Task.Delay(500);
                await VS.StatusBar.ShowMessageAsync("");
            }
        }
    }
}
