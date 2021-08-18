using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System;
using System.Linq;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{
    [Command(PackageIds.idAddDesignerFile)]
    internal sealed class CommandAddDesignerFile : BaseCommand<CommandAddDesignerFile>
    {
        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
            ThreadHelper.JoinableTaskFactory.Run(CheckAvailabilityAsync);
        }
        private async Task CheckAvailabilityAsync()
        {
            var items = await VS.Solutions.GetActiveItemsAsync();
            
            bool visible = false;
            foreach (var item in items)
            {
                if (item is File file)
                {
                    var subtype = await file.GetAttributeAsync(ProjectFileConstants.SubType);
                    if (subtype == ProjectFileAttributeValue.Form || subtype == ProjectFileAttributeValue.UserControl)
                    {
                        visible = true;
                        foreach (var child in file.Children)
                        {
                            if (child.FullPath.ToLower().EndsWith(".designer.prg"))
                            {
                                visible = false;
                            }
                        }
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
            await VS.MessageBox.ShowAsync("Not ready yet");
        }
    }
}
