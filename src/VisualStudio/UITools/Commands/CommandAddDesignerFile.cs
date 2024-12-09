﻿using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System;
using System.Linq;
using XSharpModel;
using Task = System.Threading.Tasks.Task;
using XSharp.CodeDom;
using System.IO;
using System.Text;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Text.RegularExpressions;
using XSharp.Settings;
namespace XSharp.Project
{
    [Command(PackageIds.idAddDesignerFile)]
    internal sealed class CommandAddDesignerFile : BaseCommand<CommandAddDesignerFile>
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

            bool visible = false;
            foreach (var item in items)
            {
                if (item is PhysicalFile file)
                {
                    var subtype = await file.GetAttributeAsync(ProjectFileConstants.SubType);
                    if (subtype == ProjectFileAttributeValue.Form || subtype == ProjectFileAttributeValue.UserControl)
                    {
                        visible = true;
                        currentFile = file;
                        foreach (var child in file.Children)
                        {
                            if (child.FullPath.ToLower().EndsWith(".designer.prg"))
                            {
                                visible = false;
                                currentFile = null;
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
            if (currentFile != null)
            {
                var newfile = System.IO.Path.ChangeExtension(currentFile.FullPath, ".designer.prg");
                await VS.StatusBar.ShowMessageAsync("Creating designer file:" + newfile);
                System.IO.File.WriteAllText(newfile, "");
                var project = currentFile.ContainingProject;
                dynamic projectNode = null;
                if (project != null)
                {
                    var xproject = XSolution.FindProjectByFileName(project.FullPath);
                    if (xproject != null)
                    {
                        xproject.ProjectNode.AddFileNode(newfile);
                        projectNode = xproject.ProjectNode;
                    }

                    XSettings.Information($"Reading file data for {currentFile.FullPath} from database");
                    dynamic fileNode;
                    fileNode = projectNode.FindChild(currentFile.FullPath);
                    bool ok = fileNode.GenerateDesignerFile();
                    if (!ok)
                    {
                        await VS.MessageBox.ShowErrorAsync("Error creating designer file");
                    }
                }
                await VS.StatusBar.ShowMessageAsync("");
            }
        }
    }
}
