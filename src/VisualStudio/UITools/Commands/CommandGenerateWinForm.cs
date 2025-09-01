//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio.Shell;

using System;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using System.Collections.Generic;
using XSharp.VOEditors;

using XSharpModel;

using Task = System.Threading.Tasks.Task;
using Microsoft.VisualStudio.Project;
namespace XSharp.Project
{
    [Command(PackageIds.idGenerateWinForm)]
    internal sealed class CommandGenerateWinForm : BaseCommand<CommandGenerateWinForm>
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
            Command.Enabled = items.Count() == 1;
        }

        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            if (currentFile != null)
            {
                var baseName = Path.GetFileName(currentFile.FullPath);
                var form = new CreateWindowsForm();
                // the baseName is usually File.Form.xsfrm
                baseName = baseName.Substring(baseName.IndexOf('.')+1);
                baseName = Path.GetFileNameWithoutExtension(baseName);
                form.tbOriginalForm.Text = baseName;
                form.Path = Path.GetDirectoryName(currentFile.FullPath);
                var newName = "frm" + baseName.Replace(" ","_");
                // check to see if the newName is already used
                var project = await VS.Solutions.GetActiveProjectAsync();
                var xproject = XSolution.FindProject(project.FullPath,"");
                var ns = xproject.ProjectNode.RootNameSpace;
                form.project = xproject;
                form.tbNewForm.Text = newName;
                form.ShowDialog();
                if (form.DialogResult == DialogResult.OK)
                {
                    await VS.StatusBar.ShowMessageAsync("Creating Windows Form for " + form.tbNewForm.Text);
                    var oldform = currentFile.FullPath;
                    var newclass = form.tbNewForm.Text.Trim();
                    var newform = Path.Combine(form.Path, newclass+".prg");
                    await VS.StatusBar.ShowMessageAsync("");
                    if (!VOWindowEditor.ConvertXsfrmToPrg(oldform, newclass, newform, ns))
                    {

                        await VS.MessageBox.ShowErrorAsync("Failed to create the Windows Form '" + newform + "'","");
                        return;
                    }
                    var newdesignerform = Path.ChangeExtension(newform, ".designer.prg");
                    var x = await project.AddExistingFilesAsync(new[] { newform, newdesignerform });
                    var prjNode = (ProjectNode) xproject.ProjectNode;
                    var file = prjNode.FindChild(newform);
                    if (file is FileNode fileNode && fileNode.IsLink)
                    {
                        file.ItemNode.SetMetadata("Link", null);
                    }
                    file = prjNode.FindChild(newdesignerform);
                    if (file is FileNode designerfileNode && designerfileNode.IsLink)
                    {
                        file.ItemNode.SetMetadata("Link", null);
                    }





                    // verify the references of the project and add System.Windows.Forms if needed and also System.Drawing
                    var usings = new List<string>();
                    if (xproject.FindSystemType("System.Windows.Forms.Form", usings) == null)
                    {
                        await project.References.AddAsync("System.Windows.Forms");
                    }
                    if (xproject.FindSystemType("System.Drawing.Point", usings) == null)
                    {
                        await project.References.AddAsync("System.Drawing");
                    }



                }
            }
        }
    }
}
