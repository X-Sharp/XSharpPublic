using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System;
using XSharpModel;
using Task = System.Threading.Tasks.Task;
using System.Collections.Generic;

namespace XSharp.Project
{
    [Command(PackageIds.idRebuildIntellisense)]
    internal sealed class CommandRebuildIntellisense : BaseCommand<CommandRebuildIntellisense>
    {
        protected override Task InitializeCompletedAsync()
        {
            Command.Supported = true;
            return base.InitializeCompletedAsync();
        }
        protected override void BeforeQueryStatus(EventArgs e)
        {
            Command.Enabled = XSolution.HasProject;
            base.BeforeQueryStatus(e);
        }
        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            if (await VS.MessageBox.ShowConfirmAsync("Rebuild Intellisense Database",
                "This will delete the current intellisense database, \r\n" +
                "rescan all sources and referenced assemblies\r\n"+
                "\r\nOK to continue?"))
            {
                await VS.StatusBar.ShowMessageAsync("Rebuilding intellisense database");
                XDatabase.DeleteOnClose = true;
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                await VS.Commands.ExecuteAsync(KnownCommands.File_SaveAll);
                var windows = await VS.Windows.GetAllDocumentWindowsAsync();
                
                var fileNames = new List<string>();
                string current = null;
                foreach (var window in windows)
                {
                    var docview = await window.GetDocumentViewAsync();
                    if (docview != null)
                        fileNames.Add("\""+docview.FilePath+ "\"");
                }
                if (fileNames.Count > 0)
                {
                    var window = await VS.Windows.GetCurrentWindowAsync();
                    var view = await window.GetDocumentViewAsync();
                    if (view != null)
                    {
                        current = view.FilePath;
                    }

                }
                _ = await VS.Commands.ExecuteAsync(KnownCommands.Window_CloseAllDocuments);
                ModelWalker.Stop();
                var solFile = XSolution.FileName;
                var projects = XSharpProjectNode.AllProjects;
                XSolution.Close();
                XSolution.Open(solFile);
                foreach (var project in projects)
                {
                    project.ReloadProjectModel();
                }
                ModelWalker.Walk();
                foreach (var filename in fileNames)
                {
                    _ = await VS.Commands.ExecuteAsync(KnownCommands.File_OpenFile, filename);
                }
                if (! string.IsNullOrEmpty(current))
                {
                    var curWin = await VS.Windows.FindDocumentWindowAsync(current);
                    _ = await curWin.ShowAsync();
                }
            }
        }
        void getProjects (SolutionItem item, List<SolutionItem> items)
        {
            if (item.Type == SolutionItemType.Project)
                items.Add(item);
            foreach (var child in item.Children)
            {
                getProjects(child, items);
            }
        }
    }
}
