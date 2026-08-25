//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio.Shell;

using System;
using System.Linq;
using System.Threading.Tasks;

using XSharp.Project.ShadowDesigner;
using XSharpModel;

namespace XSharp.Project
{
    /// <summary>
    /// File-context-menu command for Stage 5 of the shadow-designer bridge (see
    /// E:\VSDesigner\research\06-document-binding-substitution.md): after editing
    /// properties or adding/removing/reordering controls in the shadow Designer, run this
    /// to fully regenerate the real Form1.Designer.prg from the companion project's current
    /// state. Visible only for a SDK-style project's .prg file that already has an open
    /// shadow companion project.
    /// </summary>
    [Command(PackageIds.idSyncDesignerChanges)]
    internal sealed class CommandSyncDesignerChanges : BaseCommand<CommandSyncDesignerChanges>
    {
        private XSharpFileNode _currentFile;

        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
            _currentFile = null;
            ThreadHelper.JoinableTaskFactory.Run(CheckAvailabilityAsync);
        }

        private async Task CheckAvailabilityAsync()
        {
            bool visible = false;
            var items = await VS.Solutions.GetActiveItemsAsync();
            foreach (var item in items)
            {
                if (item is PhysicalFile file)
                {
                    var project = await VS.Solutions.GetActiveProjectAsync();
                    var xproject = project != null ? XSolution.FindProject(project.FullPath, "") : null;
                    if (xproject?.ProjectNode is XSharpProjectNode prjNode &&
                        prjNode.FindChild(file.FullPath) is XSharpFileNode fileNode &&
                        fileNode.HasDesigner &&
                        prjNode is XSharpSdkProjectNode)
                    {
                        _currentFile = fileNode;
                        visible = true;
                    }
                }
            }
            Command.Visible = visible;
            Command.Enabled = visible;
        }

        protected override async Task ExecuteAsync(OleMenuCmdEventArgs e)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            if (_currentFile == null) return;

            await VS.Commands.ExecuteAsync(KnownCommands.File_SaveAll);

            if (!ShadowDesignerBridge.TryResolveCompanionPaths(_currentFile, out var location, out string error))
            {
                await VS.MessageBox.ShowErrorAsync("X# WinForms Designer", error);
                return;
            }

            DesignerChangesSync.SyncResult result;
            try
            {
                result = DesignerChangesSync.Sync(location);
            }
            catch (Exception ex)
            {
                await VS.MessageBox.ShowErrorAsync("X# WinForms Designer", ex.ToString());
                return;
            }

            await VS.Commands.ExecuteAsync(KnownCommands.File_OpenFile, location.DesignerPrgPath);

            string skippedText = result.SkippedStatements.Count > 0
                ? $" ({result.SkippedStatements.Count} statement(s) skipped -- unsupported shape, check manually: " +
                  string.Join("; ", result.SkippedStatements) + ")"
                : "";
            await VS.StatusBar.ShowMessageAsync(
                $"Regenerated Form1.Designer.prg: {result.FieldCount} field(s), {result.StatementCount} statement(s).{skippedText}");
        }
    }
}
