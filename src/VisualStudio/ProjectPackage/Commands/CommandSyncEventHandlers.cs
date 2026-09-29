//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio.Shell;

using System;
using System.Threading.Tasks;

using XSharp.Project.ShadowDesigner;
using XSharpModel;

namespace XSharp.Project
{
    /// <summary>
    /// File-context-menu command for the shadow-designer bridge: after double-clicking a
    /// control in the shadow Designer (which lands the new handler stub in the companion
    /// project's C# files, not the real .prg), run this to detect what's new and write it
    /// into the real X# source. Visible only for a SDK-style project's .prg file that
    /// already has an open shadow companion project.
    /// </summary>
    [Command(PackageIds.idSyncEventHandlers)]
    internal sealed class CommandSyncEventHandlers : BaseCommand<CommandSyncEventHandlers>
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

            // The Designer-added stub/wiring can sit unsaved in an open document buffer --
            // both sync steps read the companion files from disk, not the live buffer.
            await VS.Commands.ExecuteAsync(KnownCommands.File_SaveAll);

            if (!ShadowDesignerBridge.TryResolveCompanionPaths(_currentFile, out var location, out string error))
            {
                await VS.MessageBox.ShowErrorAsync("X# WinForms Designer", error);
                return;
            }

            EventHandlerSync.SyncResult result;
            CompanionResourceSync.SyncResult resxResult;
            try
            {
                result = EventHandlerSync.Sync(location);
                resxResult = CompanionResourceSync.Sync(location);
            }
            catch (Exception ex)
            {
                await VS.MessageBox.ShowErrorAsync("X# WinForms Designer", ex.ToString());
                return;
            }

            if (!result.HasChanges && resxResult.CopiedFileNames.Count == 0)
            {
                await VS.StatusBar.ShowMessageAsync("Nothing new to sync -- no new handler stubs, event wiring, or resource files found.");
                return;
            }

            await VS.Commands.ExecuteAsync(KnownCommands.File_OpenFile, location.MainPrgPath);
            string resxText = resxResult.CopiedFileNames.Count > 0
                ? $", {resxResult.CopiedFileNames.Count} resource file(s)"
                : "";
            await VS.StatusBar.ShowMessageAsync(
                $"Synced {result.NewHandlerNames.Count} handler stub(s) and {result.NewWiringDescriptions.Count} event wiring statement(s){resxText}.");
        }
    }
}
