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

using XSharp.ProjectSystem.ShadowDesigner;
using XSharpModel;

namespace XSharp.Project
{
    /// <summary>
    /// File-context-menu command for the shadow-designer bridge: after editing properties or
    /// adding/removing/reordering controls in the shadow Designer, run this to fully
    /// regenerate the real Form1.Designer.prg from the companion project's current state.
    /// Visible only for a SDK-style project's .prg file that already has an open shadow
    /// companion project.
    /// </summary>
    [Command(PackageIds.idSyncDesignerChanges)]
    internal sealed class CommandSyncDesignerChanges : BaseCommand<CommandSyncDesignerChanges>
    {
        private string _currentPath;
        private XProject _currentProject;

        protected override void BeforeQueryStatus(EventArgs e)
        {
            base.BeforeQueryStatus(e);
            _currentPath = null;
            _currentProject = null;
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
                    // SDK-style projects, loaded by the CPS project system
                    var xproject = XSolution.FindFile(file.FullPath)?.Project;
                    bool sdkProject = ShadowDesignerBridge.IsCpsProject(xproject);
                    if (sdkProject && ShadowDesignerBridge.HasDesignerFile(file.FullPath))
                    {
                        _currentPath = file.FullPath;
                        _currentProject = xproject;
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
            if (_currentPath == null) return;

            await VS.Commands.ExecuteAsync(KnownCommands.File_SaveAll);

            if (!ShadowDesignerBridge.TryResolveCompanionPaths(_currentPath, _currentProject, out var location, out string error))
            {
                await VS.MessageBox.ShowErrorAsync("X# WinForms Designer", error);
                return;
            }

            DesignerChangesSync.SyncResult result;
            CompanionResourceSync.SyncResult resxResult;
            try
            {
                result = DesignerChangesSync.Sync(location);
                resxResult = CompanionResourceSync.Sync(location);
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
            string resxText = resxResult.CopiedFileNames.Count > 0
                ? $" Synced {resxResult.CopiedFileNames.Count} resource file(s)."
                : "";
            await VS.StatusBar.ShowMessageAsync(
                $"Regenerated Form1.Designer.prg: {result.FieldCount} field(s), {result.StatementCount} statement(s).{skippedText}{resxText}");
        }
    }
}
