//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using EnvDTE80;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace XSharp.Project.ShadowDesigner
{
    /// <summary>
    /// Automatically runs the two manual sync commands (EventHandlerSync then
    /// DesignerChangesSync, in that order -- the same order already confirmed working
    /// together manually) whenever a companion project's Form1.cs/Form1.Designer.cs is
    /// saved, so Designer edits reach the real .prg without a manual right-click.
    ///
    /// "On save" specifically (via IVsRunningDocTableEvents.OnAfterSave), not an idle timer
    /// or tab-switch hook: both sync commands already read the companion files from disk,
    /// not the live editor buffer -- the same reason the manual commands force-run
    /// File.SaveAll first. So on-save is the earliest moment the data is actually available,
    /// not an arbitrary cadence choice.
    ///
    /// NOT YET CONFIRMED whether the out-of-process WinForms Designer auto-saves its dirty
    /// buffer on tab-switch-away the way real C#/VB Designer surfaces do. If it doesn't, a
    /// user who switches tabs without an explicit Ctrl+S/Save All won't get auto-synced --
    /// they'd still see it work via the existing manual commands, which force a save
    /// themselves, so this is a UX gap, not a correctness gap.
    /// </summary>
    internal sealed class CompanionSaveWatcher : IVsRunningDocTableEvents
    {
        private static readonly object _lock = new object();
        private static CompanionSaveWatcher _instance;

        // Keyed by normalized full path of either companion file (Form1.cs or
        // Form1.Designer.cs) -> the location to sync. A single process-wide table since the
        // RDT event itself is process/session-wide, not per-project.
        private static readonly Dictionary<string, ShadowDesignerBridge.CompanionLocation> _watched =
            new Dictionary<string, ShadowDesignerBridge.CompanionLocation>(StringComparer.OrdinalIgnoreCase);

        private readonly IVsRunningDocumentTable _rdt;
        private readonly DTE2 _dte;
        private bool _syncing;

        private CompanionSaveWatcher(IVsRunningDocumentTable rdt, DTE2 dte)
        {
            _rdt = rdt;
            _dte = dte;
            _rdt.AdviseRunningDocTableEvents(this, out _);
        }

        /// <summary>
        /// Registers a companion's paths for auto-sync-on-save, lazily advising the RDT the
        /// first time this is called in the session. Safe to call every time
        /// ShadowDesignerBridge.TryOpen succeeds -- idempotent (overwrites any existing
        /// entry for the same paths).
        /// </summary>
        public static void Watch(ProjectNode projectMgr, ShadowDesignerBridge.CompanionLocation location)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (_instance == null)
            {
                lock (_lock)
                {
                    if (_instance == null)
                    {
                        var rdt = projectMgr.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable;
                        var dte = projectMgr.GetService(typeof(SDTE)) as DTE2;
                        if (rdt == null || dte == null)
                        {
                            return;
                        }
                        _instance = new CompanionSaveWatcher(rdt, dte);
                    }
                }
            }
            _watched[location.CompanionFormCsPath] = location;
            _watched[location.CompanionDesignerCsPath] = location;
        }

        public int OnAfterSave(uint docCookie)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (_syncing)
            {
                // Re-entrancy guard: File.SaveAll below can itself trigger further
                // OnAfterSave callbacks (for other dirty documents) -- without this, that
                // could recurse indefinitely.
                return VSConstants.S_OK;
            }

            string path = GetDocumentMoniker(docCookie);
            if (string.IsNullOrEmpty(path) || !_watched.TryGetValue(path, out var location))
            {
                return VSConstants.S_OK;
            }

            _syncing = true;
            try
            {
                // The save that triggered this event only guarantees ONE of the two
                // companion files is current on disk -- the other may still be dirty in a
                // different editor tab (e.g. a new handler stub in Form1.cs while only
                // Form1.Designer.cs was just saved). Force both current first, same safety
                // net the manual sync commands already use.
                _dte.ExecuteCommand("File.SaveAll");
                EventHandlerSync.Sync(location);
                DesignerChangesSync.Sync(location);
            }
            catch
            {
                // Auto-sync is best-effort -- a failure here must not disrupt the save the
                // user just performed. The manual commands remain available as a fallback.
            }
            finally
            {
                _syncing = false;
            }
            return VSConstants.S_OK;
        }

        private string GetDocumentMoniker(uint docCookie)
        {
            int hr = _rdt.GetDocumentInfo(docCookie, out _, out _, out _, out string moniker, out _, out _, out _);
            return hr == VSConstants.S_OK ? moniker : null;
        }

        public int OnAfterAttributeChange(uint docCookie, uint grfAttribs) => VSConstants.S_OK;
        public int OnAfterDocumentWindowHide(uint docCookie, IVsWindowFrame pFrame) => VSConstants.S_OK;
        public int OnAfterFirstDocumentLock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining) => VSConstants.S_OK;
        public int OnBeforeDocumentWindowShow(uint docCookie, int fFirstShow, IVsWindowFrame pFrame) => VSConstants.S_OK;
        public int OnBeforeLastDocumentUnlock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining) => VSConstants.S_OK;
    }
}
