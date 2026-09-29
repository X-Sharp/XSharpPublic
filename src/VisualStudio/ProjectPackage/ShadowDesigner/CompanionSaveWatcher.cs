//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using EnvDTE;
using EnvDTE80;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Threading;
using Logger = XSharp.Project.Logger;

namespace XSharp.Project.ShadowDesigner
{
    /// <summary>
    /// Automatically runs the two manual sync commands (EventHandlerSync then
    /// DesignerChangesSync, in that order) whenever a companion project's
    /// Form1.cs/Form1.Designer.cs is saved, so Designer edits reach the real .prg without a
    /// manual right-click.
    ///
    /// "On save" specifically (via IVsRunningDocTableEvents.OnAfterSave), not an idle timer
    /// or tab-switch hook: both sync commands read the companion files from disk, not the
    /// live editor buffer -- the same reason the manual commands force-run File.SaveAll
    /// first. So on-save is the earliest moment the data is actually available, not an
    /// arbitrary cadence choice.
    ///
    /// Also intercepts the Designer navigating to the companion file's CODE view
    /// (event-handler creation/navigation) via OnBeforeDocumentWindowShow, and redirects to
    /// the real .prg instead -- see that method's own doc comment for how the Design view is
    /// told apart from the Code view.
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

        // Keyed by the companion project's folder -> every form's location known to live
        // there. A .resx the Designer writes (e.g. a newly added Form1.nl.resx from changing
        // the form's Language property) doesn't necessarily touch Form1.cs/Form1.Designer.cs
        // at all, so it's never a key in _watched above -- this lets OnAfterSave still
        // recognize "a resx under a known companion folder was just saved" and resolve which
        // form it belongs to by filename prefix, via CompanionResourceSync.
        private static readonly Dictionary<string, List<ShadowDesignerBridge.CompanionLocation>> _companionDirs =
            new Dictionary<string, List<ShadowDesignerBridge.CompanionLocation>>(StringComparer.OrdinalIgnoreCase);

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

            string companionDir = Path.GetDirectoryName(location.CompanionDesignerCsPath);
            if (!string.IsNullOrEmpty(companionDir))
            {
                if (!_companionDirs.TryGetValue(companionDir, out var locations))
                {
                    locations = new List<ShadowDesignerBridge.CompanionLocation>();
                    _companionDirs[companionDir] = locations;
                }
                locations.RemoveAll(l => string.Equals(l.MainPrgPath, location.MainPrgPath, StringComparison.OrdinalIgnoreCase));
                locations.Add(location);
            }
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
            if (string.IsNullOrEmpty(path))
            {
                return VSConstants.S_OK;
            }

            if (!_watched.TryGetValue(path, out var location))
            {
                // Not Form1.cs/Form1.Designer.cs -- still worth checking whether this is a
                // .resx living directly in a known companion folder (e.g. a new
                // Form1.nl.resx from changing the form's Language property), since that can
                // be saved without either code file ever becoming dirty.
                TryHandleResxOnlySave(path);
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
                CompanionResourceSync.Sync(location);
            }
            catch (Exception ex)
            {
                // Auto-sync is best-effort -- a failure here must not disrupt the save the
                // user just performed. The manual commands remain available as a fallback.
                Logger.Exception(ex, "CompanionSaveWatcher.OnAfterSave: auto-sync failed");
            }
            finally
            {
                _syncing = false;
            }
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Handles a save of a .resx file that isn't Form1.cs/Form1.Designer.cs itself --
        /// resolves which form it belongs to (by companion folder + filename prefix) and
        /// copies it to the real project, without running the code-sync steps that only
        /// apply to Form1.cs/Form1.Designer.cs saves.
        /// </summary>
        private void TryHandleResxOnlySave(string path)
        {
            if (!path.EndsWith(".resx", StringComparison.OrdinalIgnoreCase))
            {
                return;
            }
            string dir = Path.GetDirectoryName(path);
            if (string.IsNullOrEmpty(dir) || !_companionDirs.TryGetValue(dir, out var locations))
            {
                return;
            }
            string fileName = Path.GetFileName(path);
            var location = locations.FirstOrDefault(l =>
            {
                string className = Path.GetFileNameWithoutExtension(l.MainPrgPath);
                return fileName.StartsWith(className, StringComparison.OrdinalIgnoreCase);
            });
            if (location == null)
            {
                return;
            }

            _syncing = true;
            try
            {
                CompanionResourceSync.Sync(location);
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "CompanionSaveWatcher.TryHandleResxOnlySave: resx sync failed");
            }
            finally
            {
                _syncing = false;
            }
        }

        private string GetDocumentMoniker(uint docCookie)
        {
            int hr = _rdt.GetDocumentInfo(docCookie, out _, out _, out _, out string moniker, out _, out _, out _);
            return hr == VSConstants.S_OK ? moniker : null;
        }

        public int OnAfterAttributeChange(uint docCookie, uint grfAttribs) => VSConstants.S_OK;
        public int OnAfterDocumentWindowHide(uint docCookie, IVsWindowFrame pFrame) => VSConstants.S_OK;
        public int OnAfterFirstDocumentLock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining) => VSConstants.S_OK;

        /// <summary>
        /// Fires whenever any RDT document's window is about to show, including unrelated
        /// documents -- filtered down to the two watched companion files. When the Designer
        /// navigates to the companion's Code view (e.g. to show/create an event handler),
        /// this redirects the user to the real .prg instead, so the companion .cs never
        /// stays visible. The Designer's own Design-surface view of the same file must NOT be
        /// redirected -- see <see cref="IsDesignerView"/> for how the two are told apart.
        /// </summary>
        public int OnBeforeDocumentWindowShow(uint docCookie, int fFirstShow, IVsWindowFrame pFrame)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (_syncing)
            {
                return VSConstants.S_OK;
            }

            string path = GetDocumentMoniker(docCookie);
            if (string.IsNullOrEmpty(path) || !_watched.TryGetValue(path, out var location))
            {
                return VSConstants.S_OK;
            }

            if (IsDesignerView(pFrame))
            {
                return VSConstants.S_OK;
            }

            _syncing = true;

            EventHandlerSync.SyncResult result;
            try
            {
                // Same reasoning as OnAfterSave: the sync command reads from disk, and the
                // Designer's own navigation flow may have flushed its pending edits into the
                // companion buffer without that being on disk yet.
                _dte.ExecuteCommand("File.SaveAll");
                result = EventHandlerSync.Sync(location);
                CompanionResourceSync.Sync(location);
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "CompanionSaveWatcher.OnBeforeDocumentWindowShow: sync failed");
                _syncing = false;
                return VSConstants.S_OK;
            }

            if (result.NewHandlerNames.Count > 0)
            {
                // Newly-created handler stub: its name is already known from the sync
                // result, no live caret read needed -- close/redirect immediately, before
                // the frame ever paints.
                try
                {
                    ThreadHelper.JoinableTaskFactory.Run(() => RedirectNowAsync(location, pFrame, result.NewHandlerNames[0]));
                }
                catch (Exception ex)
                {
                    Logger.Exception(ex, "CompanionSaveWatcher.OnBeforeDocumentWindowShow: redirect failed");
                }
                finally
                {
                    _syncing = false;
                }
                return VSConstants.S_OK;
            }

            // Navigating to an already-existing handler: unlike the new-handler case above,
            // the target caret position isn't known yet -- it's set by the Designer's own
            // navigation call as a side effect of showing this same frame, so it isn't
            // available until that call has actually finished. Let this call return
            // normally (frame shows, navigation completes for real) and resolve/redirect
            // from a deferred continuation instead of doing it inline here. Trade-off,
            // accepted: this sub-case gets a brief, real flicker; the create-handler path
            // above is unaffected.
            //
            // Deliberately fire-and-forget (this call must return now so the frame can
            // finish showing) -- uses the package's own JoinableTaskFactory rather than the
            // static ThreadHelper one so VS can still track/join the pending operation.
            XSharpProjectPackage.XInstance.JoinableTaskFactory.RunAsync(async () =>
            {
                await Task.Yield();
                await XSharpProjectPackage.XInstance.JoinableTaskFactory.SwitchToMainThreadAsync();
                try
                {
                    string handlerName = TryGetCaretMethodName(pFrame, path);
                    await RedirectNowAsync(location, pFrame, handlerName);
                }
                catch (Exception ex)
                {
                    Logger.Exception(ex, "CompanionSaveWatcher.OnBeforeDocumentWindowShow: deferred redirect failed");
                }
                finally
                {
                    _syncing = false;
                }
            }).Task.FileAndForget("XSharp/ShadowDesigner/ExistingHandlerRedirect");
            return VSConstants.S_OK;
        }

        /// <summary>
        /// True when the given frame is the WinForms Designer's own Design-surface view of
        /// the companion file, false when it's the Code (text editor) view of the same file --
        /// see OnBeforeDocumentWindowShow's doc comment for how this was determined (both
        /// views share the same VSFPROPID_guidEditorType, so that property can't be used;
        /// VSFPROPID_EditorCaption's " [Design]" suffix reliably can). Fails safe: if the
        /// caption can't be read at all, treat it as the Designer view (don't redirect
        /// something we can't positively identify as the Code view).
        /// </summary>
        private static bool IsDesignerView(IVsWindowFrame frame)
        {
            try
            {
                frame.GetProperty((int)__VSFPROPID.VSFPROPID_EditorCaption, out object captionObj);
                string caption = captionObj as string;
                return caption != null && caption.IndexOf("[Design]", StringComparison.Ordinal) >= 0;
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "CompanionSaveWatcher.IsDesignerView: GetProperty(EditorCaption) failed, treating as Designer view (fail-safe)");
                return true;
            }
        }

        /// <summary>
        /// Closes the companion frame that was about to show and opens/positions the real
        /// .prg at the target handler (or just opens it, caret at file-open position, if
        /// handlerName is null). Sync-to-disk and handler-name resolution both happen in the
        /// caller (OnBeforeDocumentWindowShow) BEFORE this runs -- see its doc comment for
        /// why the new-handler and existing-handler cases resolve the name differently (one
        /// immediately, one via a deferred continuation).
        /// </summary>
        private async Task RedirectNowAsync(ShadowDesignerBridge.CompanionLocation location, IVsWindowFrame companionFrame, string handlerName)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            try
            {
                companionFrame.CloseFrame((uint)__FRAMECLOSE.FRAMECLOSE_NoSave);
            }
            catch (Exception ex)
            {
                // Fall through and still open the real .prg -- worst case the user sees both.
                Logger.Exception(ex, "CompanionSaveWatcher.RedirectNowAsync: CloseFrame failed");
            }

            _dte.ItemOperations.OpenFile(location.MainPrgPath, EnvDTE.Constants.vsViewKindTextView);

            if (handlerName == null)
            {
                Logger.Information("CompanionSaveWatcher.RedirectNowAsync: could not resolve a target handler -- left caret at file open position.");
            }
            else if (!await NavigateToMethodAsync(location.MainPrgPath, handlerName))
            {
                Logger.Error($"CompanionSaveWatcher.RedirectNowAsync: could not navigate to handler '{handlerName}' -- left caret at file open position.");
            }
        }

        /// <summary>
        /// Reads the companion frame's live caret position and finds the enclosing C# method
        /// at that position by parsing the companion file with Roslyn. Used only for the
        /// "navigating to an existing handler, nothing new to sync" case -- a newly created
        /// handler already has its name from EventHandlerSync.SyncResult directly.
        ///
        /// Gets the view via VSFPROPID_DocView + IVsCodeWindow.GetPrimaryView() rather than
        /// IVsWindowFrame.QueryViewInterface(IVsTextView), which doesn't work for this frame.
        /// </summary>
        private static string TryGetCaretMethodName(IVsWindowFrame companionFrame, string companionPath)
        {
            try
            {
                companionFrame.GetProperty((int)__VSFPROPID.VSFPROPID_DocView, out object docView);

                IVsTextView textView = docView as IVsTextView;
                if (textView == null && docView is IVsCodeWindow codeWindow)
                {
                    codeWindow.GetPrimaryView(out textView);
                }
                if (textView == null || textView.GetCaretPos(out int line, out int column) != VSConstants.S_OK)
                {
                    return null;
                }

                string source = File.ReadAllText(companionPath);
                var tree = CSharpSyntaxTree.ParseText(source);
                var sourceText = tree.GetText();
                if (line < 0 || line >= sourceText.Lines.Count)
                {
                    return null;
                }

                int position = Math.Min(sourceText.Lines[line].Start + Math.Max(0, column), sourceText.Length - 1);
                var enclosingMethod = tree.GetRoot().FindToken(position).Parent
                    ?.AncestorsAndSelf().OfType<MethodDeclarationSyntax>().FirstOrDefault();
                return enclosingMethod?.Identifier.Text;
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "CompanionSaveWatcher.TryGetCaretMethodName failed");
                return null;
            }
        }

        /// <summary>
        /// Positions the caret one line INSIDE "METHOD handlerName(...)" in the just-opened
        /// active document (the line right after the declaration), rather than on the
        /// declaration line itself -- lands inside the handler body for both a freshly
        /// generated stub (whose template's next line is always the "// TODO: implement"
        /// comment) and a typical single-line existing handler declaration. Falls back to the
        /// declaration line itself if there's no next line. Line-based text search rather than
        /// a real parse -- adequate given the name is unique in practice (freshly-inserted
        /// stubs are always appended before the last END CLASS; an existing handler's name
        /// came from a real parse of the companion file already), not meant as a
        /// general-purpose code navigator.
        ///
        /// _dte.ActiveDocument.Selection isn't necessarily populated the instant the document
        /// was opened (the editor view can still be finishing initialization) -- poll briefly
        /// rather than fail on the first null read.
        /// </summary>
        private async Task<bool> NavigateToMethodAsync(string prgPath, string handlerName)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            string[] lines = File.ReadAllLines(prgPath);
            var pattern = new Regex($@"\bMETHOD\s+{Regex.Escape(handlerName)}\s*\(", RegexOptions.IgnoreCase);
            int declLineIndex = Array.FindIndex(lines, l => pattern.IsMatch(l));
            if (declLineIndex < 0)
            {
                return false;
            }

            TextSelection selection = null;
            for (int attempt = 0; attempt < 20 && selection == null; attempt++)
            {
                selection = _dte.ActiveDocument?.Selection as TextSelection;
                if (selection == null)
                {
                    await Task.Delay(50);
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                }
            }

            if (selection == null)
            {
                Logger.Error($"CompanionSaveWatcher.NavigateToMethodAsync: gave up waiting for a TextSelection on {prgPath}.");
                return false;
            }

            int targetLine = (declLineIndex + 1 < lines.Length) ? declLineIndex + 2 : declLineIndex + 1; // 1-based; +2 = the line after the declaration
            selection.MoveToLineAndOffset(targetLine, 1);
            selection.SelectLine();
            return true;
        }

        public int OnBeforeLastDocumentUnlock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining) => VSConstants.S_OK;
    }
}
