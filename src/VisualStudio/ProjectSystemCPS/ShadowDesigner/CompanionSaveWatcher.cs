//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Moved from ProjectPackage, which suppresses VSTHRD010 for the whole project: the UI thread
// requirements of this code are handled explicitly (ThreadHelper) and were not rewritten.
#pragma warning disable VSTHRD010

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Threading;

using Community.VisualStudio.Toolkit;


using static XSharp.ProjectSystem.ShadowDesigner.ShadowDesignerBridge;


namespace XSharp.ProjectSystem.ShadowDesigner
{
    /// <summary>
    /// Automatically runs the two manual sync commands (EventHandlerSync then
    /// DesignerChangesSync, in that order) whenever a companion project's
    /// Form1.cs/Form1.Designer.cs is saved, so Designer edits reach the real .prg without a
    /// manual right-click.
    ///
    /// "On save" specifically (via IVsRunningDocTableEvents.OnAfterSave), not an idle timer
    /// or tab-switch hook: both sync commands read the companion files from disk, not the
    /// live editor buffer, which is also why the other companion documents are saved first
    /// (SaveCompanionDocuments). So on-save is the earliest moment the data is actually
    /// available, not an arbitrary cadence choice.
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
        private static readonly ConcurrentDictionary<string, CompanionLocation> _watched =
            new ConcurrentDictionary<string, CompanionLocation>(StringComparer.OrdinalIgnoreCase);

        // Keyed by the companion project's folder -> every form's location known to live
        // there. A .resx the Designer writes (e.g. a newly added Form1.nl.resx from changing
        // the form's Language property) doesn't necessarily touch Form1.cs/Form1.Designer.cs
        // at all, so it's never a key in _watched above -- this lets OnAfterSave still
        // recognize "a resx under a known companion folder was just saved" and resolve which
        // form it belongs to by filename prefix, via CompanionResourceSync.
        private static readonly ConcurrentDictionary<string, List<CompanionLocation>> _companionDirs =
            new ConcurrentDictionary<string, List<CompanionLocation>>(StringComparer.OrdinalIgnoreCase);

        private readonly IVsRunningDocumentTable _rdt;
        private bool _syncing;

        private CompanionSaveWatcher(IVsRunningDocumentTable rdt)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            _rdt = rdt;
            ErrorHandler.ThrowOnFailure(_rdt.AdviseRunningDocTableEvents(this, out _));
            // Make sure that Debug/Start saves documents first to trigger generating the PRG from the CS file
            VS.Commands.InterceptAsync(VSConstants.VSStd97CmdID.Start, () =>
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                SaveDirtyDocuments();
                return CommandProgression.Continue;
            }).FileAndForget("XSharp/ShadowDesigner/Start");

            VS.Events.SolutionEvents.OnAfterCloseSolution += SolutionEvents_OnAfterCloseSolution;

        }


        private void SolutionEvents_OnAfterCloseSolution()
        {
            _watched.Clear();
            _companionDirs.Clear();
        }

        /// <summary>
        /// Registers a companion's paths for auto-sync-on-save, lazily advising the RDT the
        /// first time this is called in the session. Safe to call every time
        /// ShadowDesignerBridge.TryOpenAsync prepares the companion -- idempotent (overwrites any existing
        /// entry for the same paths).
        /// </summary>
        internal static void Watch(IServiceProvider services, CompanionLocation location)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (_instance == null)
            {
                lock (_lock)
                {
                    if (_instance == null)
                    {
                        var rdt = services.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable
                            ?? throw new InvalidOperationException("Could not obtain the running document table for shadow designer synchronization.");
                        _instance = new CompanionSaveWatcher(rdt);
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
                    locations = new List<CompanionLocation>();
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
                // Saving other dirty RDT documents triggers further OnAfterSave callbacks.
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
                // Form1.Designer.cs was just saved). Force both current first.
                SaveCompanionDocuments(location);
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
        static bool saving = false;
        /// <summary>
        /// Saves every dirty document, before Debug/Start only (VS saves them for the build anyway). Not used by the
        /// syncs: called from OnAfterSave, saving the solution as well failed with a recursive save error, see
        /// SaveCompanionDocuments.
        /// </summary>
        private void SaveDirtyDocuments()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
			// Prevent recursive calls
            if (! saving)
            {
                saving = true;
                try
                {
                    int hr = _rdt.SaveDocuments((uint)__VSRDTSAVEOPTIONS.RDTSAVEOPT_SaveIfDirty,
                        null, VSConstants.VSITEMID_NIL, VSConstants.VSCOOKIE_NIL);
                    ErrorHandler.ThrowOnFailure(hr);
                    if (hr != VSConstants.S_OK)
                    {
                        throw new InvalidOperationException("Saving documents was cancelled; shadow designer synchronization was not performed.");
                    }
                }
                finally
                {
                    // Also after a failed or cancelled save, otherwise nothing is saved anymore
                    saving = false;
                }
            }
        }

        /// <summary>
        /// Saves the companion's own documents that are still dirty (the other code file, a resx), since the syncs
        /// read them from disk. Deliberately not all documents (File.SaveAll, SaveDirtyDocuments): that also saved
        /// the user's other documents and unsaved solution changes (which is how the companion ended up in the saved
        /// solution file), and saving the solution from inside OnAfterSave failed with "A recursive save operation
        /// is in progress for '....slnx'" (VS 2026, after adding a Solution Folder).
        /// </summary>
        private void SaveCompanionDocuments(CompanionLocation location)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            string companionDir = Path.GetDirectoryName(location.CompanionDesignerCsPath);
            if (string.IsNullOrEmpty(companionDir) ||
                ErrorHandler.Failed(_rdt.GetRunningDocumentsEnum(out IEnumRunningDocuments documents)) || documents == null)
            {
                return;
            }
            // Collected first: saving while enumerating could change the table
            var toSave = new List<uint>();
            var cookies = new uint[1];
            while (documents.Next(1, cookies, out uint fetched) == VSConstants.S_OK && fetched == 1)
            {
                string path = GetDocumentMoniker(cookies[0]);
                if (path != null &&
                    string.Equals(Path.GetDirectoryName(path), companionDir, StringComparison.OrdinalIgnoreCase) &&
                    (path.EndsWith(".cs", StringComparison.OrdinalIgnoreCase) || path.EndsWith(".resx", StringComparison.OrdinalIgnoreCase)))
                {
                    toSave.Add(cookies[0]);
                }
            }
            foreach (uint cookie in toSave)
            {
                if (ErrorHandler.Succeeded(_rdt.GetDocumentInfo(cookie, out _, out _, out _, out string path, out IVsHierarchy hierarchy, out uint itemId, out _)))
                {
                    int hr = _rdt.SaveDocuments((uint)__VSRDTSAVEOPTIONS.RDTSAVEOPT_SaveIfDirty, hierarchy, itemId, cookie);
                    if (ErrorHandler.Failed(hr))
                    {
                        Logger.Information($"CompanionSaveWatcher: could not save {path}, hr=0x{hr:X8}");
                    }
                }
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
                SaveCompanionDocuments(location);
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
            // finish showing) -- ThreadHelper.JoinableTaskFactory lets VS track/join the pending operation.
            // VSSDK007 wants a package JoinableTaskFactory; this static, package independent code uses
            // ThreadHelper and hands the task to FileAndForget (fault reporting) instead.
#pragma warning disable VSSDK007
            ThreadHelper.JoinableTaskFactory.RunAsync(async () =>
            {
                await Task.Yield();
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
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
            }).FileAndForget("XSharp/ShadowDesigner/ExistingHandlerRedirect");
#pragma warning restore VSSDK007
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
        private async Task RedirectNowAsync(CompanionLocation location, IVsWindowFrame companionFrame, string handlerName)
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

            ReloadOpenDocument(location.MainPrgPath);
            VsShellUtilities.OpenDocument(ServiceProvider.GlobalProvider, location.MainPrgPath,
                VSConstants.LOGVIEWID.TextView_guid, out _, out _, out IVsWindowFrame frame);

            if (handlerName == null)
            {
                Logger.Information("CompanionSaveWatcher.RedirectNowAsync: could not resolve a target handler -- left caret at file open position.");
            }
            else if (!await NavigateToMethodAsync(frame, location.MainPrgPath, handlerName))
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
        /// source document (the line right after the declaration), rather than on the
        /// declaration line itself -- lands inside the handler body for both a freshly
        /// generated stub (whose template's next line is always the "// TODO: implement"
        /// comment) and a typical single-line existing handler declaration. Falls back to the
        /// declaration line itself if there's no next line. Line-based text search rather than
        /// a real parse -- adequate given the name is unique in practice (freshly-inserted
        /// stubs are always appended before the last END CLASS; an existing handler's name
        /// came from a real parse of the companion file already), not meant as a
        /// general-purpose code navigator.
        ///
        /// The returned frame's text view may still be initializing; poll that frame briefly
        /// rather than navigating whichever document happens to be active.
        ///
        /// The declaration is searched in the editor buffer, not in the file: a new stub is
        /// written to disk by EventHandlerSync, and a .prg that is already open is only
        /// reloaded by VS a moment later. A line number taken from the file then pointed past
        /// the end of the old, shorter buffer (caret at the end of the file, while a second
        /// try worked). RedirectNowAsync therefore reloads the document first
        /// (ReloadOpenDocument); the buffer is still polled briefly in case that was skipped.
        /// </summary>
        private async Task<bool> NavigateToMethodAsync(IVsWindowFrame frame, string prgPath, string handlerName)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var pattern = new Regex($@"\bMETHOD\s+{Regex.Escape(handlerName)}\s*\(", RegexOptions.IgnoreCase);
            if (!File.ReadLines(prgPath).Any(l => pattern.IsMatch(l)))
            {
                return false;
            }

            IVsTextView textView = null;
            for (int attempt = 0; attempt < 20 && textView == null; attempt++)
            {
                textView = VsShellUtilities.GetTextView(frame);
                if (textView == null)
                {
                    await Task.Delay(50);
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                }
            }

            if (textView == null)
            {
                Logger.Error($"CompanionSaveWatcher.NavigateToMethodAsync: gave up waiting for the text view of {prgPath}.");
                return false;
            }

            ErrorHandler.ThrowOnFailure(textView.GetBuffer(out IVsTextLines buffer));
            int declLineIndex = -1;
            int lineCount = 0;
            const int maxAttempts = 40;
            for (int attempt = 0; attempt < maxAttempts; attempt++)
            {
                ErrorHandler.ThrowOnFailure(buffer.GetLineCount(out lineCount));
                declLineIndex = FindLine(buffer, lineCount, pattern);
                if (declLineIndex >= 0)
                {
                    if (attempt > 0)
                    {
                        Logger.Information($"CompanionSaveWatcher.NavigateToMethodAsync: waited {attempt * 50} ms for {prgPath} to be reloaded");
                    }
                    break;
                }
                await Task.Delay(50);
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            }
            if (declLineIndex < 0)
            {
                Logger.Error($"CompanionSaveWatcher.NavigateToMethodAsync: '{handlerName}' not found in the editor buffer of {prgPath} after {maxAttempts * 50} ms.");
                return false;
            }

            int targetLine = (declLineIndex + 1 < lineCount) ? declLineIndex + 1 : declLineIndex;
            int endLine = targetLine;
            int endColumn;
            if (targetLine + 1 < lineCount)
            {
                endLine++;
                endColumn = 0;
            }
            else
            {
                ErrorHandler.ThrowOnFailure(buffer.GetLengthOfLine(targetLine, out endColumn));
            }
            ErrorHandler.ThrowOnFailure(textView.SetSelection(targetLine, 0, endLine, endColumn));
            ErrorHandler.ThrowOnFailure(textView.EnsureSpanVisible(new TextSpan
            {
                iStartLine = targetLine,
                iStartIndex = 0,
                iEndLine = endLine,
                iEndIndex = endColumn,
            }));
            return true;
        }

        /// <summary>
        /// Reloads <paramref name="path"/> from disk when it is open in an editor without unsaved changes. EventHandlerSync
        /// writes new handler stubs to the file; VS reloads an open document only when the UI thread is idle, which
        /// never happens while the new-handler redirect runs (JoinableTaskFactory.Run in OnBeforeDocumentWindowShow),
        /// so NavigateToMethodAsync would search a buffer without the new stub. A document with unsaved changes is
        /// left alone: reloading would lose them.
        /// </summary>
        private void ReloadOpenDocument(string path)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (ErrorHandler.Failed(_rdt.FindAndLockDocument((uint)_VSRDTFLAGS.RDT_NoLock, path,
                out _, out _, out IntPtr docDataPtr, out _)) || docDataPtr == IntPtr.Zero)
            {
                return;
            }
            try
            {
                if (System.Runtime.InteropServices.Marshal.GetObjectForIUnknown(docDataPtr) is IVsPersistDocData docData &&
                    ErrorHandler.Succeeded(docData.IsDocDataDirty(out int dirty)) && dirty == 0)
                {
                    // The file change notification of the sync's own write would reload it a second time
                    int hr = docData.ReloadDocData((uint)_VSRELOADDOCDATA.RDD_IgnoreNextFileChange);
                    Logger.Information($"CompanionSaveWatcher.ReloadOpenDocument: reloaded {path}, hr=0x{hr:X8}");
                }
            }
            catch (Exception ex)
            {
                // Best-effort: NavigateToMethodAsync still polls the buffer
                Logger.Exception(ex, $"CompanionSaveWatcher.ReloadOpenDocument: could not reload {path}");
            }
            finally
            {
                System.Runtime.InteropServices.Marshal.Release(docDataPtr);
            }
        }

        /// <summary>The 0-based index of the first buffer line matching <paramref name="pattern"/>, or -1.</summary>
        private static int FindLine(IVsTextLines buffer, int lineCount, Regex pattern)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            for (int line = 0; line < lineCount; line++)
            {
                if (ErrorHandler.Succeeded(buffer.GetLengthOfLine(line, out int length)) &&
                    ErrorHandler.Succeeded(buffer.GetLineText(line, 0, line, length, out string text)) &&
                    pattern.IsMatch(text))
                {
                    return line;
                }
            }
            return -1;
        }

        public int OnBeforeLastDocumentUnlock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining) => VSConstants.S_OK;
    }
}
