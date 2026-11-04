//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.TextManager.Interop;
using XSharp.LanguageService.FindReferences;
using XSharp.Support;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService.Rename
{
    /// <summary>What a rename did. When <see cref="Error"/> is set, nothing was changed.</summary>
    internal sealed class RenameResult
    {
        internal int Changed { get; set; }
        internal IReadOnlyList<string> Files { get; set; } = Array.Empty<string>();
        internal string Error { get; set; }
    }

    /// <summary>
    /// Applies a <see cref="RenamePlan"/> through editor buffers, in one linked undo transaction. Files shown
    /// in an editor window are changed in their buffer and stay unsaved. All other files are loaded in an invisible
    /// editor, changed and saved; the invisible editors are kept until the solution closes so that Ctrl+Z reaches
    /// them, and a file that such an undo (or redo) changes is saved again, until it is opened in a window.
    /// </summary>
    internal static class RenameApplier
    {
        private static readonly List<ClosedFile> _keptFiles = new List<ClosedFile>();
        private static bool _solutionEventsHooked;

        private sealed class ClosedFile
        {
            internal string Path;
            internal IVsInvisibleEditor Editor;
            internal IVsPersistDocData DocData;
            internal ITextBuffer Buffer;
            internal bool SaveOnChange;
        }

        internal static async System.Threading.Tasks.Task<RenameResult> ApplyAsync(RenamePlan plan, string oldName, string newName)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var componentModel = await VS.Services.GetComponentModelAsync();
            var adapters = componentModel.GetService<IVsEditorAdaptersFactoryService>();
            var invisibleEditors = await VS.GetRequiredServiceAsync<SVsInvisibleEditorManager, IVsInvisibleEditorManager>();
            var undo = await VS.GetRequiredServiceAsync<SVsLinkedUndoTransactionManager, IVsLinkedUndoTransactionManager>();
            var rdt = new RunningDocumentTable(ServiceProvider.GlobalProvider);
            HookSolutionEvents();
            _undoHistories = componentModel.GetService<ITextUndoHistoryRegistry>();

            // 1. open (in a window) or closed; closed files must exist and be writable before anything is loaded
            var openFiles = new HashSet<string>(plan.Files.Where(HasWindow), StringComparer.OrdinalIgnoreCase);
            foreach (var file in plan.Files.Where(f => !openFiles.Contains(f)))
            {
                if (!File.Exists(file))
                    return new RenameResult { Error = $"'{file}' does not exist. Nothing was renamed." };
                if ((File.GetAttributes(file) & FileAttributes.ReadOnly) != 0)
                    return new RenameResult { Error = $"'{file}' is read-only. Nothing was renamed." };
            }

            // 2. a buffer per file, and the old name still at every place
            var buffers = new List<(string File, ITextBuffer Buffer, ClosedFile Closed, string Original)>();
            var loaded = new List<ClosedFile>();
            string error = null;
            foreach (var file in plan.Files)
            {
                ITextBuffer buffer = null;
                ClosedFile closed = null;
                if (openFiles.Contains(file))
                {
                    if (rdt.FindDocument(file) is IVsTextBuffer open)
                        buffer = adapters.GetDocumentBuffer(open);
                }
                else
                {
                    closed = Load(invisibleEditors, adapters, file);
                    if (closed != null && !_keptFiles.Contains(closed))
                        loaded.Add(closed);
                    buffer = closed?.Buffer;
                }
                if (buffer == null)
                {
                    error = $"'{file}' cannot be opened. Nothing was renamed.";
                    break;
                }
                var text = buffer.CurrentSnapshot.GetText();
                if (plan.Changes.Any(h => SameFile(h.FilePath, file) && !RenamePlanner.StillMatches(text, h, oldName)))
                {
                    error = $"'{file}' changed since the search. Nothing was renamed.";
                    break;
                }
                buffers.Add((file, buffer, closed, text));
            }
            if (error != null)
            {
                Release(loaded);
                return new RenameResult { Error = error };
            }

            // 3. all changes in one undo step; closed files are saved only when every edit succeeded
            var saved = new List<ClosedFile>();
            // not mdtGlobal: VS saves every document of a global transaction, also the open ones; the linked undo
            // reaches the hidden buffers of the closed files from any open file that took part (the origin always does)
            ErrorHandler.ThrowOnFailure(undo.OpenLinkedUndo((uint)LinkedTransactionFlags.mdtDefault, $"Rename '{oldName}' to '{newName}'"));
            try
            {
                foreach (var (file, buffer, _, _) in buffers)
                {
                    using (var edit = buffer.CreateEdit())
                    {
                        foreach (var hit in plan.Changes.Where(h => SameFile(h.FilePath, file)))
                            edit.Replace(new Span(hit.Start, hit.Length), newName);
                        edit.Apply();
                    }
                }
                foreach (var closed in buffers.Select(b => b.Closed).Where(c => c != null))
                {
                    Save(closed);
                    saved.Add(closed);
                }
                ErrorHandler.ThrowOnFailure(undo.CloseLinkedUndo());
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "Rename failed");
                undo.AbortLinkedUndo();
                // do not rely on the abort alone: every buffer must hold its old text again
                foreach (var (file, buffer, _, original) in buffers)
                {
                    try
                    {
                        if (buffer.CurrentSnapshot.GetText() != original)
                        {
                            using (var edit = buffer.CreateEdit())
                            {
                                edit.Replace(new Span(0, buffer.CurrentSnapshot.Length), original);
                                edit.Apply();
                            }
                        }
                    }
                    catch (Exception inner)
                    {
                        Logger.Exception(inner, $"Rename: restoring {file} failed");
                    }
                }
                // files saved with the new name before the failure get the old text back on disk
                foreach (var closed in saved)
                {
                    try { Save(closed); } catch (Exception inner) { Logger.Exception(inner, $"Rename: restoring {closed.Path} failed"); }
                }
                Release(loaded);
                return new RenameResult { Error = $"Rename failed: {ex.Message}. Nothing was renamed." };
            }

            // 4. keep the invisible editors for undo, and let the code model see the new name in the closed files
            foreach (var closed in buffers.Select(b => b.Closed).Where(c => c != null))
            {
                Keep(closed);
                var xfile = XSolution.FindFile(closed.Path);
                xfile?.Project?.WalkFile(xfile);
            }
            Logger.Information($"Rename '{oldName}' to '{newName}': {plan.Changes.Count} changes in {plan.Files.Count} files ({openFiles.Count} open)");
            return new RenameResult { Changed = plan.Changes.Count, Files = plan.Files };
        }

        private static ITextUndoHistoryRegistry _undoHistories;

        /// <summary>Is the file shown in an editor window? Documents without a window are changed like closed files.</summary>
        private static bool HasWindow(string file)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            return VsShellUtilities.IsDocumentOpen(ServiceProvider.GlobalProvider, file, Guid.Empty, out _, out _, out var frame) && frame != null;
        }

        /// <summary>Gives back the invisible editors of a rename that did not happen.</summary>
        private static void Release(IEnumerable<ClosedFile> files)
        {
            foreach (var closed in files)
            {
                try
                {
                    if (closed.Editor != null && Marshal.IsComObject(closed.Editor))
                        Marshal.ReleaseComObject(closed.Editor);
                }
                catch (Exception ex)
                {
                    Logger.Exception(ex, $"Rename: releasing {closed.Path} failed");
                }
            }
        }

        /// <summary>Renames a file in its project, so that the project file and open windows follow.</summary>
        internal static async System.Threading.Tasks.Task<bool> RenameFileAsync(string filePath, string newFileNameWithoutExtension)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            try
            {
                var dte = await VS.GetRequiredServiceAsync<EnvDTE.DTE, EnvDTE.DTE>();
                var item = dte.Solution.FindProjectItem(filePath);
                if (item == null)
                    return false;
                item.Name = newFileNameWithoutExtension + Path.GetExtension(filePath);
                await ParseOpenBufferAsync(Path.Combine(Path.GetDirectoryName(filePath), item.Name));
                return true;
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, $"Rename: renaming {filePath} failed");
                return false;
            }
        }

        /// <summary>
        /// The project reads a renamed file from disk. An open file keeps the rename unsaved, so the code model
        /// would still see the old name under the new path: parse the editor text instead. The editor (classifier,
        /// outlining, error tags) keeps the XFile of the old path: move that object to the new path.
        /// </summary>
        private static async Task ParseOpenBufferAsync(string filePath)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var rdt = new RunningDocumentTable(ServiceProvider.GlobalProvider);
            if (!(rdt.FindDocument(filePath) is IVsTextBuffer open))
                return;
            var componentModel = await VS.Services.GetComponentModelAsync();
            var buffer = componentModel.GetService<IVsEditorAdaptersFactoryService>().GetDocumentBuffer(open);
            if (buffer == null)
                return;
            var xfile = buffer.GetFile();
            if (xfile != null && !SameFile(xfile.FullPath, filePath))
                xfile.Rename(filePath);
            xfile = xfile ?? XSolution.FindFile(filePath);
            xfile?.ParseContents(buffer.CurrentSnapshot.GetText());
        }

        private static bool SameFile(string a, string b) => string.Equals(a, b, StringComparison.OrdinalIgnoreCase);

        private static ClosedFile Load(IVsInvisibleEditorManager manager, IVsEditorAdaptersFactoryService adapters, string file)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var kept = _keptFiles.FirstOrDefault(k => SameFile(k.Path, file));
            if (kept != null)
                return kept;
            if (ErrorHandler.Failed(manager.RegisterInvisibleEditor(file, null, (uint)_EDITORREGFLAGS.RIEF_ENABLECACHING, null, out var editor)))
                return null;
            var iid = typeof(IVsTextLines).GUID;
            if (ErrorHandler.Failed(editor.GetDocData(0, ref iid, out var ptr)) || ptr == IntPtr.Zero)
                return null;
            try
            {
                var lines = Marshal.GetObjectForIUnknown(ptr) as IVsTextLines;
                var buffer = lines == null ? null : adapters.GetDocumentBuffer(lines);
                return buffer == null ? null : new ClosedFile { Path = file, Editor = editor, DocData = lines as IVsPersistDocData, Buffer = buffer };
            }
            finally
            {
                Marshal.Release(ptr);
            }
        }

        private static void Save(ClosedFile closed)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (closed.DocData == null)
                throw new InvalidOperationException($"'{closed.Path}' cannot be saved");
            ErrorHandler.ThrowOnFailure(closed.DocData.SaveDocData(VSSAVEFLAGS.VSSAVE_SilentSave, out _, out var cancelled));
            if (cancelled != 0)
                throw new InvalidOperationException($"saving '{closed.Path}' was cancelled");
        }

        /// <summary>An undo (or redo) of the rename changes the closed file again: save it, it has no window.</summary>
        private static void Keep(ClosedFile closed)
        {
            if (_keptFiles.Contains(closed))
                return;
            _keptFiles.Add(closed);
            closed.SaveOnChange = true;
            ITextUndoHistory history = null;
            _undoHistories?.TryGetHistory(closed.Buffer, out history);
            closed.Buffer.PostChanged += (s, e) =>
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                if (!closed.SaveOnChange)
                    return;
                // opened in an editor: from now on the user saves it, as any other file
                if (HasWindow(closed.Path))
                {
                    closed.SaveOnChange = false;
                    return;
                }
                // only the undo or redo of the rename, never another edit of the hidden buffer
                if (history != null && history.State == TextUndoHistoryState.Idle)
                    return;
                try
                {
                    Save(closed);
                    var xfile = XSolution.FindFile(closed.Path);
                    xfile?.Project?.WalkFile(xfile);
                }
                catch (Exception ex)
                {
                    Logger.Exception(ex, $"Rename: saving {closed.Path} after undo failed");
                }
            };
        }

        private static void HookSolutionEvents()
        {
            if (_solutionEventsHooked)
                return;
            _solutionEventsHooked = true;
            VS.Events.SolutionEvents.OnBeforeCloseSolution += () =>
            {
                foreach (var closed in _keptFiles)
                    closed.SaveOnChange = false;
                _keptFiles.Clear();
            };
        }
    }
}
