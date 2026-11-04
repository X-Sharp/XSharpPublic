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
    /// Applies a <see cref="RenamePlan"/> through editor buffers, in one global linked undo transaction. Open files
    /// are changed in their buffer and stay unsaved. Closed files are loaded in an invisible editor, changed and
    /// saved; the invisible editors are kept until the solution closes so that Ctrl+Z reaches them, and a file that
    /// such an undo changes is saved again.
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

            // 1. a buffer per file, and the old name still at every place
            var buffers = new List<(string File, ITextBuffer Buffer, ClosedFile Closed)>();
            foreach (var file in plan.Files)
            {
                ITextBuffer buffer = null;
                ClosedFile closed = null;
                if (rdt.FindDocument(file) is IVsTextBuffer open)
                {
                    buffer = adapters.GetDocumentBuffer(open);
                }
                else
                {
                    if ((File.GetAttributes(file) & FileAttributes.ReadOnly) != 0)
                        return new RenameResult { Error = $"'{file}' is read-only. Nothing was renamed." };
                    closed = Load(invisibleEditors, adapters, file);
                    buffer = closed?.Buffer;
                }
                if (buffer == null)
                    return new RenameResult { Error = $"'{file}' cannot be opened. Nothing was renamed." };
                var text = buffer.CurrentSnapshot.GetText();
                if (plan.Changes.Any(h => SameFile(h.FilePath, file) && !RenamePlanner.StillMatches(text, h, oldName)))
                    return new RenameResult { Error = $"'{file}' changed since the search. Nothing was renamed." };
                buffers.Add((file, buffer, closed));
            }

            // 2. all changes in one undo step; closed files are saved only when every edit succeeded
            var saved = new List<ClosedFile>();
            ErrorHandler.ThrowOnFailure(undo.OpenLinkedUndo((uint)LinkedTransactionFlags2.mdtGlobal, $"Rename '{oldName}' to '{newName}'"));
            try
            {
                foreach (var (file, buffer, _) in buffers)
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
                // the aborted edits are gone from the buffers: write the old text back where it was saved already
                foreach (var closed in saved)
                {
                    try { Save(closed); } catch (Exception inner) { Logger.Exception(inner, $"Rename: restoring {closed.Path} failed"); }
                }
                return new RenameResult { Error = $"Rename failed: {ex.Message}. Nothing was renamed." };
            }

            // 3. keep the invisible editors for undo, and let the code model see the new name in the closed files
            foreach (var closed in buffers.Select(b => b.Closed).Where(c => c != null))
            {
                Keep(closed);
                var xfile = XSolution.FindFile(closed.Path);
                xfile?.Project?.WalkFile(xfile);
            }
            return new RenameResult { Changed = plan.Changes.Count, Files = plan.Files };
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
                return true;
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, $"Rename: renaming {filePath} failed");
                return false;
            }
        }

        private static bool SameFile(string a, string b) => string.Equals(a, b, StringComparison.OrdinalIgnoreCase);

        private static ClosedFile Load(IVsInvisibleEditorManager manager, IVsEditorAdaptersFactoryService adapters, string file)
        {
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
            closed.Buffer.PostChanged += (s, e) =>
            {
                if (!closed.SaveOnChange)
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
