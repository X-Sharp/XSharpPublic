//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.TextManager.Interop;
using XSharp.LanguageService.FindReferences;
using XSharp.LanguageService.Rename;
using XSharp.Support;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService.Commands
{
    /// <summary>
    /// Rename (Ctrl+R, Ctrl+R) for X# symbols: dialog, search with the Find All References engine, optional preview,
    /// changes in all files in one undo step. Places that may refer to the symbol but are not sure are never changed;
    /// they are listed in the Find All References window afterwards.
    /// </summary>
    internal class RenameCommand : AbstractCommand
    {
        public static async Task InitializeAsync()
        {
            await VS.Commands.InterceptAsync(VSConstants.VSStd2KCmdID.RENAME, () => Execute(Rename));
        }

        private sealed class HitList : IReferenceSink
        {
            private readonly List<ReferenceHit> _hits = new List<ReferenceHit>();
            public void Add(IReadOnlyList<ReferenceHit> batch) { lock (_hits) _hits.AddRange(batch); }
            internal IReadOnlyList<ReferenceHit> Hits { get { lock (_hits) return _hits.ToList(); } }
        }

        private static void Rename(DocumentView doc)
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                try
                {
                    await RenameAsync(doc);
                }
                catch (Exception ex)
                {
                    Logger.Exception(ex, "Rename failed");
                    await VS.MessageBox.ShowErrorAsync("Rename", $"Rename failed: {ex.Message}");
                }
            });
        }

        private static async Task RenameAsync(DocumentView doc)
        {
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                var selection = FindReferencesCommand.SelectTarget(doc.TextView, "Rename");
                if (selection.Message != null)
                {
                    await VS.StatusBar.ShowMessageAsync(selection.Message);
                    return;
                }
                var location = selection.Location;
                var target = selection.Target;
                // a constructor is named after its class: renaming it renames the class
                if (target.Kind == Kind.Constructor && target is IXMemberSymbol ctor && ctor.ParentType != null)
                    target = ctor.ParentType;
                var family = new OverrideFamily(target);
                var refusal = RenameGuard.Refusal(target, family.BaseTypeOf, name => location.FindType(name));
                if (refusal != null)
                {
                    await VS.MessageBox.ShowWarningAsync("Rename", refusal);
                    return;
                }
                var oldName = target.Name;
                var file = location.File;
                var typeFile = (target as XSourceTypeSymbol)?.File?.FullPath;
                bool canRenameFile = typeFile != null &&
                    string.Equals(Path.GetFileNameWithoutExtension(typeFile), oldName, StringComparison.OrdinalIgnoreCase);
                var documents = await FindReferencesCommand.CreateDocumentSourceAsync();
                var dialog = new RenameDialog(oldName, target.KindKeyword, canRenameFile, name => Validate(oldName, name, file));

                // the search runs while the dialog is open, again when the comment/string options change; a new
                // search cancels the running one and waits for it: they would share files and lookups
                RenamePlan plan = null;
                CancellationTokenSource running = null;
                var oneSearch = new SemaphoreSlim(1, 1);
                async Task SearchAsync()
                {
                    running?.Cancel();
                    var cancellation = new CancellationTokenSource();
                    running = cancellation;
                    plan = null;
                    dialog.ShowSearching();
                    await oneSearch.WaitAsync();
                    try
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        if (cancellation.IsCancellationRequested)
                            return;
                        var hits = new HitList();
                        await FindReferencesEngine.FindAsync(target, new OverrideFamily(target),
                            ReferenceSearchOptions.ForRename(dialog.Comments, dialog.Strings), location, documents, hits, cancellation.Token);
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        if (cancellation.IsCancellationRequested)
                            return;
                        plan = RenamePlanner.Build(hits.Hits);
                        dialog.ShowSearchResult(plan.Changes.Count, plan.Files.Count, plan.NotChanged.Count);
                    }
                    catch (OperationCanceledException)
                    {
                    }
                    catch (Exception ex)
                    {
                        Logger.Exception(ex, "Rename: search failed");
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        dialog.ShowError($"The search failed: {ex.Message}");
                    }
                    finally
                    {
                        oneSearch.Release();
                    }
                }
                // WPF events cannot await; SearchAsync handles its own errors
#pragma warning disable VSSDK007
                dialog.Loaded += (s, e) => ThreadHelper.JoinableTaskFactory.RunAsync(SearchAsync).FileAndForget("XSharp/Rename/Search");
                dialog.SearchOptionsChanged += (s, e) => ThreadHelper.JoinableTaskFactory.RunAsync(SearchAsync).FileAndForget("XSharp/Rename/Search");
#pragma warning restore VSSDK007
                var ok = dialog.ShowModal() == true;
                if (!ok || plan == null)
                {
                    running?.Cancel();
                    return;
                }
                var newName = dialog.NewName;
                if (RenameName.Check(oldName, newName, s => ReferenceDocumentSource.Lex(file, s)) == RenameNameResult.Unchanged)
                    return;

                var scope = new RenameScope
                {
                    BaseOf = family.BaseTypeOf,
                    FindTypeOrFunction = name => (IXSymbol)location.FindType(name) ?? XSharpLookup.SearchFunction(location, name).FirstOrDefault(),
                    LocalsOf = member => (member as XSourceMemberSymbol)?.GetLocals(location) ?? Enumerable.Empty<IXSymbol>()
                };
                var conflicts = RenameConflicts.Find(target, newName, scope);
                if (conflicts.Count > 0)
                {
                    var list = string.Join(", ", conflicts.Select(c => $"{c.KindKeyword} {c.FullName}".Trim()));
                    if (!await VS.MessageBox.ShowConfirmAsync("Rename", $"'{newName}' already exists: {list}. Rename anyway?"))
                        return;
                }
                if (dialog.Preview)
                {
                    plan = RenamePreview.Show(plan, oldName, newName, ReadText);
                    if (plan == null)
                        return;
                }
                var result = await RenameApplier.ApplyAsync(plan, oldName, newName);
                if (result.Error != null)
                {
                    await VS.MessageBox.ShowErrorAsync("Rename", result.Error);
                    return;
                }
                // the file follows its class only when the declaration in it was renamed
                if (dialog.RenameFile && typeFile != null &&
                    plan.Changes.Any(h => h.IsDefinition && string.Equals(h.FilePath, typeFile, StringComparison.OrdinalIgnoreCase)) &&
                    !await RenameApplier.RenameFileAsync(typeFile, newName.StartsWith("@@") ? newName.Substring(2) : newName))
                {
                    await VS.MessageBox.ShowWarningAsync("Rename", $"'{Path.GetFileName(typeFile)}' could not be renamed.");
                }
                await ReportAsync(target, oldName, newName, result, plan);
            }
        }

        private static string Validate(string oldName, string newName, XFile file)
        {
            switch (RenameName.Check(oldName, newName, s => ReferenceDocumentSource.Lex(file, s)))
            {
                case RenameNameResult.Empty:
                    return "Enter a name.";
                case RenameNameResult.Invalid:
                    return $"'{newName}' is not a valid name.";
                case RenameNameResult.Keyword:
                    return $"'{newName}' is a keyword. Write @@{newName} to use it as a name.";
            }
            return null;
        }

        /// <summary>The status bar message, and the places that were not changed in the Find All References window.</summary>
        private static async Task ReportAsync(IXSymbol target, string oldName, string newName, RenameResult result, RenamePlan plan)
        {
            var message = $"Renamed '{oldName}' to '{newName}': {result.Changed} change(s) in {result.Files.Count} file(s).";
            if (plan.NotChanged.Count > 0)
            {
                message += $" {plan.NotChanged.Count} place(s) were not changed, see Find All References.";
                var (source, _) = await ReferencesWindow.OpenAsync($"'{oldName}' not changed", target, await FindReferencesCommand.CreateTextFormatAsync());
                source.Add(plan.NotChanged);
                source.Complete();
            }
            if (await HasOtherLanguageProjectsAsync())
                message += " C#/VB projects in this solution were not searched.";
            await VS.StatusBar.ShowMessageAsync(message);
        }

        private static async System.Threading.Tasks.Task<bool> HasOtherLanguageProjectsAsync()
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            try
            {
                var projects = await VS.Solutions.GetAllProjectsAsync();
                return projects.Any(p => p.FullPath != null && (p.FullPath.EndsWith(".csproj", StringComparison.OrdinalIgnoreCase) ||
                    p.FullPath.EndsWith(".vbproj", StringComparison.OrdinalIgnoreCase)));
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "Rename: listing the projects failed");
                return false;
            }
        }

        /// <summary>The editor text of an open file, otherwise the text on disk. UI thread.</summary>
        private static string ReadText(string path)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var rdt = new RunningDocumentTable(ServiceProvider.GlobalProvider);
            if (rdt.FindDocument(path) is IVsTextBuffer open)
            {
                var adapters = (Package.GetGlobalService(typeof(Microsoft.VisualStudio.ComponentModelHost.SComponentModel))
                    as Microsoft.VisualStudio.ComponentModelHost.IComponentModel)?.GetService<IVsEditorAdaptersFactoryService>();
                var buffer = adapters?.GetDocumentBuffer(open);
                if (buffer != null)
                    return buffer.CurrentSnapshot.GetText();
            }
            return File.Exists(path) ? File.ReadAllText(path) : "";
        }
    }
}
