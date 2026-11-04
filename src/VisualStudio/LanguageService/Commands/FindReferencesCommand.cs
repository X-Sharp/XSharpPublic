//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Windows.Media;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.ComponentModelHost;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.FindAllReferences;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Threading;
using Microsoft.VisualStudio.Utilities;
using XSharp.LanguageService.FindReferences;
using XSharp.Support;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService.Commands
{
    internal class FindReferencesCommand : AbstractCommand
    {
        private static CancellationTokenSource _cancellation;
        private static ReferencesTableDataSource _source;
        private static Microsoft.VisualStudio.Shell.TableManager.ITableManager _sourceManager;
        private static IFindAllReferencesWindow _window;
        private static EventHandler _windowClosed;

        public static async Task InitializeAsync()
        {
            await VS.Commands.InterceptAsync(VSConstants.VSStd97CmdID.FindReferences, () => Execute(FindReferences));
        }

        private static void FindReferences(DocumentView doc)
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                var symbols = doc.TextView.GetSymbolUnderCursor(out _, out var location, out _);
                IXSymbol target = null;
                int candidates = 0;
                OverrideFamily family = null;
                try
                {
                    // on a declaration: the declared overload; on a call site: the overload that takes these arguments
                    var name = location == null ? null : OverloadFilter.NameAt(location.Document?.Tokens, location.Position);
                    target = name == null ? null : FindReferencesEngine.DeclaredEntity(location.File?.EntityList, name);
                    if (target == null && location != null)
                    {
                        var matches = OverloadFilter.Filter(SymbolResolver.ForCreation(symbols, location.Document?.Tokens, name),
                            OverloadFilter.ArgumentsAt(location.Document?.Tokens, location.Position),
                            id => SymbolResolver.TypeOf(location, id));
                        target = matches.FirstOrDefault();
                        candidates = matches.Count;
                    }
                    // Foo{} of a class without constructors: the lookup makes one up, search the class instead
                    if (target != null && target.Kind == Kind.Constructor && target is IXMemberSymbol ctor &&
                        ctor.ParentType != null && ctor.ParentType.GetConstructors().Length == 0)
                    {
                        target = ctor.ParentType;
                    }
                    if (target != null)
                        family = new OverrideFamily(target);
                }
                catch (Exception ex)
                {
                    Logger.Exception(ex, "FindReferences: cannot determine the symbol under the cursor");
                    target = null;
                }
                if (target == null || location == null)
                {
                    await VS.StatusBar.ShowMessageAsync("No X# symbol under the cursor");
                    return;
                }
                if (!FindReferencesEngine.IsInSource(target))
                {
                    await VS.StatusBar.ShowMessageAsync($"Find References: '{target.FullName}' is not declared in the X# source of this solution");
                    return;
                }
                var title = target.Kind == Kind.Constructor ? FindReferencesEngine.SearchName(target) + "{}" : target.Name;
                var documents = await CreateDocumentSourceAsync();
                var service = await VS.GetRequiredServiceAsync<SVsFindAllReferences, IFindAllReferencesService>();
                var window = service.StartSearch($"'{title}' references");
                var source = new ReferencesTableDataSource(target, await CreateTextFormatAsync());
                // After "Keep Results" StartSearch opens a new window. The kept window keeps its results,
                // its running search and its Closed handler; only a reused window gives up the previous search.
                if (_sourceManager != null && window.Manager == _sourceManager)
                {
                    _cancellation?.Cancel();
                    if (_source != null)
                        _sourceManager.RemoveSource(_source);
                    if (_window != null)
                        _window.Closed -= _windowClosed;
                }
                var cancellation = new CancellationTokenSource();
                _cancellation = cancellation;
                window.Manager.AddSource(source, ReferencesTableDataSource.Columns);
                _source = source;
                _sourceManager = window.Manager;
                _window = window;
                _windowClosed = (s, e) => cancellation.Cancel();
                window.Closed += _windowClosed;
                if (candidates > 1 && target is IXMemberSymbol overload)
                {
                    // the arguments do not tell the overloads apart
                    await VS.StatusBar.ShowMessageAsync($"Find References: {candidates} overloads of '{target.Name}' fit these arguments, listing {target.Name}({overload.ParameterList})");
                }
                // Do not await the search: the command runs inside JoinableTaskFactory.Run on the UI thread,
                // so awaiting would block Visual Studio until the whole solution has been searched.
                // The package's factory tracks the task, so VS waits for it on shutdown (VSSDK007).
                var package = XSharpLanguagePackage.Instance;
                var factory = package?.JoinableTaskFactory ?? ThreadHelper.JoinableTaskFactory;
                factory.RunAsync(async delegate
                {
                    // shutting down VS cancels the search instead of waiting for it
                    var shutdown = package?.DisposalToken.Register(cancellation.Cancel);
                    try
                    {
                        var stats = await FindReferencesEngine.FindAsync(target, family,
                            ReferenceSearchOptions.ForFindReferences(FindReferencesEngine.IncludeLateBound),
                            location, documents, source, cancellation.Token);
                        if (stats.Failed > 0)
                            await VS.StatusBar.ShowMessageAsync($"Find References: {stats.Failed} file(s) could not be searched, see the X# log");
                    }
                    catch (OperationCanceledException)
                    {
                        Logger.Information($"FindReferences '{target.Name}' cancelled");
                    }
                    finally
                    {
                        shutdown?.Dispose();
                        source.Complete();
                    }
                }).Task.Forget();
            });
        }

        /// <summary>The editor colors for the Text column, and the reference highlight that C# uses for the hit.</summary>
        private static async System.Threading.Tasks.Task<ReferenceTextFormat> CreateTextFormatAsync()
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var componentModel = await VS.Services.GetComponentModelAsync();
            var formatMap = componentModel.GetService<IClassificationFormatMapService>().GetClassificationFormatMap("text");
            var editorFormatMap = componentModel.GetService<IEditorFormatMapService>().GetEditorFormatMap("text");
            var highlight = editorFormatMap.GetProperties("MarkerFormatDefinition/HighlightedReference");
            var hitBackground = highlight?[EditorFormatDefinition.BackgroundBrushId] as Brush;
            return new ReferenceTextFormat(formatMap, componentModel.GetService<IClassificationTypeRegistryService>(), hitBackground);
        }

        /// <summary>Collects the open X# buffers on the UI thread; the search itself runs in the background.</summary>
        private static async System.Threading.Tasks.Task<ReferenceDocumentSource> CreateDocumentSourceAsync()
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var componentModel = await VS.Services.GetComponentModelAsync();
            var adapters = componentModel.GetService<IVsEditorAdaptersFactoryService>();
            var openBuffers = new Dictionary<string, ITextBuffer>(StringComparer.OrdinalIgnoreCase);
            foreach (var info in new RunningDocumentTable(ServiceProvider.GlobalProvider))
            {
                if (info.DocData is IVsTextBuffer vsBuffer)
                {
                    var buffer = adapters.GetDocumentBuffer(vsBuffer);
                    if (buffer != null && buffer.ContentType.IsOfType(Constants.LanguageName))
                        openBuffers[info.Moniker] = buffer;
                }
            }
            return new ReferenceDocumentSource(componentModel.GetService<ITextBufferFactoryService>(),
                componentModel.GetService<IContentTypeRegistryService>(), openBuffers);
        }
    }
}
