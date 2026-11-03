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
                // on a declaration: the declared overload; on a call site: the overload that takes these arguments
                var name = location == null ? null : OverloadFilter.NameAt(location.Document?.Tokens, location.Position);
                var declared = name == null ? null : FindReferencesEngine.DeclaredEntity(location.File?.EntityList, name);
                var target = declared != null ? declared : location == null ? null :
                    OverloadFilter.Filter(SymbolResolver.ForCreation(symbols, location.Document?.Tokens, name),
                        OverloadFilter.ArgumentsAt(location.Document?.Tokens, location.Position),
                        id => SymbolResolver.TypeOf(location, id)).FirstOrDefault();
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
                // Foo{} of a class without constructors: the lookup makes one up, search the class instead
                if (target.Kind == Kind.Constructor && target is IXMemberSymbol ctor && ctor.ParentType != null &&
                    ctor.ParentType.GetConstructors().Length == 0)
                {
                    target = ctor.ParentType;
                }
                var title = target.Kind == Kind.Constructor ? FindReferencesEngine.SearchName(target) + "{}" : target.Name;
                var documents = await CreateDocumentSourceAsync();
                // a new search cancels the running one
                _cancellation?.Cancel();
                var cancellation = new CancellationTokenSource();
                _cancellation = cancellation;
                var service = await VS.GetRequiredServiceAsync<SVsFindAllReferences, IFindAllReferencesService>();
                var window = service.StartSearch($"'{title}' references");
                var source = new ReferencesTableDataSource(target, await CreateTextFormatAsync());
                // the window keeps every source it was given: drop the previous search
                if (_source != null)
                    _sourceManager?.RemoveSource(_source);
                window.Manager.AddSource(source, ReferencesTableDataSource.Columns);
                _source = source;
                _sourceManager = window.Manager;
                window.Closed += (s, e) => cancellation.Cancel();
                _ = ThreadHelper.JoinableTaskFactory.RunAsync(async delegate
                {
                    try
                    {
                        var stats = await FindReferencesEngine.FindAsync(target, location, documents, source, cancellation.Token);
                        if (stats.Failed > 0)
                            await VS.StatusBar.ShowMessageAsync($"Find References: {stats.Failed} file(s) could not be searched, see the X# log");
                    }
                    catch (OperationCanceledException)
                    {
                        Logger.Information($"FindReferences '{target.Name}' cancelled");
                    }
                    finally
                    {
                        source.Complete();
                    }
                });
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
