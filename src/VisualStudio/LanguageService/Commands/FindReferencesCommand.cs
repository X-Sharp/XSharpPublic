//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.ComponentModelHost;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Utilities;
using XSharp.LanguageService.FindReferences;
using XSharp.Support;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService.Commands
{
    internal class FindReferencesCommand : AbstractCommand
    {
        private static CancellationTokenSource _cancellation;

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
                var target = symbols.FirstOrDefault();
                if (target == null || location == null)
                {
                    await VS.StatusBar.ShowMessageAsync("No X# symbol under the cursor");
                    return;
                }
                var documents = await CreateDocumentSourceAsync();
                _cancellation?.Cancel();
                var cancellation = new CancellationTokenSource();
                _cancellation = cancellation;
                var sink = new LogReferenceSink();
                _ = ThreadHelper.JoinableTaskFactory.RunAsync(async delegate
                {
                    try
                    {
                        await FindReferencesEngine.FindAsync(target, location, documents, sink, cancellation.Token);
                    }
                    catch (OperationCanceledException)
                    {
                        Logger.Information($"FindReferences '{target.FullName}' cancelled");
                    }
                });
            });
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

        /// <summary>Temporary sink until the Find All References window is wired up.</summary>
        private sealed class LogReferenceSink : IReferenceSink
        {
            public void Add(IReadOnlyList<ReferenceHit> batch)
            {
                foreach (var hit in batch)
                {
                    Logger.Information($"FindReferences hit {(hit.IsDefinition ? "def" : "ref")} {System.IO.Path.GetFileName(hit.FilePath)}:{hit.Line + 1}:{hit.Column + 1} {hit.LineText.Trim()}");
                }
            }
        }
    }
}
