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
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Threading;
using Microsoft.VisualStudio.Utilities;
using XSharp.LanguageService.FindReferences;
using XSharp.Support;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService.Commands
{
    /// <summary>The symbol a command works on, or the message why there is none.</summary>
    internal sealed class TargetSelection
    {
        internal IXSymbol Target { get; set; }
        /// <summary>How many overloads fit the arguments at the call site</summary>
        internal int Candidates { get; set; }
        internal XSharpSearchLocation Location { get; set; }
        internal OverrideFamily Family { get; set; }
        /// <summary>Set when there is no target to work on: show it and stop</summary>
        internal string Message { get; set; }
    }

    internal class FindReferencesCommand : AbstractCommand
    {
        public static async Task InitializeAsync()
        {
            await VS.Commands.InterceptAsync(VSConstants.VSStd97CmdID.FindReferences, () => Execute(FindReferences));
        }

        /// <summary>
        /// The symbol under the cursor: on a declaration the declared overload, on a call site the overload that takes
        /// these arguments. UI thread. <paramref name="commandName"/> prefixes the messages.
        /// </summary>
        internal static TargetSelection SelectTarget(ITextView view, string commandName)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var symbols = view.GetSymbolUnderCursor(out _, out var location, out _);
            var selection = new TargetSelection { Location = location };
            IXSymbol target = null;
            try
            {
                var name = location == null ? null : OverloadFilter.NameAt(location.Document?.Tokens, location.Position);
                target = name == null ? null : FindReferencesEngine.DeclaredEntity(location.File?.EntityList, name);
                if (target == null && location != null)
                {
                    var matches = OverloadFilter.Filter(SymbolResolver.ForCreation(symbols, location.Document?.Tokens, name),
                        OverloadFilter.ArgumentsAt(location.Document?.Tokens, location.Position),
                        id => SymbolResolver.TypeOf(location, id));
                    target = matches.FirstOrDefault();
                    selection.Candidates = matches.Count;
                }
                // Foo{} of a class without constructors: the lookup makes one up, use the class instead
                if (target != null && target.Kind == Kind.Constructor && target is IXMemberSymbol ctor &&
                    ctor.ParentType != null && ctor.ParentType.GetConstructors().Length == 0)
                {
                    target = ctor.ParentType;
                }
                if (target != null)
                    selection.Family = new OverrideFamily(target);
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, $"{commandName}: cannot determine the symbol under the cursor");
                target = null;
            }
            if (target == null || location == null)
                selection.Message = "No X# symbol under the cursor";
            else if (!FindReferencesEngine.IsInSource(target))
                selection.Message = $"{commandName}: '{target.FullName}' is not declared in the X# source of this solution";
            selection.Target = target;
            return selection;
        }

        private static void FindReferences(DocumentView doc)
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                var selection = SelectTarget(doc.TextView, "Find References");
                if (selection.Message != null)
                {
                    await VS.StatusBar.ShowMessageAsync(selection.Message);
                    return;
                }
                var target = selection.Target;
                var family = selection.Family;
                var location = selection.Location;
                var candidates = selection.Candidates;
                var title = target.Kind == Kind.Constructor ? FindReferencesEngine.SearchName(target) + "{}" : target.Name;
                var documents = await CreateDocumentSourceAsync();
                var (source, token) = await ReferencesWindow.OpenAsync($"'{title}' references", target, await CreateTextFormatAsync());
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
                    var shutdown = package == null ? null : CancellationTokenSource.CreateLinkedTokenSource(token, package.DisposalToken);
                    try
                    {
                        var stats = await FindReferencesEngine.FindAsync(target, family,
                            ReferenceSearchOptions.ForFindReferences(FindReferencesEngine.IncludeLateBound),
                            location, documents, source, shutdown?.Token ?? token);
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
        internal static async System.Threading.Tasks.Task<ReferenceTextFormat> CreateTextFormatAsync()
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
        internal static async System.Threading.Tasks.Task<ReferenceDocumentSource> CreateDocumentSourceAsync()
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
