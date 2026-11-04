//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Core.Imaging;
using Microsoft.VisualStudio.Search.Data;
using Microsoft.VisualStudio.Threading;
using Microsoft.VisualStudio.Utilities;
using XSharp.Settings;
using XSharp.Support;

namespace XSharp.LanguageService.NavigateTo
{
    /// <summary>
    /// The X# source for the Code Search (Ctrl+T) of VS 2022 17.6+ and VS 2026, built like Roslyn's. The Code Search
    /// asks classic INavigateToItemProviders only for the first query of a session, so X# needs its own source here;
    /// the classic provider (NavigateToProviderFactory) serves the classic Go To window. Both share the snapshot of the
    /// declarations and the name matching.
    /// </summary>
    [Export(typeof(ISearchItemsSourceProvider))]
    [Name("XSharpSearchItemsSourceProvider")]
    [ProducesResultType(CodeSearchResultType.Class)]
    [ProducesResultType(CodeSearchResultType.Constant)]
    [ProducesResultType(CodeSearchResultType.Delegate)]
    [ProducesResultType(CodeSearchResultType.Enum)]
    [ProducesResultType(CodeSearchResultType.EnumItem)]
    [ProducesResultType(CodeSearchResultType.Event)]
    [ProducesResultType(CodeSearchResultType.Field)]
    [ProducesResultType(CodeSearchResultType.Interface)]
    [ProducesResultType(CodeSearchResultType.Method)]
    [ProducesResultType(CodeSearchResultType.Property)]
    [ProducesResultType(CodeSearchResultType.Structure)]
    internal sealed class XSharpSearchItemsSourceProvider : ISearchItemsSourceProvider, ISearchResultViewFactory
    {
        public ISearchItemsSource CreateItemsSource()
        {
            return new XSharpSearchItemsSource(this);
        }

        public SearchResultViewBase CreateSearchResultView(SearchResult searchResult)
        {
            return searchResult is XSharpSearchResult result ? new XSharpSearchResultView(result.Entry) : null;
        }

        public Task<IReadOnlyList<SearchResultPreviewPanelBase>> GetPreviewPanelsAsync(SearchResult searchResult, SearchResultViewBase searchResultView)
        {
            return Task.FromResult<IReadOnlyList<SearchResultPreviewPanelBase>>(Array.Empty<SearchResultPreviewPanelBase>());
        }
    }

    internal sealed class XSharpSearchItemsSource : CodeSearchItemsSourceBase
    {
        private readonly XSharpSearchItemsSourceProvider _provider;

        internal XSharpSearchItemsSource(XSharpSearchItemsSourceProvider provider)
        {
            _provider = provider;
        }

        public override async Task PerformSearchAsync(ISearchQuery searchQuery, ISearchCallback searchCallback, CancellationToken cancellationToken)
        {
            var watch = System.Diagnostics.Stopwatch.StartNew();
            int found = 0, total = 0;
            var outcome = "done";
            var searchValue = searchQuery.QueryString?.Trim() ?? "";
            try
            {
                if (searchValue.Length == 0)
                    return;
                bool types = searchQuery.FiltersStates.TryGetValue("Types", out var t) && t == "True";
                bool members = searchQuery.FiltersStates.TryGetValue("Members", out var m) && m == "True";
                // "current document" / "current project", as Roslyn reads the scope
                string document = null, project = null;
                var scope = (searchQuery as ICodeSearchQuery)?.Scope ?? SearchScopes.EntireSolution;
                if (scope == SearchScopes.CurrentDocument || scope == SearchScopes.CurrentProject)
                {
                    var active = await NavigateToProvider.ActiveDocumentAsync().ConfigureAwait(false);
                    if (active.Path == null || (scope == SearchScopes.CurrentProject && active.Project == null))
                        return;
                    if (scope == SearchScopes.CurrentDocument)
                        document = active.Path;
                    else
                        project = active.Project;
                }
                // never match on the thread that asked: the snapshot may be ready and the loop would run there
                await TaskScheduler.Default;
                var entries = await NavigateToSnapshot.Current.GetAsync(cancellationToken).ConfigureAwait(false);
                total = entries.Count;
                foreach (var entry in entries)
                {
                    cancellationToken.ThrowIfCancellationRequested();
                    if (!entry.PassesFilter(types, members) || !entry.InScope(document, project))
                        continue;
                    var match = NameMatcher.Match(searchValue, entry.Name);
                    if (match == NameMatch.None)
                        continue;
                    searchCallback.AddItem(new XSharpSearchResult(_provider, entry, ResultType(entry), (int)match));
                    found++;
                }
            }
            catch (OperationCanceledException)
            {
                outcome = "cancelled";
            }
            catch (Exception ex)
            {
                outcome = "failed";
                Logger.Exception(ex, "Navigate To: search failed");
            }
            finally
            {
                Logger.Information($"Navigate To (Code Search) '{searchValue}': {found} of {total} entries, {outcome}, {watch.ElapsedMilliseconds} ms");
            }
        }

        private static string ResultType(NavigateToEntry entry)
        {
            switch (NavigateToProvider.ItemKind(entry.Kind))
            {
                case "Class": return CodeSearchResultType.Class;
                case "Structure": return CodeSearchResultType.Structure;
                case "Interface": return CodeSearchResultType.Interface;
                case "Delegate": return CodeSearchResultType.Delegate;
                case "Enum": return CodeSearchResultType.Enum;
                case "EnumItem": return CodeSearchResultType.EnumItem;
                case "Property": return CodeSearchResultType.Property;
                case "Field": return CodeSearchResultType.Field;
                case "Constant": return CodeSearchResultType.Constant;
                case "Event": return CodeSearchResultType.Event;
                default: return CodeSearchResultType.Method;
            }
        }
    }

    internal sealed class XSharpSearchResult : CodeSearchResult
    {
        internal NavigateToEntry Entry { get; }

        internal XSharpSearchResult(XSharpSearchItemsSourceProvider provider, NavigateToEntry entry, string resultType, int rank)
            : base(provider, resultType, entry.Name, entry.Container, null, entry.FilePath,
                  $"{entry.FilePath}|{entry.Line}", 1f / rank, SearchResultFlags.PrimaryIsPatternSearch, "X#")
        {
            Entry = entry;
            ResultId = $"xsharp|{entry.FilePath}|{entry.Line}|{entry.Column}|{entry.Name}";
        }
    }

    internal sealed class XSharpSearchResultView : CodeSearchResultViewBase
    {
        private readonly NavigateToEntry _entry;

        internal XSharpSearchResultView(NavigateToEntry entry)
            : base(new HighlightedText(entry.Name, null), new HighlightedText(Describe(entry), null), primaryIcon: Icon(entry))
        {
            _entry = entry;
            FileLocation = new HighlightedText(entry.FilePath, null);
        }

        private static string Describe(NavigateToEntry entry)
        {
            var file = Path.GetFileName(entry.FilePath);
            var where = string.IsNullOrEmpty(entry.ProjectName) ? file : $"{file}, {entry.ProjectName}";
            return string.IsNullOrEmpty(entry.Container) ? where : $"{entry.Container} ({where})";
        }

        private static ImageId Icon(NavigateToEntry entry)
        {
            var moniker = entry.Kind.GetImageMoniker(entry.Visibility);
            return new ImageId(moniker.Guid, moniker.Id);
        }

        public override void Invoke(CancellationToken cancellationToken)
        {
            XDocuments.Open(_entry.FilePath, _entry.Line, _entry.Column, false);
        }
    }
}
