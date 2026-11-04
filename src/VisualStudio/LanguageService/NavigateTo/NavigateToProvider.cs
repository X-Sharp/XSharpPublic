//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Threading;
using System.Threading.Tasks;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Language.NavigateTo.Interfaces;
using Microsoft.VisualStudio.Shell;
using XSharp.Support;
using XSharpModel;
using Task = System.Threading.Tasks.Task;

namespace XSharp.LanguageService.NavigateTo
{
    /// <summary>Go To All (Ctrl+T) for the X# types, members, functions, DEFINEs and GLOBALs of the solution.</summary>
    [Export(typeof(INavigateToItemProviderFactory))]
    internal sealed class NavigateToProviderFactory : INavigateToItemProviderFactory
    {
        public bool TryCreateNavigateToItemProvider(IServiceProvider serviceProvider, out INavigateToItemProvider provider)
        {
            provider = new NavigateToProvider();
            return true;
        }
    }

    internal sealed class NavigateToProvider : INavigateToItemProvider2
    {
        internal const string Language = "X#";
        private const int ProgressStep = 1000;
        private static readonly HashSet<string> _kinds = new HashSet<string>
        {
            NavigateToItemKind.Class, NavigateToItemKind.Structure, NavigateToItemKind.Interface,
            NavigateToItemKind.Delegate, NavigateToItemKind.Enum, NavigateToItemKind.EnumItem,
            NavigateToItemKind.Method, NavigateToItemKind.Property, NavigateToItemKind.Field,
            NavigateToItemKind.Constant, NavigateToItemKind.Event
        };

        private CancellationTokenSource _cancellation;

        public ISet<string> KindsProvided => _kinds;
        public bool CanFilter => true;

        public void StartSearch(INavigateToCallback callback, string searchValue)
        {
            StartSearch(callback, searchValue, null);
        }

        public void StartSearch(INavigateToCallback callback, string searchValue, INavigateToFilterParameters filter)
        {
            var cancellation = new CancellationTokenSource();
            Interlocked.Exchange(ref _cancellation, cancellation)?.Cancel();
            var kinds = filter?.Kinds;
            var currentDocumentOnly = (callback.Options as INavigateToOptions2)?.SearchCurrentDocument == true;
            Task.Run(() => SearchAsync(callback, searchValue, kinds, currentDocumentOnly, cancellation.Token));
        }

        private static async Task SearchAsync(INavigateToCallback callback, string searchValue, ISet<string> kinds,
            bool currentDocumentOnly, CancellationToken ct)
        {
            try
            {
                string currentDocument = null;
                if (currentDocumentOnly)
                {
                    currentDocument = (await VS.Documents.GetActiveDocumentViewAsync())?.FilePath;
                    if (currentDocument == null)
                        return;
                }
                var entries = await NavigateToSnapshot.Current.GetAsync(ct).ConfigureAwait(false);
                for (int i = 0; i < entries.Count; i++)
                {
                    ct.ThrowIfCancellationRequested();
                    if (i % ProgressStep == 0)
                        callback.ReportProgress(i, entries.Count);
                    var entry = entries[i];
                    if (currentDocument != null && !string.Equals(entry.FilePath, currentDocument, StringComparison.OrdinalIgnoreCase))
                        continue;
                    var kind = ItemKind(entry.Kind);
                    if (kinds != null && kinds.Count > 0 && !kinds.Contains(kind))
                        continue;
                    var match = NameMatcher.Match(searchValue, entry.Name);
                    if (match == NameMatch.None)
                        continue;
                    callback.AddItem(new NavigateToItem(entry.Name, kind, Language, entry.Container, entry,
                        MatchKindOf(match), NavigateToDisplayFactory.Instance));
                }
                callback.ReportProgress(entries.Count, entries.Count);
            }
            catch (OperationCanceledException)
            {
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "Navigate To: search failed");
            }
            finally
            {
                callback.Done();
            }
        }

        public void StopSearch()
        {
            _cancellation?.Cancel();
        }

        public void Dispose()
        {
            StopSearch();
        }

        /// <summary>The Go To All kind of an X# declaration.</summary>
        internal static string ItemKind(Kind kind)
        {
            switch (kind)
            {
                case Kind.Class:
                    return NavigateToItemKind.Class;
                case Kind.Structure:
                case Kind.VOStruct:
                case Kind.Union:
                    return NavigateToItemKind.Structure;
                case Kind.Interface:
                    return NavigateToItemKind.Interface;
                case Kind.Delegate:
                    return NavigateToItemKind.Delegate;
                case Kind.Enum:
                    return NavigateToItemKind.Enum;
                case Kind.EnumMember:
                    return NavigateToItemKind.EnumItem;
                case Kind.Access:
                case Kind.Assign:
                case Kind.Property:
                    return NavigateToItemKind.Property;
                case Kind.Field:
                case Kind.VOGlobal:
                    return NavigateToItemKind.Field;
                case Kind.VODefine:
                case Kind.Define:
                    return NavigateToItemKind.Constant;
                case Kind.Event:
                    return NavigateToItemKind.Event;
                default:
                    // Method, Function, Procedure, Operator, VODLL
                    return NavigateToItemKind.Method;
            }
        }

        private static MatchKind MatchKindOf(NameMatch match)
        {
            switch (match)
            {
                case NameMatch.Exact:
                    return MatchKind.Exact;
                case NameMatch.Prefix:
                    return MatchKind.Prefix;
                case NameMatch.CamelCase:
                    return MatchKind.Regular;
                default:
                    return MatchKind.Substring;
            }
        }
    }
}
