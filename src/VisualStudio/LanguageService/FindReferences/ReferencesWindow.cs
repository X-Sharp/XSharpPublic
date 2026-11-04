//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Threading;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.FindAllReferences;
using Microsoft.VisualStudio.Shell.TableManager;
using XSharpModel;

namespace XSharp.LanguageService.FindReferences
{
    /// <summary>
    /// The Find All References window for X# results: Find All References fills it with a search, Rename with the
    /// places it did not change. One source per reused window: a new search cancels and replaces the previous one;
    /// a window the user kept ("Keep Results") keeps its results and its running search.
    /// </summary>
    internal static class ReferencesWindow
    {
        private static CancellationTokenSource _cancellation;
        private static ReferencesTableDataSource _source;
        private static ITableManager _sourceManager;
        private static IFindAllReferencesWindow _window;
        private static EventHandler _windowClosed;

        /// <summary>
        /// Opens (or reuses) the window titled <paramref name="title"/> with a new, empty source for
        /// <paramref name="target"/>. The token is cancelled by the next search and when the window closes.
        /// </summary>
        internal static async System.Threading.Tasks.Task<(ReferencesTableDataSource Source, CancellationToken Token)> OpenAsync(
            string title, IXSymbol target, ReferenceTextFormat format)
        {
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            var service = await VS.GetRequiredServiceAsync<SVsFindAllReferences, IFindAllReferencesService>();
            var window = service.StartSearch(title);
            var source = new ReferencesTableDataSource(target, format);
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
            return (source, cancellation.Token);
        }
    }
}
