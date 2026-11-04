//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using XSharpModel;

namespace XSharp.LanguageService.NavigateTo
{
    /// <summary>
    /// All declarations of the solution in memory, so that a search does not touch the database per keystroke.
    /// Read again when the code model changed, but at most every <see cref="ReloadInterval"/>: results may be that
    /// old. Concurrent searches share one load.
    /// </summary>
    internal sealed class NavigateToSnapshot
    {
        internal static readonly TimeSpan ReloadInterval = TimeSpan.FromSeconds(2);

        internal static NavigateToSnapshot Current { get; } = new NavigateToSnapshot(
            () => NavigateToEntry.FromRows(XDatabase.GetAllProjectTypes(), XDatabase.GetAllProjectMembers()),
            () => XDatabase.ChangeCounter,
            () => DateTime.UtcNow);

        private readonly Func<IReadOnlyList<NavigateToEntry>> _load;
        private readonly Func<long> _counter;
        private readonly Func<DateTime> _now;
        private readonly object _lock = new object();
        private IReadOnlyList<NavigateToEntry> _entries;
        private long _loadedCounter;
        private DateTime _loadedAt;
        private Task<IReadOnlyList<NavigateToEntry>> _loading;

        internal NavigateToSnapshot(Func<IReadOnlyList<NavigateToEntry>> load, Func<long> counter, Func<DateTime> now)
        {
            _load = load;
            _counter = counter;
            _now = now;
        }

        /// <summary>The entries; <paramref name="ct"/> stops the waiting for a load, not the load itself.</summary>
        internal async Task<IReadOnlyList<NavigateToEntry>> GetAsync(CancellationToken ct)
        {
            Task<IReadOnlyList<NavigateToEntry>> loading;
            lock (_lock)
            {
                if (_entries != null && (_counter() == _loadedCounter ||
                    (_entries.Count > 0 && _now() - _loadedAt < ReloadInterval)))
                {
                    return _entries;
                }
                if (_loading == null)
                {
                    // the counter before the load: a change during the load makes the next search load again
                    var counter = _counter();
                    _loading = Task.Run(() =>
                    {
                        try
                        {
                            var entries = _load() ?? Array.Empty<NavigateToEntry>();
                            lock (_lock)
                            {
                                _entries = entries;
                                _loadedCounter = counter;
                                _loadedAt = _now();
                            }
                            return entries;
                        }
                        finally
                        {
                            lock (_lock)
                                _loading = null;
                        }
                    });
                }
                loading = _loading;
            }
            if (ct.CanBeCanceled)
            {
                var cancelled = new TaskCompletionSource<bool>();
                using (ct.Register(() => cancelled.TrySetResult(true)))
                {
                    if (await Task.WhenAny(loading, cancelled.Task).ConfigureAwait(false) != loading)
                        ct.ThrowIfCancellationRequested();
                }
            }
            return await loading.ConfigureAwait(false);
        }
    }
}
