//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using XSharp.Support;

namespace XSharp.LanguageService.InlayHints
{
    /// <summary>
    /// Runs the inlay hint computations one at a time for the whole IDE, so they do not compete with each other
    /// and with the code model walker. A newer request for the same key (document) replaces a queued one and
    /// cancels a running one.
    /// </summary>
    internal sealed class InlayHintQueue
    {
        internal static InlayHintQueue Instance { get; } = new InlayHintQueue((ms, ct) => Task.Delay(ms, ct));

        private readonly Func<int, CancellationToken, Task> _delay;
        private readonly object _gate = new object();
        private readonly List<KeyValuePair<object, Func<CancellationToken, Task>>> _pending = new List<KeyValuePair<object, Func<CancellationToken, Task>>>();
        private readonly Dictionary<object, CancellationTokenSource> _debounce = new Dictionary<object, CancellationTokenSource>();
        private object _runningKey;
        private CancellationTokenSource _running;
        private bool _pumping;

        internal InlayHintQueue(Func<int, CancellationToken, Task> delay)
        {
            _delay = delay;
        }

        internal void Request(object key, Func<CancellationToken, Task> work)
        {
            lock (_gate)
            {
                // a debounced request for the same key was asked for an older state
                if (_debounce.TryGetValue(key, out var debounced))
                {
                    _debounce.Remove(key);
                    debounced.Cancel();
                }
                _pending.RemoveAll(p => Equals(p.Key, key));
                if (_running != null && Equals(_runningKey, key))
                    _running.Cancel();
                _pending.Add(new KeyValuePair<object, Func<CancellationToken, Task>>(key, work));
                if (_pumping)
                    return;
                _pumping = true;
            }
            // fire and forget: PumpAsync logs its own failures
            _ = Task.Run(PumpAsync);
        }

        /// <summary>Requests the work after <paramref name="delayMs"/> without a newer request for the same key.</summary>
        internal void RequestDebounced(object key, Func<CancellationToken, Task> work, int delayMs = 1000)
        {
            CancellationTokenSource cts;
            lock (_gate)
            {
                if (_debounce.TryGetValue(key, out var previous))
                    previous.Cancel();
                cts = new CancellationTokenSource();
                _debounce[key] = cts;
            }
            _ = _delay(delayMs, cts.Token).ContinueWith(t =>
            {
                bool run;
                lock (_gate)
                {
                    if (_debounce.TryGetValue(key, out var current) && current == cts)
                        _debounce.Remove(key);
                    // only cancelled while it is in _debounce, under the lock: no Cancel after the Dispose
                    run = t.Status == TaskStatus.RanToCompletion && !cts.IsCancellationRequested;
                }
                cts.Dispose();
                if (run)
                    Request(key, work);
            }, TaskScheduler.Default);
        }

        private async Task PumpAsync()
        {
            while (true)
            {
                Func<CancellationToken, Task> work;
                CancellationTokenSource cts;
                lock (_gate)
                {
                    if (_pending.Count == 0)
                    {
                        _pumping = false;
                        return;
                    }
                    _runningKey = _pending[0].Key;
                    work = _pending[0].Value;
                    _pending.RemoveAt(0);
                    _running = cts = new CancellationTokenSource();
                }
                try
                {
                    await work(cts.Token).ConfigureAwait(false);
                }
                catch (OperationCanceledException)
                {
                    // replaced by a newer request
                }
                catch (Exception ex)
                {
                    Logger.Exception(ex, "Inlay hints: computation failed");
                }
                finally
                {
                    lock (_gate)
                    {
                        _running = null;
                        _runningKey = null;
                    }
                    cts.Dispose();
                }
            }
        }
    }
}
