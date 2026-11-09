using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using XSharp.LanguageService.InlayHints;
using Xunit;

namespace XSharp.LanguageService.Tests.InlayHints
{
    public class InlayHintQueueTests
    {
        private static readonly TimeSpan Timeout = TimeSpan.FromSeconds(10);

        private static InlayHintQueue NewQueue() => new InlayHintQueue((ms, ct) => Task.Delay(ms, ct));

        private static void Wait(Task task) => Assert.True(task.Wait(Timeout), "timed out");

        [Fact]
        public void RunsOneAtATime()
        {
            var queue = NewQueue();
            var log = new ConcurrentQueue<string>();
            var releaseA = new TaskCompletionSource<bool>();
            var startedA = new TaskCompletionSource<bool>();
            var doneB = new TaskCompletionSource<bool>();
            queue.Request("a", async ct =>
            {
                log.Enqueue("a+");
                startedA.SetResult(true);
                await releaseA.Task;
                log.Enqueue("a-");
            });
            queue.Request("b", ct =>
            {
                log.Enqueue("b+");
                doneB.SetResult(true);
                return Task.CompletedTask;
            });
            Wait(startedA.Task);
            Thread.Sleep(100);
            Assert.Equal(new[] { "a+" }, log);
            releaseA.SetResult(true);
            Wait(doneB.Task);
            Assert.Equal(new[] { "a+", "a-", "b+" }, log);
        }

        [Fact]
        public void QueuedRequestIsReplaced()
        {
            var queue = NewQueue();
            var releaseA = new TaskCompletionSource<bool>();
            var startedA = new TaskCompletionSource<bool>();
            var ran = new ConcurrentQueue<string>();
            var doneB = new TaskCompletionSource<bool>();
            queue.Request("a", async ct => { startedA.SetResult(true); await releaseA.Task; });
            Wait(startedA.Task);
            queue.Request("b", ct => { ran.Enqueue("b1"); return Task.CompletedTask; });
            queue.Request("b", ct => { ran.Enqueue("b2"); doneB.SetResult(true); return Task.CompletedTask; });
            releaseA.SetResult(true);
            Wait(doneB.Task);
            Thread.Sleep(100);
            Assert.Equal(new[] { "b2" }, ran);
        }

        [Fact]
        public void NewerRequestCancelsRunningOne()
        {
            var queue = NewQueue();
            var started = new TaskCompletionSource<bool>();
            var cancelled = new TaskCompletionSource<bool>();
            var second = new TaskCompletionSource<bool>();
            queue.Request("a", async ct =>
            {
                started.SetResult(true);
                try
                {
                    await Task.Delay(System.Threading.Timeout.Infinite, ct);
                }
                catch (OperationCanceledException)
                {
                    cancelled.SetResult(true);
                    throw;
                }
            });
            Wait(started.Task);
            queue.Request("a", ct => { second.SetResult(true); return Task.CompletedTask; });
            Wait(cancelled.Task);
            Wait(second.Task);
        }

        [Fact]
        public void DebounceCollapsesBursts()
        {
            var delays = new List<TaskCompletionSource<bool>>();
            var queue = new InlayHintQueue((ms, ct) =>
            {
                var tcs = new TaskCompletionSource<bool>();
                ct.Register(() => tcs.TrySetCanceled());
                lock (delays)
                    delays.Add(tcs);
                return tcs.Task;
            });
            int runs = 0;
            var done = new TaskCompletionSource<bool>();
            for (int i = 0; i < 3; i++)
                queue.RequestDebounced("a", ct => { Interlocked.Increment(ref runs); done.TrySetResult(true); return Task.CompletedTask; });
            lock (delays)
            {
                Assert.Equal(3, delays.Count);
                Assert.True(delays[0].Task.IsCanceled);
                Assert.True(delays[1].Task.IsCanceled);
                delays[2].SetResult(true);
            }
            Wait(done.Task);
            Thread.Sleep(100);
            Assert.Equal(1, runs);
        }

        [Fact]
        public void ARequestCancelsAPendingDebounceOfTheSameKey()
        {
            // the debounced work was asked for an older state; it would run once more after the newer request
            var delays = new List<(TaskCompletionSource<bool> Delay, CancellationToken Token)>();
            var queue = new InlayHintQueue((ms, ct) =>
            {
                var tcs = new TaskCompletionSource<bool>();
                ct.Register(() => tcs.TrySetCanceled());
                lock (delays)
                    delays.Add((tcs, ct));
                return tcs.Task;
            });
            var ran = new ConcurrentQueue<string>();
            var done = new TaskCompletionSource<bool>();
            queue.RequestDebounced("a", ct => { ran.Enqueue("debounced"); return Task.CompletedTask; });
            queue.Request("a", ct => { ran.Enqueue("now"); done.TrySetResult(true); return Task.CompletedTask; });
            Wait(done.Task);
            lock (delays)
            {
                Assert.True(delays[0].Token.IsCancellationRequested);
                delays[0].Delay.TrySetResult(true);
            }
            Thread.Sleep(100);
            Assert.Equal(new[] { "now" }, ran);
        }

        [Fact]
        public void DebounceTokensAreDisposed()
        {
            var tokens = new List<CancellationToken>();
            var delays = new List<TaskCompletionSource<bool>>();
            var queue = new InlayHintQueue((ms, ct) =>
            {
                var tcs = new TaskCompletionSource<bool>();
                ct.Register(() => tcs.TrySetCanceled());
                lock (delays)
                {
                    delays.Add(tcs);
                    tokens.Add(ct);
                }
                return tcs.Task;
            });
            var done = new TaskCompletionSource<bool>();
            queue.RequestDebounced("a", ct => Task.CompletedTask);
            queue.RequestDebounced("a", ct => { done.TrySetResult(true); return Task.CompletedTask; });
            lock (delays)
                delays[1].SetResult(true);
            Wait(done.Task);
            Thread.Sleep(100);
            // the WaitHandle of a token whose source is disposed throws
            Assert.All(tokens, t => Assert.Throws<ObjectDisposedException>(() => t.WaitHandle));
        }
    }
}
