using System;
using System.Collections.Generic;
using System.Threading;
using XSharp.LanguageService.FindReferences;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class FindReferencesEngineTests
    {
        [Fact]
        public void FailingFileDoesNotEndTheSearch()
        {
            var stats = new FindReferencesStats();
            var searched = new List<string>();
            FindReferencesEngine.SearchEach(new[] { "a.prg", "missing.prg", "c.prg" }, path =>
            {
                if (path == "missing.prg")
                    throw new System.IO.FileNotFoundException(path);
                searched.Add(path);
            }, stats, CancellationToken.None);
            Assert.Equal(new[] { "a.prg", "c.prg" }, searched);
            Assert.Equal(1, stats.Failed);
        }

        [Fact]
        public void CancellationStillEndsTheSearch()
        {
            var stats = new FindReferencesStats();
            using (var cts = new CancellationTokenSource())
            {
                Assert.Throws<OperationCanceledException>(() =>
                    FindReferencesEngine.SearchEach(new[] { "a.prg", "b.prg" }, path =>
                    {
                        cts.Cancel();
                        cts.Token.ThrowIfCancellationRequested();
                    }, stats, cts.Token));
            }
            Assert.Equal(0, stats.Failed);
        }
    }
}
