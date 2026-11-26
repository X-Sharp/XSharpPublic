//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace XSharp.LanguageService.Tests.InlayHints
{
    public class LookupThreadingTests
    {
        [Fact]
        public void NestedSearchesArePerThread()
        {
            var barrier = new Barrier(2);
            int CountOnOwnThread(string name)
            {
                XSharpLookup.NestedSearches.Clear();
                XSharpLookup.NestedSearches.Add(name);
                barrier.SignalAndWait();
                var count = XSharpLookup.NestedSearches.Count;
                barrier.SignalAndWait();
                XSharpLookup.NestedSearches.Clear();
                return count;
            }
            var a = Task.Run(() => CountOnOwnThread("a"));
            var b = Task.Run(() => CountOnOwnThread("b"));
            Assert.Equal(1, a.Result);
            Assert.Equal(1, b.Result);
        }
    }
}
