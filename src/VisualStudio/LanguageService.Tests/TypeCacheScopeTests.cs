//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    /// <summary>
    /// The type cache of one search (XTypeCacheScope). Only the thread that opened the scope sees it, nested scopes
    /// share it, and it is gone when the outermost scope is disposed.
    /// </summary>
    public class TypeCacheScopeTests
    {
        private static XSourceTypeSymbol NewType(string name) =>
            new XSourceTypeSymbol(name, Kind.Class, Modifiers.Public, default, default, null, null);

        [Fact]
        public void NoScope_TryGetMisses_AddIsIgnored()
        {
            XTypeCacheScope.Add("k", NewType("Foo"));
            Assert.False(XTypeCacheScope.IsOpen);
            Assert.False(XTypeCacheScope.TryGet("k", out _));
        }

        [Fact]
        public void Scope_CachesTypesAndNull()
        {
            var foo = NewType("Foo");
            using (XTypeCacheScope.Begin())
            {
                XTypeCacheScope.Add("a", foo);
                XTypeCacheScope.Add("b", null);
                Assert.True(XTypeCacheScope.TryGet("a", out var a));
                Assert.Same(foo, a);
                Assert.True(XTypeCacheScope.TryGet("b", out var b));
                Assert.Null(b);
                Assert.False(XTypeCacheScope.TryGet("c", out _));
            }
            Assert.False(XTypeCacheScope.IsOpen);
            Assert.False(XTypeCacheScope.TryGet("a", out _));
        }

        [Fact]
        public void Scope_Nested_InnerDisposeKeepsCache()
        {
            var foo = NewType("Foo");
            using (XTypeCacheScope.Begin())
            {
                XTypeCacheScope.Add("a", foo);
                var inner = XTypeCacheScope.Begin();
                inner.Dispose();
                inner.Dispose();
                Assert.True(XTypeCacheScope.IsOpen);
                Assert.True(XTypeCacheScope.TryGet("a", out var a));
                Assert.Same(foo, a);
            }
            Assert.False(XTypeCacheScope.TryGet("a", out _));
        }

        [Fact]
        public void Scope_DisposedAfterException_NoCacheLeft()
        {
            Action search = () =>
            {
                using (XTypeCacheScope.Begin())
                {
                    XTypeCacheScope.Add("a", NewType("Foo"));
                    throw new InvalidOperationException();
                }
            };
            Assert.Throws<InvalidOperationException>(search);
            Assert.False(XTypeCacheScope.IsOpen);
            Assert.False(XTypeCacheScope.TryGet("a", out _));
        }

        [Fact]
        public void Scope_OtherThread_SeesNoCache()
        {
            using (XTypeCacheScope.Begin())
            {
                XTypeCacheScope.Add("a", NewType("Foo"));
                bool other = Task.Factory.StartNew(() => XTypeCacheScope.IsOpen || XTypeCacheScope.TryGet("a", out _),
                    TaskCreationOptions.LongRunning).Result;
                Assert.False(other);
                Assert.True(XTypeCacheScope.TryGet("a", out _));
            }
        }

        [Fact]
        public void Key_DiffersByProject()
        {
            var rows = new List<XDbResult>
            {
                new XDbResult { IdType = 10, IdProject = 5 },
                new XDbResult { IdType = 11, IdProject = 5 }
            };
            Assert.Equal(XTypeCacheScope.KeyOf(1, rows), XTypeCacheScope.KeyOf(1, rows));
            Assert.NotEqual(XTypeCacheScope.KeyOf(1, rows), XTypeCacheScope.KeyOf(2, rows));
            Assert.Equal("1|10:5,11:5,", XTypeCacheScope.KeyOf(1, rows));
        }
    }
}
