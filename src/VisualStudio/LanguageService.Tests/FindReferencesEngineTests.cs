using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using XSharp.LanguageService.FindReferences;
using XSharpModel;
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

        private static XSourceMemberSymbol Member(string text, string declaration)
        {
            int start = text.IndexOf(declaration);
            int stop = text.IndexOf('\n', start);
            int line = text.Substring(0, start).Count(c => c == '\n');
            return new XSourceMemberSymbol("Run", Kind.Method, Modifiers.Public, new TextRange(line, 0, line + 1, 0),
                new TextInterval(start, stop + 20 > text.Length ? text.Length - 1 : stop + 20), "VOID", null);
        }

        [Fact]
        public void DeclaredEntityIsTheEntityWhoseNameTheTokenIs()
        {
            var text = "CLASS Over\nMETHOD Run(n AS INT) AS VOID\n    SELF:Run(\"a\")\nMETHOD Run(s AS STRING) AS VOID\n    RETURN\n";
            var number = Member(text, "METHOD Run(n");
            var name = Member(text, "METHOD Run(s");
            var entities = new List<XSourceEntity> { number, name };
            var tokens = TestLexer.Lex(text);
            var declarationOfString = tokens.First(t => t.StartIndex == text.IndexOf("Run(s"));
            var call = tokens.First(t => t.StartIndex == text.IndexOf("Run(\"a\")"));
            Assert.Same(name, FindReferencesEngine.DeclaredEntity(entities, declarationOfString));
            Assert.Null(FindReferencesEngine.DeclaredEntity(entities, call));
        }

        private static XSourceMemberSymbol Constructor(string text, string declaration)
        {
            var member = Member(text, declaration);
            return new XSourceMemberSymbol(".ctor", Kind.Constructor, Modifiers.Public, member.Range, member.Interval, "", null);
        }

        [Fact]
        public void DeclaredEntityOfTheConstructorKeyword()
        {
            var text = "CLASS Foo\nCONSTRUCTOR(n AS INT)\n    x := Foo{}\nCLASS Bar\nCONSTRUCTOR()\n    RETURN\n";
            var foo = Constructor(text, "CONSTRUCTOR(n");
            var bar = Constructor(text, "CONSTRUCTOR()");
            var entities = new List<XSourceEntity> { foo, bar };
            var tokens = TestLexer.Lex(text);
            Assert.Same(foo, FindReferencesEngine.DeclaredEntity(entities, tokens.First(t => t.StartIndex == text.IndexOf("CONSTRUCTOR(n"))));
            Assert.Same(bar, FindReferencesEngine.DeclaredEntity(entities, tokens.First(t => t.StartIndex == text.IndexOf("CONSTRUCTOR()"))));
            Assert.Null(FindReferencesEngine.DeclaredEntity(entities, tokens.First(t => t.StartIndex == text.IndexOf("Foo{"))));
        }

        [Fact]
        public void CreationResolvesToTheConstructors()
        {
            var text = "x := Shape{1}\ny := Shape\nz := Plain{}\n";
            var tokens = TestLexer.Lex(text);
            var shape = new XSourceTypeSymbol("Shape", Kind.Class, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(0, 100), null, null);
            var ctor = new XSourceMemberSymbol(".ctor", Kind.Constructor, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(50, 60), "", null);
            shape.AddMember(ctor);
            var plain = new XSourceTypeSymbol("Plain", Kind.Class, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(200, 300), null, null);
            var creation = tokens.First(t => t.StartIndex == text.IndexOf("Shape{"));
            var typeOnly = tokens.First(t => t.StartIndex == text.IndexOf("Shape\n"));
            var plainCreation = tokens.First(t => t.StartIndex == text.IndexOf("Plain{"));
            Assert.Equal(new IXSymbol[] { ctor }, SymbolResolver.ForCreation(new IXSymbol[] { shape }, tokens, creation));
            Assert.Equal(new IXSymbol[] { shape }, SymbolResolver.ForCreation(new IXSymbol[] { shape }, tokens, typeOnly));
            // no declared constructor: the class itself
            Assert.Equal(new IXSymbol[] { plain }, SymbolResolver.ForCreation(new IXSymbol[] { plain }, tokens, plainCreation));
        }

        [Fact]
        public void GroupOfAMemberIsItsDeclaration()
        {
            var foo = new XSourceTypeSymbol("Foo", Kind.Class, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(0, 100), null, null);
            foo.Namespace = "Test";
            var bar = new XSourceTypeSymbol("Bar", Kind.Class, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(200, 300), null, null);
            bar.Namespace = "Test";
            var fooClose = new XSourceMemberSymbol("Close", Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(50, 60), "", null);
            foo.AddMember(fooClose);
            var barClose = new XSourceMemberSymbol("Close", Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(250, 260), "", null);
            bar.AddMember(barClose);
            Assert.Contains("Test.Foo.Close", FindReferencesEngine.GroupOf(fooClose));
            Assert.NotEqual(FindReferencesEngine.GroupOf(fooClose), FindReferencesEngine.GroupOf(barClose));
            // ACCESS, ASSIGN and PROPERTY with one name are one property: one group
            var access = new XSourceMemberSymbol("Name", Kind.Access, Modifiers.Public, new TextRange(4, 1, 5, 1), new TextInterval(70, 80), "", null);
            foo.AddMember(access);
            var assign = new XSourceMemberSymbol("Name", Kind.Assign, Modifiers.Public, new TextRange(6, 1, 7, 1), new TextInterval(85, 95), "", null);
            foo.AddMember(assign);
            Assert.Equal(FindReferencesEngine.GroupOf(access), FindReferencesEngine.GroupOf(assign));
        }

        [Fact]
        public void OnlySourceSymbolsCanBeSearched()
        {
            var file = new XFile(@"C:\Test\A.prg", null);
            var type = new XSourceTypeSymbol("Foo", Kind.Class, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(0, 100), file, null);
            var close = new XSourceMemberSymbol("Close", Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(50, 60), "", null);
            type.AddMember(close);
            var orphan = new XSourceMemberSymbol("Close", Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(50, 60), "", null);
            Assert.True(FindReferencesEngine.IsInSource(type));
            Assert.True(FindReferencesEngine.IsInSource(close));
            Assert.False(FindReferencesEngine.IsInSource(orphan));
            Assert.False(FindReferencesEngine.IsInSource(new XKeywordSymbol("IF")));
        }

        [Fact]
        public void SearchNameOfAConstructorIsItsClass()
        {
            var type = new XSourceTypeSymbol("Foo", Kind.Class, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(0, 100), null, null);
            var ctor = new XSourceMemberSymbol(".ctor", Kind.Constructor, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(50, 60), "", null);
            type.AddMember(ctor);
            var run = new XSourceMemberSymbol("Run", Kind.Method, Modifiers.Public, new TextRange(4, 1, 5, 1), new TextInterval(70, 80), "", null);
            type.AddMember(run);
            Assert.Equal("Foo", FindReferencesEngine.SearchName(ctor));
            Assert.Equal("Run", FindReferencesEngine.SearchName(run));
            Assert.Equal("Foo", FindReferencesEngine.SearchName(type));
        }

        [Fact]
        public void MembersCanBeReachedLateBound()
        {
            var type = new XSourceTypeSymbol("Foo", Kind.Class, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(0, 100), null, null);
            XSourceMemberSymbol Add(string name, Kind kind)
            {
                var member = new XSourceMemberSymbol(name, kind, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(50, 60), "", null);
                type.AddMember(member);
                return member;
            }
            Assert.True(FindReferencesEngine.CanBeLateBound(Add("Close", Kind.Method)));
            Assert.True(FindReferencesEngine.CanBeLateBound(Add("Name", Kind.Access)));
            Assert.True(FindReferencesEngine.CanBeLateBound(Add("Name", Kind.Assign)));
            Assert.True(FindReferencesEngine.CanBeLateBound(Add("Size", Kind.Property)));
            Assert.True(FindReferencesEngine.CanBeLateBound(Add("nCount", Kind.Field)));
            Assert.False(FindReferencesEngine.CanBeLateBound(Add(".ctor", Kind.Constructor)));
            Assert.False(FindReferencesEngine.CanBeLateBound(Add("gnCount", Kind.VOGlobal)));
            Assert.False(FindReferencesEngine.CanBeLateBound(Add("MAX_SIZE", Kind.VODefine)));
            Assert.False(FindReferencesEngine.CanBeLateBound(type));
            Assert.False(FindReferencesEngine.CanBeLateBound(new XSourceMemberSymbol("Start", Kind.Function, Modifiers.Public,
                new TextRange(2, 1, 3, 1), new TextInterval(50, 60), "", null)));
        }

        [Fact]
        public void LateBoundHitsHaveTheirOwnGroup()
        {
            var type = new XSourceTypeSymbol("Foo", Kind.Class, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(0, 100), null, null);
            var close = new XSourceMemberSymbol("Close", Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(50, 60), "", null);
            type.AddMember(close);
            var group = FindReferencesEngine.LateBoundGroup(close);
            Assert.Contains("Close", group);
            Assert.NotEqual(FindReferencesEngine.GroupOf(close), group);
        }
    }
}
