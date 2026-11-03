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
    }
}
