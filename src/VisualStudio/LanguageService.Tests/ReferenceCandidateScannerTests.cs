using System.Collections.Generic;
using System.Linq;
using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.SyntaxTree;
using XSharp.LanguageService.FindReferences;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class ReferenceCandidateScannerTests
    {
        [Fact]
        public void FindsIdentifierCaseInsensitive()
        {
            var tokens = TestLexer.Lex("local oFoo as Foo\noFoo := FOO{}\n");
            var hits = ReferenceCandidateScanner.FindIdentifierTokens(tokens, "foo");
            Assert.Equal(new[] { "Foo", "FOO" }, hits.Select(t => t.Text).ToArray());
        }

        [Fact]
        public void SkipsCommentsAndStrings()
        {
            var tokens = TestLexer.Lex("// Foo\n/* Foo */\n&& Foo\n? \"Foo\"\nx := Foo\n", "dialect:VO");
            var hits = ReferenceCandidateScanner.FindIdentifierTokens(tokens, "Foo");
            Assert.Single(hits);
            Assert.Equal(5, hits[0].Line);
        }

        [Fact]
        public void SkipsTextEndText()
        {
            var tokens = TestLexer.Lex("FUNCTION Start()\nTEXT\nFoo\nENDTEXT\nFoo()\n", "dialect:VO");
            var hits = ReferenceCandidateScanner.FindIdentifierTokens(tokens, "Foo");
            Assert.Single(hits);
            Assert.Equal(5, hits[0].Line);
        }

        [Fact]
        public void SkipsInactiveIfdef()
        {
            var tokens = TestLexer.Lex("#ifdef NOTDEFINED\nFoo()\n#endif\nFoo()\n");
            var hits = ReferenceCandidateScanner.FindIdentifierTokens(tokens, "Foo");
            Assert.Single(hits);
            Assert.Equal(4, hits[0].Line);
        }

        [Fact]
        public void MayContainIsCaseInsensitive()
        {
            Assert.True(ReferenceCandidateScanner.MayContain("x := FOO", "foo"));
            Assert.False(ReferenceCandidateScanner.MayContain("x := Bar", "foo"));
        }
    }

    internal static class TestLexer
    {
        internal static IList<IToken> Lex(string text, params string[] options)
        {
            var parseOptions = XSharpParseOptions.FromVsValues(options.ToList());
            XSharp.Parser.VsParser.Lex(text, "test.prg", parseOptions, new NullErrorListener(), out ITokenStream stream, out _);
            return ((BufferedTokenStream)stream).GetTokens();
        }

        private class NullErrorListener : XSharp.Parser.VsParser.IErrorListener
        {
            public void ReportError(string fileName, LinePositionSpan span, string errorCode, string message, object[] args) { }
            public void ReportWarning(string fileName, LinePositionSpan span, string errorCode, string message, object[] args) { }
        }
    }
}
