using System.Collections.Generic;
using System.Linq;
using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
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
        public void ConstructorCandidatesAreCreationsChainsAndDeclarations()
        {
            var text = "CLASS Sub INHERIT Foo\nCONSTRUCTOR(n AS INT)\n    SUPER(n)\n    SUPER:Bar()\n" +
                "CONSTRUCTOR()\n    SELF(2)\n    SELF:Bar()\n    LOCAL o AS Foo\n    o := foo{1}\n";
            var hits = ReferenceCandidateScanner.FindConstructorTokens(TestLexer.Lex(text), "Foo");
            Assert.Equal(new[] { "CONSTRUCTOR", "SUPER", "CONSTRUCTOR", "SELF", "foo" }, hits.Select(t => t.Text).ToArray());
        }

        [Fact]
        public void GenericCreationIsAConstructorCandidate()
        {
            var text = "x := Box<INT>{3, TRUE}\ny := Box < 3\nLOCAL o AS Box<STRING>\n";
            var tokens = TestLexer.Lex(text);
            var hits = ReferenceCandidateScanner.FindConstructorTokens(tokens, "Box");
            Assert.Equal(new[] { text.IndexOf("Box<INT>") }, hits.Select(t => t.StartIndex).ToArray());
        }

        private static string[] Holes(string text, bool allowDot = false)
        {
            return ReferenceCandidateScanner.InterpolationHoles(text, allowDot)
                .Select(h => text.Substring(h.Start, h.Length)).ToArray();
        }

        [Fact]
        public void InterpolationHolesAreTheExpressions()
        {
            Assert.Equal(new[] { "nCount", "oFoo:Name" }, Holes("i\"{nCount} items of {oFoo:Name}\""));
            Assert.Equal(new[] { "x" }, Holes("ie\"\\t{x}\\n\""));
            Assert.Equal(new[] { "x" }, Holes("ei\"{x}\""));
        }

        [Fact]
        public void InterpolationHolesSkipEscapedBracesAndFormats()
        {
            Assert.Equal(new[] { "x" }, Holes("i\"{{Foo}} {x::N2}\""));
            Assert.Equal(new[] { "oFoo:Name" }, Holes("i\"{oFoo:Name}\""));
            // with /allowdot a single colon starts the format
            Assert.Equal(new[] { "oFoo.Name" }, Holes("i\"{oFoo.Name:N2}\"", allowDot: true));
            Assert.Equal(new[] { "Foo{1}" }, Holes("i\"{Foo{1}}\""));
            Assert.Empty(Holes("i\"{unclosed\""));
        }

        [Fact]
        public void InterpolatedCandidatesAreAtTheirPlaceInTheFile()
        {
            var text = "FUNCTION Start()\n    ? i\"{foo} and {{Foo}} {Bar(foo)::N2}\"\n    ? \"{Foo}\"\n";
            var tokens = TestLexer.Lex(text);
            var hits = ReferenceCandidateScanner.FindInterpolatedTokens(tokens, "Foo", s => TestLexer.Lex(s), false);
            Assert.Equal(2, hits.Count);
            var first = text.IndexOf("{foo}") + 1;
            var second = text.IndexOf("(foo)") + 1;
            Assert.Equal(new[] { first, second }, hits.Select(h => h.Token.StartIndex).ToArray());
            Assert.All(hits, h =>
            {
                Assert.Equal("foo", text.Substring(h.Token.StartIndex, h.Token.StopIndex - h.Token.StartIndex + 1));
                Assert.Equal(2, h.Token.Line);
                Assert.Equal(XSharpLexer.INTERPOLATED_STRING_CONST, h.String.Type);
            });
            Assert.Equal(first - text.LastIndexOf('\n', first) - 1, hits[0].Token.Column);
            // the expression of the second hole, without its format
            Assert.Equal(new[] { "Bar", "(", "foo", ")" }, hits[1].Tokens.Select(t => t.Text).ToArray());
        }

        [Fact]
        public void SplicedTokensReplaceTheStringByTheExpression()
        {
            var text = "x := i\"{foo}\" + y\n";
            var tokens = TestLexer.Lex(text);
            var hit = ReferenceCandidateScanner.FindInterpolatedTokens(tokens, "foo", s => TestLexer.Lex(s), false).Single();
            var spliced = ReferenceCandidateScanner.Splice(tokens, hit);
            var texts = spliced.Where(t => t.Channel == XSharpLexer.DefaultTokenChannel).Select(t => t.Text).ToArray();
            Assert.Equal(new[] { "x", ":=", "foo", "+", "y" }, texts.Take(5).ToArray());
            Assert.Contains(hit.Token, spliced);
        }

        [Fact]
        public void SendsAreNamesAfterAColon()
        {
            var tokens = TestLexer.Lex("o:Speak()\nSpeak()\nSELF:Speak\no:Speak:Speak\n");
            var names = ReferenceCandidateScanner.FindIdentifierTokens(tokens, "Speak");
            Assert.Equal(new[] { true, false, true, true, true }, names.Select(t => ReferenceCandidateScanner.IsSend(tokens, t)).ToArray());
        }

        [Fact]
        public void SymbolCandidatesAreSymbolLiteralsOfTheName()
        {
            var tokens = TestLexer.Lex("Send(o, #Close)\nIVarGet(o, #close)\nx := #CloseAll\n? \"#Close\"\n", "dialect:VO");
            var hits = ReferenceCandidateScanner.FindSymbolTokens(tokens, "Close");
            Assert.Equal(new[] { 1, 2 }, hits.Select(t => t.Line).ToArray());
        }

        [Fact]
        public void WordsInCommentsAreFound()
        {
            var text = "// Close the file\n/* close */ x := 1\n/// <summary>Close</summary>\n// CloseAll\n";
            var hits = ReferenceCandidateScanner.FindWordInText(TestLexer.Lex(text), "Close", TextTokenKinds.Comments);
            Assert.Equal(new[] { text.IndexOf("Close the"), text.IndexOf("close */"), text.IndexOf("Close</") }, hits.Select(h => h.Start).ToArray());
            Assert.All(hits, h => Assert.Equal(5, h.Length));
            Assert.All(hits, h => Assert.Equal(TextTokenKinds.Comments, h.Kind));
        }

        [Fact]
        public void WordsInStringsAreFoundOutsideInterpolationHoles()
        {
            var text = "? \"Close it\", i\"Close {Close()}\", \"CloseAll\"\n";
            var hits = ReferenceCandidateScanner.FindWordInText(TestLexer.Lex(text), "Close", TextTokenKinds.Strings);
            Assert.Equal(new[] { text.IndexOf("Close it"), text.IndexOf("Close {") }, hits.Select(h => h.Start).ToArray());
        }

        [Fact]
        public void WordsInInactiveCodeAreFound()
        {
            var text = "#ifdef NOTDEFINED\nFoo()\n#endif\nFoo()\n";
            var hits = ReferenceCandidateScanner.FindWordInText(TestLexer.Lex(text), "Foo", TextTokenKinds.InactiveCode);
            Assert.Equal(new[] { text.IndexOf("Foo()") }, hits.Select(h => h.Start).ToArray());
        }

        [Fact]
        public void KindsSelectWhatIsSearched()
        {
            var text = "// Foo\n? \"Foo\"\n";
            Assert.Single(ReferenceCandidateScanner.FindWordInText(TestLexer.Lex(text), "Foo", TextTokenKinds.Comments));
            Assert.Empty(ReferenceCandidateScanner.FindWordInText(TestLexer.Lex(text), "Foo", TextTokenKinds.None));
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
