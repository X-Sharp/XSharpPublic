using XSharp.LanguageService.FindReferences;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class XDocumentIndexerTests
    {
        [Fact]
        public void GroupsTokensPerZeroBasedLine()
        {
            var tokens = TestLexer.Lex("FUNCTION Foo()\nlocal x\nx := Foo()\n");
            XDocumentIndexer.BuildIndexes(tokens, out var lineTokens, out _, out _);
            Assert.True(lineTokens.ContainsKey(0));
            Assert.True(lineTokens.ContainsKey(1));
            Assert.True(lineTokens.ContainsKey(2));
            Assert.Equal("FUNCTION", lineTokens[0][0].Text);
        }

        [Fact]
        public void IndexesIdentifiersCaseInsensitive()
        {
            var tokens = TestLexer.Lex("FUNCTION Foo()\nlocal x\nx := foo()\n");
            XDocumentIndexer.BuildIndexes(tokens, out _, out var identifiers, out _);
            Assert.Equal(2, identifiers["FOO"].Count);
        }

        [Fact]
        public void CommentOnlyLineHasNoIdentifiers()
        {
            var tokens = TestLexer.Lex("// Bar\nx := 1\n");
            XDocumentIndexer.BuildIndexes(tokens, out _, out var identifiers, out _);
            Assert.False(identifiers.ContainsKey("Bar"));
        }

        [Fact]
        public void MarksContinuedLines()
        {
            var tokens = TestLexer.Lex("x := 1 + ;\n  2\ny := 3\n");
            XDocumentIndexer.BuildIndexes(tokens, out _, out _, out var lineState);
            Assert.True(lineState.Get(1, out var flags) && flags.HasFlag(LineFlags.IsContinued));
            Assert.False(lineState.Get(2, out var flags2) && flags2.HasFlag(LineFlags.IsContinued));
        }

        [Fact]
        public void MarksAttributeLines()
        {
            var tokens = TestLexer.Lex("[Serializable];\nCLASS Foo\nEND CLASS\n");
            XDocumentIndexer.BuildIndexes(tokens, out _, out _, out var lineState);
            Assert.True(lineState.Get(0, out var flags) && flags.HasFlag(LineFlags.StartsWithAttribute));
        }
    }
}
