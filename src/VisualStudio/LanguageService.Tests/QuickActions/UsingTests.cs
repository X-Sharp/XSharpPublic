using System.Linq;
using XSharp.LanguageService.Editors.LightBulb.QuickActions;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests.QuickActions
{
    public class UsingCandidatesTests
    {
        private static readonly (string, string)[] Types = { ("A.B", "Foo"), ("C", "Foo"), ("C", "Bar") };

        [Fact]
        public void OffersEveryNamespaceWithTheName()
        {
            Assert.Equal(new[] { "A.B", "C" }, UsingCandidates.Namespaces(Types, "Foo", new string[0], "", false));
        }

        [Fact]
        public void SkipsNamespacesAlreadyUsed()
        {
            Assert.Equal(new[] { "C" }, UsingCandidates.Namespaces(Types, "Foo", new[] { "a.b" }, "", false));
        }

        [Fact]
        public void KeepsOtherCaseWhenCaseSensitive()
        {
            Assert.Equal(new[] { "A.B", "C" }, UsingCandidates.Namespaces(Types, "Foo", new[] { "a.b" }, "", true));
        }

        [Fact]
        public void SkipsCurrentNamespaceAndParents()
        {
            Assert.Equal(new[] { "C" }, UsingCandidates.Namespaces(Types, "Foo", new string[0], "A.B.X", false));
        }

        [Fact]
        public void SkipsGlobalTypes()
        {
            Assert.Empty(UsingCandidates.Namespaces(new[] { ("", "Foo") }, "Foo", new string[0], "", false));
        }

        [Fact]
        public void AppliesOnlyToBareNamesThatAreUnresolvedOrTypes()
        {
            var file = new XFile(@"C:\Test\A.prg", null);
            var type = new XSourceTypeSymbol("Helper", Kind.Class, Modifiers.Public, new TextRange(1, 1, 2, 1), new TextInterval(0, 10), file, null);
            var method = new XSourceMemberSymbol("Help", Kind.Method, Modifiers.Public, new TextRange(1, 1, 2, 1), new TextInterval(0, 10), "VOID", null);
            Assert.True(UsingCandidates.Applies(ReceiverKind.None, new IXSymbol[0]));
            // the code model finds types without their USING; the namespace filter decides
            Assert.True(UsingCandidates.Applies(ReceiverKind.None, new IXSymbol[] { type }));
            Assert.False(UsingCandidates.Applies(ReceiverKind.None, new IXSymbol[] { method }));
            // in VO an unknown name comes back as an undeclared variable
            var undeclared = new XSourceUndeclaredVariableSymbol(method, "Helper", new TextRange(1, 1, 1, 7), new TextInterval(0, 6));
            Assert.True(UsingCandidates.Applies(ReceiverKind.None, new IXSymbol[] { undeclared }));
            // SELF:Missing must not offer USING System.Reflection
            Assert.False(UsingCandidates.Applies(ReceiverKind.Self, new IXSymbol[0]));
            Assert.False(UsingCandidates.Applies(ReceiverKind.Variable, new IXSymbol[0]));
        }

        [Fact]
        public void NameIsComparedLikeTheProject()
        {
            Assert.Equal(new[] { "A.B", "C" }, UsingCandidates.Namespaces(Types, "foo", new string[0], "", false));
            Assert.Empty(UsingCandidates.Namespaces(Types, "foo", new string[0], "", true));
        }
    }

    public class UsingInserterTests
    {
        private static CodeStyle Style(System.Func<string, string> keyword = null)
        {
            return new CodeStyle(XDialect.VO, keyword ?? (s => s.ToUpperInvariant()), "    ", "\r\n");
        }

        [Fact]
        public void GoesAfterTheLastUsing()
        {
            var lines = new[] { "USING System", "USING System.IO", "", "CLASS Foo" };
            var result = UsingInserter.Insert(lines, new[] { 0, 1 }, "X.Y", Style());
            Assert.Equal(2, result.Line);
            Assert.Equal("USING X.Y\r\n", result.Text);
        }

        [Fact]
        public void WithoutUsingsGoesBeforeFirstCode()
        {
            var lines = new[] { "// header", "#include \"x.vh\"", "", "CLASS Foo" };
            var result = UsingInserter.Insert(lines, Enumerable.Empty<int>(), "X.Y", Style());
            Assert.Equal(3, result.Line);
        }

        [Fact]
        public void SkipsBlockComments()
        {
            var lines = new[] { "/* header", "   more */", "FUNCTION Start() AS VOID" };
            var result = UsingInserter.Insert(lines, Enumerable.Empty<int>(), "X.Y", Style());
            Assert.Equal(2, result.Line);
        }

        [Fact]
        public void UsesKeywordCase()
        {
            var lines = new[] { "CLASS Foo" };
            var result = UsingInserter.Insert(lines, Enumerable.Empty<int>(), "X.Y", Style(s => s.ToLowerInvariant()));
            Assert.Equal("using X.Y\r\n", result.Text);
        }
    }
}
