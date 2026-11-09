using System.Collections.Generic;
using System.Linq;
using LanguageService.SyntaxTree;
using XSharp.LanguageService.InlayHints;
using Xunit;

namespace XSharp.LanguageService.Tests.InlayHints
{
    public class HintCandidatesTests
    {
        private static IReadOnlyList<CallCandidate> Calls(string text) => HintCandidates.Calls(TestLexer.Lex(text));

        private static IReadOnlyList<DeclarationCandidate> Declarations(string text) => HintCandidates.Declarations(TestLexer.Lex(text));

        private static string Text(IList<IToken> tokens) => string.Join(" ", tokens.Select(t => t.Text));

        [Fact]
        public void FindsMethodAndFunctionCalls()
        {
            var calls = Calls("o:Open(1, TRUE)\nFoo(\"a\")\n");
            Assert.Equal(new[] { "Open", "Foo" }, calls.Select(c => c.Name.Text));
            Assert.Equal(2, calls[0].Arguments.Count);
            Assert.Single(calls[1].Arguments);
            Assert.False(calls[0].IsCreation);
        }

        [Fact]
        public void FindsConstructorCall()
        {
            var call = Assert.Single(Calls("x := Customer{1}\n"));
            Assert.Equal("Customer", call.Name.Text);
            Assert.True(call.IsCreation);
        }

        [Fact]
        public void FindsGenericConstructorAndMethodCalls()
        {
            var calls = Calls("x := List<STRING>{10}\no:Foo<INT>(1, 2)\n");
            Assert.Equal(new[] { "List", "Foo" }, calls.Select(c => c.Name.Text));
            Assert.True(calls[0].IsCreation);
            Assert.Single(calls[0].Arguments);
            Assert.False(calls[1].IsCreation);
            Assert.True(calls[1].IsMemberAccess);
            Assert.Equal(2, calls[1].Arguments.Count);
        }

        [Fact]
        public void NestedCallsAreBothFound()
        {
            var calls = Calls("Foo(Bar(1), 2)\n");
            Assert.Equal(new[] { "Foo", "Bar" }, calls.Select(c => c.Name.Text));
            Assert.Equal(2, calls[0].Arguments.Count);
            Assert.Single(calls[1].Arguments);
        }

        [Fact]
        public void ArgumentOnContinuedLineKeepsItsToken()
        {
            var call = Assert.Single(Calls("Foo(1, ;\n   TRUE)\n"));
            var first = call.Arguments[1].First();
            Assert.Equal("TRUE", first.Text);
            Assert.Equal(2, first.Line);
        }

        [Fact]
        public void EmptyArgumentIsKept()
        {
            var call = Assert.Single(Calls("Foo(1,,3)\n"));
            Assert.Equal(3, call.Arguments.Count);
            Assert.Empty(call.Arguments[1]);
        }

        [Fact]
        public void NoArgumentsNoCandidate()
        {
            Assert.Empty(Calls("Foo()\n"));
        }

        [Fact]
        public void DeclarationIsNotACall()
        {
            Assert.Empty(Calls("FUNCTION Foo(n := 1 AS INT) AS VOID\n"));
            Assert.Empty(Calls("CLASS C\nMETHOD Bar(a, b) CLIPPER\nEND CLASS\n"));
        }

        [Fact]
        public void FindsImpliedDeclarations()
        {
            var decls = Declarations("VAR a := 1\nLOCAL IMPLIED b := 2\nFOREACH VAR c IN list\nNEXT\nFOR VAR i := 1 TO 3\nNEXT\nUSING VAR s := x\n");
            Assert.Equal(new[] { ImpliedDeclarationKind.Var, ImpliedDeclarationKind.LocalImplied, ImpliedDeclarationKind.ForEach,
                ImpliedDeclarationKind.For, ImpliedDeclarationKind.Using }, decls.Select(d => d.Kind));
            Assert.Equal(new[] { "a", "b", "c", "i", "s" }, decls.Select(d => d.Name.Text));
            Assert.Equal("list", Text(decls[2].Initializer));
            Assert.Equal("1", Text(decls[3].Initializer));
        }

        [Fact]
        public void SeveralDeclaratorsAreSplit()
        {
            var decls = Declarations("VAR a := 1, b := Foo(2, 3)\n");
            Assert.Equal(new[] { "a", "b" }, decls.Select(d => d.Name.Text));
            Assert.All(decls, d => Assert.Equal(2, d.DeclaratorCount));
            Assert.Equal("Foo ( 2 , 3 )", Text(decls[1].Initializer));
            Assert.Equal("1", Text(decls[0].Initializer));
        }

        [Theory]
        // the comma between type arguments does not end the declarator: the double-click edit put AS in the middle
        [InlineData("VAR d := Dictionary<STRING, INT>{}\n", "Dictionary < STRING , INT > { }")]
        [InlineData("VAR d := System.Collections.Generic.Dictionary<STRING, List<INT>>{}\n", "System . Collections . Generic . Dictionary < STRING , List < INT > > { }")]
        [InlineData("VAR a := <Dictionary<STRING, INT>>{}\n", "< Dictionary < STRING , INT > > { }")]
        [InlineData("VAR p := Pair<STRING, INT>(\"a\", 1)\n", "Pair < STRING , INT > ( \"a\" , 1 )")]
        public void TypeArgumentsStayInTheInitializer(string source, string initializer)
        {
            var decl = Assert.Single(Declarations(source));
            Assert.Equal(1, decl.DeclaratorCount);
            Assert.Equal(initializer, Text(decl.Initializer));
        }

        [Fact]
        public void AComparisonBeforeACommaIsNoTypeArgument()
        {
            var decls = Declarations("VAR a := x < y, b := 2\n");
            Assert.Equal(new[] { "a", "b" }, decls.Select(d => d.Name.Text));
            Assert.Equal("x < y", Text(decls[0].Initializer));
        }

        [Fact]
        public void OutVarIsNotACandidate()
        {
            Assert.Empty(Declarations("Foo(OUT VAR x)\n"));
        }

        [Fact]
        public void ALargeFileIsScannedInLinearTime()
        {
            // in the editor the token index does not always match the position in the list; a lookup of each
            // call name in the list made the scan of a large file take seconds
            var tokens = TestLexer.Lex(string.Concat(Enumerable.Repeat("Foo(1, 2)\n", 20000)));
            foreach (var token in tokens.OfType<CommonToken>())
                token.TokenIndex = -1;
            var watch = System.Diagnostics.Stopwatch.StartNew();
            var calls = HintCandidates.Calls(tokens);
            watch.Stop();
            Assert.Equal(20000, calls.Count);
            Assert.Equal(2, calls[19999].Arguments.Count);
            Assert.True(watch.ElapsedMilliseconds < 1000, $"{watch.ElapsedMilliseconds} ms");
        }
    }
}
