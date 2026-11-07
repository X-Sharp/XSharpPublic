using System;
using System.Linq;
using XSharp.LanguageService.InlayHints;
using Xunit;

namespace XSharp.LanguageService.Tests.InlayHints
{
    public class TypeRewriteTests
    {
        private static string Rewrite(string text, string type, Func<string, string> keyword = null, int index = 0)
        {
            var tokens = TestLexer.Lex(text);
            var decl = HintCandidates.Declarations(tokens)[index];
            var edit = TypeRewrite.For(decl, tokens, type, keyword ?? (s => s.ToUpperInvariant()));
            if (edit == null)
                return null;
            var result = text;
            foreach (var change in edit.Changes.OrderByDescending(c => c.Start))
                result = result.Substring(0, change.Start) + change.Text + result.Substring(change.Start + change.Length);
            return result;
        }

        [Fact]
        public void VarBecomesLocalWithTypeAtTheEnd()
        {
            Assert.Equal("LOCAL o := Foo(1) AS Customer // c\n", Rewrite("VAR o := Foo(1) // c\n", "Customer"));
        }

        [Fact]
        public void LocalImpliedBecomesLocal()
        {
            Assert.Equal("LOCAL n := Bar() AS INT\n", Rewrite("LOCAL IMPLIED n := Bar()\n", "INT"));
        }

        [Fact]
        public void InitializerOnContinuedLine()
        {
            Assert.Equal("LOCAL o := ;\n   Foo() AS Customer\n", Rewrite("VAR o := ;\n   Foo()\n", "Customer"));
        }

        [Fact]
        public void ForEachGetsAs()
        {
            Assert.Equal("FOREACH c AS Customer IN list\nNEXT\n", Rewrite("FOREACH VAR c IN list\nNEXT\n", "Customer"));
        }

        [Fact]
        public void SeveralDeclaratorsAreNotRewritten()
        {
            Assert.Null(Rewrite("VAR a := 1, b := Foo()\n", "Customer", index: 1));
        }

        [Fact]
        public void ForAndUsingAreNotRewritten()
        {
            Assert.Null(Rewrite("FOR VAR i := 1 TO 3\nNEXT\n", "INT"));
            Assert.Null(Rewrite("USING VAR s := Foo()\n", "Stream"));
        }

        [Fact]
        public void StaticVarIsNotRewritten()
        {
            Assert.Null(Rewrite("STATIC VAR o := Foo()\n", "Customer"));
        }

        [Fact]
        public void KeywordCaseIsApplied()
        {
            Assert.Equal("local o := Foo(1) as Customer\n", Rewrite("var o := Foo(1)\n", "Customer", s => s.ToLowerInvariant()));
        }
    }
}
