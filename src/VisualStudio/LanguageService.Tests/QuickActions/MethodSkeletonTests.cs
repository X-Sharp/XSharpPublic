using System;
using XSharp.LanguageService.Editors.LightBulb.QuickActions;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests.QuickActions
{
    public class MethodSkeletonTests
    {
        private static CodeStyle Style(XDialect dialect = XDialect.VO, Func<string, string> keyword = null)
        {
            return new CodeStyle(dialect, keyword ?? (s => s.ToUpperInvariant()), "    ", "\r\n");
        }

        private static CallArgument Arg(string name, string type) => new CallArgument(name, type);

        [Fact]
        public void VoClassWithEndClass()
        {
            var result = MethodSkeleton.Build("Foo", new[] { Arg("oItem", "Customer"), Arg(null, null) }, "VOID",
                new TargetClass("Bar", true, 10, "    "), Style());
            Assert.Equal(10, result.Line);
            Assert.Equal("\r\n    METHOD Foo(oItem AS Customer, p2 AS USUAL) AS VOID\r\n        \r\n        RETURN\r\n", result.Text);
        }

        [Fact]
        public void CaretGoesOnTheEmptyBodyLine()
        {
            var result = MethodSkeleton.Build("Foo", new CallArgument[0], "VOID", new TargetClass("Bar", true, 10, "    "), Style());
            Assert.Equal("\r\n    METHOD Foo() AS VOID\r\n        ".Length, result.Caret);
        }

        [Fact]
        public void VoStyleClassGetsClassClause()
        {
            var result = MethodSkeleton.Build("Foo", new[] { Arg("n", "INT") }, "VOID", new TargetClass("Bar", false, 20, ""), Style());
            Assert.Equal(20, result.Line);
            Assert.Equal("\r\nMETHOD Foo(n AS INT) AS VOID CLASS Bar\r\n    \r\n    RETURN\r\n", result.Text);
        }

        [Fact]
        public void CoreUsesObjectAndVoid()
        {
            var result = MethodSkeleton.Build("Foo", new[] { Arg(null, null) }, "VOID", new TargetClass("Bar", true, 3, "    "), Style(XDialect.Core));
            Assert.Equal("\r\n    METHOD Foo(p1 AS OBJECT) AS VOID\r\n        \r\n        RETURN\r\n", result.Text);
        }

        [Fact]
        public void ResultTypeThrowsLikeCSharp()
        {
            var result = MethodSkeleton.Build("Foo", new CallArgument[0], "INT", new TargetClass("Bar", true, 3, "    "), Style());
            Assert.Equal("\r\n    METHOD Foo() AS INT\r\n        \r\n        THROW System.NotImplementedException{}\r\n", result.Text);
        }

        [Fact]
        public void VoidIsRecognisedInAnyCase()
        {
            var result = MethodSkeleton.Build("Foo", new CallArgument[0], "void", new TargetClass("Bar", true, 3, ""), Style(keyword: s => s.ToLowerInvariant()));
            Assert.Equal("\r\nmethod Foo() as void\r\n    \r\n    return\r\n", result.Text);
        }

        [Theory]
        [InlineData("END CLASS", true)]
        [InlineData("   ENDCLASS", true)]
        [InlineData("end class // Bar", true)]
        [InlineData("ENDDEFINE", true)]
        [InlineData("END DEFINE", true)]
        [InlineData("END METHOD", false)]
        [InlineData("END FUNCTION", false)]
        [InlineData("ENDIF", false)]
        [InlineData("RETURN NIL", false)]
        public void EndClassLine(string line, bool expected)
        {
            Assert.Equal(expected, MethodSkeleton.IsEndClassLine(line));
        }

        [Fact]
        public void DuplicateNamesGetSuffix()
        {
            var result = MethodSkeleton.Build("Foo", new[] { Arg("oItem", null), Arg("oItem", null) }, "VOID", new TargetClass("Bar", true, 3, ""), Style());
            Assert.Contains("METHOD Foo(oItem AS USUAL, oItem2 AS USUAL)", result.Text);
        }

        [Fact]
        public void GeneratedNameDoesNotClashWithArgumentName()
        {
            var result = MethodSkeleton.Build("Foo", new[] { Arg(null, null), Arg("p1", null) }, "VOID", new TargetClass("Bar", true, 3, ""), Style());
            Assert.Contains("METHOD Foo(p1 AS USUAL, p12 AS USUAL)", result.Text);
        }

        [Fact]
        public void NoArguments()
        {
            var result = MethodSkeleton.Build("Foo", new CallArgument[0], "VOID", new TargetClass("Bar", true, 3, ""), Style());
            Assert.Contains("METHOD Foo() AS VOID\r\n", result.Text);
        }

        [Fact]
        public void KeywordCaseLower()
        {
            var result = MethodSkeleton.Build("Foo", new[] { Arg(null, null) }, "void", new TargetClass("Bar", true, 3, ""), Style(keyword: s => s.ToLowerInvariant()));
            Assert.Contains("method Foo(p1 as usual) as void", result.Text);
        }
    }
}
