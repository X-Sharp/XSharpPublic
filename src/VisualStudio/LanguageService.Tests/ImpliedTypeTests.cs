using System.Linq;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class ImpliedTypeTests
    {
        private static System.Collections.Generic.IList<global::LanguageService.SyntaxTree.IToken> Expression(string text)
        {
            return TestLexer.Lex(text + "\n").Where(t => t.Channel == XSharpLexer.DefaultTokenChannel && t.Type != XSharpLexer.EOS && t.Type != XSharpLexer.Eof).ToList();
        }

        [Theory]
        [InlineData("<DbFile>{o}", "DbFile")]
        [InlineData("<STRING>{\"a\", \"b\"}", "STRING")]
        [InlineData("<System.Text.StringBuilder>{}", "System.Text.StringBuilder")]
        [InlineData("<List<INT>>{}", "List<INT>")]
        [InlineData("{1, 2}", null)]
        [InlineData("<DbFile>{o}:Clone()", null)]
        [InlineData("list", null)]
        public void ElementTypeOfATypedArrayLiteral(string expression, string expected)
        {
            Assert.Equal(expected, XSharpLookup.TypedArrayElementType(Expression(expression)));
        }

        [Fact]
        public void AResolvedElementTypeNamesTheVariable()
        {
            var fixture = new RenameFixture();
            var dbFile = fixture.Type("DbFile");
            Assert.True(XSharpLookup.TypedArrayElement(Expression("<DbFile>{o}"), fixture.FindType, out var type, out var typeName));
            Assert.Same(dbFile, type);
            Assert.Equal("DbFile", typeName);
        }

        [Fact]
        public void AnUnresolvedElementTypeLeavesTheVariableUnnamed()
        {
            // the variable keeps the name the parser gave it, so no hint shows a type that does not exist
            var fixture = new RenameFixture();
            Assert.True(XSharpLookup.TypedArrayElement(Expression("<Unknown>{o}"), fixture.FindType, out var type, out var typeName));
            Assert.Null(type);
            Assert.Null(typeName);
        }

        [Fact]
        public void NoTypedArrayLiteralNoElement()
        {
            var fixture = new RenameFixture();
            fixture.Type("list");
            Assert.False(XSharpLookup.TypedArrayElement(Expression("list"), fixture.FindType, out var type, out var typeName));
            Assert.Null(type);
            Assert.Null(typeName);
        }

        [Theory]
        // VAR list := s:Load() where Load returns List<STRING>: keep the type arguments of the declaration
        [InlineData(ImpliedKind.Assignment, "System.Collections.Generic.List<T>", "List<STRING>", "System.Collections.Generic.List<STRING>")]
        [InlineData(ImpliedKind.Using, "System.Collections.Generic.List<T>", "List<STRING>", "System.Collections.Generic.List<STRING>")]
        [InlineData(ImpliedKind.InCollection, "System.Collections.Generic.KeyValuePair<TKey,TValue>", "KeyValuePair<STRING,INT>", "System.Collections.Generic.KeyValuePair<STRING,INT>")]
        // no type arguments known: the resolved name stays
        [InlineData(ImpliedKind.Assignment, "System.Collections.Generic.List<T>", "s:Load(\"a\")", "System.Collections.Generic.List<T>")]
        [InlineData(ImpliedKind.Assignment, "System.Collections.Generic.List<T>", null, "System.Collections.Generic.List<T>")]
        public void ImpliedTypeNameKeepsTypeArguments(ImpliedKind kind, string resolved, string declared, string expected)
        {
            Assert.Equal(expected, XSharpLookup.ImpliedTypeName(resolved, true, kind, declared));
        }

        [Fact]
        public void NonGenericTypeNameIsTheResolvedName()
        {
            Assert.Equal("DbFile", XSharpLookup.ImpliedTypeName("DbFile", false, ImpliedKind.Assignment, "Foo<INT>"));
        }

        [Theory]
        [InlineData("List<STRING>{}", "List<STRING>")]
        [InlineData("System.Collections.Generic.Dictionary<STRING, INT>{}", "System.Collections.Generic.Dictionary<STRING,INT>")]
        [InlineData("s:Load()", null)]
        public void DeclaredTypeOfACreation(string expression, string expected)
        {
            Assert.Equal(expected, XSharpLookup.CreatedTypeName(Expression(expression)));
        }
    }
}
