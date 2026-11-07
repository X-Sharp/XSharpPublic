using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageService.FindReferences;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class OverloadFilterTests
    {
        private static int? CountAt(string text, string marker)
        {
            var tokens = TestLexer.Lex(text);
            var position = text.IndexOf(marker);
            var token = tokens.First(t => t.StartIndex == position);
            return OverloadFilter.Arguments(tokens, token)?.Count;
        }

        [Fact]
        public void CountsArgumentsOfACall()
        {
            Assert.Equal(0, CountAt("x := o:Foo()\n", "Foo"));
            Assert.Equal(1, CountAt("x := o:Foo(1)\n", "Foo"));
            Assert.Equal(3, CountAt("x := o:Foo(a, Bar(1, 2), {1, 2})\n", "Foo"));
            Assert.Equal(3, CountAt("x := o:Foo(1,,3)\n", "Foo"));
        }

        [Fact]
        public void NoCallGivesNoCount()
        {
            Assert.Null(CountAt("x := o:Foo\n", "Foo"));
            Assert.Null(CountAt("x := o:Foo + 1\n", "Foo"));
        }

        [Fact]
        public void CountAtCaretInsideOrAfterTheName()
        {
            var text = "x := o:Foo(1, 2)\n";
            var tokens = TestLexer.Lex(text);
            Assert.Equal(2, OverloadFilter.ArgumentsAt(tokens, text.IndexOf("Foo") + 1)?.Count);
            Assert.Equal(2, OverloadFilter.ArgumentsAt(tokens, text.IndexOf("Foo") + 3)?.Count);
            Assert.Null(OverloadFilter.ArgumentsAt(tokens, text.IndexOf("1"))?.Count);
        }

        [Fact]
        public void CurlyArgumentListOfACreation()
        {
            Assert.Equal(2, CountAt("x := Foo{1, \"a\"}\n", "Foo"));
            Assert.Equal(0, CountAt("x := Foo{}\n", "Foo"));
            Assert.Equal(1, CountAt("x := Foo{Bar(1, 2)}\n", "Foo"));
        }

        [Fact]
        public void NameAtFindsChainsAndTheConstructorKeyword()
        {
            var text = "CONSTRUCTOR(n AS INT)\n    SUPER(n, 1)\n    SELF()\n";
            var tokens = TestLexer.Lex(text);
            Assert.Equal("CONSTRUCTOR", OverloadFilter.NameAt(tokens, text.IndexOf("CONSTRUCTOR") + 2)?.Text);
            Assert.Equal(2, OverloadFilter.ArgumentsAt(tokens, text.IndexOf("SUPER") + 1)?.Count);
            Assert.Equal(0, OverloadFilter.ArgumentsAt(tokens, text.IndexOf("SELF") + 1)?.Count);
        }

        [Fact]
        public void CallWithCommentBeforeParenthesis()
        {
            Assert.Equal(2, CountAt("x := o:Foo /* c */ (1, 2)\n", "Foo"));
        }

        private static XSourceMemberSymbol Method(string name, CallingConvention convention, params (string Name, string Type, string Default, ParamType Kind)[] parameters)
        {
            var member = new XSourceMemberSymbol(name, Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(0, 10), "VOID", null);
            member.CallingConvention = convention;
            foreach (var p in parameters)
            {
                var par = new XSourceParameterSymbol(member, p.Name, new TextRange(2, 1, 2, 1), new TextInterval(0, 1), p.Type);
                par.Value = p.Default;
                par.ParamType = p.Kind;
                member.AddParameter(par);
            }
            return member;
        }

        [Fact]
        public void KeepsTheOverloadWithMatchingParameterCount()
        {
            var none = Method("Foo", CallingConvention.None);
            var one = Method("Foo", CallingConvention.None, ("a", "INT", null, ParamType.As));
            var two = Method("Foo", CallingConvention.None, ("a", "INT", null, ParamType.As), ("b", "INT", null, ParamType.As));
            var symbols = new List<IXSymbol> { none, one, two };
            Assert.Equal(new IXSymbol[] { one }, OverloadFilter.Filter(symbols, 1));
            Assert.Equal(new IXSymbol[] { none }, OverloadFilter.Filter(symbols, 0));
        }

        [Fact]
        public void DefaultsAndParamArraysWidenTheCount()
        {
            var withDefault = Method("Foo", CallingConvention.None, ("a", "INT", null, ParamType.As), ("b", "INT", "0", ParamType.As));
            var withParams = Method("Bar", CallingConvention.None, ("a", "INT", null, ParamType.As), ("rest", "OBJECT[]", null, ParamType.Params));
            Assert.True(OverloadFilter.Accepts(withDefault, 1));
            Assert.True(OverloadFilter.Accepts(withDefault, 2));
            Assert.False(OverloadFilter.Accepts(withDefault, 3));
            Assert.True(OverloadFilter.Accepts(withParams, 1));
            Assert.True(OverloadFilter.Accepts(withParams, 5));
            Assert.False(OverloadFilter.Accepts(withParams, 0));
        }

        [Fact]
        public void ClipperMethodsAcceptAnyCount()
        {
            var clipper = Method("Foo", CallingConvention.Clipper, ("a", "USUAL", null, ParamType.As));
            Assert.True(OverloadFilter.Accepts(clipper, 0));
            Assert.True(OverloadFilter.Accepts(clipper, 4));
        }

        [Fact]
        public void UntypedParametersMakeAClipperMethod()
        {
            // VO: METHOD Open(cName, lShared) - no AS, so the parser leaves ParamType unset (0)
            var untyped = Method("Open", CallingConvention.None, ("cName", "USUAL", null, 0), ("lShared", "USUAL", null, 0));
            var typedUsual = Method("Open", CallingConvention.None, ("cName", "USUAL", null, ParamType.As), ("lShared", "USUAL", null, ParamType.As));
            Assert.True(OverloadFilter.Accepts(untyped, 1));
            Assert.True(OverloadFilter.Accepts(untyped, 3));
            Assert.False(OverloadFilter.Accepts(typedUsual, 1));
            Assert.False(OverloadFilter.Accepts(typedUsual, 3));
        }

        [Fact]
        public void NoCountOrNoMatchKeepsAll()
        {
            var one = Method("Foo", CallingConvention.None, ("a", "INT", null, ParamType.As));
            var two = Method("Foo", CallingConvention.None, ("a", "INT", null, ParamType.As), ("b", "INT", null, ParamType.As));
            var symbols = new List<IXSymbol> { one, two };
            Assert.Equal(symbols, OverloadFilter.Filter(symbols, null));
            Assert.Equal(symbols, OverloadFilter.Filter(symbols, 5));
        }

        private static IList<string> TypesAt(string text, string marker, System.Func<string, string> identifierType = null)
        {
            var tokens = TestLexer.Lex(text);
            var position = text.IndexOf(marker);
            var token = tokens.First(t => t.StartIndex == position);
            var arguments = OverloadFilter.Arguments(tokens, token);
            return arguments.Select(a => OverloadFilter.ArgumentType(a, t => identifierType?.Invoke(t.Text))).ToList();
        }

        [Fact]
        public void TypesOfLiteralArguments()
        {
            Assert.Equal(new[] { "System.Int32", "System.String", "System.Boolean", "System.Double", "XSharp.__Date", "XSharp.__Symbol" },
                TypesAt("x := o:Foo(1, \"a\", TRUE, 1.5, 2020.01.01, #sym)\n", "Foo"));
        }

        [Fact]
        public void TypesOfIdentifierAndConstructorArguments()
        {
            Assert.Equal(new[] { "System.String", "Bar", null, null },
                TypesAt("x := o:Foo(s, Bar{1, 2}, a + 1, )\n", "Foo", n => n == "s" ? "STRING" : null));
        }

        [Fact]
        public void TypesKeepTheMatchingOverload()
        {
            var number = Method("Run", CallingConvention.None, ("n", "INT", null, ParamType.As));
            var text = Method("Run", CallingConvention.None, ("s", "STRING", null, ParamType.As));
            var symbols = new List<IXSymbol> { number, text };
            Assert.Equal(new IXSymbol[] { number }, OverloadFilter.FilterByTypes(symbols, new[] { "System.Int32" }));
            Assert.Equal(new IXSymbol[] { text }, OverloadFilter.FilterByTypes(symbols, new[] { "System.String" }));
            Assert.Equal(symbols, OverloadFilter.FilterByTypes(symbols, new string[] { null }));
        }

        [Fact]
        public void IntegerArgumentWidensToFloatingPoint()
        {
            var real = Method("Run", CallingConvention.None, ("n", "REAL8", null, ParamType.As));
            var text = Method("Run", CallingConvention.None, ("s", "STRING", null, ParamType.As));
            var number = Method("Run", CallingConvention.None, ("n", "INT", null, ParamType.As));
            Assert.Equal(new IXSymbol[] { real }, OverloadFilter.FilterByTypes(new List<IXSymbol> { real, text }, new[] { "System.Int32" }));
            // the exact match wins over the widening
            Assert.Equal(new IXSymbol[] { number }, OverloadFilter.FilterByTypes(new List<IXSymbol> { real, number }, new[] { "System.Int32" }));
        }

        [Fact]
        public void UsualAcceptsAnythingButExactMatchWins()
        {
            var usual = Method("Run", CallingConvention.None, ("u", "USUAL", null, ParamType.As));
            var text = Method("Run", CallingConvention.None, ("s", "STRING", null, ParamType.As));
            var symbols = new List<IXSymbol> { usual, text };
            Assert.Equal(new IXSymbol[] { text }, OverloadFilter.FilterByTypes(symbols, new[] { "System.String" }));
            Assert.Equal(new IXSymbol[] { usual }, OverloadFilter.FilterByTypes(symbols, new[] { "System.Int32" }));
        }

        [Fact]
        public void ClassTypesMatchByNameOtherwiseKept()
        {
            var foo = Method("Run", CallingConvention.None, ("f", "LibA.Foo", null, ParamType.As));
            var bar = Method("Run", CallingConvention.None, ("b", "Bar", null, ParamType.As));
            var symbols = new List<IXSymbol> { foo, bar };
            Assert.Equal(new IXSymbol[] { foo }, OverloadFilter.FilterByTypes(symbols, new[] { "Foo" }));
            // the inheritance is unknown: a subclass could be passed to either
            Assert.Equal(symbols, OverloadFilter.FilterByTypes(symbols, new[] { "SubFoo" }));
        }

        [Fact]
        public void NoCompatibleOverloadKeepsAll()
        {
            var number = Method("Run", CallingConvention.None, ("n", "INT", null, ParamType.As));
            var date = Method("Run", CallingConvention.None, ("d", "DATE", null, ParamType.As));
            var symbols = new List<IXSymbol> { number, date };
            Assert.Equal(symbols, OverloadFilter.FilterByTypes(symbols, new[] { "System.String" }));
        }

        [Fact]
        public void ParamArrayElementsUseTheElementType()
        {
            var numbers = Method("Run", CallingConvention.None, ("n", "INT[]", null, ParamType.Params));
            var texts = Method("Run", CallingConvention.None, ("s", "STRING[]", null, ParamType.Params));
            var symbols = new List<IXSymbol> { numbers, texts };
            Assert.Equal(new IXSymbol[] { texts }, OverloadFilter.FilterByTypes(symbols, new[] { "System.String", "System.String" }));
        }
    }
}
