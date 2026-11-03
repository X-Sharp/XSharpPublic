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
            return OverloadFilter.ArgumentCount(tokens, token);
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
            Assert.Equal(2, OverloadFilter.ArgumentCountAt(tokens, text.IndexOf("Foo") + 1));
            Assert.Equal(2, OverloadFilter.ArgumentCountAt(tokens, text.IndexOf("Foo") + 3));
            Assert.Null(OverloadFilter.ArgumentCountAt(tokens, text.IndexOf("1")));
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
        public void NoCountOrNoMatchKeepsAll()
        {
            var one = Method("Foo", CallingConvention.None, ("a", "INT", null, ParamType.As));
            var two = Method("Foo", CallingConvention.None, ("a", "INT", null, ParamType.As), ("b", "INT", null, ParamType.As));
            var symbols = new List<IXSymbol> { one, two };
            Assert.Equal(symbols, OverloadFilter.Filter(symbols, null));
            Assert.Equal(symbols, OverloadFilter.Filter(symbols, 5));
        }
    }
}
