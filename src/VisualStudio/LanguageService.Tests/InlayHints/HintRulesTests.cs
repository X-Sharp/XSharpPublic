using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageService.InlayHints;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests.InlayHints
{
    public class HintRulesTests
    {
        private static XSourceMemberSymbol Method(string name, CallingConvention convention, params (string Name, string Type, ParamType Kind)[] parameters)
        {
            var member = new XSourceMemberSymbol(name, Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(0, 10), "VOID", null);
            member.CallingConvention = convention;
            foreach (var p in parameters)
            {
                var par = new XSourceParameterSymbol(member, p.Name, new TextRange(2, 1, 2, 1), new TextInterval(0, 1), p.Type);
                par.ParamType = p.Kind;
                member.AddParameter(par);
            }
            return member;
        }

        private static XSourceMemberSymbol Typed(string name, params string[] parameters)
        {
            return Method(name, CallingConvention.None, parameters.Select(p => (p, "USUAL", ParamType.As)).ToArray());
        }

        /// <summary>The hints of the first call in the text as "text@argument" pairs.</summary>
        private static string[] Hints(string text, IXMemberSymbol member)
        {
            var call = HintCandidates.Calls(TestLexer.Lex(text)).First();
            return HintRules.ParameterHints(call, member).Select(h => h.Text + "@" + text.Substring(h.Position).Split(',', ')')[0].Trim()).ToArray();
        }

        [Fact]
        public void LiteralArgumentsGetHints()
        {
            var open = Typed("Open", "cFileName", "lShared", "lReadOnly", "nRetry");
            Assert.Equal(new[] { "lShared:@TRUE", "lReadOnly:@FALSE", "nRetry:@3" }, Hints("o:Open(cPath, TRUE, FALSE, 3)\n", open));
        }

        [Fact]
        public void NegativeNumberIsALiteral()
        {
            Assert.Equal(new[] { "n:@-1" }, Hints("Foo(-1)\n", Typed("Foo", "n")));
        }

        [Fact]
        public void ConstructorArgumentGetsHint()
        {
            Assert.Equal(new[] { "oItem:@Customer{}" }, Hints("o:Add(Customer{})\n", Typed("Add", "oItem")));
        }

        [Fact]
        public void VariableArgumentGetsNoHint()
        {
            Assert.Empty(Hints("Foo(nCount)\n", Typed("Foo", "nCount")));
            Assert.Empty(Hints("Foo(nOther)\n", Typed("Foo", "nCount")));
            Assert.Empty(Hints("Foo(n + 1)\n", Typed("Foo", "nCount")));
        }

        [Fact]
        public void NilAndNullGetHints()
        {
            Assert.Equal(new[] { "a:@NIL", "b:@NULL_OBJECT" }, Hints("Foo(NIL, NULL_OBJECT)\n", Typed("Foo", "a", "b")));
        }

        [Fact]
        public void StringAndDateLiteralsGetHints()
        {
            Assert.Equal(new[] { "a:@\"x\"", "b:@2020.01.01" }, Hints("Foo(\"x\", 2020.01.01)\n", Typed("Foo", "a", "b")));
        }

        [Fact]
        public void ExtraArgumentsOfClipperCallGetNoHint()
        {
            var clipper = Method("Foo", CallingConvention.Clipper, ("a", "USUAL", ParamType.As), ("b", "USUAL", ParamType.As));
            Assert.Equal(new[] { "a:@1", "b:@2" }, Hints("Foo(1, 2, 3)\n", clipper));
        }

        [Fact]
        public void ParamsGetsOnlyFirstHint()
        {
            var m = Method("Foo", CallingConvention.None, ("n", "INT", ParamType.As), ("args", "OBJECT[]", ParamType.Params));
            Assert.Equal(new[] { "n:@1", "args:@2" }, Hints("Foo(1, 2, 3)\n", m));
        }

        [Fact]
        public void EmptyArgumentKeepsPositions()
        {
            Assert.Equal(new[] { "a:@1", "c:@3" }, Hints("Foo(1,,3)\n", Typed("Foo", "a", "b", "c")));
        }

        [Fact]
        public void ExtensionMethodCalledOnAnInstanceSkipsTheFirstParameter()
        {
            var ext = Typed("Ext", "oSelf", "lFlag");
            ext.Signature.IsExtension = true;
            Assert.Equal(new[] { "lFlag:@TRUE" }, Hints("o:Ext(TRUE)\n", ext));
            Assert.Equal(new[] { "lFlag:@TRUE" }, Hints("Ext(o, TRUE)\n", ext));
        }

        [Fact]
        public void SingleFitNeedsExactlyOne()
        {
            var one = Typed("Foo", "a");
            var two = Typed("Foo", "a", "b");
            var otherOne = Typed("Foo", "x");
            Assert.Null(HintRules.SingleFit(new List<IXSymbol> { one, otherOne }, 1));
            Assert.Null(HintRules.SingleFit(new List<IXSymbol> { two }, 1));
            Assert.Same(one, HintRules.SingleFit(new List<IXSymbol> { one }, 1));
            Assert.Null(HintRules.SingleFit(new List<IXSymbol>(), 1));
        }

        [Fact]
        public void NoDeclaredParametersNoHint()
        {
            var clipper = Method("Foo", CallingConvention.Clipper);
            Assert.Null(HintRules.SingleFit(new List<IXSymbol> { clipper }, 2));
        }

        [Theory]
        [InlineData("Customer{}", true)]
        [InlineData("\"a\"", true)]
        [InlineData("42", true)]
        [InlineData("(Customer) o", true)]
        [InlineData("Foo()", false)]
        [InlineData("o:Bar", false)]
        public void TypeVisibleCases(string initializer, bool visible)
        {
            var decl = HintCandidates.Declarations(TestLexer.Lex("VAR x := " + initializer + "\n")).Single();
            Assert.Equal(visible, HintRules.TypeIsVisible(decl.Initializer));
        }

        [Theory]
        // the parser stores the initializer text as type name; it stays when the lookup cannot resolve it
        [InlineData("FOREACH VAR d IN <DbFile>{o}\nNEXT\n", "<DbFile>{o}", false)]
        [InlineData("VAR x := Foo( 1 )\n", "Foo(1)", false)]
        [InlineData("VAR x := y\n", "y", false)]
        [InlineData("VAR x := y\n", "$VAR$", false)]
        [InlineData("VAR x := y\n", "", false)]
        [InlineData("VAR x := o:Child()\n", "DbFile", true)]
        [InlineData("VAR x := y\n", "List<Customer>", true)]
        public void UnresolvedTypeNameCases(string source, string typeName, bool resolved)
        {
            var decl = HintCandidates.Declarations(TestLexer.Lex(source)).Single();
            Assert.Equal(resolved, HintRules.IsResolvedTypeName(typeName, decl.Initializer));
        }

        [Theory]
        [InlineData("System.Collections.Generic.List<MyApp.Customer>", "List<Customer>")]
        [InlineData("MyApp.Data.Customer", "Customer")]
        [InlineData("STRING", "STRING")]
        [InlineData("System.Collections.Generic.Dictionary<STRING,MyApp.Customer>", "Dictionary<STRING,Customer>")]
        [InlineData("MyApp.Customer[]", "Customer[]")]
        public void ShortTypeNames(string full, string expected)
        {
            Assert.Equal(expected, HintRules.ShortTypeName(full));
        }
    }
}
