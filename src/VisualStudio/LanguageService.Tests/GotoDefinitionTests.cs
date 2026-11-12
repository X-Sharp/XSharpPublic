using System.Collections.Generic;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class GotoDefinitionTests
    {
        [Fact]
        public void AKeywordIsNoTarget()
        {
            // F12 on the CLASS keyword: nothing to go to, as when nothing is found
            Assert.Empty(XSharpGotoDefinition.Targets(new List<IXSymbol> { new XKeywordSymbol("CLASS") }));
        }

        [Fact]
        public void TheTypeStaysNextToAKeyword()
        {
            var type = new XSourceTypeSymbol("Crate", Kind.Class, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(0, 100), null, null);
            Assert.Equal(new IXSymbol[] { type }, XSharpGotoDefinition.Targets(new List<IXSymbol> { new XKeywordSymbol("CLASS"), type }));
        }


        private static XSourceMemberSymbol Method(string name, params (string Name, string Type)[] parameters)
        {
            var member = new XSourceMemberSymbol(name, Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(0, 10), "VOID", null);
            member.CallingConvention = CallingConvention.None;
            foreach (var p in parameters)
            {
                var par = new XSourceParameterSymbol(member, p.Name, new TextRange(2, 1, 2, 1), new TextInterval(0, 1), p.Type);
                par.ParamType = ParamType.As;
                member.AddParameter(par);
            }
            return member;
        }

        // F12 with the caret in <name> of <text>; identifiers have no known type
        private static IList<IXSymbol> PickAt(string text, string name, params IXSymbol[] overloads)
        {
            var tokens = TestLexer.Lex(text);
            return XSharpGotoDefinition.Overloads(overloads, tokens, text.IndexOf(name) + 1, t => null);
        }

        private static readonly XSourceMemberSymbol OpenNone = Method("Open");
        private static readonly XSourceMemberSymbol OpenKey = Method("Open", ("key", "STRING"));
        private static readonly XSourceMemberSymbol OpenNumber = Method("Open", ("number", "INT"));

        [Fact]
        public void TheArgumentCountPicksTheOverload()
        {
            Assert.Equal(new IXSymbol[] { OpenKey }, PickAt("c:Open(x)\n", "Open", OpenNone, OpenKey));
            Assert.Equal(new IXSymbol[] { OpenNone }, PickAt("c:Open()\n", "Open", OpenNone, OpenKey));
        }

        [Fact]
        public void TheArgumentTypePicksTheOverload()
        {
            Assert.Equal(new IXSymbol[] { OpenKey }, PickAt("c:Open(\"x\")\n", "Open", OpenNone, OpenKey, OpenNumber));
        }

        [Fact]
        public void UnknownArgumentTypesKeepTheCandidates()
        {
            Assert.Equal(new IXSymbol[] { OpenKey, OpenNumber }, PickAt("c:Open(x)\n", "Open", OpenNone, OpenKey, OpenNumber));
        }

        [Fact]
        public void WithoutACallAllOverloadsStay()
        {
            Assert.Equal(new IXSymbol[] { OpenNone, OpenKey }, PickAt("x := c:Open\n", "Open", OpenNone, OpenKey));
        }

        [Fact]
        public void ASingleTargetIsKept()
        {
            Assert.Equal(new IXSymbol[] { OpenKey }, PickAt("c:Open()\n", "Open", OpenKey));
        }
    }
}
