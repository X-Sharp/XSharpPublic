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
    }
}
