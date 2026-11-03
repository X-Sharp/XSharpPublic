using XSharp.LanguageService.FindReferences;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class ReferenceLineTextTests
    {
        [Fact]
        public void SplitsAroundHitAndTrimsTheLine()
        {
            var parts = ReferenceLineText.Split("    oFoo := Foo{}  \r\n", 12, 3);
            Assert.Equal("oFoo := ", parts.Before);
            Assert.Equal("Foo", parts.Hit);
            Assert.Equal("{}", parts.After);
        }

        [Fact]
        public void HitAtLineStart()
        {
            var parts = ReferenceLineText.Split("Foo()", 0, 3);
            Assert.Equal("", parts.Before);
            Assert.Equal("Foo", parts.Hit);
            Assert.Equal("()", parts.After);
        }

        [Fact]
        public void HitOutsideTheLineGivesTheWholeTrimmedLine()
        {
            var parts = ReferenceLineText.Split("  x := 1\r\n", 40, 3);
            Assert.Equal("x := 1", parts.Before);
            Assert.Equal("", parts.Hit);
            Assert.Equal("", parts.After);
        }
    }
}
