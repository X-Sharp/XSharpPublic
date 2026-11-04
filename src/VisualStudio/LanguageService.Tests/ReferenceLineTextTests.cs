using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageService.FindReferences;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class ReferenceLineTextTests
    {
        private static string Show(IEnumerable<ReferenceLineText.Segment> segments)
        {
            return string.Join("|", segments.Select(s => $"{s.Text}:{s.Classification ?? "-"}{(s.IsHit ? "*" : "")}"));
        }

        [Fact]
        public void SegmentsFollowClassificationsAndHitOnTheTrimmedLine()
        {
            var classifications = new[] { new ClassifiedRange(9, 2, "operator") };
            var segments = ReferenceLineText.Segments("    oFoo := Foo{}  \r\n", 12, 3, classifications);
            Assert.Equal("oFoo :-|:=:operator| :-|Foo:-*|{}:-", Show(segments));
        }

        [Fact]
        public void ClassifiedHitKeepsItsClassification()
        {
            var classifications = new[] { new ClassifiedRange(0, 5, "keyword") };
            var segments = ReferenceLineText.Segments("CLASS Foo", 6, 3, classifications);
            Assert.Equal("CLASS:keyword| :-|Foo:-*", Show(segments));
        }

        [Fact]
        public void HitOutsideTheLineGivesTheTrimmedLineOnly()
        {
            var segments = ReferenceLineText.Segments("  x := 1\r\n", 40, 3, new ClassifiedRange[0]);
            Assert.Equal("x := 1:-", Show(segments));
        }

        [Fact]
        public void ClassifyMapsTokensOfOneLine()
        {
            var text = "x := 1\nlocal y as string // note\n";
            var tokens = TestLexer.Lex(text);
            var lineStart = text.IndexOf("local");
            var lineEnd = text.Length - 1;
            var ranges = ReferenceLineText.Classify(tokens, lineStart, lineEnd);
            Assert.Contains(new ClassifiedRange(0, 5, "keyword"), ranges);
            Assert.Contains(new ClassifiedRange(8, 2, "keyword"), ranges);
            Assert.Contains(new ClassifiedRange(18, 7, "comment"), ranges);
            Assert.DoesNotContain(ranges, r => r.Start < 0 || r.Start >= lineEnd - lineStart);
        }
    }
}
