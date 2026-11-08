using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageService.InlayHints;
using Xunit;

namespace XSharp.LanguageService.Tests.InlayHints
{
    public class InlayHintRangeTests
    {
        [Theory]
        // first visible, last visible, line count -> first, last computed: one screen above and below
        [InlineData(100, 130, 1000, 69, 161)]
        [InlineData(10, 40, 50, 0, 49)]
        [InlineData(0, 0, 1, 0, 0)]
        public void LinesAddOneScreenAboveAndBelow(int firstVisible, int lastVisible, int lineCount, int first, int last)
        {
            Assert.Equal((first, last), InlayHintRange.Lines(firstVisible, lastVisible, lineCount));
        }

        [Fact]
        public void RangeInsideACoveredSpanIsCovered()
        {
            Assert.True(InlayHintRange.IsCovered(new[] { (0, 100) }, 10, 50));
        }

        [Fact]
        public void RangeReachingOutOfTheCoveredSpanIsNotCovered()
        {
            Assert.False(InlayHintRange.IsCovered(new[] { (0, 100) }, 50, 150));
        }

        [Fact]
        public void NothingIsCoveredBeforeTheFirstComputation()
        {
            Assert.False(InlayHintRange.IsCovered(new (int, int)[0], 0, 1));
        }

        [Fact]
        public void AdjacentRangesJoin()
        {
            Assert.Equal(new[] { (0, 200) }, InlayHintRange.AddCovered(new[] { (0, 100) }, 100, 200));
        }

        [Fact]
        public void DistantRangesStaySeparate()
        {
            Assert.Equal(new[] { (0, 100), (300, 400) }, InlayHintRange.AddCovered(new[] { (0, 100) }, 300, 400));
        }

        [Fact]
        public void ARangeOverTwoSpansJoinsThem()
        {
            Assert.Equal(new[] { (0, 400) }, InlayHintRange.AddCovered(new[] { (300, 400), (0, 100) }, 50, 350));
        }

        [Fact]
        public void MergeKeepsOldHintsAndTakesTheFreshOnesAtTheSamePlace()
        {
            var old = new[] { Hint(InlayHintKind.Parameter, 5, "a:"), Hint(InlayHintKind.Parameter, 50, "old:") };
            var fresh = new[] { Hint(InlayHintKind.Parameter, 50, "new:"), Hint(InlayHintKind.Type, 50, " AS INT"), Hint(InlayHintKind.Type, 60, " AS STRING") };

            var merged = InlayHintRange.Merge(old, fresh);

            Assert.Equal(new[] { "a:", "new:", " AS INT", " AS STRING" }, merged.Select(h => h.Text));
        }

        private static InlayHint Hint(InlayHintKind kind, int position, string text)
            => new InlayHint { Kind = kind, Position = position, Text = text };
    }
}
