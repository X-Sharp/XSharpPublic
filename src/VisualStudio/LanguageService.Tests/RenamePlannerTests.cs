using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageService.FindReferences;
using XSharp.LanguageService.Rename;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class RenamePlannerTests
    {
        private static ReferenceHit Hit(string file, int start, ReferenceUncertainty uncertainty = ReferenceUncertainty.None)
        {
            return new ReferenceHit { FilePath = file, Start = start, Length = 5, Uncertainty = uncertainty };
        }

        [Fact]
        public void SureAndUncertainHitsAreSeparated()
        {
            var sure = Hit("a.prg", 10);
            var lateBound = Hit("a.prg", 20, ReferenceUncertainty.LateBound);
            var unread = Hit("b.prg", 0, ReferenceUncertainty.FileNotRead);
            var plan = RenamePlanner.Build(new[] { sure, lateBound, unread });
            Assert.Equal(new[] { sure }, plan.Changes);
            Assert.Equal(new[] { lateBound, unread }, plan.NotChanged);
            Assert.Equal(new[] { "a.prg" }, plan.Files);
        }

        [Fact]
        public void LateBoundSendsAndSymbolsAreChangedWhenAsked()
        {
            var sure = Hit("a.prg", 10);
            var lateBound = Hit("a.prg", 20, ReferenceUncertainty.LateBound);
            var symbol = Hit("a.prg", 30, ReferenceUncertainty.Symbol);
            var unresolved = Hit("a.prg", 40, ReferenceUncertainty.Unresolved);
            var plan = RenamePlanner.Build(new[] { sure, lateBound, symbol, unresolved }, includeLateBound: true);
            Assert.Equal(new[] { 31, 20, 10 }, plan.Changes.Select(h => h.Start).ToArray());
            Assert.Equal(new[] { unresolved }, plan.NotChanged);
        }

        [Fact]
        public void ASymbolIsChangedWithoutItsHash()
        {
            var symbol = new ReferenceHit { FilePath = "a.prg", Start = 8, Column = 8, Length = 6, Uncertainty = ReferenceUncertainty.Symbol };
            var change = RenamePlanner.Build(new[] { symbol }, includeLateBound: true).Changes.Single();
            Assert.Equal(9, change.Start);
            Assert.Equal(9, change.Column);
            Assert.Equal(5, change.Length);
            Assert.True(RenamePlanner.StillMatches("Send(u, #Speak)", change, "Speak"));
        }

        [Fact]
        public void DuplicateHitsAreChangedOnce()
        {
            var plan = RenamePlanner.Build(new[] { Hit("a.prg", 10), Hit("A.PRG", 10) });
            Assert.Single(plan.Changes);
            Assert.Single(plan.Files);
        }

        [Fact]
        public void ChangesRunBackwardsPerFile()
        {
            var plan = RenamePlanner.Build(new[] { Hit("a.prg", 10), Hit("a.prg", 50), Hit("b.prg", 5), Hit("a.prg", 30) });
            Assert.Equal(new[] { 50, 30, 10 }, plan.Changes.Where(h => h.FilePath == "a.prg").Select(h => h.Start).ToArray());
            Assert.Equal(new[] { "a.prg", "b.prg" }, plan.Files);
        }

        [Fact]
        public void StillMatchesDetectsChangedText()
        {
            var hit = Hit("a.prg", 5);
            Assert.True(RenamePlanner.StillMatches("x := Close()", hit, "close"));
            Assert.False(RenamePlanner.StillMatches("x := Shut()", hit, "Close"));
            Assert.False(RenamePlanner.StillMatches("x :=", hit, "Close"));
        }

        [Fact]
        public void WithoutDropsUncheckedChanges()
        {
            var keep = Hit("a.prg", 10);
            var drop = Hit("b.prg", 20);
            var plan = RenamePlanner.Without(RenamePlanner.Build(new[] { keep, drop }), new HashSet<ReferenceHit> { drop });
            Assert.Equal(new[] { keep }, plan.Changes);
            Assert.Equal(new[] { "a.prg" }, plan.Files);
        }
    }
}
