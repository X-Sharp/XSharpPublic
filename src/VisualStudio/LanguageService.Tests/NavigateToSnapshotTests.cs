using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using XSharp.LanguageService.NavigateTo;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class NavigateToSnapshotTests
    {
        private sealed class Fake
        {
            internal int Loads;
            internal long Counter;
            internal DateTime Now = new DateTime(2026, 11, 4, 12, 0, 0, DateTimeKind.Utc);
            internal IReadOnlyList<NavigateToEntry> Result = new[] { Entry("A") };
            internal Func<IReadOnlyList<NavigateToEntry>> Load;

            internal Fake()
            {
                Load = () => { Interlocked.Increment(ref Loads); return Result; };
            }

            internal NavigateToSnapshot Create() => new NavigateToSnapshot(() => Load(), () => Counter, () => Now);
        }

        private static NavigateToEntry Entry(string name) => new NavigateToEntry { Name = name, FilePath = "a.prg" };

        [Fact]
        public async Task FirstSearchLoads()
        {
            var fake = new Fake();
            var entries = await fake.Create().GetAsync(CancellationToken.None);
            Assert.Equal(1, fake.Loads);
            Assert.Same(fake.Result, entries);
        }

        [Fact]
        public async Task UnchangedCounterReuses()
        {
            var fake = new Fake();
            var snapshot = fake.Create();
            await snapshot.GetAsync(CancellationToken.None);
            fake.Now = fake.Now.AddMinutes(5);
            await snapshot.GetAsync(CancellationToken.None);
            Assert.Equal(1, fake.Loads);
        }

        [Fact]
        public async Task ChangedCounterWithinTwoSecondsReuses()
        {
            var fake = new Fake();
            var snapshot = fake.Create();
            await snapshot.GetAsync(CancellationToken.None);
            fake.Counter++;
            fake.Now = fake.Now.AddSeconds(1);
            await snapshot.GetAsync(CancellationToken.None);
            Assert.Equal(1, fake.Loads);
        }

        [Fact]
        public async Task ChangedCounterAfterTwoSecondsReloads()
        {
            var fake = new Fake();
            var snapshot = fake.Create();
            await snapshot.GetAsync(CancellationToken.None);
            fake.Counter++;
            fake.Now = fake.Now.AddSeconds(2.1);
            var newer = new[] { Entry("B") };
            fake.Result = newer;
            var entries = await snapshot.GetAsync(CancellationToken.None);
            Assert.Equal(2, fake.Loads);
            Assert.Same(newer, entries);
        }

        [Fact]
        public async Task ConcurrentSearchesShareOneLoad()
        {
            var fake = new Fake();
            var gate = new ManualResetEventSlim(false);
            fake.Load = () => { Interlocked.Increment(ref fake.Loads); gate.Wait(); return fake.Result; };
            var snapshot = fake.Create();
            var first = snapshot.GetAsync(CancellationToken.None);
            var second = snapshot.GetAsync(CancellationToken.None);
            gate.Set();
            var results = await Task.WhenAll(first, second);
            Assert.Equal(1, fake.Loads);
            Assert.Same(results[0], results[1]);
        }

        [Fact]
        public async Task EmptyDatabaseGivesNoEntriesAndLoadsAgainAfterAChange()
        {
            var fake = new Fake { Result = Array.Empty<NavigateToEntry>() };
            var snapshot = fake.Create();
            Assert.Empty(await snapshot.GetAsync(CancellationToken.None));
            fake.Counter++;
            fake.Result = new[] { Entry("A") };
            Assert.Single(await snapshot.GetAsync(CancellationToken.None));
            Assert.Equal(2, fake.Loads);
        }

        private static XDbResult Type(string name, Kind kind, string file, int line, string ns = "") =>
            new XDbResult { TypeName = name, Namespace = ns, Kind = kind, FileName = file, StartLine = line, StartColumn = 1, Project = "Lib.xsproj" };

        private static XDbResult Member(string type, string name, Kind kind, string file, int line, string project = "Lib.xsproj") =>
            new XDbResult { TypeName = type, MemberName = name, Kind = kind, FileName = file, StartLine = line, StartColumn = 5, Project = project };

        [Fact]
        public void FromRowsBuildsOneEntryPerDeclaration()
        {
            var types = new[]
            {
                Type(XLiterals.GlobalName, Kind.Class, "funcs.prg", 0),
                Type("Part", Kind.Class, "part_a.prg", 1),
                Type("Part", Kind.Class, "part_b.prg", 1),
            };
            var members = new[]
            {
                Member(XLiterals.GlobalName, "Start", Kind.Function, "funcs.prg", 3),
                Member(XLiterals.GlobalName, "MAX_LEN", Kind.VODefine, "funcs.prg", 1),
                Member("Part", "Part", Kind.Constructor, "part_a.prg", 2),
                Member("Part", "Walk", Kind.Method, "part_a.prg", 5),
                Member("Part", "Walk", Kind.Method, "PART_A.prg", 5, "App.xsproj"),
                Member("Part", "Name", Kind.Access, "part_b.prg", 4),
                Member("Part", "Name", Kind.Assign, "part_b.prg", 6),
            };
            var entries = NavigateToEntry.FromRows(types, members);
            Assert.DoesNotContain(entries, e => e.Name == XLiterals.GlobalName);
            Assert.Equal(2, entries.Count(e => e.Name == "Part"));
            Assert.Equal("", entries.Single(e => e.Name == "Start").Container);
            Assert.Equal("", entries.Single(e => e.Name == "MAX_LEN").Container);
            Assert.Equal("Part", entries.Single(e => e.Name == "Walk").Container);
            Assert.Single(entries, e => e.Name == "Name");
            Assert.Equal(Kind.Access, entries.Single(e => e.Name == "Name").Kind);
            Assert.Equal(6, entries.Count);
        }

        [Fact]
        public void FromRowsLeavesOutTheBuiltInFunctions()
        {
            var saved = XSolution.BuiltInFunctions;
            XSolution.BuiltInFunctions = @"C:\Temp\XSharp.Intellisense\BuiltInFunctions.prg";
            try
            {
                var members = new[]
                {
                    Member(XLiterals.GlobalName, "NameOf", Kind.Function, @"c:\temp\xsharp.intellisense\builtinfunctions.prg", 1, "App.xsproj"),
                    Member(XLiterals.GlobalName, "Start", Kind.Function, "funcs.prg", 3),
                };
                var entries = NavigateToEntry.FromRows(Array.Empty<XDbResult>(), members);
                Assert.Equal(new[] { "Start" }, entries.Select(e => e.Name).ToArray());
            }
            finally
            {
                XSolution.BuiltInFunctions = saved;
            }
        }

        [Theory]
        [InlineData("Class", true, false, true)]
        [InlineData("Method", true, false, false)]
        [InlineData("Class", false, true, false)]
        [InlineData("VODefine", false, true, true)]
        [InlineData("Class", true, true, true)]
        [InlineData("Method", false, false, true)]
        public void TypeAndMemberFiltersLikeCSharp(string kind, bool typesOnly, bool membersOnly, bool expected)
        {
            var entry = new NavigateToEntry { Name = "X", Kind = (Kind)Enum.Parse(typeof(Kind), kind) };
            Assert.Equal(expected, entry.PassesFilter(typesOnly, membersOnly));
        }
    }
}
