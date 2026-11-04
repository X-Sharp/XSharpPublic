using System;
using System.Collections.Generic;
using XSharp.LanguageService.FindReferences;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class OverrideFamilyTests
    {
        private static readonly XFile File = new XFile(@"C:\Test\A.prg", null);
        private readonly Dictionary<IXTypeSymbol, IXTypeSymbol> _bases = new Dictionary<IXTypeSymbol, IXTypeSymbol>();
        private int _next;

        private int _baseLookups;

        private IXTypeSymbol BaseOf(IXTypeSymbol type)
        {
            _baseLookups++;
            return _bases.TryGetValue(type, out var result) ? result : null;
        }

        private OverrideFamily Family(IXSymbol target) => new OverrideFamily(target, BaseOf);

        private XSourceTypeSymbol Type(string name, XSourceTypeSymbol baseType = null)
        {
            _next += 1000;
            var type = new XSourceTypeSymbol(name, Kind.Class, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(_next, _next + 900), File, null);
            type.Namespace = "Test";
            if (baseType != null)
                _bases[type] = baseType;
            return type;
        }

        private XSourceMemberSymbol Member(XSourceTypeSymbol parent, string name, Kind kind = Kind.Method, params string[] parameterTypes)
        {
            _next += 10;
            var member = new XSourceMemberSymbol(name, kind, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(_next, _next + 5), "VOID", null);
            member.File = File;
            int i = 0;
            foreach (var type in parameterTypes)
                member.AddParameter(new XSourceParameterSymbol(member, "p" + i++, new TextRange(2, 1, 2, 1), new TextInterval(_next, _next + 1), type));
            parent.AddMember(member);
            return member;
        }

        [Fact]
        public void OverrideBelongsToTheBaseMembersFamily()
        {
            var baseType = Type("Base");
            var sub = Type("Sub", baseType);
            var baseClose = Member(baseType, "Close");
            var subClose = Member(sub, "Close");
            Assert.Same(baseClose, Family(subClose).Root(subClose));
            Assert.True(Family(baseClose).Contains(subClose));
            Assert.True(Family(subClose).Contains(baseClose));
        }

        [Fact]
        public void SiblingOverridesShareTheFamily()
        {
            var baseType = Type("Base");
            Member(baseType, "Close");
            var first = Member(Type("First", baseType), "Close");
            var second = Member(Type("Second", baseType), "Close");
            Assert.True(Family(first).Contains(second));
        }

        [Fact]
        public void InheritedThroughAClassWithoutTheMember()
        {
            var baseType = Type("Base");
            var baseClose = Member(baseType, "Close");
            var middle = Type("Middle", baseType);
            var leafClose = Member(Type("Leaf", middle), "Close");
            Assert.Same(baseClose, Family(leafClose).Root(leafClose));
        }

        [Fact]
        public void OverloadInSubclassIsNotFamily()
        {
            var baseType = Type("Base");
            var baseRun = Member(baseType, "Run", Kind.Method, "INT");
            var subRun = Member(Type("Sub", baseType), "Run", Kind.Method, "STRING");
            Assert.Same(subRun, Family(subRun).Root(subRun));
            Assert.False(Family(baseRun).Contains(subRun));
        }

        [Fact]
        public void UnrelatedClassesAreSeparate()
        {
            var one = Member(Type("One"), "Close");
            var two = Member(Type("Two"), "Close");
            Assert.False(Family(one).Contains(two));
        }

        [Fact]
        public void PropertyLikeMembersMatchByName()
        {
            var baseType = Type("Base");
            var access = Member(baseType, "Name", Kind.Access);
            var property = Member(Type("Sub", baseType), "Name", Kind.Property);
            Assert.True(Family(access).Contains(property));
        }

        [Fact]
        public void CyclicOrMissingBaseEndsTheWalk()
        {
            var a = Type("A");
            var b = Type("B", a);
            _bases[a] = b;
            var close = Member(b, "Close");
            Member(a, "Close");
            Assert.NotNull(Family(close).Root(close));
            var lonely = Member(Type("Lonely"), "Close");
            Assert.Same(lonely, Family(lonely).Root(lonely));
        }

        [Fact]
        public void BaseClassesAreLookedUpOncePerSearch()
        {
            var baseType = Type("Base");
            var baseClose = Member(baseType, "Close");
            var sub = Type("Sub", baseType);
            var family = Family(baseClose);
            for (int i = 0; i < 10; i++)
                Assert.True(family.Contains(Member(sub, "Close")));
            // Base (for the target) and Sub (for the candidates), once each
            Assert.Equal(2, _baseLookups);
        }

        [Fact]
        public void LocalsAndTypesHaveNoFamily()
        {
            var type = Type("Base");
            var close = Member(type, "Close");
            Assert.False(Family(type).Contains(close));
            Assert.False(Family(close).Contains(type));
            Assert.Null(Family(type).TargetRoot);
        }
    }
}
