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

        private IXTypeSymbol BaseOf(IXTypeSymbol type) => _bases.TryGetValue(type, out var result) ? result : null;

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
            Assert.Same(baseClose, OverrideFamily.Root(subClose, BaseOf));
            Assert.True(OverrideFamily.Contains(baseClose, subClose, BaseOf));
            Assert.True(OverrideFamily.Contains(subClose, baseClose, BaseOf));
        }

        [Fact]
        public void SiblingOverridesShareTheFamily()
        {
            var baseType = Type("Base");
            Member(baseType, "Close");
            var first = Member(Type("First", baseType), "Close");
            var second = Member(Type("Second", baseType), "Close");
            Assert.True(OverrideFamily.Contains(first, second, BaseOf));
        }

        [Fact]
        public void InheritedThroughAClassWithoutTheMember()
        {
            var baseType = Type("Base");
            var baseClose = Member(baseType, "Close");
            var middle = Type("Middle", baseType);
            var leafClose = Member(Type("Leaf", middle), "Close");
            Assert.Same(baseClose, OverrideFamily.Root(leafClose, BaseOf));
        }

        [Fact]
        public void OverloadInSubclassIsNotFamily()
        {
            var baseType = Type("Base");
            var baseRun = Member(baseType, "Run", Kind.Method, "INT");
            var subRun = Member(Type("Sub", baseType), "Run", Kind.Method, "STRING");
            Assert.Same(subRun, OverrideFamily.Root(subRun, BaseOf));
            Assert.False(OverrideFamily.Contains(baseRun, subRun, BaseOf));
        }

        [Fact]
        public void UnrelatedClassesAreSeparate()
        {
            var one = Member(Type("One"), "Close");
            var two = Member(Type("Two"), "Close");
            Assert.False(OverrideFamily.Contains(one, two, BaseOf));
        }

        [Fact]
        public void PropertyLikeMembersMatchByName()
        {
            var baseType = Type("Base");
            var access = Member(baseType, "Name", Kind.Access);
            var property = Member(Type("Sub", baseType), "Name", Kind.Property);
            Assert.True(OverrideFamily.Contains(access, property, BaseOf));
        }

        [Fact]
        public void CyclicOrMissingBaseEndsTheWalk()
        {
            var a = Type("A");
            var b = Type("B", a);
            _bases[a] = b;
            var close = Member(b, "Close");
            Member(a, "Close");
            Assert.NotNull(OverrideFamily.Root(close, BaseOf));
            var lonely = Member(Type("Lonely"), "Close");
            Assert.Same(lonely, OverrideFamily.Root(lonely, BaseOf));
        }

        [Fact]
        public void LocalsAndTypesHaveNoFamily()
        {
            var type = Type("Base");
            var close = Member(type, "Close");
            Assert.False(OverrideFamily.Contains(type, close, BaseOf));
            Assert.False(OverrideFamily.Contains(close, type, BaseOf));
        }
    }
}
