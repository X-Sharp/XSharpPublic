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

        /// <summary>A method with named parameters; type null = untyped, value = default value.</summary>
        private XSourceMemberSymbol MemberWith(XSourceTypeSymbol parent, string name, params (string Name, string Type, ParamType Kind, string Value)[] parameters)
        {
            _next += 10;
            var member = new XSourceMemberSymbol(name, Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(_next, _next + 5), "VOID", null);
            member.File = File;
            foreach (var p in parameters)
            {
                var parameter = new XSourceParameterSymbol(member, p.Name, new TextRange(2, 1, 2, 1), new TextInterval(_next, _next + 1), p.Type);
                if (p.Kind != 0)
                    parameter.ParamType = p.Kind;
                parameter.Value = p.Value;
                member.AddParameter(parameter);
            }
            parent.AddMember(member);
            return member;
        }

        private static (string, string, ParamType, string) P(string name, string type, ParamType kind = ParamType.As, string value = null) => (name, type, kind, value);

        [Fact]
        public void ParameterNamesDoNotSeparateOverrides()
        {
            var baseType = Type("Base");
            var baseMember = MemberWith(baseType, "FocusChange", P("oFocusChangeEvent", "FocusChangeEvent"));
            var subMember = MemberWith(Type("Sub", baseType), "FocusChange", P("oEvent", "FocusChangeEvent"));
            Assert.Same(baseMember, Family(subMember).Root(subMember));
            Assert.True(Family(baseMember).Contains(subMember));
        }

        [Fact]
        public void DefaultValuesDoNotSeparateOverrides()
        {
            var baseType = Type("Base");
            var baseMember = MemberWith(baseType, "Show", P("nMode", "INT", ParamType.As, "1"));
            var subMember = MemberWith(Type("Sub", baseType), "Show", P("nMode", "INT"));
            Assert.Same(baseMember, Family(subMember).Root(subMember));
        }

        [Fact]
        public void UntypedMethodsAreOneSlotWhateverTheParameters()
        {
            var baseType = Type("Base");
            var baseClose = MemberWith(baseType, "Close", P("oEvent", null, 0));
            var subClose = MemberWith(Type("Sub", baseType), "Close");
            Assert.Same(baseClose, Family(subClose).Root(subClose));
            // VO dialect: the parser types an untyped parameter as USUAL
            var otherClose = MemberWith(Type("Other", baseType), "Close", P("oEvent", "USUAL", 0), P("lForce", "USUAL", 0));
            Assert.Same(baseClose, Family(otherClose).Root(otherClose));
        }

        [Fact]
        public void RefParameterIsADifferentSlot()
        {
            var baseType = Type("Base");
            var baseRun = MemberWith(baseType, "Run", P("x", "INT"));
            var subRun = MemberWith(Type("Sub", baseType), "Run", P("x", "INT", ParamType.Ref));
            Assert.Same(subRun, Family(subRun).Root(subRun));
            Assert.False(Family(baseRun).Contains(subRun));
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
