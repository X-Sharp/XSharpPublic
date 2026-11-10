using System.Collections.Generic;
using XSharp.LanguageService.Editors.LightBulb.QuickActions;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests.QuickActions
{
    public class GenerateMethodRuleTests
    {
        private static readonly XFile File = new XFile(@"C:\Test\A.prg", null);
        private readonly Dictionary<IXTypeSymbol, IXTypeSymbol> _bases = new Dictionary<IXTypeSymbol, IXTypeSymbol>();
        private int _next;

        private IXTypeSymbol BaseOf(IXTypeSymbol type) => _bases.TryGetValue(type, out var result) ? result : null;

        private XSourceTypeSymbol Type(string name, XSourceTypeSymbol baseType = null)
        {
            _next += 1000;
            var type = new XSourceTypeSymbol(name, Kind.Class, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(_next, _next + 900), File, null);
            if (baseType != null)
                _bases[type] = baseType;
            return type;
        }

        private void Member(XSourceTypeSymbol parent, string name)
        {
            _next += 10;
            var member = new XSourceMemberSymbol(name, Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(_next, _next + 5), "USUAL", null);
            member.File = File;
            parent.AddMember(member);
        }

        private IXTypeSymbol Target(ReceiverKind kind, IXTypeSymbol receiver, IXTypeSymbol enclosing, bool isCall = true, bool knownGlobal = false,
            bool resolved = false, string name = "Foo")
        {
            return GenerateMethodRule.Target(kind, receiver, enclosing, isCall, resolved, name, _ => knownGlobal, BaseOf, false);
        }

        [Fact]
        public void UndeclaredVariableCountsAsUnresolved()
        {
            var member = new XSourceMemberSymbol("Test", Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(0, 5), "VOID", null);
            var symbols = new IXSymbol[] { new XSourceUndeclaredVariableSymbol(member, "LoadData", new TextRange(2, 1, 2, 9), new TextInterval(0, 8)) };
            Assert.False(GenerateMethodRule.IsResolved(symbols));
            Assert.True(GenerateMethodRule.IsResolved(new IXSymbol[] { member }));
        }

        [Fact]
        public void ResolvedNameIsNotOffered()
        {
            // e.g. an extension method: not among the members of the class, but the lookup finds it
            Assert.Null(Target(ReceiverKind.Self, null, Type("Bar"), resolved: true));
        }

        [Theory]
        [InlineData("PCount")]
        [InlineData("pcount")]
        [InlineData("ArgCount")]
        [InlineData("_GetInst")]
        [InlineData("PCall")]
        [InlineData("DoDefault")]
        [InlineData("_Args")]
        public void BareCallToIntrinsicIsNotOffered(string name)
        {
            Assert.Null(Target(ReceiverKind.None, null, Type("Bar"), name: name));
        }

        [Fact]
        public void SelfCallInSourceClassIsOffered()
        {
            var bar = Type("Bar");
            Assert.Same(bar, Target(ReceiverKind.Self, null, bar));
        }

        [Fact]
        public void TypedVariableOfSourceClassIsOffered()
        {
            var bar = Type("Bar");
            var customer = Type("Customer");
            Assert.Same(customer, Target(ReceiverKind.Variable, customer, bar));
        }

        [Fact]
        public void ExistingMemberIsNotOffered()
        {
            var parent = Type("Parent");
            Member(parent, "foo");
            var bar = Type("Bar", parent);
            Assert.Null(Target(ReceiverKind.Self, null, bar));
        }

        [Fact]
        public void NoMethodInBaseClassSuppresses()
        {
            var parent = Type("Parent");
            Member(parent, "NoMethod");
            var bar = Type("Bar", parent);
            Assert.Null(Target(ReceiverKind.Self, null, bar));
        }

        [Fact]
        public void UnknownReceiverIsNotOffered()
        {
            Assert.Null(Target(ReceiverKind.Variable, null, Type("Bar")));
        }

        [Fact]
        public void SuperAndChainsAreNotOffered()
        {
            var bar = Type("Bar", Type("Parent"));
            Assert.Null(Target(ReceiverKind.Super, null, bar));
            Assert.Null(Target(ReceiverKind.Other, null, bar));
        }

        [Fact]
        public void BareCallInClassIsOffered()
        {
            var bar = Type("Bar");
            Assert.Same(bar, Target(ReceiverKind.None, null, bar));
        }

        [Fact]
        public void BareCallWithKnownFunctionIsNotOffered()
        {
            Assert.Null(Target(ReceiverKind.None, null, Type("Bar"), knownGlobal: true));
        }

        [Fact]
        public void BareCallOutsideClassIsNotOffered()
        {
            Assert.Null(Target(ReceiverKind.None, null, null));
        }

        [Fact]
        public void AccessWithoutParenIsNotOffered()
        {
            Assert.Null(Target(ReceiverKind.Self, null, Type("Bar"), isCall: false));
        }

        [Fact]
        public void CyclicBaseTypesDoNotHang()
        {
            var a = Type("A");
            var b = Type("B", a);
            _bases[a] = b;
            Assert.Same(b, Target(ReceiverKind.Self, null, b));
        }
    }
}
