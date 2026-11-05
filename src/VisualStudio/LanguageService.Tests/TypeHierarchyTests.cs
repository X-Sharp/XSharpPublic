using System;
using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageService.GoToImplementation;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class TypeHierarchyTests
    {
        private static readonly XFile File = new XFile(@"C:\Test\H.prg", null);
        private readonly Dictionary<IXTypeSymbol, IXTypeSymbol> _bases = new Dictionary<IXTypeSymbol, IXTypeSymbol>();
        private readonly Dictionary<IXTypeSymbol, List<IXTypeSymbol>> _interfaces = new Dictionary<IXTypeSymbol, List<IXTypeSymbol>>();
        private readonly List<XSourceTypeSymbol> _all = new List<XSourceTypeSymbol>();
        private int _next;

        private XSourceTypeSymbol Type(string name, XSourceTypeSymbol baseType = null, string ns = "Test", Kind kind = Kind.Class, params XSourceTypeSymbol[] interfaces)
        {
            _next += 1000;
            var type = new XSourceTypeSymbol(name, kind, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(_next, _next + 900), File, null);
            type.Namespace = ns;
            if (baseType != null)
                _bases[type] = baseType;
            _interfaces[type] = interfaces.Cast<IXTypeSymbol>().ToList();
            _all.Add(type);
            return type;
        }

        private XSourceTypeSymbol Interface(string name, params XSourceTypeSymbol[] bases) => Type(name, null, "Test", Kind.Interface, bases);

        private XSourceMemberSymbol Member(XSourceTypeSymbol parent, string name, params string[] parameterTypes)
        {
            _next += 10;
            var member = new XSourceMemberSymbol(name, Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(_next, _next + 5), "VOID", null);
            member.File = File;
            int i = 0;
            foreach (var type in parameterTypes)
                member.AddParameter(new XSourceParameterSymbol(member, "p" + i++, new TextRange(2, 1, 2, 1), new TextInterval(_next, _next + 1), type));
            parent.AddMember(member);
            return member;
        }

        private IXTypeSymbol BaseOf(IXTypeSymbol type) => _bases.TryGetValue(type, out var result) ? result : null;

        private IEnumerable<IXTypeSymbol> InterfacesOf(IXTypeSymbol type) => _interfaces.TryGetValue(type, out var result) ? result : Enumerable.Empty<IXTypeSymbol>();

        /// <summary>As the database does: every type whose base class has this short name, whatever it resolves to.</summary>
        private IEnumerable<IXTypeSymbol> ByBaseShortName(string shortName) =>
            _all.Where(t => BaseOf(t) != null && string.Equals(BaseOf(t).Name, shortName, StringComparison.OrdinalIgnoreCase));

        /// <summary>As the database does: every type that names an interface with this short name.</summary>
        private IEnumerable<IXTypeSymbol> ByInterfaceShortName(string shortName) =>
            _all.Where(t => InterfacesOf(t).Any(i => string.Equals(i.Name, shortName, StringComparison.OrdinalIgnoreCase)));

        private static string[] Names(IEnumerable<IXSymbol> symbols) => symbols.Select(s => s.Name).OrderBy(n => n).ToArray();

        [Theory]
        [InlineData("Ns.List<Dog>", "List")]
        [InlineData("Window", "Window")]
        [InlineData("A.B.C", "C")]
        [InlineData("IComparable<Dog>", "IComparable")]
        public void ShortNameStripsNamespaceAndTypeArguments(string typeName, string expected)
        {
            Assert.Equal(expected, TypeHierarchy.ShortName(typeName));
        }

        [Theory]
        [InlineData("CLASS Foo INHERIT Bar IMPLEMENTS IA, Ns.IB", "IA|Ns.IB")]
        [InlineData("CLASS Foo IMPLEMENTS IComparable<Dog>, IA", "IComparable<Dog>|IA")]
        [InlineData("INTERFACE IFoo INHERIT IA, IB", "IA|IB")]
        [InlineData("CLASS Foo INHERIT Bar", "")]
        [InlineData("class foo implements ia where T IS NEW()", "ia")]
        [InlineData("PUBLIC PARTIAL CLASS Foo INHERIT Bar IMPLEMENTS IDictionary<STRING, INT>", "IDictionary<STRING, INT>")]
        public void DeclaredInterfaceNamesReadsTheDeclarationLine(string line, string expected)
        {
            var names = string.Join("|", TypeHierarchy.DeclaredInterfaceNames(line));
            Assert.Equal(expected, names);
        }

        [Fact]
        public void DerivedTypesFindsAllLevels()
        {
            var baseType = Type("Base");
            var mid = Type("Mid", baseType);
            Type("Leaf", mid);
            Type("Other", baseType);
            Type("Unrelated");
            Assert.Equal(new[] { "Leaf", "Mid", "Other" }, Names(TypeHierarchy.DerivedTypes(baseType, ByBaseShortName, BaseOf)));
        }

        [Fact]
        public void DerivedTypesRejectsSameShortNameElsewhere()
        {
            var window = Type("Window", ns: "Ns1");
            var otherWindow = Type("Window", ns: "Ns2");
            Type("Mine", window);
            Type("Theirs", otherWindow);
            Assert.Equal(new[] { "Mine" }, Names(TypeHierarchy.DerivedTypes(window, ByBaseShortName, BaseOf)));
        }

        [Fact]
        public void DerivedTypesSurvivesACycle()
        {
            var a = Type("A");
            var b = Type("B", a);
            _bases[a] = b;
            var result = TypeHierarchy.DerivedTypes(a, ByBaseShortName, BaseOf);
            // the target is never its own descendant
            Assert.Equal(new[] { "B" }, Names(result));
        }

        [Fact]
        public void ImplementersFollowDerivedInterfaces()
        {
            var ia = Interface("IA");
            var ib = Interface("IB", ia);
            Type("C1", null, "Test", Kind.Class, ia);
            Type("C2", null, "Test", Kind.Class, ib);
            Assert.Equal(new[] { "C1", "C2" }, Names(TypeHierarchy.Implementers(ia, ByInterfaceShortName, InterfacesOf)));
        }

        [Fact]
        public void ImplementersRejectCandidatesNotResolvingToTheInterface()
        {
            var foo = Interface("IFoo");
            var otherFoo = Type("IFoo", null, "Other", Kind.Interface);
            Type("C", null, "Test", Kind.Class, otherFoo);
            Assert.Empty(TypeHierarchy.Implementers(foo, ByInterfaceShortName, InterfacesOf));
        }

        [Fact]
        public void BaseTypesListsBaseClassThenInterfaces()
        {
            var ia = Interface("IA");
            var baseType = Type("Base");
            var sub = Type("Sub", baseType, "Test", Kind.Class, ia);
            Assert.Equal(new[] { "Base", "IA" }, TypeHierarchy.BaseTypes(sub, BaseOf, InterfacesOf).Select(t => t.Name).ToArray());
            Assert.Empty(TypeHierarchy.BaseTypes(baseType, BaseOf, InterfacesOf));
        }

        [Fact]
        public void OverridesUseTheSlotRule()
        {
            var baseType = Type("Base");
            var close = Member(baseType, "Close", "USUAL");
            var run = Member(baseType, "Run", "INT");
            var sub = Type("Sub", baseType);
            Member(sub, "Close");
            Member(sub, "Run", "STRING");
            var derived = TypeHierarchy.DerivedTypes(baseType, ByBaseShortName, BaseOf);
            Assert.Equal(new[] { "Close" }, Names(TypeHierarchy.Overrides(close, derived)));
            Assert.Empty(TypeHierarchy.Overrides(run, derived));
        }

        [Fact]
        public void OverriddenMemberSkipsClassesWithoutTheMember()
        {
            var baseType = Type("Base");
            var baseSpeak = Member(baseType, "Speak");
            var mid = Type("Mid", baseType);
            var leafSpeak = Member(Type("Leaf", mid), "Speak");
            Assert.Same(baseSpeak, TypeHierarchy.OverriddenMember(leafSpeak, BaseOf));
            Assert.Null(TypeHierarchy.OverriddenMember(baseSpeak, BaseOf));
        }

        private static XDbResult Part(string file, string baseTypeName, string sourceCode) =>
            new XDbResult { TypeName = "Part", Namespace = "Test", FileName = file, BaseTypeName = baseTypeName, SourceCode = sourceCode };

        [Fact]
        public void DeclaringPartPrefersThePartWithInherit()
        {
            var parts = new[]
            {
                Part("a.prg", "", "PARTIAL CLASS Part"),
                Part("b.prg", "Animal", "PARTIAL CLASS Part INHERIT Animal"),
                Part("c.prg", "", "PARTIAL CLASS Part"),
            };
            Assert.Equal("b.prg", SourceHierarchy.DeclaringPart(parts).FileName);
            var implementing = new[] { Part("a.prg", "", "PARTIAL CLASS Part"), Part("b.prg", "", "PARTIAL CLASS Part IMPLEMENTS IAnimal") };
            Assert.Equal("b.prg", SourceHierarchy.DeclaringPart(implementing).FileName);
        }

        [Fact]
        public void DeclaringPartFallsBackToTheFirst()
        {
            var parts = new[] { Part("a.prg", "", "PARTIAL CLASS Part"), Part("b.prg", "", "PARTIAL CLASS Part") };
            Assert.Equal("a.prg", SourceHierarchy.DeclaringPart(parts).FileName);
        }

        [Fact]
        public void TypeFullNameOfARowIsNamespaceAndName()
        {
            Assert.Equal("Test.Part", SourceHierarchy.TypeFullName(Part("a.prg", "", "")));
            Assert.Equal("Part", SourceHierarchy.TypeFullName(new XDbResult { TypeName = "Part", Namespace = "" }));
        }

        [Fact]
        public void ImplementedMembersMatchesInterfaceMember()
        {
            var ia = Interface("IA");
            var speak = Member(ia, "Speak");
            Member(ia, "Walk");
            var c = Type("C", null, "Test", Kind.Class, ia);
            var cSpeak = Member(c, "Speak");
            Assert.Equal(new IXMemberSymbol[] { speak }, TypeHierarchy.ImplementedMembers(cSpeak, InterfacesOf(c)));
        }
    }
}
