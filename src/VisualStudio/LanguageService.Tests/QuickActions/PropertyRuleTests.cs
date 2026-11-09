using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageService.Editors.LightBulb.QuickActions;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests.QuickActions
{
    public class PropertyRuleTests
    {
        private static readonly XFile File = new XFile(@"C:\Test\A.prg", null);
        private readonly Dictionary<IXTypeSymbol, IXTypeSymbol> _bases = new Dictionary<IXTypeSymbol, IXTypeSymbol>();
        private readonly Dictionary<int, string> _lines = new Dictionary<int, string>();
        private int _line = 1;

        private IXTypeSymbol BaseOf(IXTypeSymbol type) => _bases.TryGetValue(type, out var result) ? result : null;

        private XSourceTypeSymbol Type(string name, XSourceTypeSymbol baseType = null)
        {
            var type = new XSourceTypeSymbol(name, Kind.Class, Modifiers.Public, new TextRange(0, 1, 100, 1), new TextInterval(0, 5000), File, null);
            if (baseType != null)
                _bases[type] = baseType;
            return type;
        }

        private XSourceMemberSymbol Member(XSourceTypeSymbol parent, Kind kind, string name, string type = "STRING", Modifiers modifiers = Modifiers.Public,
            string attribute = null, params (string Name, string Type)[] parameters)
        {
            return MemberWith("", parent, kind, name, type, modifiers, attribute, parameters);
        }

        private XSourceMemberSymbol MemberWith(string prefix, XSourceTypeSymbol parent, Kind kind, string name, string type = "STRING",
            Modifiers modifiers = Modifiers.Public, string attribute = null, params (string Name, string Type)[] parameters)
        {
            if (attribute != null)
                _lines[_line++] = "    " + attribute;
            var header = _line;
            _lines[header] = "    " + prefix + kind.ToString().ToUpperInvariant() + " " + name;
            _line += 3;
            var member = new XSourceMemberSymbol(name, kind, modifiers, new TextRange(header, 1, header + 2, 1), new TextInterval(header * 10, header * 10 + 25), type, null);
            member.File = File;
            foreach (var p in parameters)
                member.AddParameter(new XSourceParameterSymbol(member, p.Name, new TextRange(header, 1, header, 1), new TextInterval(header * 10, header * 10 + 1), p.Type));
            parent.AddMember(member);
            return member;
        }

        private IReadOnlyList<AccessAssignPair> Pairs(XSourceTypeSymbol type, IEnumerable<XSourceMemberSymbol> inFile = null, bool hasEndClass = true)
        {
            inFile = inFile ?? type.Members.OfType<XSourceMemberSymbol>().ToList();
            return PropertyRule.Pairs(type, inFile, i => _lines.TryGetValue(i, out var t) ? t : "", hasEndClass, BaseOf, false);
        }

        [Fact]
        public void PairsAccessAndAssign()
        {
            var type = Type("Customer");
            var access = Member(type, Kind.Access, "Name");
            var assign = Member(type, Kind.Assign, "name", "", parameters: ("cValue", "STRING"));
            var pair = Assert.Single(Pairs(type));
            Assert.Same(access, pair.Access);
            Assert.Same(assign, pair.Assign);
        }

        [Fact]
        public void SingleAccessIsAPair()
        {
            var type = Type("Customer");
            Member(type, Kind.Access, "Name");
            Member(type, Kind.Method, "Show");
            var pair = Assert.Single(Pairs(type));
            Assert.Null(pair.Assign);
        }

        [Fact]
        public void VoStyleClassHasNoPairs()
        {
            var type = Type("Customer");
            Member(type, Kind.Access, "Name");
            Assert.Empty(Pairs(type, hasEndClass: false));
        }

        [Fact]
        public void BaseClassNameIsNotOffered()
        {
            var basis = Type("Basis");
            Member(basis, Kind.Property, "Name");
            var type = Type("Customer", basis);
            Member(type, Kind.Access, "Name");
            Member(type, Kind.Access, "City");
            Assert.Equal(new[] { "City" }, Pairs(type).Select(p => p.Name));
        }

        [Fact]
        public void PartialOtherPartIsNotOffered()
        {
            var type = Type("Customer");
            var access = Member(type, Kind.Access, "Name");
            Member(type, Kind.Assign, "Name", "", parameters: ("cValue", "STRING"));
            Assert.Empty(Pairs(type, new[] { access }));
        }

        [Fact]
        public void DifferentTypesAreNotOffered()
        {
            var type = Type("Customer");
            Member(type, Kind.Access, "Name");
            Member(type, Kind.Assign, "Name", "", parameters: ("nValue", "INT"));
            Assert.Empty(Pairs(type));
        }

        [Fact]
        public void DifferentNonVisibilityModifiersAreNotOffered()
        {
            var type = Type("Customer");
            Member(type, Kind.Access, "Name", modifiers: Modifiers.Public | Modifiers.Static);
            Member(type, Kind.Assign, "Name", "", parameters: ("cValue", "STRING"));
            Assert.Empty(Pairs(type));
        }

        [Fact]
        public void AttributeIsNotOffered()
        {
            var type = Type("Customer");
            Member(type, Kind.Access, "Name", attribute: "[Obsolete]");
            Member(type, Kind.Access, "City");
            Assert.Equal(new[] { "City" }, Pairs(type).Select(p => p.Name));
        }

        [Fact]
        public void IndexParametersMustMatch()
        {
            var type = Type("Customer");
            Member(type, Kind.Access, "Item", parameters: ("n", "INT"));
            Member(type, Kind.Assign, "Item", "", parameters: new[] { ("c", "STRING"), ("n", "STRING") });
            Assert.Empty(Pairs(type));
        }

        [Fact]
        public void OtherModifiersInTheHeaderAreNotOffered()
        {
            var type = Type("Customer");
            MemberWith("STATIC ", type, Kind.Access, "Name", modifiers: Modifiers.Public | Modifiers.Static);
            Member(type, Kind.Access, "City");
            Assert.Equal(new[] { "City" }, Pairs(type).Select(p => p.Name));
        }

        [Fact]
        public void InternalAndProtectedAreNotOffered()
        {
            // neither is wider than the other: no visibility for the property
            var type = Type("Customer");
            MemberWith("INTERNAL ", type, Kind.Access, "Name", modifiers: Modifiers.Internal);
            MemberWith("PROTECTED ", type, Kind.Assign, "Name", "", Modifiers.Protected, parameters: ("cValue", "STRING"));
            Assert.Empty(Pairs(type));
        }

        [Fact]
        public void CaretOnAssignFindsThePair()
        {
            var type = Type("Customer");
            Member(type, Kind.Access, "Name");
            var assign = Member(type, Kind.Assign, "Name", "", parameters: ("cValue", "STRING"));
            var pairs = Pairs(type);
            Assert.Same(assign, PropertyRule.At(pairs, assign.Range.StartLine).Assign);
            Assert.Null(PropertyRule.At(pairs, assign.Range.StartLine + 1));
        }
    }
}
