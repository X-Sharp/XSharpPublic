using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageService.Rename;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    /// <summary>Types and members for the rename tests, wired like the code model does it.</summary>
    internal sealed class RenameFixture
    {
        internal static readonly XFile File = new XFile(@"C:\Test\A.prg", null);
        internal readonly Dictionary<IXTypeSymbol, IXTypeSymbol> Bases = new Dictionary<IXTypeSymbol, IXTypeSymbol>();
        internal readonly Dictionary<string, IXTypeSymbol> Types = new Dictionary<string, IXTypeSymbol>(System.StringComparer.OrdinalIgnoreCase);
        private int _next;

        internal IXTypeSymbol BaseOf(IXTypeSymbol type) => Bases.TryGetValue(type, out var result) ? result : null;
        internal IXTypeSymbol FindType(string name) => Types.TryGetValue(name, out var result) ? result : null;

        internal XSourceTypeSymbol Type(string name, XSourceTypeSymbol baseType = null, Kind kind = Kind.Class)
        {
            _next += 1000;
            var type = new XSourceTypeSymbol(name, kind, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(_next, _next + 900), File, null);
            type.Namespace = "Test";
            if (baseType != null)
                Bases[type] = baseType;
            Types[name] = type;
            return type;
        }

        internal XSourceMemberSymbol Member(XSourceTypeSymbol parent, string name, Kind kind = Kind.Method, params string[] parameterNames)
        {
            _next += 10;
            var member = new XSourceMemberSymbol(name, kind, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(_next, _next + 5), "VOID", null);
            member.File = File;
            foreach (var p in parameterNames)
                member.AddParameter(new XSourceParameterSymbol(member, p, new TextRange(2, 1, 2, 1), new TextInterval(_next, _next + 1), "USUAL"));
            parent.AddMember(member);
            return member;
        }

        internal XSourceVariableSymbol Local(XSourceMemberSymbol owner, string name)
        {
            _next += 2;
            return new XSourceVariableSymbol(owner, name, new TextRange(2, 1, 2, 1), new TextInterval(_next, _next + 1), "USUAL");
        }
    }

    public class RenameGuardTests
    {
        private readonly RenameFixture _f = new RenameFixture();

        private string Refusal(IXSymbol target, IXTypeSymbol external = null)
        {
            return RenameGuard.Refusal(target, _f.BaseOf, _f.FindType, t => t != external);
        }

        [Fact]
        public void InterfaceMembersAreRefused()
        {
            var iface = _f.Type("IFoo", kind: Kind.Interface);
            var run = _f.Member(iface, "Run");
            Assert.Equal("'Run' belongs to an interface. Renaming interface members is not supported.", Refusal(run));
        }

        [Fact]
        public void ImplementationsAreRefused()
        {
            var iface = _f.Type("IFoo", kind: Kind.Interface);
            _f.Member(iface, "Run");
            var foo = _f.Type("Foo");
            foo.AddInterface("IFoo");
            var run = _f.Member(foo, "Run");
            Assert.Equal("'Run' implements a member of interface 'IFoo'. Renaming it is not supported.", Refusal(run));
        }

        [Fact]
        public void OverridesOfAssemblyMembersAreRefused()
        {
            var ext = _f.Type("Ext");
            _f.Member(ext, "Dispose");
            var foo = _f.Type("Foo", ext);
            var dispose = _f.Member(foo, "Dispose");
            Assert.Equal("'Dispose' overrides a member of 'Ext' from a referenced assembly and cannot be renamed.", Refusal(dispose, ext));
        }

        [Fact]
        public void PlainMembersAndTypesMayBeRenamed()
        {
            var ext = _f.Type("Ext");
            var foo = _f.Type("Foo", ext);
            var run = _f.Member(foo, "Run", Kind.Method, "n");
            Assert.Null(Refusal(run, ext));
            Assert.Null(Refusal(foo, ext));
            Assert.Null(Refusal(_f.Local(run, "x"), ext));
        }
    }

    public class RenameConflictsTests
    {
        private readonly RenameFixture _f = new RenameFixture();
        private readonly Dictionary<IXMemberSymbol, List<IXSymbol>> _locals = new Dictionary<IXMemberSymbol, List<IXSymbol>>();

        private IList<IXSymbol> Conflicts(IXSymbol target, string newName)
        {
            var scope = new RenameScope
            {
                BaseOf = _f.BaseOf,
                FindTypeOrFunction = name => _f.FindType(name),
                LocalsOf = m => _locals.TryGetValue(m, out var list) ? list : new List<IXSymbol>()
            };
            return RenameConflicts.Find(target, newName, scope);
        }

        [Fact]
        public void MemberClashesWithMemberOfBaseType()
        {
            var baseType = _f.Type("Base");
            var shut = _f.Member(baseType, "Shut");
            var derived = _f.Type("Derived", baseType);
            var close = _f.Member(derived, "Close");
            Assert.Equal(new IXSymbol[] { shut }, Conflicts(close, "shut"));
            Assert.Empty(Conflicts(close, "Open"));
        }

        [Fact]
        public void CaseOnlyRenameHasNoConflict()
        {
            var foo = _f.Type("Foo");
            var close = _f.Member(foo, "Close");
            Assert.Empty(Conflicts(close, "CLOSE"));
            Assert.Empty(Conflicts(foo, "FOO"));
        }

        [Fact]
        public void LocalClashesWithParameter()
        {
            var foo = _f.Type("Foo");
            var run = _f.Member(foo, "Run", Kind.Method, "nCount");
            var x = _f.Local(run, "x");
            _locals[run] = new List<IXSymbol> { x };
            var conflicts = Conflicts(x, "NCOUNT");
            Assert.Single(conflicts);
            Assert.Equal("nCount", conflicts[0].Name);
        }

        [Fact]
        public void TypeClashesWithExistingType()
        {
            var foo = _f.Type("Foo");
            var bar = _f.Type("Bar");
            Assert.Equal(new IXSymbol[] { bar }, Conflicts(foo, "Bar"));
        }

        [Fact]
        public void AccessAssignPartnerIsNoConflict()
        {
            var foo = _f.Type("Foo");
            var access = _f.Member(foo, "Name", Kind.Access);
            _f.Member(foo, "Name", Kind.Assign);
            Assert.Empty(Conflicts(access, "NAME"));
            Assert.Empty(Conflicts(access, "Title"));
        }
    }
}
