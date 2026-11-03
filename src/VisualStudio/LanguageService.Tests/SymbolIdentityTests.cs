using XSharp.LanguageService.FindReferences;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class SymbolIdentityTests
    {
        private static readonly XFile FileA = new XFile(@"C:\Test\A.prg", null);
        private static readonly XFile FileB = new XFile(@"C:\Test\B.prg", null);

        private static XSourceTypeSymbol Type(string name, XFile file, int start, string ns = "")
        {
            var type = new XSourceTypeSymbol(name, Kind.Class, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(start, start + 100), file, null);
            type.Namespace = ns;
            return type;
        }

        private static XSourceMemberSymbol Member(XSourceTypeSymbol parent, string name, Kind kind, int start)
        {
            var member = new XSourceMemberSymbol(name, kind, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(start, start + 10), "USUAL", null);
            member.File = parent.File;
            parent.AddMember(member);
            return member;
        }

        [Fact]
        public void SameDeclarationIsSame()
        {
            var type = Type("Foo", FileA, 0);
            var first = Member(type, "Close", Kind.Method, 50);
            var again = new XSourceMemberSymbol("Close", Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(50, 60), "USUAL", null);
            again.File = new XFile(@"c:\test\a.prg", null);
            Assert.True(SymbolIdentity.IsSame(first, again));
        }

        [Fact]
        public void SameNameOtherTypeIsNotSame()
        {
            var closeA = Member(Type("ClassA", FileA, 0), "Close", Kind.Method, 50);
            var closeB = Member(Type("ClassB", FileA, 200), "Close", Kind.Method, 250);
            Assert.False(SymbolIdentity.IsSame(closeA, closeB));
        }

        [Fact]
        public void AccessAndAssignAreSame()
        {
            var type = Type("Foo", FileA, 0);
            var access = Member(type, "Name", Kind.Access, 50);
            var assign = Member(type, "NAME", Kind.Assign, 70);
            Assert.True(SymbolIdentity.IsSame(access, assign));
        }

        [Fact]
        public void PartialTypesAreSame()
        {
            var partA = Type("Foo", FileA, 0, "Ns");
            var partB = Type("foo", FileB, 300, "ns");
            Assert.True(SymbolIdentity.IsSame(partA, partB));
        }

        [Fact]
        public void MemberWithoutFileMatchesByTypeAndSignature()
        {
            var type = Type("Foo", FileA, 0, "Ns");
            var declared = Member(type, "Close", Kind.Method, 50);
            var fromDatabase = new XSourceMemberSymbol("CLOSE", Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(50, 60), "USUAL", null);
            Type("foo", null, 0, "ns").AddMember(fromDatabase);
            Assert.True(SymbolIdentity.IsSame(fromDatabase, declared));
            Assert.True(SymbolIdentity.IsSame(declared, fromDatabase));
        }

        [Fact]
        public void MemberWithoutFileInOtherTypeIsNotSame()
        {
            var declared = Member(Type("Bar", FileA, 0), "Close", Kind.Method, 50);
            var fromDatabase = new XSourceMemberSymbol("Close", Kind.Method, Modifiers.Public, new TextRange(2, 1, 3, 1), new TextInterval(50, 60), "USUAL", null);
            Type("Foo", null, 0).AddMember(fromDatabase);
            Assert.False(SymbolIdentity.IsSame(fromDatabase, declared));
        }

        [Fact]
        public void ConstructorRefersToItsClass()
        {
            var type = Type("Foo", FileA, 0, "LibA");
            var ctor = Member(type, ".ctor", Kind.Constructor, 50);
            var otherPart = Type("Foo", FileB, 0, "LibA");
            Assert.True(SymbolIdentity.IsReferenceTo(type, ctor));
            Assert.True(SymbolIdentity.IsReferenceTo(otherPart, ctor));
        }

        [Fact]
        public void ConstructorOfSameNamedClassElsewhereDoesNotReferToTheClass()
        {
            var type = Type("Foo", FileA, 0, "LibA");
            var ctor = Member(Type("Foo", FileB, 0, "Other"), ".ctor", Kind.Constructor, 50);
            Assert.False(SymbolIdentity.IsReferenceTo(type, ctor));
        }

        [Fact]
        public void MethodDoesNotReferToItsClass()
        {
            var type = Type("Foo", FileA, 0, "LibA");
            Assert.False(SymbolIdentity.IsReferenceTo(type, Member(type, "Run", Kind.Method, 50)));
            Assert.True(SymbolIdentity.IsReferenceTo(type, type));
        }

        [Fact]
        public void LocalShadowingFieldIsNotSame()
        {
            var type = Type("Foo", FileA, 0);
            var field = Member(type, "nCount", Kind.Field, 20);
            var method = Member(type, "Run", Kind.Method, 50);
            var local = new XSourceVariableSymbol(method, "nCount", new TextRange(4, 1, 4, 10), new TextInterval(55, 61), "INT");
            Assert.False(SymbolIdentity.IsSame(field, local));
            Assert.False(SymbolIdentity.IsSame(local, field));
        }
    }
}
