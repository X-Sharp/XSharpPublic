using System;
using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageService.Editors.LightBulb.QuickActions;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests.QuickActions
{
    public abstract class OverrideTestBase
    {
        protected static readonly XFile File = new XFile(@"C:\Test\A.prg", null);
        protected readonly Dictionary<IXTypeSymbol, IXTypeSymbol> Bases = new Dictionary<IXTypeSymbol, IXTypeSymbol>();
        protected readonly HashSet<IXTypeSymbol> VoClasses = new HashSet<IXTypeSymbol>();
        private int _next;

        protected IXTypeSymbol BaseOf(IXTypeSymbol type) => Bases.TryGetValue(type, out var result) ? result : null;

        protected XSourceTypeSymbol Type(string name, XSourceTypeSymbol baseType = null, bool vo = false)
        {
            _next += 1000;
            var type = new XSourceTypeSymbol(name, Kind.Class, Modifiers.Public, new TextRange(1, 1, 10, 1), new TextInterval(_next, _next + 900), File, null);
            if (baseType != null)
                Bases[type] = baseType;
            if (vo)
                VoClasses.Add(type);
            return type;
        }

        protected XSourceMemberSymbol Member(XSourceTypeSymbol parent, string name, Kind kind = Kind.Method, Modifiers modifiers = Modifiers.Public,
            string returnType = "", params (string Name, string Type)[] parameters)
        {
            _next += 10;
            var member = new XSourceMemberSymbol(name, kind, modifiers, new TextRange(2, 1, 3, 1), new TextInterval(_next, _next + 5), returnType, null);
            member.File = File;
            foreach (var p in parameters)
                member.AddParameter(new XSourceParameterSymbol(member, p.Name, new TextRange(2, 1, 2, 1), new TextInterval(_next, _next + 1), p.Type));
            parent?.AddMember(member);
            return member;
        }

        private protected static CodeStyle Style(XDialect dialect = XDialect.VO)
        {
            return new CodeStyle(dialect, s => s.ToUpperInvariant(), "    ", "\r\n");
        }
    }

    public class OverrideCandidatesTests : OverrideTestBase
    {
        private IList<string> Names(IXTypeSymbol type) =>
            OverrideCandidates.For(type, BaseOf, t => VoClasses.Contains(t), false).Select(m => m.Name).ToList();

        [Fact]
        public void VoBaseMethodsAreCandidates()
        {
            var window = Type("Window", vo: true);
            Member(window, "Show");
            Member(window, "Close");
            Member(window, "Init", Kind.Constructor);
            Member(window, "Axit", Kind.Destructor);
            var derived = Type("MyWindow", window, vo: true);
            Assert.Equal(new[] { "Close", "Show" }, Names(derived));
        }

        [Fact]
        public void AlreadyOverriddenIsSkipped()
        {
            var window = Type("Window", vo: true);
            Member(window, "Show");
            Member(window, "Close");
            var derived = Type("MyWindow", window, vo: true);
            Member(derived, "show");
            Assert.Equal(new[] { "Close" }, Names(derived));
        }

        [Fact]
        public void OnlyVirtualMembersOfCoreClasses()
        {
            var basis = Type("Basis");
            Member(basis, "Plain");
            Member(basis, "Virt", modifiers: Modifiers.Public | Modifiers.Virtual);
            Member(basis, "Abst", modifiers: Modifiers.Public | Modifiers.Abstract);
            Member(basis, "Sealed", modifiers: Modifiers.Public | Modifiers.Override | Modifiers.Sealed);
            Member(basis, "Stat", modifiers: Modifiers.Public | Modifiers.Static | Modifiers.Virtual);
            var derived = Type("Derived", basis);
            Assert.Equal(new[] { "Abst", "Virt" }, Names(derived));
        }

        [Fact]
        public void NearestBaseFirst()
        {
            var a = Type("A", vo: true);
            Member(a, "Alpha");
            var b = Type("B", a, vo: true);
            Member(b, "Beta");
            var c = Type("C", b, vo: true);
            Assert.Equal(new[] { "Beta", "Alpha" }, Names(c));
        }

        [Fact]
        public void OverriddenInBetweenIsNotOfferedAgain()
        {
            var a = Type("A", vo: true);
            Member(a, "Show");
            var b = Type("B", a, vo: true);
            Member(b, "Show");
            var c = Type("C", b, vo: true);
            Assert.Equal(new[] { "Show" }, Names(c));
            Assert.Same(b, OverrideCandidates.For(c, BaseOf, t => VoClasses.Contains(t), false)[0].Parent);
        }

        [Fact]
        public void DifferentParameterTypesAreNotTheSameMember()
        {
            var basis = Type("Basis", vo: true);
            Member(basis, "Foo", parameters: ("n", "INT"));
            var derived = Type("Derived", basis, vo: true);
            Member(derived, "Foo", parameters: ("c", "STRING"));
            Assert.Equal(new[] { "Foo" }, Names(derived));
        }

        [Fact]
        public void FinalizeIsNotOffered()
        {
            var obj = Type("Object");
            obj.Namespace = "System";
            Member(obj, "Finalize", modifiers: Modifiers.Protected | Modifiers.Virtual, returnType: "VOID");
            Member(obj, "ToString", modifiers: Modifiers.Public | Modifiers.Virtual, returnType: "STRING");
            Assert.Equal(new[] { "ToString" }, Names(Type("Derived", obj)));
        }

        [Fact]
        public void QualifiedDllParameterTypesMatchSourceOverride()
        {
            var form = Type("Form");
            Member(form, "OnPaint", modifiers: Modifiers.Protected | Modifiers.Virtual, returnType: "VOID",
                parameters: ("e", "System.Windows.Forms.PaintEventArgs"));
            var derived = Type("MyForm", form);
            Member(derived, "OnPaint", modifiers: Modifiers.Protected | Modifiers.Override, returnType: "VOID", parameters: ("e", "PaintEventArgs"));
            Assert.Empty(Names(derived));
        }

        [Fact]
        public void BasePropertyMatchesAccessInDerived()
        {
            var basis = Type("Basis");
            Member(basis, "Caption", Kind.Property, Modifiers.Public | Modifiers.Virtual, "STRING");
            var derived = Type("Derived", basis);
            Member(derived, "Caption", Kind.Access, returnType: "STRING");
            Assert.Empty(Names(derived));
        }

        [Fact]
        public void ObjectMembersAloneAreNotWorthOffering()
        {
            var obj = Type("Object");
            obj.Namespace = "System";
            Member(obj, "ToString", modifiers: Modifiers.Public | Modifiers.Virtual, returnType: "STRING");
            var plain = Type("Plain", obj);
            Assert.False(OverrideCandidates.WorthOffering(OverrideCandidates.For(plain, BaseOf, t => VoClasses.Contains(t), false)));
            var window = Type("Window", obj, vo: true);
            Member(window, "Show");
            var derived = Type("MyWindow", window, vo: true);
            Assert.True(OverrideCandidates.WorthOffering(OverrideCandidates.For(derived, BaseOf, t => VoClasses.Contains(t), false)));
        }

        [Fact]
        public void PropertiesAccessAndAssignAreCandidates()
        {
            var basis = Type("Basis", vo: true);
            Member(basis, "Caption", Kind.Access, returnType: "STRING");
            Member(basis, "Caption", Kind.Assign, parameters: ("cValue", "STRING"));
            Member(basis, "Field1", Kind.Field);
            var derived = Type("Derived", basis, vo: true);
            Member(derived, "Caption", Kind.Access, returnType: "STRING");
            var result = OverrideCandidates.For(derived, BaseOf, t => VoClasses.Contains(t), false);
            Assert.Single(result);
            Assert.Equal(Kind.Assign, result[0].Kind);
        }
    }

    public class OverrideSkeletonTests : OverrideTestBase
    {
        [Fact]
        public void TypedMethodReturnsSuper()
        {
            var foo = Member(null, "Foo", returnType: "LOGIC", parameters: ("n", "INT"));
            var text = OverrideSkeleton.Build(foo, "    ", false, Style());
            Assert.Equal("\r\n    METHOD Foo(n AS INT) AS LOGIC\r\n        RETURN SUPER:Foo(n)\r\n", text);
        }

        [Fact]
        public void VoidMethodCallsSuper()
        {
            var foo = Member(null, "Foo", returnType: "VOID", parameters: ("n", "INT"));
            var text = OverrideSkeleton.Build(foo, "", false, Style(XDialect.Core));
            Assert.Equal("\r\nMETHOD Foo(n AS INT) AS VOID\r\n    SUPER:Foo(n)\r\n    RETURN\r\n", text);
        }

        [Fact]
        public void UntypedVoMethodReturnsSuper()
        {
            var foo = Member(null, "Foo", parameters: ("n", "INT"));
            var text = OverrideSkeleton.Build(foo, "", false, Style());
            Assert.Equal("\r\nMETHOD Foo(n AS INT)\r\n    RETURN SUPER:Foo(n)\r\n", text);
        }

        [Fact]
        public void ClipperMethodPassesArgs()
        {
            var foo = Member(null, "Foo", parameters: new[] { ("a", ""), ("b", "") });
            var text = OverrideSkeleton.Build(foo, "", false, Style());
            Assert.Equal("\r\nMETHOD Foo(a, b) CLIPPER\r\n    RETURN SUPER:Foo(_Args())\r\n", text);
        }

        [Fact]
        public void ClipperWithoutParametersKeepsClipperAndType()
        {
            var destroy = Member(null, "Destroy", returnType: "USUAL");
            destroy.CallingConvention = CallingConvention.Clipper;
            Assert.Equal("\r\nMETHOD Destroy() AS USUAL CLIPPER\r\n    RETURN SUPER:Destroy(_Args())\r\n", OverrideSkeleton.Build(destroy, "", false, Style()));
        }

        [Fact]
        public void AbstractMethodThrows()
        {
            var foo = Member(null, "Foo", modifiers: Modifiers.Public | Modifiers.Abstract, returnType: "LOGIC");
            var text = OverrideSkeleton.Build(foo, "", true, Style(XDialect.Core));
            Assert.Equal("\r\nOVERRIDE METHOD Foo() AS LOGIC\r\n    THROW NotImplementedException{}\r\n", text);
        }

        [Fact]
        public void AbstractPropertyThrows()
        {
            var prop = Member(null, "Text", Kind.Property, Modifiers.Public | Modifiers.Abstract, "STRING");
            var text = OverrideSkeleton.Build(prop, "", true, Style(XDialect.Core));
            Assert.Equal("\r\nOVERRIDE PROPERTY Text AS STRING GET THROW NotImplementedException{} SET THROW NotImplementedException{}\r\n", text);
        }

        [Fact]
        public void RefParametersArePassedByReference()
        {
            var foo = Member(null, "Foo", returnType: "LOGIC", parameters: ("n", "INT"));
            ((XSourceParameterSymbol)foo.Parameters[0]).ParamType = ParamType.Ref;
            var text = OverrideSkeleton.Build(foo, "", false, Style());
            Assert.Contains("METHOD Foo(n REF INT) AS LOGIC", text);
            Assert.Contains("RETURN SUPER:Foo(REF n)", text);
        }

        [Fact]
        public void AccessAndAssign()
        {
            var access = Member(null, "Caption", Kind.Access, returnType: "STRING");
            Assert.Equal("\r\nACCESS Caption AS STRING\r\n    RETURN SUPER:Caption\r\n", OverrideSkeleton.Build(access, "", false, Style()));
            var assign = Member(null, "Caption", Kind.Assign, parameters: ("cValue", "STRING"));
            Assert.Equal("\r\nASSIGN Caption(cValue AS STRING)\r\n    SUPER:Caption := cValue\r\n", OverrideSkeleton.Build(assign, "", false, Style()));
        }

        [Fact]
        public void PropertyGetsBothAccessors()
        {
            var prop = Member(null, "Text", Kind.Property, Modifiers.Public | Modifiers.Virtual, "STRING");
            var text = OverrideSkeleton.Build(prop, "", true, Style(XDialect.Core));
            Assert.Equal("\r\nOVERRIDE PROPERTY Text AS STRING GET SUPER:Text SET SUPER:Text := VALUE\r\n", text);
        }

        [Fact]
        public void WriteOverrideAddsKeyword()
        {
            var foo = Member(null, "Foo", modifiers: Modifiers.Public | Modifiers.Virtual, returnType: "VOID");
            Assert.StartsWith("\r\nOVERRIDE METHOD Foo()", OverrideSkeleton.Build(foo, "", true, Style(XDialect.Core)));
        }

        [Fact]
        public void VoStyleClassGetsClassClause()
        {
            var foo = Member(null, "Foo", parameters: ("n", "INT"));
            Assert.Equal("\r\nMETHOD Foo(n AS INT) CLASS Bar\r\n    RETURN SUPER:Foo(n)\r\n", OverrideSkeleton.Build(foo, "", false, Style(), "Bar"));
            var clipper = Member(null, "Run", parameters: ("a", ""));
            Assert.StartsWith("\r\nMETHOD Run(a) CLIPPER CLASS Bar\r\n", OverrideSkeleton.Build(clipper, "", false, Style(), "Bar"));
            var access = Member(null, "Caption", Kind.Access, returnType: "STRING");
            Assert.StartsWith("\r\nACCESS Caption AS STRING CLASS Bar\r\n", OverrideSkeleton.Build(access, "", false, Style(), "Bar"));
            var assign = Member(null, "Caption", Kind.Assign, parameters: ("cValue", "STRING"));
            Assert.StartsWith("\r\nASSIGN Caption(cValue AS STRING) CLASS Bar\r\n", OverrideSkeleton.Build(assign, "", false, Style(), "Bar"));
        }

        [Fact]
        public void ProtectedVisibilityIsKept()
        {
            var foo = Member(null, "Foo", modifiers: Modifiers.Protected | Modifiers.Virtual, returnType: "VOID");
            Assert.StartsWith("\r\nPROTECTED METHOD Foo()", OverrideSkeleton.Build(foo, "", false, Style(XDialect.Core)));
        }
    }
}
