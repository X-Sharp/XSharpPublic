using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using XSharp.LanguageService.ForeignSource;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class ForeignSymbolResolverTests
    {
        private const string CratePath = @"C:\lib\Crate.cs";

        // one declaration per line, so that the expected (0-based) line numbers can be read off
        private static readonly string CrateSource = string.Join("\n",
            "namespace Lib {",                                          // 0
            "    public class Crate {",                                 // 1
            "        public Crate(int size) { }",                       // 2
            "        public void Open() { }",                           // 3
            "        public void Open(string key) { }",                 // 4
            "        public int Size { get; set; }",                    // 5
            "        public T Get<T>(int index) { return default(T); }",// 6
            "        public class Lid { }",                             // 7
            "    }",                                                    // 8
            "    public interface IBox { void Close(); }",              // 9
            "    public enum Color { Red, Green }",                     // 10
            "    public class Plain { }",                               // 11
            "}");

        private static Compilation Compile(params (string path, string source)[] files)
        {
            var trees = files.Select(f => CSharpSyntaxTree.ParseText(f.source, path: f.path));
            return CSharpCompilation.Create("Lib", trees,
                new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) },
                new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        }

        private static IReadOnlyList<ForeignLocation> Resolve(ForeignSymbolKey key)
        {
            return ForeignSymbolResolver.Resolve(new[] { Compile((CratePath, CrateSource)) }, key);
        }

        private static ForeignSymbolKey Type(string typeDocId) => new ForeignSymbolKey(typeDocId, null, null, 0);

        private static ForeignSymbolKey Member(string typeDocId, string memberDocId, string name, int parameterCount)
            => new ForeignSymbolKey(typeDocId, memberDocId, name, parameterCount);

        private static void AssertSingle(IReadOnlyList<ForeignLocation> found, int line)
        {
            Assert.Single(found);
            Assert.Equal(CratePath, found[0].FilePath);
            Assert.Equal(line, found[0].StartLine);
        }

        [Fact]
        public void TypeResolvesToItsDeclaration()
        {
            var found = Resolve(Type("T:Lib.Crate"));
            AssertSingle(found, 1);
            Assert.Equal("Crate", found[0].Name);
        }

        [Fact]
        public void OverloadPickedByDocId()
        {
            AssertSingle(Resolve(Member("T:Lib.Crate", "M:Lib.Crate.Open(System.String)", "Open", 1)), 4);
        }

        [Fact]
        public void PropertyResolves()
        {
            AssertSingle(Resolve(Member("T:Lib.Crate", "P:Lib.Crate.Size", "Size", 0)), 5);
        }

        [Fact]
        public void NestedTypeResolves()
        {
            AssertSingle(Resolve(Type("T:Lib.Crate.Lid")), 7);
        }

        [Fact]
        public void NestedTypeInMetadataSpellingResolves()
        {
            // X# takes the type name from Mono.Cecil, which separates nested types with '/' (reflection: '+')
            AssertSingle(Resolve(Type("T:Lib.Crate/Lid")), 7);
            AssertSingle(Resolve(Type("T:Lib.Crate+Lid")), 7);
        }

        [Fact]
        public void InterfaceAndEnumResolve()
        {
            AssertSingle(Resolve(Type("T:Lib.IBox")), 9);
            AssertSingle(Resolve(Type("T:Lib.Color")), 10);
        }

        [Fact]
        public void ConstructorResolves()
        {
            AssertSingle(Resolve(Member("T:Lib.Crate", "M:Lib.Crate.#ctor(System.Int32)", ".ctor", 1)), 2);
        }

        [Fact]
        public void UnresolvedDocIdFallsBackToNameAndParameterCount()
        {
            // the X# doc ID misses the ``1 of the generic method
            AssertSingle(Resolve(Member("T:Lib.Crate", "M:Lib.Crate.Get(System.Int32)", "Get", 1)), 6);
        }

        [Fact]
        public void UnknownMemberFallsBackToType()
        {
            AssertSingle(Resolve(Member("T:Lib.Crate", "M:Lib.Crate.Gone", "Gone", 0)), 1);
        }

        [Fact]
        public void ImplicitConstructorGoesToType()
        {
            AssertSingle(Resolve(Member("T:Lib.Plain", "M:Lib.Plain.#ctor", ".ctor", 0)), 11);
        }

        [Fact]
        public void PartialClassGivesBothParts()
        {
            var compilation = Compile(
                (@"C:\lib\A.cs", "namespace Lib { public partial class Duo { } }"),
                (@"C:\lib\B.cs", "namespace Lib { public partial class Duo { public void M() { } } }"));
            var found = ForeignSymbolResolver.Resolve(new[] { compilation }, Type("T:Lib.Duo"));
            Assert.Equal(new[] { @"C:\lib\A.cs", @"C:\lib\B.cs" }, found.Select(f => f.FilePath).OrderBy(p => p).ToArray());
        }

        [Fact]
        public void UnknownTypeGivesNothing()
        {
            Assert.Empty(Resolve(Type("T:Lib.Missing")));
        }

        [Fact]
        public void DuplicatesAcrossCompilationsCollapse()
        {
            // a multi-targeted project: one Roslyn project (compilation) per target framework, same files
            var found = ForeignSymbolResolver.Resolve(
                new[] { Compile((CratePath, CrateSource)), Compile((CratePath, CrateSource)) }, Type("T:Lib.Crate"));
            AssertSingle(found, 1);
        }
    }
}
