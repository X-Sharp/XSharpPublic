/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and   
limitations under the License.
*/
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

#if false // Do not use the regular X# code analysis namespaces ...
using LanguageService.CodeAnalysis;
using LanguageService.CodeAnalysis.Emit;
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.Syntax;

// The antlr4 runtime is contained in these namespaces
using LanguageService.SyntaxTree;
using LanguageService.SyntaxTree.Atn;
using LanguageService.SyntaxTree.Misc;
using LanguageService.SyntaxTree.Tree;

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

#else // ... instead, use these namespaces, that work only with our cusrom string-transforming C# compiler
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Emit;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
#endif

namespace XSTestCodeAnalysis
{
    public partial class TestClass
    {
        public static CSharpSyntaxTree ParseSource(string source) { return ParseSource("", source); }
        public static CSharpSyntaxTree ParseSource(string cmdLine, string source) {
            var args = CSharpCommandLineParser.SplitCommandLineIntoArguments(cmdLine, false);
            var cmdParser = new CSharpCommandLineParser();
            var parsedArgs = cmdParser.Parse(args, ".", null);

            return (CSharpSyntaxTree)CSharpSyntaxTree.ParseText(
                source,
                options: parsedArgs.ParseOptions
                );
        }

        public static CSharpSyntaxTree ParseStartFunction(string body) { return ParseStartFunction("", body); }
        public static CSharpSyntaxTree ParseStartFunction(string cmdLine, string body) {
            var args = CSharpCommandLineParser.SplitCommandLineIntoArguments(cmdLine, false);
            var cmdParser = new CSharpCommandLineParser();
            var parsedArgs = cmdParser.Parse(args, ".", null);

            return (CSharpSyntaxTree)CSharpSyntaxTree.ParseText(
                "FUNCTION Start() AS VOID\r\n"+body,
                options: parsedArgs.ParseOptions
                );
        }

        public static CSharpSyntaxTree VulcanRuntime { get; } = ParseSource(@"
BEGIN NAMESPACE Vulcan
    STRUCTURE __USUAL
        PRIVATE __o AS OBJECT
        STATIC _NIL := __USUAL{NULL} AS __USUAL
        PRIVATE CONSTRUCTOR(o AS OBJECT)
            __o := o
        OPERATOR ==(a AS __USUAL, b AS __USUAL) AS LOGIC
            RETURN a:__o == b:__o
        OPERATOR !=(a AS __USUAL, b AS __USUAL) AS LOGIC
            RETURN a:__o != b:__o
        OPERATOR Implicit(o AS OBJECT) AS __USUAL
            RETURN __USUAL{o}
        OPERATOR Implicit(u AS __USUAL) AS OBJECT
            RETURN u:__o
    END STRUCTURE
    CLASS VulcanImplicitNamespaceAttribute INHERIT System.Attribute
        CONSTRUCTOR(defaultNs AS STRING)
    END CLASS
END NAMESPACE
BEGIN NAMESPACE Vulcan.Internal
    [AttributeUsage(AttributeTargets.Method)];
    CLASS ClipperCallingConventionAttribute INHERIT System.Attribute
        CONSTRUCTOR(names AS STRING[])
    END CLASS
    CLASS VulcanClassLibraryAttribute INHERIT System.Attribute
        CONSTRUCTOR(globalClass AS STRING, globalNs AS STRING)
    END CLASS
    CLASS VulcanCompilerVersionAttribute INHERIT System.Attribute
        CONSTRUCTOR(version AS STRING)
    END CLASS
END NAMESPACE
BEGIN NAMESPACE VulcanRtFuncs
    STATIC CLASS Functions
        STATIC FUNCTION AsString(o AS OBJECT) AS STRING
	        RETURN o:ToString()
        UNSAFE STATIC FUNCTION FOpen(n AS STRING) AS VOID PTR
	        RETURN NULL
        UNSAFE STATIC FUNCTION FClose(v AS VOID PTR) AS VOID
	        RETURN
    END CLASS
END NAMESPACE
");

        private static byte[] _vrt;
        private static System.Reflection.Assembly _vrta;

        private static MetadataReference LoadVulcanRuntime()
        {
            if (_vrt == null)
            {
                MetadataReference[] refs = new MetadataReference[]
                {
                MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Enumerable).Assembly.Location)
                };
                var args = CSharpCommandLineParser.SplitCommandLineIntoArguments("/dialect:vulcan /r:VulcanRTFuncs.dll /r:VulcanRT.dll /unsafe", false);
                var cmdParser = new CSharpCommandLineParser();
                var parsedArgs = cmdParser.Parse(args, ".", null);
                var c = CSharpCompilation.Create(
                    "VulcanRTdummy.dll",
                    syntaxTrees: new[] { VulcanRuntime },
                    references: refs,
                    options: parsedArgs.CompilationOptions.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
                    );
                var ms = new System.IO.MemoryStream();
                EmitResult r = c.Emit(ms);
                if (!r.Success)
                {
                    string err = "";
                    r.Diagnostics.Where(d => d.IsWarningAsError || d.Severity == DiagnosticSeverity.Error).ToList().ForEach(d => err += d.ToString() + "\r\n");
                    throw new Exception(err);
                }
                ms.Seek(0, System.IO.SeekOrigin.Begin);
                _vrt = ms.ToArray();
                _vrta = System.Reflection.Assembly.Load(_vrt);
                System.AppDomain.CurrentDomain.AssemblyResolve += currentDomain_AssemblyResolve;
            }
            return MetadataReference.CreateFromStream(new System.IO.MemoryStream(_vrt));
        }

        static System.Reflection.Assembly currentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
        {
            if (args.RequestingAssembly == null)
                return null;

            if (args.Name.StartsWith("VulcanRTdummy.dll,"))
                return _vrta;

            return null;
        }

        private static CSharpCompilation CreateCompilation(string cmdLine, params CSharpSyntaxTree[] sources)
        {
            MetadataReference[] refs = new MetadataReference[]
            {
                MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Enumerable).Assembly.Location)
            };

            var args = CSharpCommandLineParser.SplitCommandLineIntoArguments(cmdLine, false);
            var cmdParser = new CSharpCommandLineParser();
            var parsedArgs = cmdParser.Parse(args, ".", null);

            if (sources.Contains(VulcanRuntime))
            {
                sources = sources.Where(s => s != VulcanRuntime).ToArray();
                refs = refs.Concat(new[] { LoadVulcanRuntime() }).ToArray();
            }

            return CSharpCompilation.Create(
                System.IO.Path.GetRandomFileName(),
                syntaxTrees: sources,
                references: refs,
                options: parsedArgs.CompilationOptions.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
                );
        }

        public static IEnumerable<Diagnostic> CompileWithErrors(params CSharpSyntaxTree[] sources) { return CompileWithErrors("", sources); }
        public static IEnumerable<Diagnostic> CompileWithErrors(string cmdLine, params CSharpSyntaxTree[] sources) {
            var c = CreateCompilation(cmdLine, sources);
            using (var ms = new System.IO.MemoryStream()) {
                EmitResult r = c.Emit(ms);
                if (!r.Success) {
                    return r.Diagnostics.Where(d => d.IsWarningAsError || d.Severity == DiagnosticSeverity.Error);
                }
                throw new Exception("Compiled without errors!");
            }
        }

        public static void CompileWithoutWarnings(params CSharpSyntaxTree[] sources) { CompileWithoutWarnings("", sources); }
        public static void CompileWithoutWarnings(string cmdLine, params CSharpSyntaxTree[] sources)
        {
            var c = CreateCompilation(cmdLine, sources);
            using (var ms = new System.IO.MemoryStream())
            {
                EmitResult r = c.Emit(ms);
                if (!r.Success)
                {
                    string err = "";
                    r.Diagnostics.Where(d => d.IsWarningAsError || d.Severity == DiagnosticSeverity.Error).ToList().ForEach(d => err += d.ToString() + "\r\n");
                    throw new Exception(err);
                }
                string wrn = "";
                r.Diagnostics.Where(d => d.Severity == DiagnosticSeverity.Warning).ToList().ForEach(d => wrn += d.ToString() + "\r\n");
                if (wrn != "")
                    throw new Exception(wrn);
            }
        }

        public static void CompileWithWarnings(params CSharpSyntaxTree[] sources) { CompileWithWarnings("", sources); }
        public static void CompileWithWarnings(string cmdLine, params CSharpSyntaxTree[] sources)
        {
            var c = CreateCompilation(cmdLine, sources);
            using (var ms = new System.IO.MemoryStream())
            {
                EmitResult r = c.Emit(ms);
                if (!r.Success)
                {
                    string err = "";
                    r.Diagnostics.Where(d => d.IsWarningAsError || d.Severity == DiagnosticSeverity.Error).ToList().ForEach(d => err += d.ToString() + "\r\n");
                    throw new Exception(err);
                }
                string wrn = "";
                r.Diagnostics.Where(d => d.Severity == DiagnosticSeverity.Warning).ToList().ForEach(d => wrn += d.ToString() + "\r\n");
                if (wrn == "")
                    throw new Exception("No warnings");
            }
        }

        public static System.Reflection.Assembly CompileAndLoadWithoutErrors(params CSharpSyntaxTree[] sources) { return CompileAndLoadWithoutErrors("", sources); }
        public static System.Reflection.Assembly CompileAndLoadWithoutErrors(string cmdLine, params CSharpSyntaxTree[] sources) {
            var c = CreateCompilation(cmdLine, sources);
            using (var ms = new System.IO.MemoryStream()) {
                EmitResult r = c.Emit(ms);
                if (!r.Success) {
                    string err = "";
                    r.Diagnostics.Where(d => d.IsWarningAsError || d.Severity == DiagnosticSeverity.Error).ToList().ForEach(d => err += d.ToString()+"\r\n");
                    throw new Exception(err);
                }
                ms.Seek(0, System.IO.SeekOrigin.Begin);
                return System.Reflection.Assembly.Load(ms.ToArray());
            }
        }

        public static void CompileAndRunWithoutExceptions(params CSharpSyntaxTree[] sources) { CompileAndRunWithoutExceptions("", sources); }
        public static void CompileAndRunWithoutExceptions(string cmdLine, params CSharpSyntaxTree[] sources) {
            var a = CompileAndLoadWithoutErrors(cmdLine, sources);
            Type t = a.GetType("Functions");

            var co = new System.IO.StringWriter();
            var stdo = Console.Out;
            Console.SetOut(co);
            try
            {
                t.InvokeMember("Start", System.Reflection.BindingFlags.Default | System.Reflection.BindingFlags.InvokeMethod,
                    binder: null,
                    target: null,
                    args: new object[] { });
            }
            catch (Exception e) {
                throw new Exception(e.InnerException?.Message ?? e.Message);
            }
            finally
            {
                Console.SetOut(stdo);
            }
        }

        public static void CompileAndRunWithExceptions(params CSharpSyntaxTree[] sources) { CompileAndRunWithExceptions("", sources); }
        public static void CompileAndRunWithExceptions(string cmdLine, params CSharpSyntaxTree[] sources) {
            var a = CompileAndLoadWithoutErrors(cmdLine, sources);
            Type t = a.GetType("Functions");

            bool e = false;

            var co = new System.IO.StringWriter();
            var stdo = Console.Out;
            Console.SetOut(co);
            try {
                try {
                    t.InvokeMember("Start", System.Reflection.BindingFlags.Default | System.Reflection.BindingFlags.InvokeMethod,
                        binder: null,
                        target: null,
                        args: new object[] { } );
                }
                catch {
                    e = true;
                }
            }
            finally {
                Console.SetOut(stdo);
            }

            if (!e)
                throw new Exception("Exception not thrown!");
        }

        public static void CompileAndRunWithoutExceptionsAndNonEmptyConsoleOutput(params CSharpSyntaxTree[] sources) { CompileAndRunWithoutExceptionsAndNonEmptyConsoleOutput("", sources); }
        public static void CompileAndRunWithoutExceptionsAndNonEmptyConsoleOutput(string cmdLine, params CSharpSyntaxTree[] sources) {
            var a = CompileAndLoadWithoutErrors(cmdLine, sources);
            Type t = a.GetType("Functions");

            var co = new System.IO.StringWriter();
            var stdo = Console.Out;
            Console.SetOut(co);
            try {
                t.InvokeMember("Start", System.Reflection.BindingFlags.Default | System.Reflection.BindingFlags.InvokeMethod,
                    binder: null,
                    target: null,
                    args: new object[] { } );
            }
            finally {
                Console.SetOut(stdo);
            }

            string output = co.ToString();
            if (output == null || output == "")
                throw new Exception("No output to console!");
        }

    }
}
