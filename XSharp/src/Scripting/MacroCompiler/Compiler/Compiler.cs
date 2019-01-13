using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler
{
    public static partial class Compilation
    {
        public static Compilation<T, Func<T[], T>> Create<T>(MacroOptions options = null)
        {
            return new Compilation<T, Func<T[], T>>(options ?? MacroOptions.Default);
        }

        public static Compilation<T, R> Create<T, R>(MacroOptions options = null) where R: class
        {
            return new Compilation<T, R>(options ?? MacroOptions.Default);
        }
    }

    public partial class Compilation<T,R> where R: class
    {
        public struct CompilationResult
        {
            public string Source;
            public R Macro;
            internal Binder<T, R> Binder;
            internal Syntax.Codeblock SyntaxTree;
            public int ParamCount { get => Binder.ParamCount; }
            public bool CreatesAutoVars { get => Binder.CreatesAutoVars; }
            public CompilationError Diagnostic;
            internal CompilationResult(string source, R macro, Binder<T, R> binder)
            {
                Source = source;
                Macro = macro;
                SyntaxTree = null;
                Binder = binder;
                Diagnostic = null;
            }
            internal CompilationResult(string source, Syntax.Codeblock syntaxTree, Binder<T, R> binder)
            {
                Source = source;
                Macro = null;
                SyntaxTree = syntaxTree;
                Binder = binder;
                Diagnostic = null;
            }
            internal CompilationResult(string source, CompilationError diagnostic)
            {
                Source = source;
                Macro = null;
                SyntaxTree = null;
                Binder = null;
                Diagnostic = diagnostic;
            }
        }

        MacroOptions options;

        internal Compilation(MacroOptions o = null)
        {
            options = o ?? MacroOptions.Default;
        }

        public CompilationResult Compile(string source)
        {
            try
            {
                Binder<T, R> binder = Binder.Create<T, R>(options);
                var ast = binder.Bind(Parse(source));
                return new CompilationResult(source, binder.Emit(ast,source), binder);
            }
            catch (CompilationError e)
            {
                return new CompilationResult(source, new CompilationError(e, source));
            }
        }

        public CompilationResult Bind(string source)
        {
            try
            {
                Binder<T, R> binder = Binder.Create<T, R>(options);
                var ast = binder.Bind(Parse(source));
                return new CompilationResult(source, binder.Emit(ast, source), binder);
            }
            catch (CompilationError e)
            {
                return new CompilationResult(source, new CompilationError(e, source));
            }
        }

        public R Emit(CompilationResult macro)
        {
            return macro.Binder.Emit(macro.SyntaxTree, macro.Source);
        }

        internal Syntax.Codeblock Parse(string source)
        {
            var lexer = new Lexer(source, options);
            var tokens = lexer.AllTokens();
            var parser = new Parser(tokens, options);
            return parser.ParseMacro();
        }
    }
}
