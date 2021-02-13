using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler
{
    public static partial class Compilation
    {    
        public static Compilation<T, Func<T[], T>> Create<T>(MacroOptions options = null)
        {
            if (options?.ParseStatements == true)
                return new ScriptCompilation<T, Func<T[], T>>(options);
            return new Compilation<T, Func<T[], T>>(options ?? MacroOptions.Default);
        }

        public static Compilation<T, R> Create<T, R>(MacroOptions options = null) where R: Delegate
        {
            if (options?.ParseStatements == true)
                return new ScriptCompilation<T, R>(options);
            return new Compilation<T, R>(options ?? MacroOptions.Default);
        }
    }

    public partial class Compilation<T,R> where R: Delegate
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

        internal MacroOptions options;

        internal Compilation(MacroOptions o = null)
        {
            options = o ?? MacroOptions.Default;
        }

        public static Compilation<T, R> Create(MacroOptions options = null) => Compilation.Create<T, R>(options);

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

        internal virtual Syntax.Node Parse(string source)
        {
            var lexer = new Lexer(source, options);
            var parser = new Parser(lexer, options);
            return parser.ParseMacro();
        }
    }
    public class ScriptCompilation<T, R> : Compilation<T, R> where R : Delegate
    {
        internal ScriptCompilation(MacroOptions o = null): base(o)
        {
            options.ParseMode = ParseMode.Statements;
        }
        internal override Syntax.Node Parse(string source)
        {
            var lexer = new Lexer(source, options);
            var parser = new Parser(lexer, options);
            return parser.ParseScript();
        }
    }
}
