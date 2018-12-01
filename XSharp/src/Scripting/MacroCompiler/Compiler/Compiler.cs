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
            public R Macro;
            public int ParamCount;
            internal CompilationResult(R macro, int paramCount) { Macro = macro; ParamCount = paramCount; }
        }

        MacroOptions options;

        internal Compilation(MacroOptions o = null)
        {
            options = o ?? MacroOptions.Default;
        }

        public CompilationResult Compile(string source)
        {
            Binder<T, R> binder = Binder.Create<T, R>(options);
            var ast = binder.Bind(Parse(source));
            return new CompilationResult(binder.Emit(ast), binder.ParamCount);
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
