using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler
{
    public static partial class Compilation
    {
        public class Options
        {
            public static readonly Options Default = new Options();

            public bool AllowFourLetterAbbreviations = true;
            public bool AllowOldStyleComments = true;
            public bool AllowSingleQuotedStrings = true;
        }

        public static Compilation<T, Func<T[], T>> Create<T>(Options options = null)
        {
            return new Compilation<T, Func<T[], T>>(options ?? Options.Default);
        }

        public static Compilation<T, R> Create<T, R>(Options options = null) where R: class
        {
            return new Compilation<T, R>(options ?? Options.Default);
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

        Compilation.Options options;

        internal Compilation(Compilation.Options o)
        {
            options = o;
        }

        public CompilationResult Compile(string source)
        {
            Binder<T, R> binder = Binder.Create<T, R>();
            var ast = binder.Bind(Parse(source));
            return new CompilationResult(binder.Emit(ast), binder.ParamCount);
        }

        internal Syntax.Codeblock Parse(string source)
        {
            var lexer = new Lexer(source);
            lexer.AllowFourLetterAbbreviations = options.AllowFourLetterAbbreviations;
            lexer.AllowOldStyleComments = options.AllowOldStyleComments;
            lexer.AllowSingleQuotedStrings = options.AllowSingleQuotedStrings;
            var tokens = lexer.AllTokens();
            var parser = new Parser(tokens);
            return parser.ParseMacro();
        }
    }
}
