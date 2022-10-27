#define DUMPTOKENS
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;
using XSharp.MacroCompiler.Syntax;

namespace XSharp.MacroCompiler
{
    public static partial class Compilation
    {    
        public static Compilation<T, Func<T[], T>> Create<T>(MacroOptions options = null)
        {
            if (options?.StrictTypedSignature == true)
                throw new InternalError("Invalid StrictTypedSignature option without delegate type");
            return new Compilation<T, Func<T[], T>>(options ?? MacroOptions.Default);
        }

        public static Compilation<T, R> Create<T, R>(MacroOptions options = null) where R: Delegate
        {
            return new Compilation<T, R>(options ?? MacroOptions.Default);
        }
    }

    public partial class Compilation<T>
    {
        public struct CompilationResult<R> where R : Delegate
        {
            public string Source;
            public R Macro;
            public byte[] AssemblyBytes;
            internal Binder<T> Binder;
            internal Syntax.Node SyntaxTree;
            public int ParamCount { get => Binder.ParamCount; }
            public bool CreatesAutoVars { get => Binder.CreatesAutoVars; }
            public CompilationError Diagnostic;
            internal CompilationResult(string source, R macro, Binder<T> binder)
            {
                Source = source;
                Macro = macro;
                SyntaxTree = null;
                Binder = binder;
                Diagnostic = null;
            }
            internal CompilationResult(string source, Syntax.Node syntaxTree, Binder<T> binder)
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

        internal List<Tuple<string, Type>> ExternLocals;
        internal List<Tuple<string, Type>> Parameters;
        internal Type ResultType;
        public void AddExternLocal(string name, Type type)
        {
            if (ExternLocals == null)
                ExternLocals = new List<Tuple<string, Type>>();
            ExternLocals.Add(new Tuple<string, Type>(name, type));
        }
        public void AddParameter(string name, Type type)
        {
            if (Parameters == null)
                Parameters = new List<Tuple<string, Type>>();
            Parameters.Add(new Tuple<string, Type>(name, type));
        }
        public void SetParamNames(params string[] paramNames)
        {
            Parameters = paramNames.Select(n => new Tuple<string, Type> (n, null)).ToList();
        }
        public void SetResultType(Type resultType)
        {
            ResultType = resultType;
        }

        internal string NameOfAssembly;
        internal string NameOfClass;
        internal string NameOfMethod;
        public void SetGeneratedNames(string nameOfMethod = null, string nameOfClass = null, string nameOfAssembly = null)
        {
            if (nameOfAssembly != null)
                NameOfAssembly = nameOfAssembly;
            if (nameOfClass != null)
                NameOfClass = nameOfClass;
            if (nameOfMethod != null)
                NameOfMethod = nameOfMethod;
        }

        internal void AddLocalsToBinder(Binder binder)
        {
            // Add params
            if (Parameters != null)
            {
                if (binder.ParameterTypes == null)
                    binder.ParameterTypes = Parameters.Select(p => Binder.FindType(p.Item2)).ToArray();
                int a = 0;
                foreach (var p in binder.ParameterTypes)
                {
                    if (a < Parameters.Count)
                        binder.AddParam(Parameters[a].Item1, p, a);
                    ++a;
                }
            }

            // Set result type
            if (ResultType != null && binder.ResultType == null)
            {
                binder.ResultType = Binder.FindType(ResultType);
            }

            // Add Locals
            if (ExternLocals != null)
                foreach (var l in ExternLocals)
                    binder.AddAutoLocal(l.Item1, Binder.FindType(l.Item2));

            // Set generated names
            if (binder is AssemblyBinder<T> b)
            {
                if (NameOfAssembly != null)
                    b.NameOfAssembly = NameOfAssembly;
                if (NameOfClass != null)
                    b.NameOfClass = NameOfClass;
                if (NameOfMethod != null)
                    b.NameOfMethod = NameOfMethod;
            }
        }

        public CompilationResult<R> Compile<R>(string source) where R: Delegate
        {
            try
            {
                var res = Bind<R>(source, Parse(source));
                EmitInternal(ref res);
                return res;
            }
            catch (CompilationError e)
            {
                if (e.Location.Line == 0)
                    e = new CompilationError(e, source);
                return new CompilationResult<R>(source, e);
            }
        }

        public CompilationResult<R> Bind<R>(string source) where R : Delegate
        {
            try
            {
                return Bind<R>(source, Parse(source));
            }
            catch (CompilationError e)
            {
                if (e.Location.Line == 0)
                    e = new CompilationError(e, source);
                return new CompilationResult<R>(source, e);
            }
        }

        public R Emit<R>(ref CompilationResult<R> macro) where R : Delegate
        {
            EmitInternal(ref macro);
            return macro.Macro;
        }

        public byte[] EmitAssembly<R>(ref CompilationResult<R> macro) where R : Delegate
        {
            return EmitAssemblyInternal(ref macro);
        }

        internal virtual Syntax.Node Parse(string source)
        {
            var lexer = new Lexer(source, options);
            IList<Token> tokens = lexer.AllTokens();
            if (options.PreProcessor && options.ParseStatements)
            {
                var pp = new Preprocessor.XSharpPreprocessor(lexer, options, "Macro", Encoding.Default);
                tokens = pp.PreProcess();
            }
            var parser = new Parser(tokens, options);
            return options.ParseStatements ? parser.ParseScript() : parser.ParseMacro();
        }
        internal virtual CompilationResult<R> Bind<R>(string source, Syntax.Node parseTree) where R : Delegate
        {
            try
            {
                Binder<T> binder = CreateBinder<R>();
                var ast = options.StrictTypedSignature
                    ? TypedCodeblock.Bound(parseTree as Syntax.Codeblock, binder)
                    : binder.Bind(parseTree);
                return new CompilationResult<R>(source, ast, binder);
            }
            catch (CompilationError e)
            {
                if (e.Location.Line == 0)
                    e = new CompilationError(e, source);
                return new CompilationResult<R>(source, e);
            }
        }
        internal virtual void EmitInternal<R>(ref CompilationResult<R> macro) where R : Delegate
        {
            if (macro.Macro == null && macro.SyntaxTree != null)
            {
                macro.Binder.GenerateMethod(macro.Source);
                macro.Binder.DeclareAutoLocals();
                macro.Macro = macro.Binder.Emit(macro.SyntaxTree) as R;
            }
        }
        internal virtual byte[] EmitAssemblyInternal<R>(ref CompilationResult<R> macro) where R : Delegate
        {
            if (macro.AssemblyBytes == null && macro.SyntaxTree != null)
            {
                macro.Binder.GenerateMethod(macro.Source);
                macro.Binder.DeclareAutoLocals();
                macro.AssemblyBytes = macro.Binder.EmitAssembly(macro.SyntaxTree);
            }
            return macro.AssemblyBytes;
        }
        internal virtual Binder<T> CreateBinder<R>() where R : Delegate
        {
            var binder = Binder.Create<T, R>(options);
            AddLocalsToBinder(binder);
            return binder;
        }
    }

    public partial class Compilation<T,R> : Compilation<T> where R: Delegate
    {
        internal Compilation(MacroOptions o = null) : base(o) { }

        public static Compilation<T, R> Create(MacroOptions options = null) => Compilation.Create<T, R>(options);

        public CompilationResult<R> Compile(string source) => Compile<R>(source);
        public CompilationResult<R> Bind(string source) => Bind<R>(source);
        public R Emit(ref CompilationResult<R> macro) => Emit<R>(ref macro);
        public byte[] EmitAssembly(ref CompilationResult<R> macro) => EmitAssembly<R>(ref macro);
    }
}
