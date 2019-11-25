using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using XSharp.MacroCompiler;

namespace XSharp.Runtime
    {
    public class MacroCompiler : IMacroCompiler2
    {
        private  MacroOptions options;
        internal Compilation<object, RuntimeCodeblockDelegate> compiler;

        public MacroCompilerResolveAmbiguousMatch Resolver
        {
            get
            {
                return options.Resolver;
            }
            set
            {
                options.Resolver = value;
            }
        }

        public MacroCompiler(): this(RuntimeState.Dialect == XSharpDialect.FoxPro ? MacroOptions.FoxPro : MacroOptions.VisualObjects) { }

        public MacroCompiler(MacroOptions o)
        {
            options = o;
            options.AllowSingleQuotedStrings = true;
            GetCompiler(true);
        }

        public MacroOptions Options => options;


        private Compilation<object, RuntimeCodeblockDelegate>  GetCompiler(bool lVo)
        {
            if (Options.AllowOldStyleComments != lVo || compiler == null)
            {
                options.AllowSingleQuotedStrings = lVo;
                compiler = Compilation.Create<object, RuntimeCodeblockDelegate>(options);
            }
            return compiler;
        }

        public ICodeblock Compile(string macro, bool lAllowSingleQuotes, Module module, out bool isCodeblock, out bool addsMemVars)

        {
            isCodeblock = macro.Replace(" ", "").StartsWith("{|");
            addsMemVars = false;
            Compilation<object, RuntimeCodeblockDelegate> compiler = GetCompiler(lAllowSingleQuotes);
            var m = compiler.Compile(macro);
            if (m.Diagnostic != null)
            {
                throw m.Diagnostic;
            }
            addsMemVars = m.CreatesAutoVars;
            return new RuntimeCodeblock(m.Macro, m.ParamCount);
        }

        public ICodeblock Compile(string macro)
        {
            Compilation<object, RuntimeCodeblockDelegate> compiler = GetCompiler(true);
            var m = compiler.Compile(macro);
            if (m.Diagnostic != null)
            {
                throw m.Diagnostic;
            }
            return new RuntimeCodeblock(m.Macro, m.ParamCount);
        }
    }
}
