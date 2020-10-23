using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using XSharp.MacroCompiler;

namespace XSharp.Runtime
{
    public class MacroCompiler : XSharp.MacroCompiler.ObjectMacro.MacroCompiler
    {
        public MacroCompiler() : base() { }

        public MacroCompiler(MacroOptions o) : base(o) { }
    }
}

namespace XSharp.MacroCompiler.ObjectMacro
{
    using MacroParamType = Object;
    using XSharp.MacroCompiler.ObjectMacro;
    public class MacroCompiler : IMacroCompiler2
    {
        private MacroOptions options;
        internal Compilation<MacroParamType, MacroCodeblockDelegate> compiler;

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

        public MacroCompiler() : this(RuntimeState.Dialect == XSharpDialect.FoxPro ? MacroOptions.FoxPro : MacroOptions.VisualObjects) { }

        public MacroCompiler(MacroOptions o)
        {
            options = o;
            options.AllowSingleQuotedStrings = true;
            GetCompiler(true);
        }

        public MacroOptions Options => options;


        private Compilation<MacroParamType, MacroCodeblockDelegate> GetCompiler(bool lVo)
        {
            if (Options.AllowOldStyleComments != lVo || compiler == null)
            {
                options.AllowSingleQuotedStrings = lVo;
                compiler = Compilation.Create<MacroParamType, MacroCodeblockDelegate>(options);
            }
            return compiler;
        }

        public ICodeblock Compile(string macro, bool lAllowSingleQuotes, Module module, out bool isCodeblock, out bool addsMemVars)

        {
            isCodeblock = macro.Replace(" ", "").StartsWith("{|");
            addsMemVars = false;
            Compilation<MacroParamType, MacroCodeblockDelegate> compiler = GetCompiler(lAllowSingleQuotes);
            var m = compiler.Compile(macro);
            if (m.Diagnostic != null)
            {
                throw m.Diagnostic;
            }
            addsMemVars = m.CreatesAutoVars;
            return new MacroCodeblock(m.Macro, m.ParamCount);
        }

        public ICodeblock Compile(string macro)
        {
            Compilation<MacroParamType, MacroCodeblockDelegate> compiler = GetCompiler(true);
            var m = compiler.Compile(macro);
            if (m.Diagnostic != null)
            {
                throw m.Diagnostic;
            }
            return new MacroCodeblock(m.Macro, m.ParamCount);
        }
    }
}

namespace XSharp.MacroCompiler.UsualMacro
{
    using MacroParamType = __Usual;
    using XSharp.MacroCompiler.UsualMacro;
    public class MacroCompiler : IMacroCompiler2
    {
        private  MacroOptions options;
        internal Compilation<MacroParamType, MacroCodeblockDelegate> compiler;

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


        private Compilation<MacroParamType, MacroCodeblockDelegate>  GetCompiler(bool lVo)
        {
            if (Options.AllowOldStyleComments != lVo || compiler == null)
            {
                options.AllowSingleQuotedStrings = lVo;
                compiler = Compilation.Create<MacroParamType, MacroCodeblockDelegate>(options);
            }
            return compiler;
        }

        public ICodeblock Compile(string macro, bool lAllowSingleQuotes, Module module, out bool isCodeblock, out bool addsMemVars)

        {
            isCodeblock = macro.Replace(" ", "").StartsWith("{|");
            addsMemVars = false;
            Compilation<MacroParamType, MacroCodeblockDelegate> compiler = GetCompiler(lAllowSingleQuotes);
            var m = compiler.Compile(macro);
            if (m.Diagnostic != null)
            {
                throw m.Diagnostic;
            }
            addsMemVars = m.CreatesAutoVars;
            return new MacroCodeblock(m.Macro, m.ParamCount);
        }

        public ICodeblock Compile(string macro)
        {
            Compilation<MacroParamType, MacroCodeblockDelegate> compiler = GetCompiler(true);
            var m = compiler.Compile(macro);
            if (m.Diagnostic != null)
            {
                throw m.Diagnostic;
            }
            return new MacroCodeblock(m.Macro, m.ParamCount);
        }
    }
}
