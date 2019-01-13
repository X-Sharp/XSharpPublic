using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using XSharp.MacroCompiler;

namespace XSharp.Runtime
    {
    public class MacroCompiler : IMacroCompiler
    {
        private  MacroOptions options;
        internal Compilation<object, RuntimeCodeblockDelegate> compiler;


        public MacroCompiler()
        {
            options = new MacroOptions() { AllowSingleQuotedStrings = true };
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

        public ICodeblock Compile(string macro, bool lAllowSingleQuotes, Module module, ref bool isCodeblock)

        {
            isCodeblock = macro.Replace(" ", "").StartsWith("{|");
            Compilation<object, RuntimeCodeblockDelegate> compiler = GetCompiler(lAllowSingleQuotes);
            var m = compiler.Compile(macro);
            if (m.Diagnostic != null)
            {
                throw m.Diagnostic;
            }
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
