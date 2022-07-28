using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using XSharp.MacroCompiler;

namespace XSharp.Runtime
{
    using ObjectCompilation = Compilation<Object, XSharp.MacroCompiler.ObjectMacro.MacroCodeblockDelegate>;
    using UsualCompilation = Compilation<__Usual, XSharp.MacroCompiler.UsualMacro.MacroCodeblockDelegate>;
    public class MacroCompiler : IMacroCompiler2, IMacroCompilerUsual
    {
        private MacroOptions options;
        private ObjectCompilation objectCompiler;
        private UsualCompilation usualCompiler;

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
            XSharp.RuntimeState.DialectChanged += DialectChanged;
        }

        private void DialectChanged(XSharpDialect oldDialect, XSharpDialect newDialect)
        {
            if (oldDialect != newDialect)
            {
                if (newDialect == XSharpDialect.FoxPro)
                {
                    options = MacroOptions.FoxPro;
                }
                else
                {
                    options = MacroOptions.VisualObjects;
                }
                objectCompiler = null;
                usualCompiler = null;
            }
        }

        public static MacroCompiler GetScriptCompiler(XSharpDialect dialect = XSharpDialect.Core)
        {
            MacroOptions options;
            switch (dialect)
            {
                case XSharpDialect.FoxPro:
                    options = MacroOptions.FoxPro;
                    break;
                case XSharpDialect.VO:
                    options = MacroOptions.VisualObjects;
                    break;
                default:
                    options = MacroOptions.Default;
                    break;
            }
            options.ParseMode = ParseMode.Statements;
            return new MacroCompiler(options);
        }
        public MacroOptions Options => options;


        internal ObjectCompilation GetObjectCompiler(bool lVo)
        {
            if (Options.AllowOldStyleComments != lVo || objectCompiler == null)
            {
                options.AllowSingleQuotedStrings = lVo;
                objectCompiler = ObjectCompilation.Create(options);
                usualCompiler = null;
            }
            return objectCompiler;
        }

        internal UsualCompilation GetUsualCompiler(bool lVo)
        {
            if (Options.AllowOldStyleComments != lVo || usualCompiler == null)
            {
                options.AllowSingleQuotedStrings = lVo;
                usualCompiler = UsualCompilation.Create(options);
                objectCompiler = null;
            }
            return usualCompiler;
        }

        public ICodeblock Compile(string macro, bool lAllowSingleQuotes, Module module, out bool isCodeblock, out bool addsMemVars)

        {
            isCodeblock = macro.Replace(" ", "").StartsWith("{|");
            addsMemVars = false;
            ObjectCompilation compiler = GetObjectCompiler(lAllowSingleQuotes);
            var m = compiler.Compile(macro);
            if (m.Diagnostic != null)
            {
                throw m.Diagnostic;
            }
            if (m.CreatesAutoVars)
            {
                addsMemVars = false;
                return new XSharp.MacroCompiler.ObjectMacro.MacroMemVarCodeblock(m.Macro, m.ParamCount);
            }
            else
                return new XSharp.MacroCompiler.ObjectMacro.MacroCodeblock(m.Macro, m.ParamCount);
        }

        public ICodeblock Compile(string macro)
        {
            ObjectCompilation compiler = GetObjectCompiler(true);
            var m = compiler.Compile(macro);
            if (m.Diagnostic != null)
            {
                throw m.Diagnostic;
            }
            if (m.CreatesAutoVars)
                return new XSharp.MacroCompiler.ObjectMacro.MacroMemVarCodeblock(m.Macro, m.ParamCount);
            else
                return new XSharp.MacroCompiler.ObjectMacro.MacroCodeblock(m.Macro, m.ParamCount);
        }

        public _Codeblock CompileCodeblock(string macro, bool lAllowSingleQuotes, Module module)
        {
            var isCodeblock = macro.Replace(" ", "").StartsWith("{|");
            UsualCompilation compiler = GetUsualCompiler(lAllowSingleQuotes);
            var m = compiler.Compile(macro);
            if (m.Diagnostic != null)
            {
                throw m.Diagnostic;
            }
            if (m.CreatesAutoVars)
                return new XSharp.MacroCompiler.UsualMacro.MacroMemVarCodeblock(m.Macro, m.ParamCount, macro, isCodeblock);
            else
                return new XSharp.MacroCompiler.UsualMacro.MacroCodeblock(m.Macro, m.ParamCount, macro, isCodeblock);
        }

        public _Codeblock CompileCodeblock(string macro)
        {
            UsualCompilation compiler = GetUsualCompiler(true);
            var m = compiler.Compile(macro);
            if (m.Diagnostic != null)
            {
                throw m.Diagnostic;
            }
            if (m.CreatesAutoVars)
                return new XSharp.MacroCompiler.UsualMacro.MacroMemVarCodeblock(m.Macro, m.ParamCount, macro, macro.Replace(" ", "").StartsWith("{|"));
            else
                return new XSharp.MacroCompiler.UsualMacro.MacroCodeblock(m.Macro, m.ParamCount, macro, macro.Replace(" ", "").StartsWith("{|"));
        }
    }
}
