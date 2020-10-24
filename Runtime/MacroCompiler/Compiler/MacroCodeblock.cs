using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler.ObjectMacro
{
    public delegate object MacroCodeblockDelegate(params object[] args);

    public class MacroCodeblock : ICodeblock
    {
        MacroCodeblockDelegate _eval;
        int _pcount;
        public MacroCodeblock(MacroCodeblockDelegate evalMethod, int pCount)
        {
            _eval = evalMethod;
            _pcount = pCount;
        }

        public object EvalBlock(params object[] args)
        {
            return _eval(args);
        }
        public int PCount() => _pcount;
    }
}

namespace XSharp.MacroCompiler.UsualMacro
{
    public delegate __Usual MacroCodeblockDelegate(params __Usual[] args);

    public class MacroCodeblock : Codeblock
    {
        MacroCodeblockDelegate _eval;
        string _source;
        public override __Usual Eval(params __Usual[] args) => _eval(args);
        public MacroCodeblock(MacroCodeblockDelegate evalMethod, int pCount) : this(evalMethod, pCount, null) { }
        public MacroCodeblock(MacroCodeblockDelegate evalMethod, int pCount, string source) : base(pCount)
        {
            _eval = evalMethod;
            _source = source;
        }
        public override string ToString() => _source;
    }

    public class MacroMemVarCodeblock : MacroCodeblock
    {
        public override __Usual Eval(params __Usual[] args)
        {
            int nLevel = XSharp.RT.Functions.__MemVarInit();
            try
            {
                return base.Eval();
            }
            finally
            {
                XSharp.RT.Functions.__MemVarRelease(nLevel);
            }
        }
        public MacroMemVarCodeblock(MacroCodeblockDelegate evalMethod, int pCount, string source) : base(evalMethod, pCount, source) { }
    }
}
