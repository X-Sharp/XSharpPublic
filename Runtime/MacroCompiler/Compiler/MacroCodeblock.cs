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
        public override __Usual Eval(params __Usual[] args) => _eval(args);
        public MacroCodeblock(MacroCodeblockDelegate evalMethod, int pCount) : base(pCount)
        {
            _eval = evalMethod;
        }
    }
}
