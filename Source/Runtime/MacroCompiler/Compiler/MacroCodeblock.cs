using static XSharp.RT.Functions;

namespace XSharp.MacroCompiler.ObjectMacro
{
    internal delegate object MacroCodeblockDelegate(params object[] args);

    internal class MacroCodeblock : ICodeblock
    {
        MacroCodeblockDelegate _eval;
        int _pcount;
        internal MacroCodeblock(MacroCodeblockDelegate evalMethod, int pCount)
        {
            _eval = evalMethod;
            _pcount = pCount;
        }

        public virtual object EvalBlock(params object[] args)
        {
            return _eval(args);
        }
        public int PCount() => _pcount;
    }

    internal class MacroMemVarCodeblock : MacroCodeblock
    {
        public override object EvalBlock(params object[] args)
        {
            int nLevel = __MemVarInit(true);
            try
            {
                return base.EvalBlock(args);
            }
            finally
            {
                __MemVarRelease(nLevel);
            }
        }
        internal MacroMemVarCodeblock(MacroCodeblockDelegate evalMethod, int pCount) : base(evalMethod, pCount) { }
    }
}

namespace XSharp.MacroCompiler.UsualMacro
{
    internal delegate __Usual MacroCodeblockDelegate(params __Usual[] args);

    class NestedCodeblock : ICodeblock
    {
        int _pcount;
        public NestedCodeblock(int pCount) { _pcount = pCount; }
        public object EvalBlock(params object[] args) => null;
        public int PCount() => _pcount;

    }

    internal class MacroCodeblock : _Codeblock
    {
        MacroCodeblockDelegate _eval;
        public override __Usual Eval(params __Usual[] args) => _eval(args);
        internal MacroCodeblock(MacroCodeblockDelegate evalMethod, int pCount, string source, bool isBlock) : 
            base(new NestedCodeblock(pCount), source, isBlock, false)
        {
            _eval = evalMethod;
        }
    }
    /// <summary>
    /// This class holds a codeblock that creates memory variables. The Eval() method initializes
    /// the memory level and releases the allocated memory variables at the end.
    /// </summary>
    internal class MacroMemVarCodeblock : MacroCodeblock
    {
        public override __Usual Eval(params __Usual[] args)
        {
            int nLevel = __MemVarInit(true);
            try
            {
                return base.Eval(args);
            }
            finally
            {
                __MemVarRelease(nLevel);
            }
        }
        internal MacroMemVarCodeblock(MacroCodeblockDelegate evalMethod, int pCount, string source, bool isBlock) : base(evalMethod, pCount, source, isBlock) { }
    }
}
