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
    using static XSharp.RT.Functions;
    public delegate __Usual MacroCodeblockDelegate(params __Usual[] args);

    class DummyCodeblock : ICodeblock
    {
        int _pcount;
        public DummyCodeblock(int pCount) { _pcount = pCount; }
        public object EvalBlock(params object[] args) => null;
        public int PCount() => _pcount;

    }

    public class MacroCodeblock : _Codeblock
    {
        MacroCodeblockDelegate _eval;
        public override __Usual Eval(params __Usual[] args) => _eval(args);
        public MacroCodeblock(MacroCodeblockDelegate evalMethod, int pCount, string source, bool isBlock) : base(new DummyCodeblock(pCount), source, isBlock, false)
        {
            _eval = evalMethod;
        }
    }

    public class MacroMemVarCodeblock : MacroCodeblock
    {
        public override __Usual Eval(params __Usual[] args)
        {
            int nLevel = __MemVarInit();
            try
            {
                return base.Eval();
            }
            finally
            {
                __MemVarRelease(nLevel);
            }
        }
        public MacroMemVarCodeblock(MacroCodeblockDelegate evalMethod, int pCount, string source, bool isBlock) : base(evalMethod, pCount, source, isBlock) { }
    }
}
