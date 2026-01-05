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
            var result = _eval(args);
            if (result is long lng)
            {
                // All operations using 2 Int32 values now return an Int64
                // We want to convert back to Int32 if possible
                if (lng >= int.MinValue && lng <= int.MaxValue)
                {
                    result = (int)lng;
                }
            }
            return result;
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
        public override __Usual Eval(params __Usual[] args)
        {
            var result = _eval(args);
            if (result.Value is long lng)
            {
                // All operations using 2 Int32 values now return an Int64
                // We want to convert back to Int32 if possible
                if (lng >= int.MinValue && lng <= int.MaxValue)
                {
                    result = new __Usual((int)lng);
                }
            }
            return result;
        }
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
