using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

namespace XSharp.MacroCompiler
{
    using static CodeGen;
    using Syntax;

    internal partial class UnaryOperatorSymbol : TypedSymbol
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            EmitUnaryOperator(ilg, this, OpType.TypeSymbol()); // TODO nkok: handle checked/unchecked
        }
    }

    internal partial class UnaryOperatorSymbolWithMethod : UnaryOperatorSymbol
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            ilg.Emit(OpCodes.Call, Method.Method);
        }
    }

    internal partial class UnaryOperatorSymbolWithType : UnaryOperatorSymbol
    {
    }
}
