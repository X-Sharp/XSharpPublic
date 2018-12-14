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

    internal partial class BinaryOperatorSymbol : TypedSymbol
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            EmitBinaryOperator(ilg, this, Type); // TODO nkok: handle checked/unchecked
        }
    }

    internal partial class BinaryOperatorSymbolWithMethod : BinaryOperatorSymbol
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            ilg.Emit(OpCodes.Call, this.Method.Method);
        }
    }

    internal partial class BinaryComparisonSymbolWithMethod : BinaryOperatorSymbolWithMethod
    {
        internal override void EmitGet(ILGenerator ilg)
        {
            base.EmitGet(ilg);
            EmitConstant_I4(ilg, 0);
            EmitBinaryOperator(ilg, this, Compilation.Get(NativeType.Int32));
        }
    }
}
