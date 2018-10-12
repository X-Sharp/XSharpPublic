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

    internal partial class BinaryOperatorSymbol : Symbol
    {
        internal virtual void Emit(Expr expr, TypeSymbol type, ILGenerator ilg)
        {
            EmitBinaryOperator(ilg, this, type); // TODO nkok: handle checked/unchecked
        }
    }

    internal partial class BinaryOperatorSymbolWithMethod : BinaryOperatorSymbol
    {
        internal override void Emit(Expr expr, TypeSymbol type, ILGenerator ilg)
        {
            ilg.Emit(OpCodes.Call, this.Method.Method);
        }
    }
}