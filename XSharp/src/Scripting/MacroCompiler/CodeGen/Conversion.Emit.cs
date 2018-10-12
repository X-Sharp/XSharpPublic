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

    internal partial class ConversionSymbol : Symbol
    {
        internal virtual void Emit(Expr expr, TypeSymbol type, ILGenerator ilg)
        {
            switch (Kind)
            {
                case ConversionKind.ImplicitNumeric:
                case ConversionKind.ExplicitNumeric:
                    EmitNumericConversion(ilg, expr.Datatype.NativeType, type.NativeType, false);
                    break;
                case ConversionKind.ImplicitUserDefined:
                case ConversionKind.ExplicitUserDefined:
                    ilg.Emit(OpCodes.Call, ((ConversionSymbolWithMethod)this).Method.Method);
                    break;
                case ConversionKind.Boxing:
                    ilg.Emit(OpCodes.Box, expr.Datatype.Type);
                    break;
                case ConversionKind.Unboxing:
                    ilg.Emit(OpCodes.Unbox, type.Type);
                    break;
                case ConversionKind.ImplicitReference:
                case ConversionKind.ExplicitReference:
                    break;
                case ConversionKind.NoConversion:
                    throw new CompileFailure(ErrorCode.NoConversion, expr.Datatype.Type, type.Type);
                default:
                    throw new NotImplementedException();
            }
        }
    }

    internal partial class ConversionChain : ConversionSymbol
    {
        internal override void Emit(Expr expr, TypeSymbol type, ILGenerator ilg)
        {
            Previous.Emit(expr, type, ilg);
            Conversion.Emit(expr, type, ilg);
        }
    }
}