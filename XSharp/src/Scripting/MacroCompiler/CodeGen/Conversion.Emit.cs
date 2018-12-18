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
            if (expr != null) expr.Emit(ilg);
            switch (Kind)
            {
                case ConversionKind.ImplicitNumeric:
                case ConversionKind.ExplicitNumeric:
                    EmitNumericConversion(ilg, expr.Datatype.NativeType, type.NativeType, false);
                    break;
                case ConversionKind.Boxing:
                    ilg.Emit(OpCodes.Box, expr.Datatype.Type);
                    break;
                case ConversionKind.Unboxing:
                    ilg.Emit(OpCodes.Unbox_Any, type.Type);
                    break;
                case ConversionKind.ImplicitReference:
                    break;
                case ConversionKind.ExplicitReference:
                    ilg.Emit(OpCodes.Castclass, type.Type);
                    break;
                case ConversionKind.Deref:
                    EmitDereference(ilg, type);
                    break;
                case ConversionKind.ImplicitEnumeration:
                    break;
                case ConversionKind.ExplicitEnumeration:
                    break;
                case ConversionKind.NoConversion:
                case ConversionKind.NoImplicitConversion:
                default:
                    throw new InternalError();
            }
        }
    }

    internal partial class ConversionSymbolWithMethod : ConversionSymbol
    {
        internal override void Emit(Expr expr, TypeSymbol type, ILGenerator ilg)
        {
            if (expr != null) expr.Emit(ilg);
            ilg.Emit(OpCodes.Call, Method.Method);
        }
    }

    internal partial class ConversionChain : ConversionSymbol
    {
        internal override void Emit(Expr expr, TypeSymbol type, ILGenerator ilg)
        {
            Previous.Emit(expr, type, ilg);
            Conversion.Emit(null, type, ilg);
        }
    }

    internal partial class ConversionSymbolToConstant : ConversionSymbol
    {
    }

    internal partial class ConversionByRef : ConversionSymbol
    {
        internal override void Emit(Expr expr, TypeSymbol type, ILGenerator ilg)
        {
            if (expr != null)
                expr.EmitAddr(ilg);
            else
                throw new InternalError();
        }
    }
}
