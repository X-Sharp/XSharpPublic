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
                case ConversionKind.Deref:
                    switch (type.NativeType)
                    {
                        case NativeType.Byte:
                            ilg.Emit(OpCodes.Ldind_I1);
                            break;
                        case NativeType.Int16:
                            ilg.Emit(OpCodes.Ldind_I2);
                            break;
                        case NativeType.Int32:
                            ilg.Emit(OpCodes.Ldind_I4);
                            break;
                        case NativeType.Boolean:
                        case NativeType.SByte:
                            ilg.Emit(OpCodes.Ldind_U1);
                            break;
                        case NativeType.Char:
                        case NativeType.UInt16:
                            ilg.Emit(OpCodes.Ldind_U2);
                            break;
                        case NativeType.UInt32:
                            ilg.Emit(OpCodes.Ldind_U4);
                            break;
                        case NativeType.UInt64:
                            ilg.Emit(OpCodes.Ldobj, type.Type);
                            break;
                        case NativeType.Int64:
                            ilg.Emit(OpCodes.Ldind_I8);
                            break;
                        case NativeType.Double:
                            ilg.Emit(OpCodes.Ldind_R4);
                            break;
                        case NativeType.Single:
                            ilg.Emit(OpCodes.Ldind_R4);
                            break;
                        default:
                            if (type.IsValueType)
                                ilg.Emit(OpCodes.Ldobj, type.Type);
                            else
                                ilg.Emit(OpCodes.Ldind_Ref);
                            break;
                    }
                    break;
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