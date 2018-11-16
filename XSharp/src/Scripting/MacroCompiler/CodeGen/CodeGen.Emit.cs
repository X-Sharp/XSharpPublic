using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;
using System.Reflection.Emit;
using System.Diagnostics;

namespace XSharp.MacroCompiler
{
    static partial class CodeGen
    {
        internal static void EmitDefault(ILGenerator ilg, TypeSymbol t)
        {
            if (t.Type.IsValueType)
            {
                switch (t.NativeType)
                {
                    case NativeType.Boolean:
                    case NativeType.Byte:
                    case NativeType.Char:
                    case NativeType.Int16:
                    case NativeType.Int32:
                    case NativeType.SByte:
                    case NativeType.UInt16:
                    case NativeType.UInt32:
                        ilg.Emit(OpCodes.Ldc_I4_0);
                        break;
                    case NativeType.UInt64:
                    case NativeType.Int64:
                        ilg.Emit(OpCodes.Ldc_I4_0);
                        break;
                    case NativeType.Double:
                        ilg.Emit(OpCodes.Ldc_R4, 0);
                        break;
                    case NativeType.Single:
                        ilg.Emit(OpCodes.Ldc_R4,0);
                        break;
                    case NativeType.Decimal:
                        ilg.Emit(OpCodes.Ldsfld, (FieldInfo)Compilation.GetMember(WellKnownMembers.System_Decimal_Zero).Member);
                        break;
                    default:
                        {
                            var l = ilg.DeclareLocal(t.Type);
                            ilg.Emit(l.LocalIndex < 256 ? OpCodes.Ldloca_S : OpCodes.Ldloca, l);
                            ilg.Emit(OpCodes.Initobj, t.Type);
                            ilg.Emit(l.LocalIndex < 256 ? OpCodes.Ldloc_S : OpCodes.Ldloc, l);
                        }
                        break;
                }
            }
            else
                ilg.Emit(OpCodes.Ldnull);

        }

        internal static void EmitLiteral(ILGenerator ilg, Constant c)
        {
            switch (c.Type.NativeType)
            {
                case NativeType.Boolean:
                    if (c.Boolean == true)
                        ilg.Emit(OpCodes.Ldc_I4_M1);
                    else
                        ilg.Emit(OpCodes.Ldc_I4_0);
                    break;
                case NativeType.SByte:
                case NativeType.Int16:
                case NativeType.Int32:
                    {
                        int? ordinal = c.Int;
                        switch (ordinal)
                        {
                            case -1:
                                ilg.Emit(OpCodes.Ldc_I4_M1);
                                break;
                            case 0:
                                ilg.Emit(OpCodes.Ldc_I4_0);
                                break;
                            case 1:
                                ilg.Emit(OpCodes.Ldc_I4_1);
                                break;
                            case 2:
                                ilg.Emit(OpCodes.Ldc_I4_2);
                                break;
                            case 3:
                                ilg.Emit(OpCodes.Ldc_I4_3);
                                break;
                            case 4:
                                ilg.Emit(OpCodes.Ldc_I4_4);
                                break;
                            case 5:
                                ilg.Emit(OpCodes.Ldc_I4_5);
                                break;
                            case 6:
                                ilg.Emit(OpCodes.Ldc_I4_6);
                                break;
                            case 7:
                                ilg.Emit(OpCodes.Ldc_I4_7);
                                break;
                            case 8:
                                ilg.Emit(OpCodes.Ldc_I4_8);
                                break;
                            default:
                                ilg.Emit(OpCodes.Ldc_I4, ordinal.Value);
                                break;
                        }
                    }
                    break;
                case NativeType.Byte:
                case NativeType.Char:
                case NativeType.UInt16:
                case NativeType.UInt32:
                    {
                        uint? ordinal = c.UInt;
                        switch (ordinal)
                        {
                            case 0:
                                ilg.Emit(OpCodes.Ldc_I4_0);
                                break;
                            case 1:
                                ilg.Emit(OpCodes.Ldc_I4_1);
                                break;
                            case 2:
                                ilg.Emit(OpCodes.Ldc_I4_2);
                                break;
                            case 3:
                                ilg.Emit(OpCodes.Ldc_I4_3);
                                break;
                            case 4:
                                ilg.Emit(OpCodes.Ldc_I4_4);
                                break;
                            case 5:
                                ilg.Emit(OpCodes.Ldc_I4_5);
                                break;
                            case 6:
                                ilg.Emit(OpCodes.Ldc_I4_6);
                                break;
                            case 7:
                                ilg.Emit(OpCodes.Ldc_I4_7);
                                break;
                            case 8:
                                ilg.Emit(OpCodes.Ldc_I4_8);
                                break;
                            default:
                                ilg.Emit(OpCodes.Ldc_I4, ordinal.Value);
                                break;
                        }
                    }
                    break;
                case NativeType.UInt64:
                    ilg.Emit(OpCodes.Ldc_I8, c.ULong.Value);
                    break;
                case NativeType.Int64:
                    ilg.Emit(OpCodes.Ldc_I8, c.Long.Value);
                    break;
                case NativeType.Single:
                    ilg.Emit(OpCodes.Ldc_R4, c.Float.Value);
                    break;
                case NativeType.Double:
                    ilg.Emit(OpCodes.Ldc_R8, c.Double.Value);
                    break;
                case NativeType.String:
                    ilg.Emit(OpCodes.Ldstr, c.String);
                    break;
                case NativeType.DateTime:
                case NativeType.Decimal:
                    // TODO nvk
                    break;
                case NativeType.Object:
                    ilg.Emit(OpCodes.Ldnull);
                    break;
                default:
                    throw new Exception("Unexpected literal kind");
            }
        }

        internal static void EmitConstant_1(ILGenerator ilg, NativeType t)
        {
            switch (t)
            {
                case NativeType.Byte:
                case NativeType.Char:
                case NativeType.Int16:
                case NativeType.Int32:
                case NativeType.SByte:
                case NativeType.UInt16:
                case NativeType.UInt32:
                    ilg.Emit(OpCodes.Ldc_I4_1);
                    break;
                case NativeType.UInt64:
                case NativeType.Int64:
                    ilg.Emit(OpCodes.Ldc_I8, (long)1);
                    break;
                case NativeType.Single:
                    ilg.Emit(OpCodes.Ldc_R4, (float)1);
                    break;
                case NativeType.Double:
                    ilg.Emit(OpCodes.Ldc_R8, (double)1);
                    break;
                case NativeType.String:
                    throw new Exception("Unsupported");
                case NativeType.DateTime:
                    throw new Exception("Unsupported");
                case NativeType.Decimal:
                    ilg.Emit(OpCodes.Ldc_I4_1);
                    ilg.Emit(OpCodes.Newobj, typeof(decimal).GetConstructor(new[] { typeof(int) })); // TODO use Compilation.GetMember
                    break;
                case NativeType.Object:
                    throw new Exception("Unsupported");
                default:
                    throw new Exception("Unsupported");
            }
        }

        internal static void EmitNumericConversion(ILGenerator ilg, NativeType fromType, NativeType toType, bool @checked)
        {
            bool fromUnsigned = fromType.IsUnsigned();

            switch (toType)
            {
                case NativeType.SByte:
                    switch (fromType)
                    {
                        case NativeType.SByte:
                            break; // NOP
                        default:
                            if (@checked)
                                ilg.Emit(fromUnsigned ? OpCodes.Conv_Ovf_I1_Un : OpCodes.Conv_Ovf_I1);
                            else
                                ilg.Emit(OpCodes.Conv_I1);
                            break;
                    }
                    break;

                case NativeType.Byte:
                    switch (fromType)
                    {
                        case NativeType.Byte:
                            break; // NOP
                        default:
                            if (@checked)
                                ilg.Emit(fromUnsigned ? OpCodes.Conv_Ovf_U1_Un : OpCodes.Conv_Ovf_U1);
                            else
                                ilg.Emit(OpCodes.Conv_U1);
                            break;
                    }
                    break;

                case NativeType.Int16:
                    switch (fromType)
                    {
                        case NativeType.SByte:
                        case NativeType.Byte:
                        case NativeType.Int16:
                            break; // NOP
                        default:
                            if (@checked)
                                ilg.Emit(fromUnsigned ? OpCodes.Conv_Ovf_I2_Un : OpCodes.Conv_Ovf_I2);
                            else
                                ilg.Emit(OpCodes.Conv_I2);
                            break;
                    }
                    break;

                case NativeType.Char:
                case NativeType.UInt16:
                    switch (fromType)
                    {
                        case NativeType.Byte:
                        case NativeType.UInt16:
                        case NativeType.Char:
                            break; // NOP
                        default:
                            if (@checked)
                                ilg.Emit(fromUnsigned ? OpCodes.Conv_Ovf_U2_Un : OpCodes.Conv_Ovf_U2);
                            else
                                ilg.Emit(OpCodes.Conv_U2);
                            break;
                    }
                    break;

                case NativeType.Int32:
                    switch (fromType)
                    {
                        case NativeType.SByte:
                        case NativeType.Byte:
                        case NativeType.Int16:
                        case NativeType.UInt16:
                        case NativeType.Int32:
                        case NativeType.Char:
                            break; // NOP
                        case NativeType.UInt32:
                            if (@checked)
                                ilg.Emit(OpCodes.Conv_Ovf_I4_Un);
                            break; // NOP in unchecked
                        default:
                            if (@checked)
                                ilg.Emit(fromUnsigned ? OpCodes.Conv_Ovf_I4_Un : OpCodes.Conv_Ovf_I4);
                            else
                                ilg.Emit(OpCodes.Conv_I4);
                            break;
                    }
                    break;

                case NativeType.UInt32:
                    switch (fromType)
                    {
                        case NativeType.Byte:
                        case NativeType.UInt16:
                        case NativeType.UInt32:
                        case NativeType.Char:
                            break; // NOP
                        case NativeType.SByte:
                        case NativeType.Int16:
                        case NativeType.Int32:
                            if (@checked)
                                ilg.Emit(OpCodes.Conv_Ovf_U4);
                            break; // NOP in unchecked
                        default:
                            if (@checked)
                                ilg.Emit(fromUnsigned ? OpCodes.Conv_Ovf_U4_Un : OpCodes.Conv_Ovf_U4);
                            else
                                ilg.Emit(OpCodes.Conv_U4);
                            break;
                    }
                    break;

                case NativeType.IntPtr:
                    switch (fromType)
                    {
                        case NativeType.IntPtr:
                            break; // NOP
                        case NativeType.SByte:
                        case NativeType.Int16:
                        case NativeType.Int32:
                            ilg.Emit(OpCodes.Conv_I); // potentially widening, so not NOP
                            break;
                        case NativeType.Byte:
                        case NativeType.UInt16:
                        case NativeType.Char:
                            // Doesn't actually matter whether we sign extend, because
                            // bit 32 can't be set in any of these types.
                            ilg.Emit(OpCodes.Conv_U); // potentially widening, so not NOP
                            break;
                        case NativeType.UInt32:
                            if (@checked)
                                ilg.Emit(OpCodes.Conv_Ovf_I_Un);
                            else
                                // Don't want to sign extend if this is a widening conversion.
                                ilg.Emit(OpCodes.Conv_U); // potentially widening, so not NOP
                            break;
                        default:
                            if (@checked)
                                ilg.Emit(fromUnsigned ? OpCodes.Conv_Ovf_I_Un : OpCodes.Conv_Ovf_I);
                            else
                                ilg.Emit(OpCodes.Conv_I);
                            break;
                    }
                    break;

                case NativeType.UIntPtr:
                    switch (fromType)
                    {
                        case NativeType.UIntPtr:
                            break; // NOP
                        case NativeType.Byte:
                        case NativeType.UInt16:
                        case NativeType.UInt32:
                        case NativeType.Char:
                            ilg.Emit(OpCodes.Conv_U); // potentially widening, so not NOP
                            break;
                        case NativeType.SByte:
                        case NativeType.Int16:
                        case NativeType.Int32:
                            if (@checked)
                                ilg.Emit(OpCodes.Conv_Ovf_U);
                            else
                                ilg.Emit(OpCodes.Conv_I); // potentially widening, so not NOP
                            break;
                        default:
                            if (@checked)
                                ilg.Emit(fromUnsigned ? OpCodes.Conv_Ovf_U_Un : OpCodes.Conv_Ovf_U);
                            else
                                ilg.Emit(OpCodes.Conv_U);
                            break;
                    }
                    break;

                case NativeType.Int64:
                    switch (fromType)
                    {
                        case NativeType.Int64:
                            break; //NOP
                        case NativeType.SByte:
                        case NativeType.Int16:
                        case NativeType.Int32:
                        case NativeType.IntPtr:
                            ilg.Emit(OpCodes.Conv_I8); // sign extend
                            break;
                        case NativeType.Byte:
                        case NativeType.UInt16:
                        case NativeType.UInt32:
                        case NativeType.Char:
                            ilg.Emit(OpCodes.Conv_U8); // 0 extend
                            break;
                        //case NativeType.Pointer:
                        case NativeType.UIntPtr:
                            if (@checked)
                                ilg.Emit(OpCodes.Conv_Ovf_I8_Un);
                            else
                                ilg.Emit(OpCodes.Conv_U8); // 0 extend if unchecked
                            break;
                        case NativeType.UInt64:
                            if (@checked)
                                ilg.Emit(OpCodes.Conv_Ovf_I8_Un);
                            break; // NOP in unchecked
                        default:
                            Debug.Assert(fromType.IsFloatingPoint());
                            if (@checked)
                                ilg.Emit(OpCodes.Conv_Ovf_I8);
                            else
                                ilg.Emit(OpCodes.Conv_I8);
                            break;
                    }
                    break;

                case NativeType.UInt64:
                    switch (fromType)
                    {
                        case NativeType.UInt64:
                            break; //NOP
                        case NativeType.Byte:
                        case NativeType.UInt16:
                        case NativeType.UInt32:
                        //case NativeType.Pointer:
                        case NativeType.UIntPtr:
                        case NativeType.Char:
                            ilg.Emit(OpCodes.Conv_U8); // 0 extend
                            break;
                        case NativeType.SByte:
                        case NativeType.Int16:
                        case NativeType.Int32:
                        case NativeType.IntPtr:
                            if (@checked)
                                ilg.Emit(OpCodes.Conv_Ovf_U8);
                            else
                                ilg.Emit(OpCodes.Conv_I8); // sign extend if unchecked
                            break;
                        case NativeType.Int64:
                            if (@checked)
                                ilg.Emit(OpCodes.Conv_Ovf_U8);
                            break; // NOP in unchecked
                        default:
                            Debug.Assert(fromType.IsFloatingPoint());
                            if (@checked)
                                ilg.Emit(OpCodes.Conv_Ovf_U8);
                            else
                                ilg.Emit(OpCodes.Conv_U8);
                            break;
                    }
                    break;

                case NativeType.Single:
                    switch (fromType)
                    {
                        case NativeType.UInt32:
                        case NativeType.UInt64:
                            ilg.Emit(OpCodes.Conv_R_Un);
                            break;
                    }
                    ilg.Emit(OpCodes.Conv_R4);
                    break;

                case NativeType.Double:
                    switch (fromType)
                    {
                        case NativeType.UInt32:
                        case NativeType.UInt64:
                            ilg.Emit(OpCodes.Conv_R_Un);
                            break;
                    }
                    ilg.Emit(OpCodes.Conv_R8);
                    break;

 /*               case NativeType.Pointer:
                    if (@checked)
                    {
                        switch (fromPredefTypeKind)
                        {
                            case NativeType.Byte:
                            case NativeType.UInt16:
                            case NativeType.UInt32:
                                ilg.Emit(OpCodes.Conv_U);
                                break;
                            case NativeType.UInt64:
                                ilg.Emit(OpCodes.Conv_Ovf_U_Un);
                                break;
                            case NativeType.SByte:
                            case NativeType.Int16:
                            case NativeType.Int32:
                            case NativeType.Int64:
                                ilg.Emit(OpCodes.Conv_Ovf_U);
                                break;
                            default:
                                throw ExceptionUtilities.UnexpectedValue(fromPredefTypeKind);
                        }
                    }
                    else
                    {
                        switch (fromPredefTypeKind)
                        {
                            case NativeType.Byte:
                            case NativeType.UInt16:
                            case NativeType.UInt32:
                            case NativeType.UInt64:
                            case NativeType.Int64:
                                ilg.Emit(OpCodes.Conv_U);
                                break;
                            case NativeType.SByte:
                            case NativeType.Int16:
                            case NativeType.Int32:
                                // This matches dev10.  Presumably, we're using conv_i,
                                // rather than conv_u, to sign-extend the value.
                                ilg.Emit(OpCodes.Conv_I);
                                break;
                            default:
                                throw ExceptionUtilities.UnexpectedValue(fromPredefTypeKind);
                        }
                    }
                    break;*/

                default:
                    throw new CompileFailure(ErrorCode.UnexpectedValue);
            }
        }

        internal static void EmitBinaryOperator(ILGenerator ilg, BinaryOperatorSymbol op, TypeSymbol type)
        {
            switch (op.Kind)
            {
                case BinaryOperatorKind.Concat:
                    ilg.Emit(OpCodes.Call, (Compilation.GetMember(WellKnownMembers.System_String_Concat) as MethodSymbol).Method);
                    break;
                case BinaryOperatorKind.Addition:
                    ilg.Emit(OpCodes.Add);
                    break;
                case BinaryOperatorKind.Subtraction:
                    ilg.Emit(OpCodes.Sub);
                    break;
                case BinaryOperatorKind.Multiplication:
                    ilg.Emit(OpCodes.Mul);
                    break;
                case BinaryOperatorKind.Division:
                    if (type.NativeType.IsUnsigned())
                        ilg.Emit(OpCodes.Div_Un);
                    else
                        ilg.Emit(OpCodes.Div);
                    break;
                case BinaryOperatorKind.Remainder:
                    if (type.NativeType.IsUnsigned())
                        ilg.Emit(OpCodes.Rem_Un);
                    else
                        ilg.Emit(OpCodes.Rem);
                    break;
                case BinaryOperatorKind.LeftShift:
                    ilg.Emit(OpCodes.Shl);
                    break;
                case BinaryOperatorKind.RightShift:
                    if (type.NativeType.IsUnsigned())
                        ilg.Emit(OpCodes.Shr_Un);
                    else
                        ilg.Emit(OpCodes.Shr);
                    break;
                case BinaryOperatorKind.And:
                    ilg.Emit(OpCodes.And);
                    break;
                case BinaryOperatorKind.Xor:
                    ilg.Emit(OpCodes.Xor);
                    break;
                case BinaryOperatorKind.Or:
                    ilg.Emit(OpCodes.Or);
                    break;
                case BinaryOperatorKind.GreaterThanAny:
                    if (type.NativeType.IsUnsigned())
                        ilg.Emit(OpCodes.Cgt_Un);
                    else
                        ilg.Emit(OpCodes.Cgt);
                    break;
                case BinaryOperatorKind.LessThanAny:
                    if (type.NativeType.IsUnsigned())
                        ilg.Emit(OpCodes.Clt_Un);
                    else
                        ilg.Emit(OpCodes.Clt);
                    break;
                case BinaryOperatorKind.GreaterThanOrEqualAny:
                    if (type.NativeType.IsUnsigned())
                        ilg.Emit(OpCodes.Clt_Un);
                    else
                        ilg.Emit(OpCodes.Clt);
                    ilg.Emit(OpCodes.Ldc_I4_0);
                    ilg.Emit(OpCodes.Ceq);
                    break;
                case BinaryOperatorKind.LessThanOrEqualAny:
                    if (type.NativeType.IsUnsigned())
                        ilg.Emit(OpCodes.Cgt_Un);
                    else
                        ilg.Emit(OpCodes.Cgt);
                    ilg.Emit(OpCodes.Ldc_I4_0);
                    ilg.Emit(OpCodes.Ceq);
                    break;
                case BinaryOperatorKind.ExactEqualAny:
                case BinaryOperatorKind.EqualAny:
                    ilg.Emit(OpCodes.Ceq);
                    break;
                case BinaryOperatorKind.NotEqualAny:
                    ilg.Emit(OpCodes.Ceq);
                    ilg.Emit(OpCodes.Ldc_I4_0);
                    ilg.Emit(OpCodes.Ceq);
                    break;
                case BinaryOperatorKind.EqString:
                case BinaryOperatorKind.EeqString:
                    ilg.Emit(OpCodes.Call, (Compilation.GetMember(WellKnownMembers.System_String_Equals) as MethodSymbol).Method);
                    break;
                case BinaryOperatorKind.NeqString:
                    ilg.Emit(OpCodes.Call, (Compilation.GetMember(WellKnownMembers.System_String_Equals) as MethodSymbol).Method);
                    ilg.Emit(OpCodes.Ldc_I4_0);
                    ilg.Emit(OpCodes.Ceq);
                    break;
                case BinaryOperatorKind.EqObject:
                case BinaryOperatorKind.EqStringObject:
                case BinaryOperatorKind.EqObjectString:
                case BinaryOperatorKind.EeqObject:
                case BinaryOperatorKind.EeqStringObject:
                case BinaryOperatorKind.EeqObjectString:
                    ilg.Emit(OpCodes.Call, (Compilation.GetMember(WellKnownMembers.System_Object_Equals) as MethodSymbol).Method);
                    break;
                case BinaryOperatorKind.NeqObject:
                case BinaryOperatorKind.NeqStringObject:
                case BinaryOperatorKind.NeqObjectString:
                    ilg.Emit(OpCodes.Call, (Compilation.GetMember(WellKnownMembers.System_Object_Equals) as MethodSymbol).Method);
                    ilg.Emit(OpCodes.Ldc_I4_0);
                    ilg.Emit(OpCodes.Ceq);
                    break;
                case BinaryOperatorKind.Exponent:
                case BinaryOperatorKind.Substr:
                case BinaryOperatorKind.DefaultValue:
                default:
                    throw new NotImplementedException();
            }
        }

        internal static void EmitCheckedBinaryOperator(ILGenerator ilg, BinaryOperatorSymbol op, TypeSymbol type)
        {
            switch (op.Kind)
            {
                case BinaryOperatorKind.Addition:
                    if (type.NativeType.IsUnsigned())
                        ilg.Emit(OpCodes.Add_Ovf_Un);
                    else
                        ilg.Emit(OpCodes.Add_Ovf);
                    break;
                case BinaryOperatorKind.Subtraction:
                    if (type.NativeType.IsUnsigned())
                        ilg.Emit(OpCodes.Sub_Ovf_Un);
                    else
                        ilg.Emit(OpCodes.Sub_Ovf);
                    break;
                case BinaryOperatorKind.Multiplication:
                    if (type.NativeType.IsUnsigned())
                        ilg.Emit(OpCodes.Mul_Ovf_Un);
                    else
                        ilg.Emit(OpCodes.Mul_Ovf);
                    break;
                default:
                    EmitBinaryOperator(ilg, op, type);
                    break;
            }
        }

        internal static void EmitUnaryOperator(ILGenerator ilg, UnaryOperatorSymbol op, TypeSymbol type)
        {
            switch (op.Kind)
            {
                case UnaryOperatorKind.Increment:
                    EmitConstant_1(ilg, type.NativeType);
                    ilg.Emit(OpCodes.Add);
                    break;
                case UnaryOperatorKind.Decrement:
                    EmitConstant_1(ilg, type.NativeType);
                    ilg.Emit(OpCodes.Sub);
                    break;
                case UnaryOperatorKind.UnaryPlus:
                    break;
                case UnaryOperatorKind.UnaryMinus:
                    ilg.Emit(OpCodes.Neg);
                    break;
                case UnaryOperatorKind.LogicalNegation:
                    ilg.Emit(OpCodes.Not);
                    break;
                case UnaryOperatorKind.BitwiseComplement:
                    ilg.Emit(OpCodes.Not);
                    break;
                case UnaryOperatorKind.True:
                    break;
                case UnaryOperatorKind.False:
                    break;
                default:
                    throw new CompileFailure(ErrorCode.UnexpectedValue);
            }
        }
    }
}