using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;
using System.Reflection.Emit;

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
                case NativeType.Byte:
                case NativeType.Char:
                case NativeType.Int16:
                case NativeType.Int32:
                case NativeType.SByte:
                case NativeType.UInt16:
                case NativeType.UInt32:
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
                    break;
                case NativeType.UInt64:
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
    }
}