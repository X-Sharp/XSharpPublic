using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;
using System.Reflection;

namespace XSharp.MacroCompiler
{
    using Syntax;

    internal enum OperandType
    {
        Error,
        Object,
        String,
        ObjectAndString,
        StringAndObject,
        Int,
        UInt,
        Long,
        ULong,
        Float,
        Double,
        Decimal,
        Bool,
        Char,
        SByte,
        Byte,
        Short,
        UShort,
        NullableInt,
        NullableUInt,
        NullableLong,
        NullableULong,
        NullableFloat,
        NullableDouble,
        NullableDecimal,
        NullableBool,
        NullableChar,
        NullableSByte,
        NullableByte,
        NullableShort,
        NullableUShort,
    }

    static internal class OperandTypeHelper
    {
        private static readonly bool[] s_nullable;

        internal const int NullableDelta = OperandType.NullableInt - OperandType.Int;

        internal static bool IsNullable(OperandType type) { return s_nullable[(int)type]; }

        static OperandTypeHelper()
        {
            var opTypes = (OperandType[])Enum.GetValues(typeof(OperandType));

            s_nullable = new bool[opTypes.Length];

            foreach (var c in opTypes)
            {
                if (c.ToString().StartsWith("Nullable"))
                    s_nullable[(int)c] = true;
            }

#if DEBUG
            Dictionary<string, OperandType> convNames = new Dictionary<string, OperandType>();
            foreach (var c in opTypes)
                convNames.Add(c.ToString(), c);
            foreach (var c in opTypes)
            {
                var ncs = "Nullable" + c.ToString();
                OperandType nc;
                if (convNames.TryGetValue(ncs, out nc))
                {
                    Debug.Assert(nc - c == NullableDelta);
                }
            }
#endif
        }

        static internal TypeSymbol TypeSymbol(this OperandType OpType)
        {
            switch (OpType)
            {
                case OperandType.Object:
                    return Compilation.Get(NativeType.Object);
                case OperandType.String:
                    return Compilation.Get(NativeType.String);
                case OperandType.ObjectAndString:
                    return Compilation.Get(NativeType.String);
                case OperandType.StringAndObject:
                    return Compilation.Get(NativeType.String);
                case OperandType.Int:
                    return Compilation.Get(NativeType.Int32);
                case OperandType.UInt:
                    return Compilation.Get(NativeType.UInt32);
                case OperandType.Long:
                    return Compilation.Get(NativeType.Int64);
                case OperandType.ULong:
                    return Compilation.Get(NativeType.UInt64);
                case OperandType.Float:
                    return Compilation.Get(NativeType.Single);
                case OperandType.Double:
                    return Compilation.Get(NativeType.Double);
                case OperandType.Decimal:
                    return Compilation.Get(NativeType.Decimal);
                case OperandType.Bool:
                    return Compilation.Get(NativeType.Boolean);
                case OperandType.NullableInt:
                    return Binder.NullableType(Compilation.Get(NativeType.Int32));
                case OperandType.NullableUInt:
                    return Binder.NullableType(Compilation.Get(NativeType.UInt32));
                case OperandType.NullableLong:
                    return Binder.NullableType(Compilation.Get(NativeType.Int64));
                case OperandType.NullableULong:
                    return Binder.NullableType(Compilation.Get(NativeType.UInt64));
                case OperandType.NullableFloat:
                    return Binder.NullableType(Compilation.Get(NativeType.Single));
                case OperandType.NullableDouble:
                    return Binder.NullableType(Compilation.Get(NativeType.Double));
                case OperandType.NullableDecimal:
                    return Binder.NullableType(Compilation.Get(NativeType.Decimal));
                case OperandType.NullableBool:
                    return Binder.NullableType(Compilation.Get(NativeType.Boolean));
                default:
                    return null;
            }
        }
    }
}