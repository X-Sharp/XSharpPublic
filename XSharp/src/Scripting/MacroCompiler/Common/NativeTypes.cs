using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Reflection;
using System.Reflection.Emit;
using System.Diagnostics;

namespace XSharp.MacroCompiler
{
    internal enum NativeType
    {
        Unknown = 0,
        Object = 1,
        //DBNull = 2,
        Boolean = 3,
        Char = 4,
        SByte = 5,
        Byte = 6,
        Int16 = 7,
        UInt16 = 8,
        Int32 = 9,
        UInt32 = 10,
        Int64 = 11,
        UInt64 = 12,
        Single = 13,
        Double = 14,
        Decimal = 15,
        DateTime = 16,
        //
        String = 18,
        Void = 19,
        __Usual = 20,
    }

    public static partial class Compilation
    {
        static string[] NativeTypeNames =
        {
            null,
            "System.Object",
            null,
            "System.Boolean",
            "System.Char",
            "System.SByte",
            "System.Byte",
            "System.Int16",
            "System.UInt16",
            "System.Int32",
            "System.UInt32",
            "System.Int64",
            "System.UInt64",
            "System.Single",
            "System.Double",
            "System.Decimal",
            "System.DateTime",
            null,
            "System.String",
            "System.Void",
            VulcanQualifiedTypeNames.Usual,
        };

        static TypeSymbol[] NativeTypeSymbols;

        internal static void InitializeNativeTypes()
        {
            var nativeTypeSymbols = new TypeSymbol[NativeTypeNames.Length];

            foreach (var m in (NativeType[])Enum.GetValues(typeof(NativeType)))
            {
                var name = NativeTypeNames[(int)m];
                if (!string.IsNullOrEmpty(name))
                {
                    Debug.Assert(name.Substring(name.LastIndexOf('.')+1) == m.ToString());
                    var t = Binder.Lookup(name) as TypeSymbol;
                    if (t != null)
                    {
                        t.NativeType = m;
                    }
                    nativeTypeSymbols[(int)m] = t;
                }
                else
                    nativeTypeSymbols[(int)m] = null;
            }

            Interlocked.CompareExchange(ref NativeTypeSymbols, nativeTypeSymbols, null);
        }

        internal static readonly int NativeTypeCount = Enum.GetValues(typeof(NativeType)).Length;

        internal static TypeSymbol GetNativeType(NativeType kind)
        {
            Debug.Assert(NativeTypeSymbols != null);
            return NativeTypeSymbols[(int)kind];
        }
    }
}