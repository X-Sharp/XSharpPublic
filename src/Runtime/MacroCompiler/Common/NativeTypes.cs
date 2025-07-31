﻿using System;
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
        IntPtr = 20,
        UIntPtr = 21,
        Ptr = 22,
        Usual = 23,
        Float = 24,
        Date = 25,
        Symbol = 26,
        Psz = 27,
        Array = 28,
        Codeblock = 29,
        Currency = 30,
        Binary = 31,
    }

    public static class NativeTypeInfo
    {
        internal static bool IsUnsigned(this NativeType t)
        {
            return t == NativeType.Char || t == NativeType.Byte || t == NativeType.UInt16 || t == NativeType.UInt32 || t == NativeType.UInt64;
        }
        internal static bool IsFloatingPoint(this NativeType t)
        {
            return t == NativeType.Single || t == NativeType.Double || t == NativeType.Decimal;
        }
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
            "System.IntPtr",
            "System.UIntPtr",
            "System.Void*",
            XSharpQualifiedTypeNames.Usual,
            XSharpQualifiedTypeNames.Float,
            XSharpQualifiedTypeNames.Date,
            XSharpQualifiedTypeNames.Symbol,
            XSharpQualifiedTypeNames.Psz ,
            XSharpQualifiedTypeNames.Array,
            XSharpQualifiedTypeNames.Codeblock,
            "XSharp.__Currency",
            "XSharp.__Binary",
        };

        static TypeSymbol[] NativeTypeSymbols;

        internal static void InitializeNativeTypes()
        {
            var nativeTypeSymbols = new TypeSymbol[NativeTypeNames.Length];

            foreach (var m in (NativeType[])Enum.GetValues(typeof(NativeType)))
            {
                var names = NativeTypeNames[(int)m];
                nativeTypeSymbols[(int)m] = null;
                if (!string.IsNullOrEmpty(names))
                {
                    Debug.Assert(names.Substring(names.LastIndexOf('.') + 1)
                        .Replace("__", "").Split('|', '(').First()
                        .Replace("Void*", "Ptr") == m.ToString());
                    foreach(var name in names.Split('|'))
                    {
                        var t = Binder.LookupFullName(name) as TypeSymbol;
                        Debug.Assert(t != null);
                        if (t == null)
                            continue;
                        t.NativeType = m;
                        nativeTypeSymbols[(int)m] = t;
                        break;
                    }
                }
            }

            Interlocked.CompareExchange(ref NativeTypeSymbols, nativeTypeSymbols, null);
        }

        internal static string NativeTypeName(NativeType kind)
        {
            switch(kind)
            {
                case NativeType.Object:
                    return "object";
                case NativeType.Boolean:
                    return "logic";
                case NativeType.Char:
                    return "char";
                case NativeType.SByte:
                    return null;
                case NativeType.Byte:
                    return "byte";
                case NativeType.Int16:
                    return "shortint";
                case NativeType.UInt16:
                    return "word";
                case NativeType.Int32:
                    return "int";
                case NativeType.UInt32:
                    return "dword";
                case NativeType.Int64:
                    return "int64";
                case NativeType.UInt64:
                    return "uint64";
                case NativeType.Single:
                    return "real4";
                case NativeType.Double:
                    return "real8";
                case NativeType.Decimal:
                    return "decimal";
                case NativeType.DateTime:
                    return "datetime";
                case NativeType.String:
                    return "string";
                case NativeType.Void:
                    return "void";
                case NativeType.IntPtr:
                    return "intptr";
                case NativeType.UIntPtr:
                    return "uintptr";
                case NativeType.Ptr:
                    return "ptr";
                case NativeType.Usual:
                    return "usual";
                case NativeType.Float:
                    return "float";
                case NativeType.Date:
                    return "date";
                case NativeType.Symbol:
                    return "symbol";
                case NativeType.Psz:
                    return "psz";
                case NativeType.Array:
                    return "array";
                case NativeType.Codeblock:
                    return "codeblock";
                case NativeType.Currency:
                    return "currency";
                case NativeType.Binary:
                    return "binary";
                default:
                    return null;
            }
    }

        internal static readonly int NativeTypeCount = Enum.GetValues(typeof(NativeType)).Length;

        internal static TypeSymbol Get(NativeType kind)
        {
            Debug.Assert(NativeTypeSymbols != null);
            var result = NativeTypeSymbols[(int)kind];
            if (result == null)
                Compilation.Error(ErrorCode.TypeNotFound,kind);
            return result;
        }
    }
}
