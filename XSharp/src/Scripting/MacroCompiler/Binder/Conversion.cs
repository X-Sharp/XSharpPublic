using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;
using System.Reflection;

namespace XSharp.MacroCompiler
{
    internal enum ConversionKind
    {
        NoConversion,
        Identity,
        ImplicitNumeric,
        //ImplicitEnumeration,
        //ImplicitThrow,
        //ImplicitTupleLiteral,
        //ImplicitTuple,
        //ExplicitTupleLiteral,
        //ExplicitTuple,
        ImplicitNullable,
        //NullLiteral,
        ImplicitReference,
        Boxing,
        //PointerToVoid,
        //NullToPointer,
        //ImplicitDynamic,
        //ExplicitDynamic,
        //ImplicitConstant,
        ImplicitUserDefined,
        //AnonymousFunction,
        //MethodGroup,
        ExplicitNumeric,
        //ExplicitEnumeration,
        ExplicitNullable,
        ExplicitReference,
        Unboxing,
        ExplicitUserDefined,
        //PointerToPointer,
        //IntegerToPointer,
        //PointerToInteger,
        //IntPtr,
        //InterpolatedString, // a conversion from an interpolated string to IFormattable or FormattableString
        ImplicitUsual,
        ExplicitUsual,
    }

    internal class ConversionSymbol : Symbol
    {
        internal readonly ConversionKind Kind;

        internal bool IsExplicit { get { return (convCost[(int)Kind] & Explicit) != 0; } }
        internal bool IsImplicit { get { return (convCost[(int)Kind] & Implicit) != 0; } }
        internal int Cost { get { return convCost[(int)Kind] & (Implicit-1); } }

        internal ConversionSymbol(ConversionKind kind) { Kind = kind; }

        internal static ConversionSymbol Create(ConversionKind kind) { return simpleConv[(int)kind]; }
        internal static ConversionSymbolWithMethod Create(ConversionKind kind, MethodSymbol method) { return new ConversionSymbolWithMethod(kind, method); }

        internal override Symbol Lookup(string name) { throw new NotImplementedException(); }

        const int Explicit = 0x20000;
        const int Implicit = 0x10000;
        private static readonly ConversionSymbol[] simpleConv;
        private static readonly int[] convCost;

        static ConversionSymbol()
        {
            var conversions = (ConversionKind[])Enum.GetValues(typeof(ConversionKind));

            simpleConv = new ConversionSymbol[conversions.Length];

            foreach (var c in conversions)
            {
                simpleConv[(int)c] = new ConversionSymbol(c);
            }

            convCost = new int[conversions.Length];

#if DEBUG
            foreach (var c in conversions)
            {
                convCost[(int)c] = -1;
            }
#endif

            convCost[(int)ConversionKind.NoConversion] = 0;

            convCost[(int)ConversionKind.Identity] = Implicit | 0;
            convCost[(int)ConversionKind.ImplicitNumeric] = Implicit | 1;
            convCost[(int)ConversionKind.ImplicitNullable] = Implicit | 2;
            //convCost[(int)ConversionKind.NullLiteral] = Implicit | 0;
            convCost[(int)ConversionKind.ImplicitReference] = Implicit | 0;
            convCost[(int)ConversionKind.Boxing] = Implicit | 3;
            convCost[(int)ConversionKind.ImplicitUsual] = Implicit | 4;
            convCost[(int)ConversionKind.ImplicitUserDefined] = Implicit | 4;

            convCost[(int)ConversionKind.ExplicitNumeric] = Explicit | 1;
            convCost[(int)ConversionKind.ExplicitNullable] = Explicit | 2;
            convCost[(int)ConversionKind.ExplicitReference] = Explicit | 0;
            convCost[(int)ConversionKind.Unboxing] = Explicit | 3;
            convCost[(int)ConversionKind.ExplicitUsual] = Explicit | 4;
            convCost[(int)ConversionKind.ExplicitUserDefined] = Explicit | 4;

#if DEBUG
            foreach (var cost in convCost)
            {
                Debug.Assert(cost >= 0);
            }
#endif
        }
    }

    internal class ConversionSymbolWithMethod : ConversionSymbol
    {
        internal MethodSymbol Method;

        internal ConversionSymbolWithMethod(ConversionKind kind, MethodSymbol method) : base(kind) { Method = method; }
    }

    internal static class ConversionEasyOut
    {
        // There are situations in which we know that there is no unusual conversion going on
        // (such as a conversion involving constants, enumerated types, and so on.) In those
        // situations we can classify conversions via a simple table lookup:

        // PERF: Use byte instead of ConversionKind so the compiler can use array literal initialization.
        //       The most natural type choice, Enum arrays, are not blittable due to a CLR limitation.
        private static readonly byte[,] s_convkind;

        static ConversionEasyOut()
        {
            const byte IDN = (byte)ConversionKind.Identity;
            const byte IRF = (byte)ConversionKind.ImplicitReference;
            const byte XRF = (byte)ConversionKind.ExplicitReference;
            const byte XNM = (byte)ConversionKind.ExplicitNumeric;
            const byte NOC = (byte)ConversionKind.NoConversion;
            const byte BOX = (byte)ConversionKind.Boxing;
            const byte UNB = (byte)ConversionKind.Unboxing;
            const byte NUM = (byte)ConversionKind.ImplicitNumeric;
            const byte NUL = (byte)ConversionKind.ImplicitNullable;
            const byte XNL = (byte)ConversionKind.ExplicitNullable;

            s_convkind = new byte[,] {
                // Converting Y to X:
                //                    -----------------regular-------------------                      ----------------nullable-------------------
                //          obj  str  bool chr  i08  i16  i32  i64  u08  u16  u32  u64  r32  r64  dec  bool chr  i08  i16  i32  i64  u08  u16  u32  u64  r32  r64  dec  
                /*  obj */{ IDN, XRF, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB, UNB },
                /*  str */{ IRF, IDN, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC },
                /* bool */{ BOX, NOC, IDN, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NUL, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC },
                /*  chr */{ BOX, NOC, NOC, IDN, XNM, XNM, NUM, NUM, XNM, NUM, NUM, NUM, NUM, NUM, NUM, NOC, NUL, XNL, XNL, NUL, NUL, XNL, NUL, NUL, NUL, NUL, NUL, NUL },
                /*  i08 */{ BOX, NOC, NOC, XNM, IDN, NUM, NUM, NUM, XNM, XNM, XNM, XNM, NUM, NUM, NUM, NOC, XNL, NUL, NUL, NUL, NUL, XNL, XNL, XNL, XNL, NUL, NUL, NUL },
                /*  i16 */{ BOX, NOC, NOC, XNM, XNM, IDN, NUM, NUM, XNM, XNM, XNM, XNM, NUM, NUM, NUM, NOC, XNL, XNL, NUL, NUL, NUL, XNL, XNL, XNL, XNL, NUL, NUL, NUL },
                /*  i32 */{ BOX, NOC, NOC, XNM, XNM, XNM, IDN, NUM, XNM, XNM, XNM, XNM, NUM, NUM, NUM, NOC, XNL, XNL, XNL, NUL, NUL, XNL, XNL, XNL, XNL, NUL, NUL, NUL },
                /*  i64 */{ BOX, NOC, NOC, XNM, XNM, XNM, XNM, IDN, XNM, XNM, XNM, XNM, NUM, NUM, NUM, NOC, XNL, XNL, XNL, XNL, NUL, XNL, XNL, XNL, XNL, NUL, NUL, NUL },
                /*  u08 */{ BOX, NOC, NOC, XNM, XNM, NUM, NUM, NUM, IDN, NUM, NUM, NUM, NUM, NUM, NUM, NOC, XNL, XNL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL },
                /*  u16 */{ BOX, NOC, NOC, XNM, XNM, XNM, NUM, NUM, XNM, IDN, NUM, NUM, NUM, NUM, NUM, NOC, XNL, XNL, XNL, NUL, NUL, XNL, NUL, NUL, NUL, NUL, NUL, NUL },
                /*  u32 */{ BOX, NOC, NOC, XNM, XNM, XNM, XNM, NUM, XNM, XNM, IDN, NUM, NUM, NUM, NUM, NOC, XNL, XNL, XNL, XNL, NUL, XNL, XNL, NUL, NUL, NUL, NUL, NUL },
                /*  u64 */{ BOX, NOC, NOC, XNM, XNM, XNM, XNM, XNM, XNM, XNM, XNM, IDN, NUM, NUM, NUM, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NUL, NUL, NUL, NUL },
                /*  r32 */{ BOX, NOC, NOC, XNM, XNM, XNM, XNM, XNM, XNM, XNM, XNM, XNM, IDN, NUM, XNM, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NUL, NUL, XNL },
                /*  r64 */{ BOX, NOC, NOC, XNM, XNM, XNM, XNM, XNM, XNM, XNM, XNM, XNM, XNM, IDN, XNM, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NUL, XNL },
                /*  dec */{ BOX, NOC, NOC, XNM, XNM, XNM, XNM, XNM, XNM, XNM, XNM, XNM, XNM, XNM, IDN, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NUL },
                /*nbool */{ BOX, NOC, XNL, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, IDN, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC, NOC },
                /* nchr */{ BOX, NOC, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NOC, IDN, XNL, XNL, NUL, NUL, XNL, NUL, NUL, NUL, NUL, NUL, NUL },
                /* ni08 */{ BOX, NOC, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NOC, XNL, IDN, NUL, NUL, NUL, XNL, XNL, XNL, XNL, NUL, NUL, NUL },
                /* ni16 */{ BOX, NOC, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NOC, XNL, XNL, IDN, NUL, NUL, XNL, XNL, XNL, XNL, NUL, NUL, NUL },
                /* ni32 */{ BOX, NOC, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NOC, XNL, XNL, XNL, IDN, NUL, XNL, XNL, XNL, XNL, NUL, NUL, NUL },
                /* ni64 */{ BOX, NOC, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NOC, XNL, XNL, XNL, XNL, IDN, XNL, XNL, XNL, XNL, NUL, NUL, NUL },
                /* nu08 */{ BOX, NOC, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NOC, XNL, XNL, NUL, NUL, NUL, IDN, NUL, NUL, NUL, NUL, NUL, NUL },
                /* nu16 */{ BOX, NOC, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NOC, XNL, XNL, XNL, NUL, NUL, XNL, IDN, NUL, NUL, NUL, NUL, NUL },
                /* nu32 */{ BOX, NOC, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NOC, XNL, XNL, XNL, XNL, NUL, XNL, XNL, IDN, NUL, NUL, NUL, NUL },
                /* nu64 */{ BOX, NOC, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, IDN, NUL, NUL, NUL },
                /* nr32 */{ BOX, NOC, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, IDN, NUL, XNL },
                /* nr64 */{ BOX, NOC, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, IDN, XNL },
                /* ndec */{ BOX, NOC, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, NOC, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, XNL, IDN }
                };
        }

        private static int TypeToIndex(TypeSymbol type)
        {
            switch (type.NativeType)
            {
                case NativeType.Object: return 0;
                case NativeType.String: return 1;
                case NativeType.Boolean: return 2;
                case NativeType.Char: return 3;
                case NativeType.SByte: return 4;
                case NativeType.Int16: return 5;
                case NativeType.Int32: return 6;
                case NativeType.Int64: return 7;
                case NativeType.Byte: return 8;
                case NativeType.UInt16: return 9;
                case NativeType.UInt32: return 10;
                case NativeType.UInt64: return 11;
                case NativeType.Single: return 12;
                case NativeType.Double: return 13;
                case NativeType.Decimal: return 14;

                /*case NativeType.Unknown:
                    if ((object)type != null && type.Type.IsNullableType())
                    {
                        TypeSymbol underlyingType = type.GetNullableUnderlyingType();

                        switch (underlyingType.NativeType)
                        {
                            case NativeType.Boolean: return 15;
                            case NativeType.Char: return 16;
                            case NativeType.SByte: return 17;
                            case NativeType.Int16: return 18;
                            case NativeType.Int32: return 19;
                            case NativeType.Int64: return 20;
                            case NativeType.Byte: return 21;
                            case NativeType.UInt16: return 22;
                            case NativeType.UInt32: return 23;
                            case NativeType.UInt64: return 24;
                            case NativeType.Single: return 25;
                            case NativeType.Double: return 26;
                            case NativeType.Decimal: return 27;
                        }
                    }

                    // fall through
                    goto default;*/

                default:
                    return -1;
            }
        }

        public static ConversionKind ClassifyConversion(TypeSymbol source, TypeSymbol target)
        {
            int sourceIndex = TypeToIndex(source);
            if (sourceIndex < 0)
            {
                return ConversionKind.NoConversion;
            }
            int targetIndex = TypeToIndex(target);
            if (targetIndex < 0)
            {
                return ConversionKind.NoConversion;
            }
            return (ConversionKind)s_convkind[sourceIndex, targetIndex];
        }
    }
}