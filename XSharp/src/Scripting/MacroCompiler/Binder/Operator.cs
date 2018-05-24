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

    internal enum BinaryOperatorKind
    {
        Error,
        Multiplication,
        Addition,
        Subtraction,
        Division,
        Remainder,
        Exponent, // X#
        LeftShift,
        RightShift,
        Equal,
        NotEqual,
        ExactEqual, // X#
        GreaterThan,
        LessThan,
        GreaterThanOrEqual,
        LessThanOrEqual,
        And,
        Xor,
        Or,
        Substr,
        DefaultValue, // X#
    }

    internal enum BinaryOperatorType
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
        NullableInt,
        NullableUInt,
        NullableLong,
        NullableULong,
        NullableFloat,
        NullableDouble,
        NullableDecimal,
        NullableBool,
    }

    internal class BinaryOperatorSymbol : Symbol
    {
        internal const int NullableDelta = BinaryOperatorType.NullableInt - BinaryOperatorType.Int;

        internal readonly BinaryOperatorKind Kind;
        internal readonly BinaryOperatorType OpType;
        internal BinaryOperatorSymbol(BinaryOperatorKind kind, BinaryOperatorType opType) { Kind = kind; OpType = opType; }

        internal static BinaryOperatorSymbol Create(BinaryOperatorKind kind, BinaryOperatorType opType) { return simpleOp[(int)kind,(int)opType]; }
        internal static BinaryOperatorSymbolWithMethod Create(BinaryOperatorKind kind, MethodSymbol method, 
            ConversionSymbol lconv, ConversionSymbol rconv) { return new BinaryOperatorSymbolWithMethod(kind, method, lconv, rconv); }

        internal override Symbol Lookup(string name) { throw new NotImplementedException(); }

        internal virtual TypeSymbol Type {
            get {
                switch (OpType)
                {
                    case BinaryOperatorType.Object:
                        return Compilation.GetNativeType(NativeType.Object);
                    case BinaryOperatorType.String:
                        return Compilation.GetNativeType(NativeType.String);
                    case BinaryOperatorType.ObjectAndString:
                        return Compilation.GetNativeType(NativeType.String);
                    case BinaryOperatorType.StringAndObject:
                        return Compilation.GetNativeType(NativeType.String);
                    case BinaryOperatorType.Int:
                        return Compilation.GetNativeType(NativeType.Int32);
                    case BinaryOperatorType.UInt:
                        return Compilation.GetNativeType(NativeType.UInt32);
                    case BinaryOperatorType.Long:
                        return Compilation.GetNativeType(NativeType.Int64);
                    case BinaryOperatorType.ULong:
                        return Compilation.GetNativeType(NativeType.UInt64);
                    case BinaryOperatorType.Float:
                        return Compilation.GetNativeType(NativeType.Single);
                    case BinaryOperatorType.Double:
                        return Compilation.GetNativeType(NativeType.Double);
                    case BinaryOperatorType.Decimal:
                        return Compilation.GetNativeType(NativeType.Decimal);
                    case BinaryOperatorType.Bool:
                        return Compilation.GetNativeType(NativeType.Boolean);
                    case BinaryOperatorType.NullableInt:
                        return Binder.NullableType(Compilation.GetNativeType(NativeType.Int32));
                    case BinaryOperatorType.NullableUInt:
                        return Binder.NullableType(Compilation.GetNativeType(NativeType.UInt32));
                    case BinaryOperatorType.NullableLong:
                        return Binder.NullableType(Compilation.GetNativeType(NativeType.Int64));
                    case BinaryOperatorType.NullableULong:
                        return Binder.NullableType(Compilation.GetNativeType(NativeType.UInt64));
                    case BinaryOperatorType.NullableFloat:
                        return Binder.NullableType(Compilation.GetNativeType(NativeType.Single));
                    case BinaryOperatorType.NullableDouble:
                        return Binder.NullableType(Compilation.GetNativeType(NativeType.Double));
                    case BinaryOperatorType.NullableDecimal:
                        return Binder.NullableType(Compilation.GetNativeType(NativeType.Decimal));
                    case BinaryOperatorType.NullableBool:
                        return Binder.NullableType(Compilation.GetNativeType(NativeType.Boolean));
                    default:
                        return null;
                }
            }
        }

        private static readonly BinaryOperatorSymbol[,] simpleOp;
        private static readonly bool[] s_nullable;

        internal static bool IsNullable(BinaryOperatorType type) { return s_nullable[(int)type]; }

        static BinaryOperatorSymbol()
        {
            var ops = (BinaryOperatorKind[])Enum.GetValues(typeof(BinaryOperatorKind));
            var opTypes = (BinaryOperatorType[])Enum.GetValues(typeof(BinaryOperatorType));

            simpleOp = new BinaryOperatorSymbol[ops.Length,opTypes.Length];

            foreach (var o in ops)
            {
                foreach (var c in opTypes)
                {
                    if (o != BinaryOperatorKind.Error && c != BinaryOperatorType.Error)
                        simpleOp[(int)o,(int)c] = new BinaryOperatorSymbol(o,c);
                }
            }

            s_nullable = new bool[opTypes.Length];

            foreach (var c in opTypes)
            {
                if (c.ToString().StartsWith("Nullable"))
                    s_nullable[(int)c] = true;
            }

#if DEBUG
            /*
            foreach (var o in ops)
            {
                if (o != BinaryOperatorKind.Error)
                    Debug.Assert(OperatorName(o) != null);
            }
            */

            Dictionary<string, BinaryOperatorType> convNames = new Dictionary<string, BinaryOperatorType>();
            foreach (var c in opTypes)
                convNames.Add(c.ToString(), c);
            foreach (var c in opTypes)
            {
                var ncs = "Nullable" + c.ToString();
                BinaryOperatorType nc;
                if (convNames.TryGetValue(ncs,out nc))
                {
                    Debug.Assert(nc - c == NullableDelta);
                }
            }
#endif
        }

        public static BinaryOperatorKind OperatorKind(TokenType tokenKind)
        {
            switch (tokenKind)
            {
                case TokenType.EXP:
                    return MacroCompiler.BinaryOperatorKind.Exponent;
                case TokenType.MULT:
                    return MacroCompiler.BinaryOperatorKind.Multiplication;
                case TokenType.DIV:
                    return MacroCompiler.BinaryOperatorKind.Division;
                case TokenType.MOD:
                    return MacroCompiler.BinaryOperatorKind.Remainder;
                case TokenType.PLUS:
                    return MacroCompiler.BinaryOperatorKind.Addition;
                case TokenType.MINUS:
                    return MacroCompiler.BinaryOperatorKind.Subtraction;
                case TokenType.LSHIFT:
                    return MacroCompiler.BinaryOperatorKind.LeftShift;
                case TokenType.RSHIFT:
                    return MacroCompiler.BinaryOperatorKind.RightShift;
                case TokenType.GT:
                    return MacroCompiler.BinaryOperatorKind.GreaterThan;
                case TokenType.LT:
                    return MacroCompiler.BinaryOperatorKind.LessThan;
                case TokenType.GTE:
                    return MacroCompiler.BinaryOperatorKind.GreaterThanOrEqual;
                case TokenType.LTE:
                    return MacroCompiler.BinaryOperatorKind.LessThanOrEqual;
                case TokenType.EQ:
                    return MacroCompiler.BinaryOperatorKind.Equal;
                case TokenType.EEQ:
                    return MacroCompiler.BinaryOperatorKind.ExactEqual;
                case TokenType.SUBSTR:
                    return MacroCompiler.BinaryOperatorKind.Substr;
                case TokenType.NEQ:
                    return MacroCompiler.BinaryOperatorKind.NotEqual;
                case TokenType.NEQ2:
                    return MacroCompiler.BinaryOperatorKind.NotEqual;
                case TokenType.AMP:
                    return MacroCompiler.BinaryOperatorKind.And;
                case TokenType.TILDE:
                    return MacroCompiler.BinaryOperatorKind.Xor;
                case TokenType.PIPE:
                    return MacroCompiler.BinaryOperatorKind.Or;
                case TokenType.AND:
                    return MacroCompiler.BinaryOperatorKind.And; // logic
                case TokenType.LOGIC_AND:
                    return MacroCompiler.BinaryOperatorKind.And; // logic
                case TokenType.LOGIC_XOR:
                    return MacroCompiler.BinaryOperatorKind.Xor; // logic
                case TokenType.OR:
                    return MacroCompiler.BinaryOperatorKind.Or; // logic
                case TokenType.LOGIC_OR:
                    return MacroCompiler.BinaryOperatorKind.Or; // logic
                case TokenType.DEFAULT:
                    return MacroCompiler.BinaryOperatorKind.DefaultValue;
                case TokenType.ASSIGN_OP:
                case TokenType.ASSIGN_ADD:
                case TokenType.ASSIGN_SUB:
                case TokenType.ASSIGN_EXP:
                case TokenType.ASSIGN_MUL:
                case TokenType.ASSIGN_DIV:
                case TokenType.ASSIGN_MOD:
                case TokenType.ASSIGN_BITAND:
                case TokenType.ASSIGN_BITOR:
                case TokenType.ASSIGN_LSHIFT:
                case TokenType.ASSIGN_RSHIFT:
                case TokenType.ASSIGN_XOR:
                default:
                    return MacroCompiler.BinaryOperatorKind.Error;
            }
        }

        public static string OperatorName(BinaryOperatorKind kind)
        {
            switch (kind)
            {
                case MacroCompiler.BinaryOperatorKind.Multiplication:
                    return OperatorNames.Multiply;
                case MacroCompiler.BinaryOperatorKind.Addition:
                    return OperatorNames.Addition;
                case MacroCompiler.BinaryOperatorKind.Subtraction:
                    return OperatorNames.Subtraction;
                case MacroCompiler.BinaryOperatorKind.Division:
                    return OperatorNames.Division;
                case MacroCompiler.BinaryOperatorKind.Remainder:
                    return OperatorNames.Modulus;
                case MacroCompiler.BinaryOperatorKind.LeftShift:
                    return OperatorNames.LeftShift;
                case MacroCompiler.BinaryOperatorKind.RightShift:
                    return OperatorNames.RightShift;
                case MacroCompiler.BinaryOperatorKind.Equal:
                    return OperatorNames.Equality;
                case MacroCompiler.BinaryOperatorKind.NotEqual:
                    return OperatorNames.Inequality;
                case MacroCompiler.BinaryOperatorKind.GreaterThan:
                    return OperatorNames.GreaterThan;
                case MacroCompiler.BinaryOperatorKind.LessThan:
                    return OperatorNames.LessThan;
                case MacroCompiler.BinaryOperatorKind.GreaterThanOrEqual:
                    return OperatorNames.GreaterThanOrEqual;
                case MacroCompiler.BinaryOperatorKind.LessThanOrEqual:
                    return OperatorNames.LessThanOrEqual;
                case MacroCompiler.BinaryOperatorKind.And:
                    return OperatorNames.BitwiseAnd;
                case MacroCompiler.BinaryOperatorKind.Xor:
                    return OperatorNames.ExclusiveOr;
                case MacroCompiler.BinaryOperatorKind.Or:
                    return OperatorNames.BitwiseOr;
                default:
                    return null;
            }
        }
    }

    internal class BinaryOperatorSymbolWithMethod : BinaryOperatorSymbol
    {
        internal MethodSymbol Method;
        internal ConversionSymbol ConvLeft;
        internal ConversionSymbol ConvRight;

        internal BinaryOperatorSymbolWithMethod(BinaryOperatorKind kind, MethodSymbol method, 
            ConversionSymbol convLeft, ConversionSymbol convRight) : base(kind, BinaryOperatorType.Error)
        {
            Method = method;
            ConvLeft = convLeft;
            ConvRight = convRight;
        }

        internal override TypeSymbol Type { get { return Method.Type; } }
    }

    internal static class BinaryOperatorEasyOut
    {
        private static BinaryOperatorSymbol[][,] s_binOp;

        static BinaryOperatorEasyOut()
        {
            const BinaryOperatorType ERR = BinaryOperatorType.Error;
            const BinaryOperatorType OBJ = BinaryOperatorType.Object;
            const BinaryOperatorType INT = BinaryOperatorType.Int;
            const BinaryOperatorType UIN = BinaryOperatorType.UInt;
            const BinaryOperatorType LNG = BinaryOperatorType.Long;
            const BinaryOperatorType ULG = BinaryOperatorType.ULong;
            const BinaryOperatorType FLT = BinaryOperatorType.Float;
            const BinaryOperatorType DBL = BinaryOperatorType.Double;
            const BinaryOperatorType DEC = BinaryOperatorType.Decimal;
            const BinaryOperatorType BOL = BinaryOperatorType.Bool;

            // Overload resolution for Y * / - % < > <= >= X
            BinaryOperatorType[,] arithmetic =
            {
                //          bool chr  i08  i16  i32  i64  u08  u16  u32  u64  r32  r64  dec
                /* bool */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
                /*  chr */{ ERR, INT, INT, INT, INT, LNG, INT, INT, UIN, ULG, FLT, DBL, DEC },
                /*  i08 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, LNG, ERR, FLT, DBL, DEC },
                /*  i16 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, LNG, ERR, FLT, DBL, DEC },
                /*  i32 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, LNG, ERR, FLT, DBL, DEC },
                /*  i64 */{ ERR, LNG, LNG, LNG, LNG, LNG, LNG, LNG, LNG, ERR, FLT, DBL, DEC },
                /*  u08 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, UIN, ULG, FLT, DBL, DEC },
                /*  u16 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, UIN, ULG, FLT, DBL, DEC },
                /*  u32 */{ ERR, UIN, LNG, LNG, LNG, LNG, UIN, UIN, UIN, ULG, FLT, DBL, DEC },
                /*  u64 */{ ERR, ULG, ERR, ERR, ERR, ERR, ULG, ULG, ULG, ULG, FLT, DBL, DEC },
                /*  r32 */{ ERR, FLT, FLT, FLT, FLT, FLT, FLT, FLT, FLT, FLT, FLT, DBL, ERR },
                /*  r64 */{ ERR, DBL, DBL, DBL, DBL, DBL, DBL, DBL, DBL, DBL, DBL, DBL, ERR },
                /*  dec */{ ERR, DEC, DEC, DEC, DEC, DEC, DEC, DEC, DEC, DEC, ERR, ERR, DEC },
            };

            // Overload resolution for Y + X
            BinaryOperatorType[,] addition =
            {
                //          bool chr  i08  i16  i32  i64  u08  u16  u32  u64  r32  r64  dec
                /* bool */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
                /*  chr */{ ERR, INT, INT, INT, INT, LNG, INT, INT, UIN, ULG, FLT, DBL, DEC },
                /*  i08 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, LNG, ERR, FLT, DBL, DEC },
                /*  i16 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, LNG, ERR, FLT, DBL, DEC },
                /*  i32 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, LNG, ERR, FLT, DBL, DEC },
                /*  i64 */{ ERR, LNG, LNG, LNG, LNG, LNG, LNG, LNG, LNG, ERR, FLT, DBL, DEC },
                /*  u08 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, UIN, ULG, FLT, DBL, DEC },
                /*  u16 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, UIN, ULG, FLT, DBL, DEC },
                /*  u32 */{ ERR, UIN, LNG, LNG, LNG, LNG, UIN, UIN, UIN, ULG, FLT, DBL, DEC },
                /*  u64 */{ ERR, ULG, ERR, ERR, ERR, ERR, ULG, ULG, ULG, ULG, FLT, DBL, DEC },
                /*  r32 */{ ERR, FLT, FLT, FLT, FLT, FLT, FLT, FLT, FLT, FLT, FLT, DBL, ERR },
                /*  r64 */{ ERR, DBL, DBL, DBL, DBL, DBL, DBL, DBL, DBL, DBL, DBL, DBL, ERR },
                /*  dec */{ ERR, DEC, DEC, DEC, DEC, DEC, DEC, DEC, DEC, DEC, ERR, ERR, DEC },
            };

            // Overload resolution for Y << >> X
            BinaryOperatorType[,] shift =
            {
                //          bool chr  i08  i16  i32  i64  u08  u16  u32  u64  r32  r64  dec
                /* bool */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
                /*  chr */{ ERR, INT, INT, INT, INT, ERR, INT, INT, ERR, ERR, ERR, ERR, ERR },
                /*  i08 */{ ERR, INT, INT, INT, INT, ERR, INT, INT, ERR, ERR, ERR, ERR, ERR },
                /*  i16 */{ ERR, INT, INT, INT, INT, ERR, INT, INT, ERR, ERR, ERR, ERR, ERR },
                /*  i32 */{ ERR, INT, INT, INT, INT, ERR, INT, INT, ERR, ERR, ERR, ERR, ERR },
                /*  i64 */{ ERR, LNG, LNG, LNG, LNG, ERR, LNG, LNG, ERR, ERR, ERR, ERR, ERR },
                /*  u08 */{ ERR, INT, INT, INT, INT, ERR, INT, INT, ERR, ERR, ERR, ERR, ERR },
                /*  u16 */{ ERR, INT, INT, INT, INT, ERR, INT, INT, ERR, ERR, ERR, ERR, ERR },
                /*  u32 */{ ERR, UIN, UIN, UIN, UIN, ERR, UIN, UIN, ERR, ERR, ERR, ERR, ERR },
                /*  u64 */{ ERR, ULG, ULG, ULG, ULG, ERR, ULG, ULG, ERR, ERR, ERR, ERR, ERR },
                /*  r32 */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
                /*  r64 */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
                /*  dec */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            };

            // Overload resolution for Y == != X
            // Note that these are the overload resolution rules; overload resolution might pick an invalid operator.
            // For example, overload resolution on object == decimal chooses the object/object overload, which then
            // is not legal because decimal must be a reference type. But we don't know to give that error *until*
            // overload resolution has chosen the reference equality operator.
            BinaryOperatorType[,] equality =
            {
                //          bool chr  i08  i16  i32  i64  u08  u16  u32  u64  r32  r64  dec
                /* bool */{ BOL, OBJ, OBJ, OBJ, OBJ, OBJ, OBJ, OBJ, OBJ, OBJ, OBJ, OBJ, OBJ },
                /*  chr */{ OBJ, INT, INT, INT, INT, LNG, INT, INT, UIN, ULG, FLT, DBL, DEC },
                /*  i08 */{ OBJ, INT, INT, INT, INT, LNG, INT, INT, LNG, ERR, FLT, DBL, DEC },
                /*  i16 */{ OBJ, INT, INT, INT, INT, LNG, INT, INT, LNG, ERR, FLT, DBL, DEC },
                /*  i32 */{ OBJ, INT, INT, INT, INT, LNG, INT, INT, LNG, ERR, FLT, DBL, DEC },
                /*  i64 */{ OBJ, LNG, LNG, LNG, LNG, LNG, LNG, LNG, LNG, ERR, FLT, DBL, DEC },
                /*  u08 */{ OBJ, INT, INT, INT, INT, LNG, INT, INT, UIN, ULG, FLT, DBL, DEC },
                /*  u16 */{ OBJ, INT, INT, INT, INT, LNG, INT, INT, UIN, ULG, FLT, DBL, DEC },
                /*  u32 */{ OBJ, UIN, LNG, LNG, LNG, LNG, UIN, UIN, UIN, ULG, FLT, DBL, DEC },
                /*  u64 */{ OBJ, ULG, ERR, ERR, ERR, ERR, ULG, ULG, ULG, ULG, FLT, DBL, DEC },
                /*  r32 */{ OBJ, FLT, FLT, FLT, FLT, FLT, FLT, FLT, FLT, FLT, FLT, DBL, OBJ },
                /*  r64 */{ OBJ, DBL, DBL, DBL, DBL, DBL, DBL, DBL, DBL, DBL, DBL, DBL, OBJ },
                /*  dec */{ OBJ, DEC, DEC, DEC, DEC, DEC, DEC, DEC, DEC, DEC, OBJ, OBJ, DEC },
            };

            // Overload resolution for Y | & ^ || && X
            BinaryOperatorType[,] logical =
            {
                //          bool chr  i08  i16  i32  i64  u08  u16  u32  u64  r32  r64  dec
                /* bool */{ BOL, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
                /*  chr */{ ERR, INT, INT, INT, INT, LNG, INT, INT, UIN, ULG, ERR, ERR, ERR },
                /*  i08 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, LNG, ERR, ERR, ERR, ERR },
                /*  i16 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, LNG, ERR, ERR, ERR, ERR },
                /*  i32 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, LNG, ERR, ERR, ERR, ERR },
                /*  i64 */{ ERR, LNG, LNG, LNG, LNG, LNG, LNG, LNG, LNG, ERR, ERR, ERR, ERR },
                /*  u08 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, UIN, ULG, ERR, ERR, ERR },
                /*  u16 */{ ERR, INT, INT, INT, INT, LNG, INT, INT, UIN, ULG, ERR, ERR, ERR },
                /*  u32 */{ ERR, UIN, LNG, LNG, LNG, LNG, UIN, UIN, UIN, ULG, ERR, ERR, ERR },
                /*  u64 */{ ERR, ULG, ERR, ERR, ERR, ERR, ULG, ULG, ULG, ULG, ERR, ERR, ERR },
                /*  r32 */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
                /*  r64 */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
                /*  dec */{ ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR },
            };

            var ops = (BinaryOperatorKind[])Enum.GetValues(typeof(BinaryOperatorKind));

            s_binOp = new BinaryOperatorSymbol[ops.Length][,];
            s_binOp[(int)BinaryOperatorKind.Error] = new BinaryOperatorSymbol[28, 28];

            for (int i = 0; i < 28; i++)
                for (int j = 0; j < 28; j++)
                    s_binOp[(int)BinaryOperatorKind.Error][i, j] = BinaryOperatorSymbol.Create(BinaryOperatorKind.Error, BinaryOperatorType.Error);

            for (int i = ((int)BinaryOperatorKind.Error)+1; i < ops.Length; i++)
                s_binOp[i] = s_binOp[(int)BinaryOperatorKind.Error];

            Func<BinaryOperatorKind, BinaryOperatorType[,], BinaryOperatorSymbol[,]> expandTable = (k, x) =>
             {
                 var res = new BinaryOperatorSymbol[28, 28];
                 for (int i = 0; i < 13; i++)
                     for (int j = 0; j < 13; j++)
                     {
                         var t = x[i, j];
                         var nt = BinaryOperatorSymbol.IsNullable(t + BinaryOperatorSymbol.NullableDelta) ? t + BinaryOperatorSymbol.NullableDelta : t;
                         res[i + 2, j + 2] = BinaryOperatorSymbol.Create(k, t);
                         res[i + 2 + 13, j + 2] = BinaryOperatorSymbol.Create(k, nt);
                         res[i + 2, j + 2 + 13] = BinaryOperatorSymbol.Create(k, nt);
                         res[i + 2 + 13, j + 2 + 13] = BinaryOperatorSymbol.Create(k, nt);
                     }
                 bool eq = k == BinaryOperatorKind.Equal || k == BinaryOperatorKind.NotEqual || k == BinaryOperatorKind.ExactEqual;
                 bool add = k == BinaryOperatorKind.Addition;
                 var op_o = eq ?
                    BinaryOperatorSymbol.Create(k, BinaryOperatorType.Object)
                    : BinaryOperatorSymbol.Create(BinaryOperatorKind.Error, BinaryOperatorType.Error);
                 var op_ss = (eq || add) ? BinaryOperatorSymbol.Create(k, BinaryOperatorType.String) : op_o;
                 var op_so = (eq || add) ? op_ss : op_o;
                 var op_os = (eq || add) ? op_ss : op_o;
                 for (int i = 0; i < 28; i++)
                 {
                     if (i == 1)
                     {
                         // str
                         res[1, 1] = op_ss;
                     }
                     else
                     {
                         // obj
                         res[0, i] = op_o;
                         res[i, 0] = op_o;
                         // str
                         res[1, i] = op_so;
                         res[i, 1] = op_os;
                     }
                 }
                 return res;
             };

            var tables = new[] { arithmetic, addition, shift, equality, logical };

            Func<BinaryOperatorKind, int> tableIndex = k =>
            {
                if (k == BinaryOperatorKind.Multiplication || k == BinaryOperatorKind.Subtraction || k == BinaryOperatorKind.Division
                    || k == BinaryOperatorKind.Remainder || k == BinaryOperatorKind.Exponent)
                    return 0;
                if (k == BinaryOperatorKind.Addition)
                    return 1;
                if (k == BinaryOperatorKind.LeftShift || k == BinaryOperatorKind.RightShift)
                    return 2;
                if (k == BinaryOperatorKind.Equal || k == BinaryOperatorKind.NotEqual || k == BinaryOperatorKind.ExactEqual)
                    return 3;
                if (k == BinaryOperatorKind.GreaterThan || k == BinaryOperatorKind.LessThan || k == BinaryOperatorKind.GreaterThanOrEqual
                    || k == BinaryOperatorKind.LessThanOrEqual || k == BinaryOperatorKind.And || k == BinaryOperatorKind.Xor
                    || k == BinaryOperatorKind.Or)
                    return 0;
                if (k == BinaryOperatorKind.Substr || k == BinaryOperatorKind.DefaultValue)
                    return -1;
                return -1;
            };

            for (int i = ((int)BinaryOperatorKind.Error) + 1; i < ops.Length; i++)
            {
                var k = (BinaryOperatorKind)i;
                var ti = tableIndex(k);
                if (ti >= 0)
                    s_binOp[i] = expandTable(k,tables[ti]);
            }
        }

        public static BinaryOperatorSymbol ClassifyOperation(BinaryOperatorKind kind, TypeSymbol left, TypeSymbol right)
        {
            int leftIdx = ConversionEasyOut.TypeToIndex(left);
            if (leftIdx < 0)
            {
                return null;
            }
            int rightIdx = ConversionEasyOut.TypeToIndex(right);
            if (rightIdx < 0)
            {
                return null;
            }
            return s_binOp[(int)kind][leftIdx, rightIdx];
        }
    }
}