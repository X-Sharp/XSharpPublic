using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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

    internal partial class BinaryOperatorSymbol : Symbol
    {
        internal readonly BinaryOperatorKind Kind;
        internal readonly OperandType OpType;
        internal BinaryOperatorSymbol(BinaryOperatorKind kind, OperandType opType) { Kind = kind; OpType = opType; }

        internal static BinaryOperatorSymbol Create(BinaryOperatorKind kind, OperandType opType) { return simpleOp[(int)kind, (int)opType]; }
        internal static BinaryOperatorSymbolWithMethod Create(BinaryOperatorKind kind, MethodSymbol method,
            ConversionSymbol lconv, ConversionSymbol rconv)
        { return new BinaryOperatorSymbolWithMethod(kind, method, lconv, rconv); }

        internal override Symbol Lookup(string name) { throw new NotImplementedException(); }

        internal virtual TypeSymbol Type { get { return OpType.TypeSymbol(); } }

        private static readonly BinaryOperatorSymbol[,] simpleOp;

        static BinaryOperatorSymbol()
        {
            var ops = (BinaryOperatorKind[])Enum.GetValues(typeof(BinaryOperatorKind));
            var opTypes = (OperandType[])Enum.GetValues(typeof(OperandType));

            simpleOp = new BinaryOperatorSymbol[ops.Length, opTypes.Length];

            foreach (var o in ops)
            {
                foreach (var c in opTypes)
                {
                    if (o != BinaryOperatorKind.Error && c != OperandType.Error)
                        simpleOp[(int)o, (int)c] = new BinaryOperatorSymbol(o, c);
                }
            }

#if DEBUG
            /*
            foreach (var o in ops)
            {
                if (o != BinaryOperatorKind.Error)
                    Debug.Assert(OperatorName(o) != null);
            }
            */
#endif
        }

        public static BinaryOperatorKind OperatorKind(TokenType tokenKind)
        {
            switch (tokenKind)
            {
                case TokenType.EXP:
                    return BinaryOperatorKind.Exponent;
                case TokenType.MULT:
                    return BinaryOperatorKind.Multiplication;
                case TokenType.DIV:
                    return BinaryOperatorKind.Division;
                case TokenType.MOD:
                    return BinaryOperatorKind.Remainder;
                case TokenType.PLUS:
                    return BinaryOperatorKind.Addition;
                case TokenType.MINUS:
                    return BinaryOperatorKind.Subtraction;
                case TokenType.LSHIFT:
                    return BinaryOperatorKind.LeftShift;
                case TokenType.RSHIFT:
                    return BinaryOperatorKind.RightShift;
                case TokenType.GT:
                    return BinaryOperatorKind.GreaterThan;
                case TokenType.LT:
                    return BinaryOperatorKind.LessThan;
                case TokenType.GTE:
                    return BinaryOperatorKind.GreaterThanOrEqual;
                case TokenType.LTE:
                    return BinaryOperatorKind.LessThanOrEqual;
                case TokenType.EQ:
                    return BinaryOperatorKind.Equal;
                case TokenType.EEQ:
                    return BinaryOperatorKind.ExactEqual;
                case TokenType.SUBSTR:
                    return BinaryOperatorKind.Substr;
                case TokenType.NEQ:
                    return BinaryOperatorKind.NotEqual;
                case TokenType.NEQ2:
                    return BinaryOperatorKind.NotEqual;
                case TokenType.AMP:
                    return BinaryOperatorKind.And;
                case TokenType.TILDE:
                    return BinaryOperatorKind.Xor;
                case TokenType.PIPE:
                    return BinaryOperatorKind.Or;
                case TokenType.AND:
                    return BinaryOperatorKind.And; // logic
                case TokenType.LOGIC_AND:
                    return BinaryOperatorKind.And; // logic
                case TokenType.LOGIC_XOR:
                    return BinaryOperatorKind.Xor; // logic
                case TokenType.OR:
                    return BinaryOperatorKind.Or; // logic
                case TokenType.LOGIC_OR:
                    return BinaryOperatorKind.Or; // logic
                case TokenType.DEFAULT:
                    return BinaryOperatorKind.DefaultValue;
                case TokenType.ASSIGN_OP:
                    return BinaryOperatorKind.Error;
                case TokenType.ASSIGN_ADD:
                    return BinaryOperatorKind.Addition;
                case TokenType.ASSIGN_SUB:
                    return BinaryOperatorKind.Subtraction;
                case TokenType.ASSIGN_EXP:
                    return BinaryOperatorKind.Exponent;
                case TokenType.ASSIGN_MUL:
                    return BinaryOperatorKind.Multiplication;
                case TokenType.ASSIGN_DIV:
                    return BinaryOperatorKind.Division;
                case TokenType.ASSIGN_MOD:
                    return BinaryOperatorKind.Remainder;
                case TokenType.ASSIGN_BITAND:
                    return BinaryOperatorKind.And; // logic
                case TokenType.ASSIGN_BITOR:
                    return BinaryOperatorKind.Or; // logic
                case TokenType.ASSIGN_LSHIFT:
                    return BinaryOperatorKind.LeftShift;
                case TokenType.ASSIGN_RSHIFT:
                    return BinaryOperatorKind.RightShift;
                case TokenType.ASSIGN_XOR:
                    return BinaryOperatorKind.Xor;
                default:
                    return BinaryOperatorKind.Error;
            }
        }

        public static string OperatorName(BinaryOperatorKind kind)
        {
            switch (kind)
            {
                case BinaryOperatorKind.Multiplication:
                    return OperatorNames.Multiply;
                case BinaryOperatorKind.Addition:
                    return OperatorNames.Addition;
                case BinaryOperatorKind.Subtraction:
                    return OperatorNames.Subtraction;
                case BinaryOperatorKind.Division:
                    return OperatorNames.Division;
                case BinaryOperatorKind.Remainder:
                    return OperatorNames.Modulus;
                case BinaryOperatorKind.LeftShift:
                    return OperatorNames.LeftShift;
                case BinaryOperatorKind.RightShift:
                    return OperatorNames.RightShift;
                case BinaryOperatorKind.Equal:
                    return OperatorNames.Equality;
                case BinaryOperatorKind.NotEqual:
                    return OperatorNames.Inequality;
                case BinaryOperatorKind.GreaterThan:
                    return OperatorNames.GreaterThan;
                case BinaryOperatorKind.LessThan:
                    return OperatorNames.LessThan;
                case BinaryOperatorKind.GreaterThanOrEqual:
                    return OperatorNames.GreaterThanOrEqual;
                case BinaryOperatorKind.LessThanOrEqual:
                    return OperatorNames.LessThanOrEqual;
                case BinaryOperatorKind.And:
                    return OperatorNames.BitwiseAnd;
                case BinaryOperatorKind.Xor:
                    return OperatorNames.ExclusiveOr;
                case BinaryOperatorKind.Or:
                    return OperatorNames.BitwiseOr;
                case BinaryOperatorKind.Exponent:
                    return OperatorNames.Exponent;
                default:
                    return null;
            }
        }
    }

    internal partial class BinaryOperatorSymbolWithMethod : BinaryOperatorSymbol
    {
        internal MethodSymbol Method;
        internal ConversionSymbol ConvLeft;
        internal ConversionSymbol ConvRight;

        internal BinaryOperatorSymbolWithMethod(BinaryOperatorKind kind, MethodSymbol method,
            ConversionSymbol convLeft, ConversionSymbol convRight) : base(kind, OperandType.Error)
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
            const OperandType ERR = OperandType.Error;
            const OperandType OBJ = OperandType.Object;
            const OperandType INT = OperandType.Int;
            const OperandType UIN = OperandType.UInt;
            const OperandType LNG = OperandType.Long;
            const OperandType ULG = OperandType.ULong;
            const OperandType FLT = OperandType.Float;
            const OperandType DBL = OperandType.Double;
            const OperandType DEC = OperandType.Decimal;
            const OperandType BOL = OperandType.Bool;

            // Overload resolution for Y * / - % < > <= >= X
            OperandType[,] arithmetic =
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
            OperandType[,] addition =
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
            OperandType[,] shift =
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
            OperandType[,] equality =
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
            OperandType[,] logical =
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
                    s_binOp[(int)BinaryOperatorKind.Error][i, j] = BinaryOperatorSymbol.Create(BinaryOperatorKind.Error, OperandType.Error);

            for (int i = ((int)BinaryOperatorKind.Error) + 1; i < ops.Length; i++)
                s_binOp[i] = s_binOp[(int)BinaryOperatorKind.Error];

            Func<BinaryOperatorKind, OperandType[,], BinaryOperatorSymbol[,]> expandTable = (k, x) =>
            {
                var res = new BinaryOperatorSymbol[28, 28];
                for (int i = 0; i < 13; i++)
                    for (int j = 0; j < 13; j++)
                    {
                        var t = x[i, j];
                        var nt = OperandTypeHelper.IsNullable(t + OperandTypeHelper.NullableDelta) ? t + OperandTypeHelper.NullableDelta : t;
                        res[i + 2, j + 2] = BinaryOperatorSymbol.Create(k, t);
                        res[i + 2 + 13, j + 2] = BinaryOperatorSymbol.Create(k, nt);
                        res[i + 2, j + 2 + 13] = BinaryOperatorSymbol.Create(k, nt);
                        res[i + 2 + 13, j + 2 + 13] = BinaryOperatorSymbol.Create(k, nt);
                    }
                bool eq = k == BinaryOperatorKind.Equal || k == BinaryOperatorKind.NotEqual || k == BinaryOperatorKind.ExactEqual;
                bool add = k == BinaryOperatorKind.Addition;
                var op_o = eq ?
                   BinaryOperatorSymbol.Create(k, OperandType.Object)
                   : BinaryOperatorSymbol.Create(BinaryOperatorKind.Error, OperandType.Error);
                var op_ss = (eq || add) ? BinaryOperatorSymbol.Create(k, OperandType.String) : op_o;
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
                    s_binOp[i] = expandTable(k, tables[ti]);
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
