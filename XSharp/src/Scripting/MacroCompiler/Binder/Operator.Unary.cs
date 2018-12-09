using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler
{
    using Syntax;

    internal enum UnaryOperatorKind
    {
        Error,
        Increment,
        Decrement,
        UnaryPlus,
        UnaryMinus,
        LogicalNegation,
        BitwiseComplement,
        True,
        False,
    }

    internal class UnaryOperatorSymbol : TypedSymbol
    {
        internal readonly UnaryOperatorKind Kind;
        internal readonly OperandType OpType;
        internal UnaryOperatorSymbol(UnaryOperatorKind kind, OperandType opType) { Kind = kind; OpType = opType; }

        internal static UnaryOperatorSymbol Create(UnaryOperatorKind kind, OperandType opType) { return simpleOp[(int)kind, (int)opType]; }
        internal static UnaryOperatorSymbolWithMethod Create(UnaryOperatorKind kind, MethodSymbol method, ConversionSymbol conv) { return new UnaryOperatorSymbolWithMethod(kind, method, conv); }

        internal override TypeSymbol Type { get { return OpType.TypeSymbol(); } }

        private static readonly UnaryOperatorSymbol[,] simpleOp;

        static UnaryOperatorSymbol()
        {
            var ops = (UnaryOperatorKind[])Enum.GetValues(typeof(UnaryOperatorKind));
            var opTypes = (OperandType[])Enum.GetValues(typeof(OperandType));

            simpleOp = new UnaryOperatorSymbol[ops.Length, opTypes.Length];

            foreach (var o in ops)
            {
                foreach (var c in opTypes)
                {
                    if (o != UnaryOperatorKind.Error && c != OperandType.Error)
                        simpleOp[(int)o, (int)c] = new UnaryOperatorSymbol(o, c);
                }
            }

#if DEBUG
            /*
            foreach (var o in ops)
            {
                if (o != UnaryOperatorKind.Error)
                    Debug.Assert(OperatorName(o) != null);
            }
            */
#endif
        }

        public static UnaryOperatorKind OperatorKind(TokenType tokenKind)
        {
            switch (tokenKind)
            {
                case TokenType.PLUS:
                    return UnaryOperatorKind.UnaryPlus;
                case TokenType.MINUS:
                    return UnaryOperatorKind.UnaryMinus;
                case TokenType.TILDE:
                    return UnaryOperatorKind.BitwiseComplement;
                case TokenType.NOT:
                case TokenType.LOGIC_NOT:
                    return UnaryOperatorKind.LogicalNegation;
                case TokenType.INC:
                    return UnaryOperatorKind.Increment;
                case TokenType.DEC:
                    return UnaryOperatorKind.Decrement;
                case TokenType.ADDROF:
                default:
                    return UnaryOperatorKind.Error;
            }
        }

        public static bool OperatorIsLogic(TokenType tokenKind)
        {
            switch (tokenKind)
            {
                case TokenType.NOT:
                case TokenType.LOGIC_NOT:
                    return true;
                default:
                    return false;
            }
        }

        public static string OperatorName(UnaryOperatorKind kind)
        {
            switch (kind)
            {
                case UnaryOperatorKind.Increment:
                    return OperatorNames.Increment;
                case UnaryOperatorKind.Decrement:
                    return OperatorNames.Decrement;
                case UnaryOperatorKind.UnaryPlus:
                    return OperatorNames.UnaryPlus;
                case UnaryOperatorKind.UnaryMinus:
                    return OperatorNames.UnaryNegation;
                case UnaryOperatorKind.LogicalNegation:
                    return OperatorNames.LogicalNot;
                case UnaryOperatorKind.BitwiseComplement:
                    return OperatorNames.OnesComplement;
                case UnaryOperatorKind.True:
                    return OperatorNames.True;
                case UnaryOperatorKind.False:
                    return OperatorNames.False;
                default:
                    return null;
            }
        }

        public static string OperatorSymbol(UnaryOperatorKind kind)
        {
            switch (kind)
            {
                case UnaryOperatorKind.Increment:
                    return "++";
                case UnaryOperatorKind.Decrement:
                    return "--";
                case UnaryOperatorKind.UnaryPlus:
                    return "+";
                case UnaryOperatorKind.UnaryMinus:
                    return "-";
                case UnaryOperatorKind.LogicalNegation:
                    return "!";
                case UnaryOperatorKind.BitwiseComplement:
                    return "~";
                case UnaryOperatorKind.True:
                    return "true";
                case UnaryOperatorKind.False:
                    return "false";
                default:
                    return null;
            }
        }
    }

    internal class UnaryOperatorSymbolWithMethod : UnaryOperatorSymbol
    {
        internal MethodSymbol Method;
        internal ConversionSymbol Conv;

        internal UnaryOperatorSymbolWithMethod(UnaryOperatorKind kind, MethodSymbol method,
            ConversionSymbol conv) : base(kind, OperandType.Error)
        {
            Method = method;
            Conv = conv;
        }

        internal override TypeSymbol Type { get { return Method.Type; } }
    }

    internal static class UnaryOperatorEasyOut
    {
        private static UnaryOperatorSymbol[][] s_unOp;

        static UnaryOperatorEasyOut()
        {
            const OperandType ERR = OperandType.Error;
            const OperandType BOL = OperandType.Bool;
            const OperandType CHR = OperandType.Char;
            const OperandType I08 = OperandType.SByte;
            const OperandType U08 = OperandType.Byte;
            const OperandType I16 = OperandType.Short;
            const OperandType U16 = OperandType.UShort;
            const OperandType I32 = OperandType.Int;
            const OperandType U32 = OperandType.UInt;
            const OperandType I64 = OperandType.Long;
            const OperandType U64 = OperandType.ULong;
            const OperandType R32 = OperandType.Float;
            const OperandType R64 = OperandType.Double;
            const OperandType DEC = OperandType.Decimal;

            OperandType[] increment =
                //bool chr  i08  i16  i32  i64  u08  u16  u32  u64  r32  r64  dec  
                { ERR, CHR, I08, I16, I32, I64, U08, U16, U32, U64, R32, R64, DEC };

            OperandType[] plus =
                //bool chr  i08  i16  i32  i64  u08  u16  u32  u64  r32  r64  dec  
                { ERR, I32, I32, I32, I32, I64, I32, I32, U32, U64, R32, R64, DEC };

            OperandType[] minus =
                //bool chr  i08  i16  i32  i64  u08  u16  u32  u64  r32  r64  dec  
                { ERR, I32, I32, I32, I32, I64, I32, I32, I64, ERR, R32, R64, DEC };

            OperandType[] logicalNegation =
                //bool chr  i08  i16  i32  i64  u08  u16  u32  u64  r32  r64  dec  
                { BOL, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR };

            OperandType[] bitwiseComplement =
                //bool chr  i08  i16  i32  i64  u08  u16  u32  u64  r32  r64  dec  
                { ERR, I32, I32, I32, I32, I64, I32, I32, U32, U64, ERR, ERR, ERR };

            var ops = (UnaryOperatorKind[])Enum.GetValues(typeof(UnaryOperatorKind));

            s_unOp = new UnaryOperatorSymbol[ops.Length][];
            s_unOp[(int)UnaryOperatorKind.Error] = new UnaryOperatorSymbol[28];

            for (int i = 0; i < 28; i++)
                s_unOp[(int)UnaryOperatorKind.Error][i] = UnaryOperatorSymbol.Create(UnaryOperatorKind.Error, OperandType.Error);

            for (int i = ((int)UnaryOperatorKind.Error) + 1; i < ops.Length; i++)
                s_unOp[i] = s_unOp[(int)UnaryOperatorKind.Error];

            Func<UnaryOperatorKind, OperandType[], UnaryOperatorSymbol[]> expandTable = (k, x) =>
            {
                var res = new UnaryOperatorSymbol[28];
                for (int i = 0; i < 13; i++)
                {
                    var t = x[i];
                    var nt = OperandTypeHelper.IsNullable(t + OperandTypeHelper.NullableDelta) ? t + OperandTypeHelper.NullableDelta : t;
                    res[i + 2] = UnaryOperatorSymbol.Create(k, t);
                    res[i + 2 + 13] = UnaryOperatorSymbol.Create(k, nt);
                }
                return res;
            };

            var tables = new[] { increment, plus, minus, logicalNegation, bitwiseComplement };

            Func<UnaryOperatorKind, int> tableIndex = k =>
            {
                if (k == UnaryOperatorKind.Increment || k == UnaryOperatorKind.Decrement)
                    return 0;
                if (k == UnaryOperatorKind.UnaryPlus)
                    return 1;
                if (k == UnaryOperatorKind.UnaryMinus)
                    return 2;
                if (k == UnaryOperatorKind.LogicalNegation)
                    return 3;
                if (k == UnaryOperatorKind.BitwiseComplement)
                    return 4;
                return -1;
            };

            for (int i = ((int)UnaryOperatorKind.Error) + 1; i < ops.Length; i++)
            {
                var k = (UnaryOperatorKind)i;
                var ti = tableIndex(k);
                if (ti >= 0)
                    s_unOp[i] = expandTable(k, tables[ti]);
            }
        }

        public static UnaryOperatorSymbol ClassifyOperation(UnaryOperatorKind kind, TypeSymbol expr)
        {
            int exprIdx = ConversionEasyOut.TypeToIndex(expr);
            if (exprIdx < 0)
            {
                return null;
            }
            return s_unOp[(int)kind][exprIdx];
        }
    }
}
