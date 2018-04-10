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

    internal enum OperatorKind
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

    internal class OperatorSymbol : Symbol
    {
        internal readonly OperatorKind Kind;
        internal OperatorSymbol(OperatorKind kind) { Kind = kind; }

        internal static OperatorSymbol Create(OperatorKind kind) { return simpleOp[(int)kind]; }
        internal static OperatorSymbolWithMethod Create(OperatorKind kind, MethodSymbol method, 
            ConversionSymbol lconv, ConversionSymbol rconv) { return new OperatorSymbolWithMethod(kind, method, lconv, rconv); }

        internal override Symbol Lookup(string name) { throw new NotImplementedException(); }

        private static readonly OperatorSymbol[] simpleOp;

        static OperatorSymbol()
        {
            var ops = (OperatorKind[])Enum.GetValues(typeof(OperatorKind));

            simpleOp = new OperatorSymbol[ops.Length];

            foreach (var c in ops)
            {
                if (c != OperatorKind.Error)
                    simpleOp[(int)c] = new OperatorSymbol(c);
            }

#if DEBUG
            /*foreach (var c in ops)
            {
                if (c != OperatorKind.Error)
                    Debug.Assert(OperatorName(c) != null);
            }*/
#endif
        }

        public static OperatorKind BinaryOperatorKind(TokenType tokenKind)
        {
            switch (tokenKind)
            {
                case TokenType.EXP:
                    return OperatorKind.Exponent;
                case TokenType.MULT:
                    return OperatorKind.Multiplication;
                case TokenType.DIV:
                    return OperatorKind.Division;
                case TokenType.MOD:
                    return OperatorKind.Remainder;
                case TokenType.PLUS:
                    return OperatorKind.Addition;
                case TokenType.MINUS:
                    return OperatorKind.Subtraction;
                case TokenType.LSHIFT:
                    return OperatorKind.LeftShift;
                case TokenType.RSHIFT:
                    return OperatorKind.RightShift;
                case TokenType.GT:
                    return OperatorKind.GreaterThan;
                case TokenType.LT:
                    return OperatorKind.LessThan;
                case TokenType.GTE:
                    return OperatorKind.GreaterThanOrEqual;
                case TokenType.LTE:
                    return OperatorKind.LessThanOrEqual;
                case TokenType.EQ:
                    return OperatorKind.Equal;
                case TokenType.EEQ:
                    return OperatorKind.ExactEqual;
                case TokenType.SUBSTR:
                    return OperatorKind.Substr;
                case TokenType.NEQ:
                    return OperatorKind.NotEqual;
                case TokenType.NEQ2:
                    return OperatorKind.NotEqual;
                case TokenType.AMP:
                    return OperatorKind.And;
                case TokenType.TILDE:
                    return OperatorKind.Xor;
                case TokenType.PIPE:
                    return OperatorKind.Or;
                case TokenType.AND:
                    return OperatorKind.And; // logic
                case TokenType.LOGIC_AND:
                    return OperatorKind.And; // logic
                case TokenType.LOGIC_XOR:
                    return OperatorKind.Xor; // logic
                case TokenType.OR:
                    return OperatorKind.Or; // logic
                case TokenType.LOGIC_OR:
                    return OperatorKind.Or; // logic
                case TokenType.DEFAULT:
                    return OperatorKind.DefaultValue;
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
                    return OperatorKind.Error;
            }
        }

        public static string OperatorName(OperatorKind kind)
        {
            switch (kind)
            {
                case OperatorKind.Multiplication:
                    return OperatorNames.Multiply;
                case OperatorKind.Addition:
                    return OperatorNames.Addition;
                case OperatorKind.Subtraction:
                    return OperatorNames.Subtraction;
                case OperatorKind.Division:
                    return OperatorNames.Division;
                case OperatorKind.Remainder:
                    return OperatorNames.Modulus;
                case OperatorKind.LeftShift:
                    return OperatorNames.LeftShift;
                case OperatorKind.RightShift:
                    return OperatorNames.RightShift;
                case OperatorKind.Equal:
                    return OperatorNames.Equality;
                case OperatorKind.NotEqual:
                    return OperatorNames.Inequality;
                case OperatorKind.GreaterThan:
                    return OperatorNames.GreaterThan;
                case OperatorKind.LessThan:
                    return OperatorNames.LessThan;
                case OperatorKind.GreaterThanOrEqual:
                    return OperatorNames.GreaterThanOrEqual;
                case OperatorKind.LessThanOrEqual:
                    return OperatorNames.LessThanOrEqual;
                case OperatorKind.And:
                    return OperatorNames.BitwiseAnd;
                case OperatorKind.Xor:
                    return OperatorNames.ExclusiveOr;
                case OperatorKind.Or:
                    return OperatorNames.BitwiseOr;
                default:
                    return null;
            }
    }
    }

    internal class OperatorSymbolWithMethod : OperatorSymbol
    {
        internal MethodSymbol Method;
        internal ConversionSymbol ConvLeft;
        internal ConversionSymbol ConvRight;

        internal OperatorSymbolWithMethod(OperatorKind kind, MethodSymbol method, 
            ConversionSymbol convLeft, ConversionSymbol convRight) : base(kind)
        {
            Method = method;
            ConvLeft = convLeft;
            ConvRight = convRight;
        }
    }
}