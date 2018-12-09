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

    internal partial class Binder
    {
        internal static BinaryOperatorSymbol BindBinaryOperation(BinaryExpr expr, BinaryOperatorKind kind, bool isLogic, bool allowDynamic = true)
        {
            if (isLogic)
            {
                Convert(ref expr.Left, Compilation.Get(NativeType.Boolean));
                Convert(ref expr.Right, Compilation.Get(NativeType.Boolean));
            }

            var res = BinaryOperation(kind, ref expr.Left, ref expr.Right, allowDynamic);

            if (res != null)
                return res;

            throw BinaryOperationError(expr, kind, isLogic);
        }

        internal static BinaryOperatorSymbol BinaryOperation(BinaryOperatorKind kind, ref Expr left, ref Expr right, bool allowDynamic = true)
        {
            var sym = BinaryOperatorEasyOut.ClassifyOperation(kind, left.Datatype, right.Datatype);
            if (sym != null)
            {
                Convert(ref left, sym.TypeOfOp);
                Convert(ref right, sym.TypeOfOp);
                return sym;
            }

            // User-defined operators
            {
                var op = UserDefinedBinaryOperator(kind, ref left, ref right);
                if (op != null)
                    return op;
            }

            // Symbol/string operations
            {
                var op = SymbolAndStringBinaryOperator(kind, ref left, ref right);
                if (op != null)
                    return op;
            }

            // Dynamic with usual
            if (allowDynamic)
            {
                var op = DynamicBinaryOperator(kind, ref left, ref right);
                if (op != null)
                    return op;
            }

            return null;
        }

        internal static BinaryOperatorSymbol UserDefinedBinaryOperator(BinaryOperatorKind kind, ref Expr left, ref Expr right)
        {
            var name = BinaryOperatorSymbol.OperatorName(kind);
            if (name != null)
            {
                MethodSymbol mop = null;
                ConversionSymbol lconv = null;
                ConversionSymbol rconv = null;
                ResolveBinaryOperator(left, right, left.Datatype.Lookup(name), ref mop, ref lconv, ref rconv);
                ResolveBinaryOperator(left, right, right.Datatype.Lookup(name), ref mop, ref lconv, ref rconv);
                if (mop != null)
                {
                    var op = BinaryOperatorSymbol.Create(kind, mop, lconv, rconv);
                    ApplyBinaryOperator(ref left, ref right, op);
                    return op;
                }
            }

            if (kind == BinaryOperatorKind.Exponent &&
                (left.Datatype.NativeType == NativeType.Usual || right.Datatype.NativeType == NativeType.Usual))
            {
                MethodSymbol mop = null;
                ConversionSymbol lconv = null;
                ConversionSymbol rconv = null;
                ResolveBinaryOperator(left, right, Compilation.Get(WellKnownMembers.XSharp_RT_Functions_POW), ref mop, ref lconv, ref rconv, false);
                ResolveBinaryOperator(left, right, left.Datatype.Lookup(OperatorNames.__UsualExponent), ref mop, ref lconv, ref rconv, false);
                ResolveBinaryOperator(left, right, right.Datatype.Lookup(OperatorNames.__UsualExponent), ref mop, ref lconv, ref rconv, false);
                if (mop != null)
                {
                    var op = BinaryOperatorSymbol.Create(kind, mop, lconv, rconv);
                    ApplyBinaryOperator(ref left, ref right, op);
                    return op;
                }
            }

            return null;
        }

        internal static void ResolveBinaryOperator(Expr left, Expr right, Symbol ops,
            ref MethodSymbol op, ref ConversionSymbol lconv, ref ConversionSymbol rconv, bool needSpecialName = true)
        {
            if (ops is MethodSymbol)
            {
                if (CheckBinaryOperator(left, right, (MethodSymbol)ops, ref lconv, ref rconv, needSpecialName))
                    op = (MethodSymbol)ops;
            }
            else if ((ops as SymbolList)?.SymbolTypes.HasFlag(MemberTypes.Method) == true)
            {
                foreach (MethodSymbol m in ((SymbolList)ops).Symbols)
                    if (m != null)
                    {
                        if (CheckBinaryOperator(left, right, m, ref lconv, ref rconv, needSpecialName))
                            op = m;
                    }
            }
        }

        internal static bool CheckBinaryOperator(Expr left, Expr right, MethodSymbol m,
            ref ConversionSymbol lconv, ref ConversionSymbol rconv, bool needSpecialName = true)
        {
            var method = m.Method;
            if (!m.Method.IsStatic || (needSpecialName && !m.Method.IsSpecialName))
                return false;
            var parameters = method.GetParameters();
            if (parameters.Length != 2)
                return false;
            var ltype = FindType(parameters[0].ParameterType);
            var rtype = FindType(parameters[1].ParameterType);
            if (TypesMatch(ltype, left.Datatype) && TypesMatch(rtype, right.Datatype))
            {
                lconv = ConversionSymbol.Create(ConversionKind.Identity);
                rconv = ConversionSymbol.Create(ConversionKind.Identity);
                return true;
            }
            var _lconv = Conversion(left, ltype);
            var _rconv = Conversion(right, rtype);
            if (_lconv.IsImplicit && _rconv.IsImplicit)
            {
                if (lconv == null && rconv == null)
                {
                    lconv = _lconv;
                    rconv = _rconv;
                    return true;
                }
                var cost = lconv?.Cost + rconv?.Cost;
                var _cost = _lconv.Cost + _rconv.Cost;
                if (_cost < cost)
                {
                    lconv = _lconv;
                    rconv = _rconv;
                    return true;
                }
            }
            return false;
        }

        static void ApplyBinaryOperator(ref Expr left, ref Expr right, BinaryOperatorSymbol op)
        {
            if (op is BinaryOperatorSymbolWithMethod)
            {
                var mop = (BinaryOperatorSymbolWithMethod)op;
                var parameters = mop.Method.Method.GetParameters();
                var lconv = mop.ConvLeft;
                if (lconv != null && lconv.Kind != ConversionKind.Identity)
                    Convert(ref left, FindType(parameters[0].ParameterType), lconv);
                var rconv = mop.ConvRight;
                if (rconv != null && rconv.Kind != ConversionKind.Identity)
                    Convert(ref right, FindType(parameters[1].ParameterType), rconv);
            }
        }

        internal static BinaryOperatorSymbol SymbolAndStringBinaryOperator(BinaryOperatorKind kind, ref Expr left, ref Expr right)
        {
            if (kind == BinaryOperatorKind.Addition)
            {
                var l = left;
                var r = right;
                if (left.Datatype.NativeType == NativeType.Symbol)
                    Convert(ref l, Compilation.Get(NativeType.String));
                if (right.Datatype.NativeType == NativeType.Symbol)
                    Convert(ref r, Compilation.Get(NativeType.String));
                var sym = BinaryOperatorEasyOut.ClassifyOperation(kind, l.Datatype, r.Datatype);
                if (sym != null)
                {
                    left = l;
                    right = r;
                    Convert(ref left, sym.TypeOfOp);
                    Convert(ref right, sym.TypeOfOp);
                    return sym;
                }
            }
            return null;
        }

        static BinaryOperatorSymbol DynamicBinaryOperator(BinaryOperatorKind kind, ref Expr left, ref Expr right)
        {
            var l = left;
            var r = right;

            if (l.Datatype.NativeType == NativeType.Object)
                Convert(ref l, Compilation.Get(NativeType.Usual));

            if (r.Datatype.NativeType == NativeType.Object)
                Convert(ref r, Compilation.Get(NativeType.Usual));

            var op = UserDefinedBinaryOperator(kind, ref l, ref r);

            if (op != null)
            {
                left = l;
                right = r;
            }

            return op;
        }
    }
}
