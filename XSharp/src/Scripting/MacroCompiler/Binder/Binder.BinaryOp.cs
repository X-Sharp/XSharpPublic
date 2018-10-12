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
        internal static BinaryOperatorSymbol BinaryOperation(BinaryOperatorKind kind, ref Expr left, ref Expr right, bool allowDynamic = true)
        {
            var sym = BinaryOperatorEasyOut.ClassifyOperation(kind, left.Datatype, right.Datatype);
            if (sym != null)
            {
                Convert(ref left, sym.Type);
                Convert(ref right, sym.Type);
                return sym;
            }

            // User-defined operators
            {
                var op = UserDefinedBinaryOperator(kind, ref left, ref right);
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
                ResolveUserDefinedBinaryOperator(left, right, left.Datatype.Lookup(name), right.Datatype.Lookup(name), ref mop, ref lconv, ref rconv);
                if (mop != null)
                {
                    var op = BinaryOperatorSymbol.Create(kind, mop, lconv, rconv);
                    ApplyBinaryOperator(ref left, ref right, op);
                    return op;
                }
            }

            if (kind == BinaryOperatorKind.Exponent &&
                (left.Datatype.NativeType == NativeType.__Usual || right.Datatype.NativeType == NativeType.__Usual))
            {
                name = OperatorNames.__UsualExponent;
                if (name != null)
                {
                    MethodSymbol mop = null;
                    ConversionSymbol lconv = null;
                    ConversionSymbol rconv = null;
                    ResolveUserDefinedBinaryOperator(left, right, left.Datatype.Lookup(name), right.Datatype.Lookup(name), ref mop, ref lconv, ref rconv, false);
                    if (mop != null)
                    {
                        var op = BinaryOperatorSymbol.Create(kind, mop, lconv, rconv);
                        ApplyBinaryOperator(ref left, ref right, op);
                        return op;
                    }
                }
            }

            return null;
        }

        internal static void ResolveUserDefinedBinaryOperator(Expr left, Expr right, Symbol left_ops, Symbol right_ops,
            ref MethodSymbol op, ref ConversionSymbol lconv, ref ConversionSymbol rconv, bool needSpecialName = true)
        {
            if (left_ops != null)
                ResolveBinaryOperator(left, right, left_ops, ref op, ref lconv, ref rconv, needSpecialName);
            if (right_ops != null)
                ResolveBinaryOperator(left, right, right_ops, ref op, ref lconv, ref rconv, needSpecialName);
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

        static BinaryOperatorSymbol DynamicBinaryOperator(BinaryOperatorKind kind, ref Expr left, ref Expr right)
        {
            var l = left;
            var r = right;

            if (l.Datatype.NativeType == NativeType.Object)
                Convert(ref l, Compilation.GetNativeType(NativeType.__Usual));

            if (r.Datatype.NativeType == NativeType.Object)
                Convert(ref r, Compilation.GetNativeType(NativeType.__Usual));

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