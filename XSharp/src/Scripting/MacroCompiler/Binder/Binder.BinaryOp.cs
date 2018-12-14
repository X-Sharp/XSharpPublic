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
        internal BinaryOperatorSymbol BindBinaryLogicOperation(BinaryExpr expr, BinaryOperatorKind kind)
        {
            return BindBinaryOperation(expr, kind, Options.Binding | BindOptions.Logic);
        }

        internal BinaryOperatorSymbol BindBinaryOperation(BinaryExpr expr, BinaryOperatorKind kind)
        {
            return BindBinaryOperation(expr, kind, Options.Binding);
        }

        internal static BinaryOperatorSymbol BindBinaryOperation(BinaryExpr expr, BinaryOperatorKind kind, BindOptions options)
        {
            if (options.HasFlag(BindOptions.Logic))
            {
                Convert(ref expr.Left, Compilation.Get(NativeType.Boolean), options);
                Convert(ref expr.Right, Compilation.Get(NativeType.Boolean), options);
            }

            var res = BinaryOperation(kind, ref expr.Left, ref expr.Right, options);

            if (res != null)
                return res;

            throw BinaryOperationError(expr, kind, options);
        }

        internal static BinaryOperatorSymbol BinaryOperation(BinaryOperatorKind kind, ref Expr left, ref Expr right, BindOptions options)
        {
            var sym = BinaryOperatorEasyOut.ClassifyOperation(kind, left.Datatype, right.Datatype);

            switch(sym?.Kind)
            {
                case BinaryOperatorKind.EqString:
                case BinaryOperatorKind.EqStringObject:
                case BinaryOperatorKind.EqObjectString:
                case BinaryOperatorKind.NeqString:
                case BinaryOperatorKind.NeqStringObject:
                case BinaryOperatorKind.NeqObjectString:
                    sym = null;
                    break;
            }

            if (sym != null)
            {
                Convert(ref left, sym.TypeOfOp, options);
                Convert(ref right, sym.TypeOfOp, options);
                return sym;
            }

            // User-defined operators
            {
                var op = UserDefinedBinaryOperator(kind, ref left, ref right, options);
                if (op != null)
                    return op;
            }

            // Symbol/string operations
            {
                var op = SymbolAndStringBinaryOperator(kind, ref left, ref right, options);
                if (op != null)
                    return op;
            }

            // Dynamic with usual
            if (options.HasFlag(BindOptions.AllowDynamic))
            {
                var op = DynamicBinaryOperator(kind, ref left, ref right, options);
                if (op != null)
                    return op;
            }

            return null;
        }

        internal static BinaryOperatorSymbol UserDefinedBinaryOperator(BinaryOperatorKind kind, ref Expr left, ref Expr right, BindOptions options)
        {
            MethodSymbol mop = null;
            ConversionSymbol lconv = null;
            ConversionSymbol rconv = null;

            bool hasUsual = left.Datatype.NativeType == NativeType.Usual || right.Datatype.NativeType == NativeType.Usual;
            bool hasString = left.Datatype.NativeType == NativeType.String || right.Datatype.NativeType == NativeType.String;

            if (left.Datatype.NativeType == NativeType.String && right.Datatype.NativeType == NativeType.String)
            {
                switch(kind)
                {
                    case BinaryOperatorKind.Equal:
                        ResolveBinaryOperator(left, right, Compilation.Get(WellKnownMembers.XSharp_VO_Functions___StringEquals), ref mop, ref lconv, ref rconv, options);
                        break;
                    case BinaryOperatorKind.NotEqual:
                        ResolveBinaryOperator(left, right, Compilation.Get(WellKnownMembers.XSharp_VO_Functions___StringNotEquals), ref mop, ref lconv, ref rconv, options);
                        break;
                }
            }
            else if (hasString || hasUsual)
            {
                switch (kind)
                {
                    case BinaryOperatorKind.Equal:
                        ResolveBinaryOperator(left, right, Compilation.Get(NativeType.Usual).Lookup(OperatorNames.__UsualInExactEquals), ref mop, ref lconv, ref rconv, options);
                        break;
                    case BinaryOperatorKind.NotEqual:
                        ResolveBinaryOperator(left, right, Compilation.Get(NativeType.Usual).Lookup(OperatorNames.__UsualInExactNotEquals), ref mop, ref lconv, ref rconv, options);
                        break;
                }
            }

            if (mop == null)
            {
                var name = BinaryOperatorSymbol.OperatorName(kind);
                if (name != null)
                {
                    ResolveBinaryOperator(left, right, left.Datatype.Lookup(name), ref mop, ref lconv, ref rconv, options | BindOptions.Special);
                    ResolveBinaryOperator(left, right, right.Datatype.Lookup(name), ref mop, ref lconv, ref rconv, options | BindOptions.Special);
                }
            }

            if (mop == null && kind == BinaryOperatorKind.Exponent && hasUsual)
            {
                ResolveBinaryOperator(left, right, Compilation.Get(WellKnownMembers.XSharp_VO_Functions_POW), ref mop, ref lconv, ref rconv, options);
                ResolveBinaryOperator(left, right, left.Datatype.Lookup(OperatorNames.__UsualExponent), ref mop, ref lconv, ref rconv, options);
                ResolveBinaryOperator(left, right, right.Datatype.Lookup(OperatorNames.__UsualExponent), ref mop, ref lconv, ref rconv, options);
            }

            if (mop != null)
            {
                var op = BinaryOperatorSymbol.Create(kind, mop, lconv, rconv);
                ApplyBinaryOperator(ref left, ref right, op);
                return op;
            }

            return null;
        }

        internal static void ResolveBinaryOperator(Expr left, Expr right, Symbol ops,
            ref MethodSymbol op, ref ConversionSymbol lconv, ref ConversionSymbol rconv, BindOptions options)
        {
            if (ops is MethodSymbol)
            {
                if (CheckBinaryOperator(left, right, (MethodSymbol)ops, ref lconv, ref rconv, options))
                    op = (MethodSymbol)ops;
            }
            else if ((ops as SymbolList)?.SymbolTypes.HasFlag(MemberTypes.Method) == true)
            {
                foreach (MethodSymbol m in ((SymbolList)ops).Symbols)
                    if (m != null)
                    {
                        if (CheckBinaryOperator(left, right, m, ref lconv, ref rconv, options))
                            op = m;
                    }
            }
        }

        internal static bool CheckBinaryOperator(Expr left, Expr right, MethodSymbol m,
            ref ConversionSymbol lconv, ref ConversionSymbol rconv, BindOptions options)
        {
            var method = m.Method;
            if (!m.Method.IsStatic || (options.HasFlag(BindOptions.Special) && !m.Method.IsSpecialName))
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
            var _lconv = Conversion(left, ltype, options);
            var _rconv = Conversion(right, rtype, options);
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

        internal static BinaryOperatorSymbol SymbolAndStringBinaryOperator(BinaryOperatorKind kind, ref Expr left, ref Expr right, BindOptions options)
        {
            if (kind == BinaryOperatorKind.Addition)
            {
                var l = left;
                var r = right;
                if (left.Datatype.NativeType == NativeType.Symbol)
                    Convert(ref l, Compilation.Get(NativeType.String), options);
                if (right.Datatype.NativeType == NativeType.Symbol)
                    Convert(ref r, Compilation.Get(NativeType.String), options);
                var sym = BinaryOperatorEasyOut.ClassifyOperation(kind, l.Datatype, r.Datatype);
                if (sym != null)
                {
                    left = l;
                    right = r;
                    Convert(ref left, sym.TypeOfOp, options);
                    Convert(ref right, sym.TypeOfOp, options);
                    return sym;
                }
            }
            return null;
        }

        static BinaryOperatorSymbol DynamicBinaryOperator(BinaryOperatorKind kind, ref Expr left, ref Expr right, BindOptions options)
        {
            var l = left;
            var r = right;

            if (l.Datatype.NativeType == NativeType.Object)
                Convert(ref l, Compilation.Get(NativeType.Usual), options);

            if (r.Datatype.NativeType == NativeType.Object)
                Convert(ref r, Compilation.Get(NativeType.Usual), options);

            var op = UserDefinedBinaryOperator(kind, ref l, ref r, options);

            if (op != null)
            {
                left = l;
                right = r;
            }

            return op;
        }
    }
}
