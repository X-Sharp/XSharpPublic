using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;

namespace XSharp.MacroCompiler
{
    using Syntax;

    internal partial class Binder
    {
        internal static void Convert(ref Expr e, TypeSymbol type, ConversionSymbol conv)
        {
            switch (conv.Kind)
            {
                case ConversionKind.Identity:
                    break;
                case ConversionKind.ConstantReduction:
                    e = LiteralExpr.Bound(((ConversionSymbolToConstant)conv).Constant);
                    break;
                case ConversionKind.NoConversion:
                    throw ConversionError(e, type);
                case ConversionKind.NoImplicitConversion:
                    throw ImplicitConversionError(e, type);
                default:
                    e = TypeConversion.Bound(e, type, conv);
                    break;
            }
        }

        internal static void Convert(ref Expr e, TypeSymbol type)
        {
            Convert(ref e, type, Conversion(e, type));
        }

        internal static TypeSymbol ConvertResult(ref Expr e1, ref Expr e2)
        {
            var conv1 = Conversion(e1, e2.Datatype);
            var conv2 = Conversion(e2, e1.Datatype);
            if (!conv1.Exists && conv2.Exists)
            {
                Convert(ref e2, e1.Datatype, conv2);
                return e1.Datatype;
            }
            if (conv1.Exists && !conv2.Exists)
            {
                Convert(ref e1, e2.Datatype, conv1);
                return e2.Datatype;
            }
            if (conv1.Exists && conv2.Exists)
            {
                int cost1 = conv1.Cost;
                int cost2 = conv2.Cost;
                if (cost1 <= cost2 && e1.Datatype.NativeType != NativeType.Usual)
                {
                    Convert(ref e1, e2.Datatype, conv1);
                    return e2.Datatype;
                }
                else
                {
                    Convert(ref e2, e1.Datatype, conv2);
                    return e1.Datatype;
                }
            }
            Convert(ref e1, Compilation.Get(NativeType.Usual));
            Convert(ref e2, Compilation.Get(NativeType.Usual));
            return Compilation.Get(NativeType.Usual);
        }

        internal static ConversionSymbol Conversion(Expr expr, TypeSymbol type, bool allowExplicit = false, bool allowDynamic = true)
        {
            var noConversion = ConversionKind.NoConversion;

            var conversion = ConversionEasyOut.ClassifyConversion(expr.Datatype, type);

            if (conversion != ConversionKind.NoConversion)
            {
                var conv = ConversionSymbol.Create(conversion);
                if (allowExplicit || conv.IsImplicit)
                    return conv;
                if (conv.Exists)
                    noConversion = ConversionKind.NoImplicitConversion;
            }

            if (TypesMatch(expr.Datatype, type))
                return ConversionSymbol.Create(ConversionKind.Identity);

            MethodSymbol converter = null;

            ResolveUserDefinedConversion(expr, type, expr.Datatype.Lookup(OperatorNames.Implicit), type.Lookup(OperatorNames.Implicit), ref converter);
            if (converter != null)
                return ConversionSymbol.Create(ConversionKind.ImplicitUserDefined, converter);

            if (allowExplicit)
            {
                ResolveUserDefinedConversion(expr, type, expr.Datatype.Lookup(OperatorNames.Explicit), type.Lookup(OperatorNames.Explicit), ref converter);
                if (converter != null)
                    return ConversionSymbol.Create(ConversionKind.ExplicitUserDefined, converter);
            }

            {
                var conv = ResolveUsualConversion(expr, type);
                if (conv != null)
                    return conv;
            }

            if (type.IsByRef != expr.Datatype.IsByRef)
            {
                var conv = ResolveByRefConversion(expr, type);
                if (conv != null)
                    return conv;
            }

            if (type.NativeType == NativeType.Object)
            {
                if (expr.Datatype.Type.IsValueType)
                    return ConversionSymbol.Create(ConversionKind.Boxing);
                else
                    return ConversionSymbol.Create(ConversionKind.ImplicitReference);
            }

            if (allowDynamic)
            {
                var conv = ResolveDynamicConversion(expr, type, allowExplicit);
                if (conv != null)
                    return conv;
            }

            if (!allowExplicit && noConversion == ConversionKind.NoConversion)
            {
                ResolveUserDefinedConversion(expr, type, expr.Datatype.Lookup(OperatorNames.Explicit), type.Lookup(OperatorNames.Explicit), ref converter);
                if (converter != null)
                    noConversion = ConversionKind.NoImplicitConversion;
            }

            return ConversionSymbol.Create(noConversion);
        }

        internal static void ResolveUserDefinedConversion(Expr expr, TypeSymbol type, Symbol src_conv, Symbol dest_conv, ref MethodSymbol converter)
        {
            if (src_conv != null)
                ResolveConversionMethod(expr, type, src_conv, ref converter);
            if (dest_conv != null)
                ResolveConversionMethod(expr, type, dest_conv, ref converter);
        }

        internal static void ResolveConversionMethod(Expr expr, TypeSymbol type, Symbol conv, ref MethodSymbol converter)
        {
            if (conv is MethodSymbol)
            {
                if (CheckConversionMethod(expr, type, (MethodSymbol)conv))
                    converter = (MethodSymbol)conv;
            }
            else if ((conv as SymbolList)?.SymbolTypes.HasFlag(MemberTypes.Method) == true)
            {
                foreach (MethodSymbol m in ((SymbolList)conv).Symbols)
                    if (m != null)
                {
                        if (CheckConversionMethod(expr, type, m))
                            converter = m;
                    }
            }
        }

        static bool CheckConversionMethod(Expr expr, TypeSymbol type, MethodSymbol m)
        {
            var method = m.Method;
            if (!m.Method.IsStatic)
                return false;
            if (!TypesMatch(FindType(m.Method.ReturnType), type))
                return false;
            var parameters = method.GetParameters();
            if (parameters.Length != 1)
                return false;
            if (!TypesMatch(FindType(parameters[0].ParameterType), expr.Datatype))
                return false;
            return true;
        }

        internal static ConversionSymbol ResolveUsualConversion(Expr expr, TypeSymbol type)
        {
            if (expr.Datatype.NativeType == NativeType.Usual && type.NativeType == NativeType.Object)
            {
                MethodSymbol converter = null;
                ResolveConversionMethod(expr, type, Compilation.Get(NativeType.Usual).Lookup(XSharpFunctionNames.ToObject), ref converter);
                if (converter != null)
                    return ConversionSymbol.Create(ConversionKind.ImplicitUserDefined, converter);
                else
                    return ConversionSymbol.Create(ConversionKind.Boxing);
            }
            return null;
        }

        internal static ConversionSymbol ResolveByRefConversion(Expr expr, TypeSymbol type)
        {
            if (type.IsByRef && TypesMatch(expr.Datatype, type.ElementType))
            {
                return ConversionSymbol.CreateByRef();
            }
            else
            {
                var inner = ConversionSymbol.Create(ConversionKind.Deref);
                var outer = Conversion(TypeConversion.Bound(expr, expr.Datatype.ElementType, inner), type);
                if (outer.Exists)
                {
                    return ConversionSymbol.Create(outer, inner);
                }
            }
            return null;
        }
        internal static ConversionSymbol ResolveDynamicConversion(Expr expr, TypeSymbol type, bool allowExplicit)
        {
            if (expr.Datatype.NativeType == NativeType.Object)
            {
                var inner = Conversion(expr, Compilation.Get(NativeType.Usual));
                var outer = Conversion(TypeConversion.Bound(expr, Compilation.Get(NativeType.Usual), inner), type, allowExplicit: allowExplicit);
                if (outer.Exists)
                {
                    return ConversionSymbol.Create(outer, inner);
                }
            }
            return null;
        }
    }
}
