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
            if (conv.Kind != ConversionKind.Identity)
                e = TypeConversion.Bound(e, type, conv);
        }

        internal static void Convert(ref Expr e, TypeSymbol type)
        {
            var conv = Conversion(e, type);
            if (conv.Kind != ConversionKind.Identity)
                e = TypeConversion.Bound(e, type, conv);
        }

        internal static ConversionSymbol Conversion(Expr expr, TypeSymbol type, bool allowDynamic = true)
        {
            var conversion = ConversionEasyOut.ClassifyConversion(expr.Datatype, type);

            if (conversion != ConversionKind.NoConversion)
                return ConversionSymbol.Create(conversion);

            if (TypesMatch(expr.Datatype, type))
                return ConversionSymbol.Create(ConversionKind.Identity);

            MethodSymbol converter = null;

            ResolveUserDefinedConversion(expr, type, expr.Datatype.Lookup(OperatorNames.Implicit), type.Lookup(OperatorNames.Implicit), ref converter);

            if (converter != null)
                return ConversionSymbol.Create(ConversionKind.ImplicitUserDefined, converter);

            ResolveUserDefinedConversion(expr, type, expr.Datatype.Lookup(OperatorNames.Explicit), type.Lookup(OperatorNames.Explicit), ref converter);

            if (converter != null)
                return ConversionSymbol.Create(ConversionKind.ExplicitUserDefined, converter);

            conversion = ResolveUsualConversion(expr, type);
            if (conversion != ConversionKind.NoConversion)
                return ConversionSymbol.Create(conversion);

            if (allowDynamic)
            {
                var conv = ResolveDynamicConversion(expr, type);
                if (conv != null)
                    return conv;
            }

            return ConversionSymbol.Create(ConversionKind.NoConversion);
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
            if (!m.Method.IsStatic || !m.Method.IsSpecialName)
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

        internal static ConversionKind ResolveUsualConversion(Expr expr, TypeSymbol type)
        {
            if (expr.Datatype.NativeType == NativeType.Usual && type.NativeType == NativeType.Object)
                return ConversionKind.Boxing;
            return ConversionKind.NoConversion;
        }

        internal static ConversionSymbol ResolveDynamicConversion(Expr expr, TypeSymbol type)
        {
            if (expr.Datatype.NativeType == NativeType.Object)
            {
                var inner = Conversion(expr, Compilation.GetNativeType(NativeType.Usual));
                var outer = Conversion(TypeConversion.Bound(expr, Compilation.GetNativeType(NativeType.Usual), inner), type);
                if (outer.Kind != ConversionKind.NoConversion)
                {
                    return ConversionSymbol.Create(outer, inner);
                }
            }
            return null;
        }
    }
}