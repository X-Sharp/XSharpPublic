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

        internal void Convert(ref Expr e, TypeSymbol type)
        {
            Convert(ref e, type, Conversion(e, type, Options.Binding));
        }

        internal void ConvertExplicit(ref Expr e, TypeSymbol type)
        {
            Convert(ref e, type, Conversion(e, type, Options.Binding | BindOptions.Explicit));
        }

        internal TypeSymbol ConvertResult(ref Expr e1, ref Expr e2)
        {
            return ConvertResult(ref e1, ref e2, Options.Binding);
        }

        internal static void Convert(ref Expr e, TypeSymbol type, BindOptions options)
        {
            Convert(ref e, type, Conversion(e, type, options));
        }

        internal static TypeSymbol ConvertResult(ref Expr e1, ref Expr e2, BindOptions options)
        {
            var conv1 = Conversion(e1, e2.Datatype, options);
            var conv2 = Conversion(e2, e1.Datatype, options);
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
            Convert(ref e1, Compilation.Get(NativeType.Usual), options);
            Convert(ref e2, Compilation.Get(NativeType.Usual), options);
            return Compilation.Get(NativeType.Usual);
        }

        internal ConversionSymbol ImplicitConversion(Expr expr, TypeSymbol type)
        {
            return Binder.Conversion(expr, type, Options.Binding);
        }

        internal ConversionSymbol ExplicitConversion(Expr expr, TypeSymbol type)
        {
            return Binder.Conversion(expr, type, Options.Binding | BindOptions.Explicit);
        }

        internal static ConversionSymbol Conversion(Expr expr, TypeSymbol type, BindOptions options)
        {
            var noConversion = ConversionKind.NoConversion;

            var conversion = ConversionEasyOut.ClassifyConversion(expr.Datatype, type);

            if (conversion != ConversionKind.NoConversion)
            {
                var conv = ConversionSymbol.Create(conversion);
                if (options.HasFlag(BindOptions.Explicit) || conv.IsImplicit)
                    return conv;
                if (conv.Exists)
                    noConversion = ConversionKind.NoImplicitConversion;
            }

            if (TypesMatch(expr.Datatype, type))
                return ConversionSymbol.Create(ConversionKind.Identity);

            MethodSymbol converter = null;

            ResolveUserDefinedConversion(expr, type, expr.Datatype.Lookup(OperatorNames.Implicit), type.Lookup(OperatorNames.Implicit), ref converter, options | BindOptions.Special);
            if (converter != null)
                return ConversionSymbol.Create(ConversionKind.ImplicitUserDefined, converter);

            if (options.HasFlag(BindOptions.Explicit))
            {
                ResolveUserDefinedConversion(expr, type, expr.Datatype.Lookup(OperatorNames.Explicit), type.Lookup(OperatorNames.Explicit), ref converter, options | BindOptions.Special);
                if (converter != null)
                    return ConversionSymbol.Create(ConversionKind.ExplicitUserDefined, converter);
            }

            {
                var conv = ResolveUsualConversion(expr, type, options);
                if (conv != null)
                    return conv;
            }

            if (type.IsByRef != expr.Datatype.IsByRef)
            {
                var conv = ResolveByRefConversion(expr, type, options);
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
            else if (expr.Datatype.NativeType == NativeType.Object)
            {
                if (!options.HasFlag(BindOptions.AllowDynamic))
                {
                    if (options.HasFlag(BindOptions.Explicit))
                        return type.IsValueType ? ConversionSymbol.Create(ConversionKind.Unboxing) : ConversionSymbol.Create(ConversionKind.ExplicitReference);
                    else
                        noConversion = ConversionKind.NoImplicitConversion;
                }
            }
            else if (type.IsReferenceType && expr.Datatype.IsReferenceType)
            {
                if (expr.Datatype.IsSubclassOf(type))
                    return ConversionSymbol.Create(ConversionKind.ImplicitReference);
                if (type.IsSubclassOf(expr.Datatype))
                {
                    if (options.HasFlag(BindOptions.Explicit))
                        return ConversionSymbol.Create(ConversionKind.ExplicitReference);
                    else
                        noConversion = ConversionKind.NoImplicitConversion;
                }
            }

            {
                var conv = ResolveEnumConversion(expr, type, options);
                if (conv != null)
                    return conv;
            }

            if (options.HasFlag(BindOptions.AllowDynamic))
            {
                var conv = ResolveDynamicConversion(expr, type, options);
                if (conv != null)
                    return conv;
            }

            if (!options.HasFlag(BindOptions.Explicit) && noConversion == ConversionKind.NoConversion)
            {
                ResolveUserDefinedConversion(expr, type, expr.Datatype.Lookup(OperatorNames.Explicit), type.Lookup(OperatorNames.Explicit), ref converter, options | BindOptions.Special);
                if (converter != null)
                    noConversion = ConversionKind.NoImplicitConversion;
            }

            return ConversionSymbol.Create(noConversion);
        }

        internal static void ResolveUserDefinedConversion(Expr expr, TypeSymbol type, Symbol src_conv, Symbol dest_conv, ref MethodSymbol converter, BindOptions options)
        {
            if (src_conv != null)
                ResolveConversionMethod(expr, type, src_conv, ref converter, options);
            if (dest_conv != null)
                ResolveConversionMethod(expr, type, dest_conv, ref converter, options);
        }

        internal static void ResolveConversionMethod(Expr expr, TypeSymbol type, Symbol conv, ref MethodSymbol converter, BindOptions options)
        {
            if (conv is MethodSymbol)
            {
                if (CheckConversionMethod(expr, type, (MethodSymbol)conv, options))
                    converter = (MethodSymbol)conv;
            }
            else if ((conv as SymbolList)?.SymbolTypes.HasFlag(MemberTypes.Method) == true)
            {
                foreach (MethodSymbol m in ((SymbolList)conv).Symbols)
                    if (m != null)
                {
                        if (CheckConversionMethod(expr, type, m, options))
                            converter = m;
                    }
            }
        }

        static bool CheckConversionMethod(Expr expr, TypeSymbol type, MethodSymbol m, BindOptions options)
        {
            var method = m.Method;
            if (!m.Method.IsStatic || (options.HasFlag(BindOptions.Special) && !m.Method.IsSpecialName))
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

        internal static ConversionSymbol ResolveUsualConversion(Expr expr, TypeSymbol type, BindOptions options)
        {
            if (expr.Datatype.NativeType == NativeType.Usual)
            {
                if (type.NativeType == NativeType.Object)
                {
                    if (options.HasFlag(BindOptions.BoxUsual))
                    {
                        return ConversionSymbol.Create(ConversionKind.Boxing);
                    }
                    else
                    {
                        MethodSymbol converter = null;
                        ResolveConversionMethod(expr, type, Compilation.Get(NativeType.Usual).Lookup(XSharpFunctionNames.ToObject), ref converter, options);
                        if (converter != null)
                            return ConversionSymbol.Create(ConversionKind.ImplicitUserDefined, converter);
                    }
                }
                else
                {
                    MethodSymbol converter = null;
                    ResolveConversionMethod(expr, Compilation.Get(NativeType.Object), Compilation.Get(NativeType.Usual).Lookup(XSharpFunctionNames.ToObject), ref converter, options);
                    if (converter != null)
                    {
                        var inner = ConversionSymbol.Create(ConversionKind.ImplicitUserDefined, converter);
                        var outer = type.IsReferenceType ? ConversionSymbol.Create(ConversionKind.ExplicitReference)
                            : type.IsValueType ? ConversionSymbol.Create(ConversionKind.Unboxing)
                            : ConversionSymbol.Create(ConversionKind.NoConversion);
                        if (outer.Exists)
                            return ConversionSymbol.Create(outer, inner);
                    }
                }
            }
            return null;
        }

        internal static ConversionSymbol ResolveByRefConversion(Expr expr, TypeSymbol type, BindOptions options)
        {
            if (type.IsByRef && TypesMatch(expr.Datatype, type.ElementType))
            {
                return ConversionSymbol.CreateByRef();
            }
            else
            {
                var inner = ConversionSymbol.Create(ConversionKind.Deref);
                var outer = Conversion(TypeConversion.Bound(expr, expr.Datatype.ElementType, inner), type, options);
                if (outer.Exists)
                {
                    return ConversionSymbol.Create(outer, inner);
                }
            }
            return null;
        }

        internal static ConversionSymbol ResolveEnumConversion(Expr expr, TypeSymbol type, BindOptions options)
        {
            if (expr.Datatype.IsEnum)
            {
                if (TypesMatch(expr.Datatype.EnumUnderlyingType, type))
                {
                    return ConversionSymbol.Create(ConversionKind.ImplicitEnumeration);
                }
                else
                {
                    var inner = ConversionSymbol.Create(ConversionKind.ImplicitEnumeration);
                    var outer = Conversion(TypeConversion.Bound(expr, expr.Datatype.EnumUnderlyingType, inner), type, options);
                    if (outer.Exists)
                    {
                        return ConversionSymbol.Create(outer, inner);
                    }
                }
            }
            if (type.IsEnum && options.HasFlag(BindOptions.Explicit))
            {
                if (TypesMatch(type.EnumUnderlyingType, expr.Datatype))
                {
                    return ConversionSymbol.Create(ConversionKind.ExplicitEnumeration);
                }
                else
                {
                    var inner = Conversion(expr, type.EnumUnderlyingType, options);
                    if (inner.Exists)
                    {
                        var outer = ConversionSymbol.Create(ConversionKind.ExplicitEnumeration);
                        return ConversionSymbol.Create(outer, inner);
                    }
                }
            }
            return null;
        }

        internal static ConversionSymbol ResolveDynamicConversion(Expr expr, TypeSymbol type, BindOptions options)
        {
            if (expr.Datatype.NativeType == NativeType.Object)
            {
                var inner = Conversion(expr, Compilation.Get(NativeType.Usual), options);
                var outer = Conversion(TypeConversion.Bound(expr, Compilation.Get(NativeType.Usual), inner), type, options);
                if (outer.Exists)
                {
                    return ConversionSymbol.Create(outer, inner);
                }
            }
            return null;
        }
    }
}
