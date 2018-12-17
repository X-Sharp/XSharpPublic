using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Reflection;

namespace XSharp.MacroCompiler
{
    using Syntax;

    internal partial class Binder
    {
        internal static ConversionSymbol ArgumentConversion(Arg arg, ParameterInfo param, BindOptions options)
        {
            if (arg == null || arg.Expr is EmptyExpr)
            {
                var type = FindType(param.ParameterType);
                if (param.HasDefaultValue && type.NativeType != NativeType.Unknown)
                {
                    return ConversionSymbol.Create(Constant.Create(param.DefaultValue, type.NativeType));
                }
                else if (param.HasDefaultValue || param.IsOptional)
                {
                    return ConversionSymbol.Create(Constant.CreateDefault(type));
                }
                else
                {
                    var defValAttr = Compilation.Get(WellKnownTypes.DefaultParameterValueAttribute);
                    foreach (var attr in param.CustomAttributes)
                    {
                        if (attr.AttributeType == defValAttr.Type)
                        {
                            int desc = attr.ConstructorArguments[1].Value as int? ?? -1;

                            var val = attr.ConstructorArguments[0];
                            switch (desc)
                            {
                                case 0:
                                    // normal .Net Object
                                    // return value  or null
                                    if (val.ArgumentType != null && val.Value != null)
                                    {
                                        var valType = FindType(val.ArgumentType);
                                        if (valType.NativeType == NativeType.Unknown)
                                        {
                                            // Enum type? can be casted to Int32
                                            return ConversionSymbol.Create(Constant.Create(val.Value, NativeType.Int32));
                                        }
                                        else
                                            return ConversionSymbol.Create(Constant.Create(val.Value, valType.NativeType));
                                    }
                                    else
                                        return ConversionSymbol.Create(Constant.Null);
                                case 1:
                                    // NIL
                                    return ConversionSymbol.Create(Constant.Nil);
                                case 2:
                                    // Date, value should be long of ticks. Return DateTime
                                    DateTime dt = new DateTime((long)val.Value);
                                    return ConversionSymbol.Create(Constant.Create(dt));
                                case 3:
                                    // Symbol, value should be a string literal or null
                                    if (val.Value == null)
                                        return ConversionSymbol.Create(Constant.Null);
                                    else
                                        return ConversionSymbol.Create(Constant.Create((string)val.Value));
                                case 4:
                                    // Psz, value should be a string or null
                                    if (val.Value == null)
                                        return ConversionSymbol.Create(Constant.Null);
                                    else
                                        return ConversionSymbol.Create(Constant.Create((string)val.Value));
                                case 5:
                                    // IntPtr, return value as IntPtr
                                    if (val.Value == null)
                                        return ConversionSymbol.Create(Constant.Null);
                                    else
                                    {
                                        int i = val.Value as int? ?? 0;
                                        IntPtr p = new IntPtr(i);
                                        return ConversionSymbol.Create(Constant.Create(p));
                                    }
                                default:
                                    return ConversionSymbol.Create(Constant.Null);
                            }
                        }
                    }
                }
                return ConversionSymbol.Create(ConversionKind.NoConversion);
            }
            var conv = Conversion(arg.Expr, FindType(param.ParameterType), options);
            return conv;
        }
        internal static ConversionSymbol VarArgumentConversion(Arg arg, TypeSymbol type, BindOptions options)
        {
            if (arg == null || arg.Expr is EmptyExpr)
            {
                return ConversionSymbol.Create(Constant.CreateDefault(type));
            }
            var conv = Conversion(arg.Expr, type, options);
            return conv;
        }
    }
}
