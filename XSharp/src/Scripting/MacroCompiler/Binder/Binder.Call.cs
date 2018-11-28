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
        internal MemberSymbol BindCall(Expr expr, ArgList args, out Expr self)
        {
            if (expr is MemberAccessExpr)
            {
                self = ((MemberAccessExpr)expr).Expr;
            }
            else
            {
                self = null;
            }

            OverloadResult ovRes = null;

            if (expr.Symbol is MemberSymbol)
            {
                if (expr.Symbol is MethodSymbol)
                {
                    CheckArguments((MethodSymbol)expr.Symbol, args, ref ovRes);
                }
            }
            else if ((expr.Symbol as SymbolList)?.SymbolTypes.HasFlag(MemberTypes.Method) == true)
            {
                var methods = expr.Symbol as SymbolList;
                for (int i = 0; i<methods.Symbols.Count; i++)
                {
                    var m = methods.Symbols[i];
                    if (m is MethodSymbol)
                    {
                        CheckArguments((MethodSymbol)m, args, ref ovRes);
                        if (ovRes?.Exact == true)
                            break;
                    }
                }
            }

            if (ovRes?.Unique == true)
            {
                ApplyConversions(args, ovRes);
                return ovRes.Method;
            }
            return null;
        }

        static void CheckArguments(MethodSymbol m, ArgList args, ref OverloadResult ovRes)
        {
            var method = m.Method;
            var parameters = method.GetParameters();
            var nParams = parameters.Length;
            var fixedArgs = args.Args.Count;
            var varArgs = 0;
            var missingArgs = 0;
            bool hasExtraArgs = false;
            if (nParams < fixedArgs)
            {
                if (m.HasParamArray)
                {
                    varArgs = fixedArgs - nParams;
                    fixedArgs = nParams;
                }
                else
                    hasExtraArgs = true;
            }
            else if (nParams > fixedArgs)
            {
                missingArgs = nParams - fixedArgs;
            }
            if (!hasExtraArgs)
            {
                var ovr = OverloadResult.Create(m, fixedArgs, varArgs, missingArgs);
                for (int p = 0; p < fixedArgs; p++)
                {
                    ovr.ArgConversion(p, ArgumentConversion(args.Args[p], parameters[p]));
                }
                if (missingArgs > 0)
                {
                    for (int p = fixedArgs; p < fixedArgs+missingArgs; p++)
                    {
                        ovr.ArgConversion(p, ArgumentConversion(null, parameters[p]));
                    }
                }
                ovRes = ovr.Better(ovRes);
            }
        }

        static void ApplyConversions(ArgList args, OverloadResult ovRes)
        {
            var parameters = ovRes.Method.Method.GetParameters();
            for (int i = 0; i < ovRes.FixedArgs; i++)
            {
                var conv = ovRes.Conversions[i];
                if (conv.Kind != ConversionKind.Identity)
                    Convert(ref args.Args[i].Expr, FindType(parameters[i].ParameterType), conv);
            }
            if (ovRes.MissingArgs > 0)
            {
                for (int i = ovRes.FixedArgs; i < ovRes.FixedArgs + ovRes.MissingArgs; i++)
                {
                    var conv = ovRes.Conversions[i];
                    args.Args.Add(new Arg(LiteralExpr.Bound(((ConversionSymbolToConstant)conv).Constant)));
                }
            }
        }
    }
}