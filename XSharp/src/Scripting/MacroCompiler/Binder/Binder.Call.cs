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
            if (parameters.Length == args.Args.Count)
            {
                var ovr = OverloadResult.Create(m, args.Args.Count);
                for (int p = 0; p < args.Args.Count; p++)
                {
                    ovr.ArgConversion(p, ArgumentConversion(args.Args[p], parameters[p]));
                }
                ovRes = ovr.Better(ovRes);
            }
        }

        static void ApplyConversions(ArgList args, OverloadResult ovRes)
        {
            var parameters = ovRes.Method.Method.GetParameters();
            for (int i = 0; i < args.Args.Count; i++)
            {
                var conv = ovRes.Conversions[i];
                if (conv.Kind != ConversionKind.Identity)
                    Convert(ref args.Args[i].Expr, FindType(parameters[i].ParameterType), conv);
            }
        }
    }
}