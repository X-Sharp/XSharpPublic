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
        internal MemberSymbol BindCall(Expr self, Symbol symbol, ArgList args)
        {
            bool isStatic = self == null;

            OverloadResult ovRes = null;

            if ((symbol as MethodSymbol)?.Method.IsStatic == isStatic || symbol is ConstructorSymbol)
            {
                CheckArguments((MethodBaseSymbol)symbol, args, ref ovRes);
            }
            else if ((symbol as SymbolList)?.HasMethodBase == true)
            {
                var methods = symbol as SymbolList;
                for (int i = 0; i<methods.Symbols.Count; i++)
                {
                    var m = methods.Symbols[i];
                    if ((m as MethodSymbol)?.Method.IsStatic == isStatic || m is ConstructorSymbol)
                    {
                        CheckArguments((MethodBaseSymbol)m, args, ref ovRes);
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

        static void CheckArguments(MethodBaseSymbol m, ArgList args, ref OverloadResult ovRes)
        {
            var method = m.MethodBase;
            var parameters = method.GetParameters();
            var nParams = parameters.Length;
            var fixedArgs = args.Args.Count;
            var varArgs = 0;
            var missingArgs = 0;
            bool hasExtraArgs = false;
            if (nParams <= fixedArgs)
            {
                if (m.HasParamArray)
                {
                    varArgs = fixedArgs - (nParams - 1);
                    fixedArgs = nParams - 1;
                }
                else if (nParams < fixedArgs)
                    hasExtraArgs = true;
            }
            else if (nParams > fixedArgs)
            {
                if (m.HasParamArray)
                    missingArgs = nParams - fixedArgs - 1;
                else
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
                    for (int p = fixedArgs; p < fixedArgs + missingArgs; p++)
                    {
                        ovr.ArgConversion(p, ArgumentConversion(null, parameters[p]));
                    }
                }
                else if (m.HasParamArray)
                {
                    var varArgType = FindType(parameters[fixedArgs].ParameterType.GetElementType());
                    for (int p = fixedArgs; p < fixedArgs + varArgs; p++)
                    {
                        ovr.ArgConversion(p, VarArgumentConversion(args.Args[p], varArgType));
                    }
                }
                ovRes = ovr.Better(ovRes);
            }
        }

        static void ApplyConversions(ArgList args, OverloadResult ovRes)
        {
            var parameters = ovRes.Method.MethodBase.GetParameters();
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
            else if (ovRes.Method.HasParamArray)
            {
                var varArgs = new List<Expr>(ovRes.VarArgs);
                var varArgType = FindType(parameters[ovRes.FixedArgs].ParameterType.GetElementType());
                for (int i = ovRes.FixedArgs; i < ovRes.FixedArgs + ovRes.VarArgs; i++)
                {
                    var conv = ovRes.Conversions[i];
                    Convert(ref args.Args[i].Expr, varArgType, conv);
                    varArgs.Add(args.Args[i].Expr);
                }
                while (args.Args.Count > ovRes.FixedArgs) args.Args.RemoveAt(args.Args.Count - 1);
                args.Args.Add(new Arg(LiteralArray.Bound(varArgs, varArgType)));
            }
        }
    }
}