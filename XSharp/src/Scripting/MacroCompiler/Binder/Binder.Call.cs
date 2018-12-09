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
        internal MemberSymbol BindMethodCall(Expr expr, Symbol symbol, ArgList args, out Expr self, bool allowDynamic = true)
        {
            OverloadResult ovRes = null;

            var res = TryBindCall(expr, symbol, args, out self, ref ovRes, allowDynamic);

            if (res != null)
                return res;

            throw MethodCallBindError(expr, symbol, args, ovRes);
        }
        internal void BindCtorCall(int i) { }

        internal Symbol BindCtorCall(Expr expr, Symbol symbol, ArgList args, bool allowDynamic = true)
        {
            if ((symbol as TypeSymbol).IsValueType && args.Args.Count == 0)
            {
                return new ObjectInitializerSymbol(symbol as TypeSymbol);
            }

            Expr dummySelf;

            OverloadResult ovRes = null;

            var res = TryBindCall(null, symbol.Lookup(SystemNames.CtorName), args, out dummySelf, ref ovRes, allowDynamic);

            if (res != null)
                return res;

            throw CtorCallBindError(expr, symbol, args, ovRes);
        }

        internal MemberSymbol TryBindCall(Expr expr, Symbol symbol, ArgList args, out Expr self, ref OverloadResult ovRes, bool allowDynamic = true)
        {
            self = (expr as MemberAccessExpr)?.Expr;

            bool isStatic = self == null;

            if ((symbol as MethodSymbol)?.Method.IsStatic == isStatic || symbol is ConstructorSymbol)
            {
                CheckArguments(symbol as MemberSymbol, ((MethodBaseSymbol)symbol).Parameters, args, ref ovRes);
            }
            else if ((symbol as SymbolList)?.HasMethodBase == true)
            {
                var methods = symbol as SymbolList;
                for (int i = 0; i<methods.Symbols.Count; i++)
                {
                    var m = methods.Symbols[i];
                    if ((m as MethodSymbol)?.Method.IsStatic == isStatic || m is ConstructorSymbol)
                    {
                        CheckArguments(m as MemberSymbol, ((MethodBaseSymbol)m).Parameters, args, ref ovRes);
                        if (ovRes?.Exact == true)
                            break;
                    }
                }
            }

            if (ovRes?.Unique == true)
            {
                ApplyConversions(args, ovRes);
                return ovRes.Symbol;
            }

            if (allowDynamic && expr != null && ovRes?.Valid != true)
            {
                if (symbol is DynamicSymbol)
                {
                    expr = self;
                    self = null;
                    Convert(ref expr, Compilation.Get(NativeType.Usual));
                    var obj = new Arg(expr ?? LiteralExpr.Bound(Constant.Create(null)));
                    var name = new Arg(LiteralExpr.Bound(Constant.Create((symbol as DynamicSymbol).Name)));
                    var arguments = new Arg(LiteralArray.Bound(args.Args));
                    args.Args.Clear();
                    args.Args.Add(obj);
                    args.Args.Add(name);
                    args.Args.Add(arguments);
                    return Compilation.Get(WellKnownMembers.XSharp_RT_Functions___InternalSend);
                }
                else if (symbol.Type().IsUsualOrObject())
                {
                    self = null;
                    Convert(ref expr, Compilation.Get(NativeType.Usual));
                    var obj = new Arg(expr);
                    var name = new Arg(LiteralExpr.Bound(Constant.Create(SystemNames.DelegateInvokeName)));
                    var arguments = new Arg(LiteralArray.Bound(args.Args));
                    args.Args.Clear();
                    args.Args.Add(obj);
                    args.Args.Add(name);
                    args.Args.Add(arguments);
                    return Compilation.Get(WellKnownMembers.XSharp_RT_Functions___InternalSend);
                }
            }

            return null;
        }

        static void CheckArguments(MemberSymbol symbol, ParameterListSymbol paramList, ArgList args, ref OverloadResult ovRes)
        {
            var parameters = paramList.Parameters;
            var nParams = parameters.Length;
            var fixedArgs = args.Args.Count;
            var varArgs = 0;
            var missingArgs = 0;
            bool hasExtraArgs = false;
            if (nParams <= fixedArgs)
            {
                if (paramList.HasParamArray)
                {
                    varArgs = fixedArgs - (nParams - 1);
                    fixedArgs = nParams - 1;
                }
                else if (nParams < fixedArgs)
                    hasExtraArgs = true;
            }
            else if (nParams > fixedArgs)
            {
                if (paramList.HasParamArray)
                    missingArgs = nParams - fixedArgs - 1;
                else
                    missingArgs = nParams - fixedArgs;
            }
            if (!hasExtraArgs)
            {
                var ovr = OverloadResult.Create(symbol, paramList, fixedArgs, varArgs, missingArgs);
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
                else if (paramList.HasParamArray)
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
            var parameters = ovRes.Parameters.Parameters;
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
            else if (ovRes.Parameters.HasParamArray)
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
