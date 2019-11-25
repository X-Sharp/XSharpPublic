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
        internal MemberSymbol BindMethodCall(Expr expr, Symbol symbol, ArgList args, out Expr self)
        {
            OverloadResult ovRes = null;

            var res = TryBindCall(expr, symbol, args, out self, ref ovRes, Options.Binding);

            if (res != null)
                return res;

            throw MethodCallBindError(expr, symbol, args, ovRes);
        }

        internal Symbol BindCtorCall(Expr expr, Symbol symbol, ArgList args)
        {
            if ((symbol as TypeSymbol).IsValueType && args.Args.Count == 0)
            {
                return new ObjectInitializerSymbol(symbol as TypeSymbol);
            }

            Expr dummySelf;

            OverloadResult ovRes = null;

            var res = TryBindCall(null, symbol.Lookup(SystemNames.CtorName), args, out dummySelf, ref ovRes, Options.Binding);

            if (res != null)
                return res;

            throw CtorCallBindError(expr, symbol, args, ovRes);
        }

        internal OverloadResult AskUserForCorrectOverload(IList<OverloadResult> matching, ArgList args)
        {
            OverloadResult res = null;
            if (Options.Resolver != null)
            {
                res = matching[0];
                var argtýpes = new System.Type[args.Args.Count];
                for (int i = 0; i < args.Args.Count; i++)
                {
                    argtýpes[i] = args.Args[i].Expr.Datatype.Type;
                }
                Symbol lhs = matching[0].Symbol;
                int result = 0;
                for (int i = 1; i < matching.Count && res != null; i++)
                {
                    var rhs = matching[i].Symbol;
                    MemberInfo m1= null, m2 = null;
                    if (lhs is ConstructorSymbol)
                    {
                        var c1 = lhs as ConstructorSymbol;
                        var c2 = rhs as ConstructorSymbol;
                        m1 = c1.MethodBase;
                        m2 = c2.MethodBase;

                    }
                    if (lhs is MethodSymbol)
                    {
                        var ms1 = lhs as MethodSymbol;
                        var ms2 = rhs as MethodSymbol;
                        m1 = ms1.MethodBase;
                        m2 = ms2.MethodBase;
                    }
                    if (lhs is PropertySymbol)
                    {
                        var p1 = lhs as PropertySymbol;
                        var p2 = rhs as PropertySymbol;
                        m1 = p1.Property;
                        m2 = p2.Property;
                    }
                    result = this.Options.Resolver(m1, m2, argtýpes);
                    switch (result)
                    {
                        case 1:
                            break;
                        case 2:
                            lhs = rhs;
                            res = matching[i];
                            break;
                        case 0:
                            res = null;
                            break;
                    }
                }
            }
            return res;
        }

        internal MemberSymbol TryBindCall(Expr expr, Symbol symbol, ArgList args, out Expr self, ref OverloadResult ovRes, BindOptions options)
        {
            self = (expr as MemberAccessExpr)?.Expr;

            bool isStatic = self == null;
            var matching = new List<OverloadResult>();

            if ((symbol as MethodSymbol)?.Method.IsStatic == isStatic || symbol is ConstructorSymbol)
            {
                CheckArguments(symbol as MemberSymbol, ((MethodBaseSymbol)symbol).Parameters, args, ref ovRes, options);
            }
            else if ((symbol as SymbolList)?.HasMethodBase == true)
            {
                var methods = symbol as SymbolList;
 
                for (int i = 0; i < methods.Symbols.Count; i++)
                {
                    var m = methods.Symbols[i];
                    if ((m as MethodSymbol)?.Method.IsStatic == isStatic || m is ConstructorSymbol)
                    {
                        CheckArguments(m as MemberSymbol, ((MethodBaseSymbol)m).Parameters, args, ref ovRes, options);
                        if (ovRes?.Exact == true)
                            break;
                        if (ovRes != null)
                        {
                            if (!matching.Contains(ovRes))
                                matching.Add(ovRes);
                            if (ovRes.Equivalent != null && !matching.Contains(ovRes.Equivalent))
                                matching.Add(ovRes.Equivalent);
                        }

                    }
                }
            }

            if (ovRes?.Unique == true)
            {
                ApplyConversions(args, ovRes);
                return ovRes.Symbol;
            }
            if (matching.Count > 1 && ovRes.Valid && Options.Resolver != null)
            {
                var res = AskUserForCorrectOverload(matching, args);
                if (res != null)
                {
                    ovRes = res;
                    ApplyConversions(args, ovRes);
                    return ovRes.Symbol;
                }
            }
            if (options.HasFlag(BindOptions.AllowDynamic) && expr != null && ovRes?.Valid != true)
            {
                if (symbol is DynamicExprSymbol symbolExpr)
                {
                    expr = self;
                    self = null;
                    Convert(ref expr, Compilation.Get(NativeType.Usual), options);
                    var obj = new Arg(expr ?? LiteralExpr.Bound(Constant.Create(null)));
                    var name = new Arg(symbolExpr.Name);
                    var arguments = new Arg(LiteralArray.Bound(args.Args));
                    args.Args.Clear();
                    args.Args.Add(obj);
                    args.Args.Add(name);
                    args.Args.Add(arguments);
                    return Compilation.Get(WellKnownMembers.XSharp_RT_Functions___InternalSend);
                }
                else if (symbol is DynamicSymbol symbolDynamic)
                {
                    expr = self;
                    self = null;
                    Convert(ref expr, Compilation.Get(NativeType.Usual), options);
                    var obj = new Arg(expr ?? LiteralExpr.Bound(Constant.Create(null)));
                    var name = new Arg(LiteralExpr.Bound(Constant.Create(symbolDynamic.Name)));
                    var arguments = new Arg(LiteralArray.Bound(args.Args));
                    args.Args.Clear();
                    args.Args.Add(obj);
                    args.Args.Add(name);
                    args.Args.Add(arguments);
                    return Compilation.Get(WellKnownMembers.XSharp_RT_Functions___InternalSend);
                }
                else if (symbol.Type()?.IsUsualOrObject() == true)
                {
                    self = null;
                    Convert(ref expr, Compilation.Get(NativeType.Usual), options);
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

        static void CheckArguments(MemberSymbol symbol, ParameterListSymbol paramList, ArgList args, ref OverloadResult ovRes, BindOptions options)
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
                    ovr.ArgConversion(p, ArgumentConversion(args.Args[p], parameters[p], options));
                }
                if (missingArgs > 0)
                {
                    for (int p = fixedArgs; p < fixedArgs + missingArgs; p++)
                    {
                        ovr.ArgConversion(p, ArgumentConversion(null, parameters[p], options));
                    }
                }
                else if (paramList.HasParamArray)
                {
                    var varArgType = FindType(parameters[fixedArgs].ParameterType.GetElementType());
                    for (int p = fixedArgs; p < fixedArgs + varArgs; p++)
                    {
                        ovr.ArgConversion(p, VarArgumentConversion(args.Args[p], varArgType, options));
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
                if (conv is ConversionSymbolToConstant)
                    Convert(ref args.Args[i].Expr, FindType(parameters[i].ParameterType), BindOptions.Default);
            }
            if (ovRes.MissingArgs > 0)
            {
                for (int i = ovRes.FixedArgs; i < ovRes.FixedArgs + ovRes.MissingArgs; i++)
                {
                    var conv = ovRes.Conversions[i];
                    var a = new Arg(LiteralExpr.Bound(((ConversionSymbolToConstant)conv).Constant));
                    Convert(ref a.Expr, FindType(parameters[i].ParameterType), BindOptions.Default);
                    args.Args.Add(a);
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
