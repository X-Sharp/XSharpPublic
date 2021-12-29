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
        internal MemberSymbol BindMethodCall(Expr expr, Symbol symbol, ArgList args, out Expr self, out Expr writeBack)
        {
            OverloadResult ovRes = null;

            var res = TryBindCall(expr, symbol, args, out self, out writeBack, ref ovRes, Options.Binding);

            if (res != null)
                return res;

            throw MethodCallBindError(expr, symbol, args, ovRes);
        }

        internal Symbol BindCtorCall(Expr expr, Symbol symbol, ArgList args, out Expr writeBack)
        {
            writeBack = null;

            if ((symbol as TypeSymbol).IsValueType && args.Args.Count == 0)
            {
                return new ObjectInitializerSymbol(symbol as TypeSymbol);
            }

            OverloadResult ovRes = null;

            var res = TryBindCall(null, symbol.Lookup(SystemNames.CtorName), args, out var _, out writeBack, ref ovRes, Options.Binding);

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
                var argtypes = new System.Type[args.Args.Count];
                for (int i = 0; i < args.Args.Count; i++)
                {
                    argtypes[i] = args.Args[i].Expr.Datatype.Type;
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
                    result = this.Options.Resolver(m1, m2, argtypes);
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

        internal MemberSymbol TryBindCall(Expr expr, Symbol symbol, ArgList args, out Expr self, out Expr writeBack, ref OverloadResult ovRes, BindOptions options)
        {
            self = (expr as MemberAccessExpr)?.Expr;
            writeBack = null;

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
                ApplyConversions(args, ovRes, out writeBack);
                return ovRes.Symbol;
            }
            // Add automatic overload resolution
            if (ovRes != null && ! ovRes.Unique && ovRes.Equivalent != null)
            {
                var ov1 = ovRes.Symbol;
                var ov2 = ovRes.Equivalent.Symbol;
                var func1 = ov1.DeclaringType.FullName;
                var func2 = ov2.DeclaringType.FullName;

                if (func1 != func2 && func1.StartsWith("XSharp") && func2.StartsWith("XSharp"))
                {
                    switch (func1)
                    {
                        case XSharpSpecialNames.XSharpVOFunctionsClass:
                        case XSharpSpecialNames.XSharpDataFunctionsClass:
                        case XSharpSpecialNames.XSharpVFPFunctionsClass:
                        case XSharpSpecialNames.XSharpXPPFunctionsClass:
                            // functions in specific runtime files override generic files
                            return ovRes.Symbol;
                        case XSharpSpecialNames.XSharpRTFunctionsClass:
                            if (func2 == XSharpSpecialNames.XSharpCoreFunctionsClass)
                                // functions in specific runtime files override generic files
                                return ovRes.Symbol;
                            break;
                    }
                    switch (func2)
                    {
                        case XSharpSpecialNames.XSharpVOFunctionsClass:
                        case XSharpSpecialNames.XSharpDataFunctionsClass:
                        case XSharpSpecialNames.XSharpVFPFunctionsClass:
                        case XSharpSpecialNames.XSharpXPPFunctionsClass:
                            // functions in specific runtime files override generic files
                            return ovRes.Equivalent.Symbol;
                        case XSharpSpecialNames.XSharpRTFunctionsClass:
                            if (func1 == XSharpSpecialNames.XSharpCoreFunctionsClass)
                                // functions in specific runtime files override generic files
                                return ovRes.Equivalent.Symbol;
                            break;
                    }
                }
            }
            if (matching.Count > 1 && ovRes.Valid && Options.Resolver != null)
            {
                var res = AskUserForCorrectOverload(matching, args);
                if (res != null)
                {
                    ovRes = res;
                    ApplyConversions(args, ovRes, out writeBack);
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
                    ApplyUsualConversions(args, out writeBack);
                    var obj = new Arg(expr ?? LiteralExpr.Bound(Constant.Create(null)));
                    var name = new Arg(symbolExpr.Name);
                    var arguments = ApplyUsualConversions(args, out writeBack);
                    args.Args.Clear();
                    args.Args.Add(obj);
                    args.Args.Add(name);
                    args.Args.Add(arguments);
                    // todo: Check for ref arguments and handle writeback from arguments array
                    return Compilation.Get(WellKnownMembers.XSharp_RT_Functions___InternalSend);
                }
                else if (symbol is DynamicSymbol symbolDynamic)
                {
                    expr = self;
                    self = null;
                    Convert(ref expr, Compilation.Get(NativeType.Usual), options);
                    var obj = new Arg(expr ?? LiteralExpr.Bound(Constant.Create(null)));
                    var name = new Arg(LiteralExpr.Bound(Constant.Create(symbolDynamic.Name)));
                    var arguments = ApplyUsualConversions(args, out writeBack);
                    args.Args.Clear();
                    args.Args.Add(obj);
                    args.Args.Add(name);
                    args.Args.Add(arguments);
                    // todo: Check for ref arguments and handle writeback from arguments array
                    return Compilation.Get(WellKnownMembers.XSharp_RT_Functions___InternalSend);
                }
                else if (symbol.Type()?.IsUsualOrObject() == true)
                {
                    self = null;
                    Convert(ref expr, Compilation.Get(NativeType.Usual), options);
                    ApplyUsualConversions(args, out writeBack);
                    var obj = new Arg(expr);
                    var name = new Arg(LiteralExpr.Bound(Constant.Create(SystemNames.DelegateInvokeName)));
                    var arguments = ApplyUsualConversions(args, out writeBack);
                    args.Args.Clear();
                    args.Args.Add(obj);
                    args.Args.Add(name);
                    args.Args.Add(arguments);
                    // todo: Check for ref arguments and handle writeback from arguments array
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
        Arg ApplyUsualConversions(ArgList args, out Expr writeBack)
        {
            writeBack = null;
            bool hasRefArgs = false;
            for (int i = 0; i < args.Args.Count; i++)
            {
                var e = args.Args[i].Expr;
                Convert(ref e, Compilation.Get(NativeType.Usual));
                if (args.Args[i].RefKind != RefKind.None)
                    hasRefArgs = true;
            }
            var arguments = new Arg(LiteralArray.Bound(args.Args));
            if (hasRefArgs)
            {
                var conv = ConversionSymbol.Create(ConversionSymbol.Create(ConversionKind.Identity), new ConversionToTemp(arguments.Expr.Datatype));
                Convert(ref arguments.Expr, arguments.Expr.Datatype, conv);
                for (int i = 0; i < args.Args.Count; i++)
                {
                    if (args.Args[i].RefKind != RefKind.None)
                        HandleVarArgWriteBack(conv, args.Args[i].Expr, i, ref writeBack);
                }
            }
            return arguments;

            void HandleVarArgWriteBack(ConversionSymbol conv, Expr e, int i, ref Expr wb)
            {
                if (e.Symbol?.HasSetAccess == true || e is AutoVarExpr || e is AliasExpr)
                {
                    // Handle writeBack
                    Expr t = IdExpr.Bound(conv.IndirectRefConversionTempLocal());
                    t = ArrayAccessExpr.Bound(t, ArgList.Bound(LiteralExpr.Bound(Constant.Create(i + 1))), this);
                    var wc = Conversion(t, e.Datatype, BindOptions.Default);
                    if (wc.Exists)
                    {
                        Convert(ref t, e.Datatype, wc);
                        SymbolExtensions.AddExpr(ref wb, AssignExpr.Bound(e, t, BindOptions.Default));
                    }
                }
            }
        }

        void ApplyConversions(ArgList args, OverloadResult ovRes, out Expr writeBack)
        {
            writeBack = null;
            var parameters = ovRes.Parameters.Parameters;
            for (int i = 0; i < ovRes.FixedArgs; i++)
            {
                var conv = ovRes.Conversions[i];
                var e = args.Args[i].Expr;
                if (conv.Kind != ConversionKind.Identity)
                    Convert(ref args.Args[i].Expr, FindType(parameters[i].ParameterType), conv);
                if (conv is ConversionSymbolToConstant)
                    Convert(ref args.Args[i].Expr, FindType(parameters[i].ParameterType), BindOptions.Default);
                HandleArgWriteBack(conv, e, ref writeBack);
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
                bool hasRefArgs = false;
                for (int i = ovRes.FixedArgs; i < ovRes.FixedArgs + ovRes.VarArgs; i++)
                {
                    var conv = ovRes.Conversions[i];
                    var e = args.Args[i].Expr;
                    Convert(ref e, varArgType, conv);
                    varArgs.Add(e);
                    if (args.Args[i].RefKind != RefKind.None)
                        hasRefArgs = true;
                }
                var varArg = new Arg(LiteralArray.Bound(varArgs, varArgType));
                if (hasRefArgs)
                {
                    var conv = ConversionSymbol.Create(ConversionSymbol.Create(ConversionKind.Identity), new ConversionToTemp(varArg.Expr.Datatype));
                    Convert(ref varArg.Expr, varArg.Expr.Datatype, conv);
                    for (int i = ovRes.FixedArgs; i < ovRes.FixedArgs + ovRes.VarArgs; i++)
                    {
                        if (args.Args[i].RefKind != RefKind.None)
                            HandleVarArgWriteBack(conv, args.Args[i].Expr, i-ovRes.FixedArgs, ref writeBack);
                    }
                }
                while (args.Args.Count > ovRes.FixedArgs) args.Args.RemoveAt(args.Args.Count - 1);
                args.Args.Add(varArg);
            }

            void HandleArgWriteBack(ConversionSymbol conv, Expr e, ref Expr wb)
            {
                if (conv.IsIndirectRefConversion())
                {
                    if (e.Symbol?.HasSetAccess == true || e is AutoVarExpr || e is AliasExpr)
                    {
                        // Handle writeBack
                        Expr t = IdExpr.Bound(conv.IndirectRefConversionTempLocal());
                        var wc = Conversion(t, e.Datatype, BindOptions.Default);
                        if (wc.Exists)
                        {
                            Convert(ref t, e.Datatype, wc);
                            SymbolExtensions.AddExpr(ref wb, AssignExpr.Bound(e, t, BindOptions.Default));
                        }
                    }
                }
            }
            void HandleVarArgWriteBack(ConversionSymbol conv, Expr e, int i, ref Expr wb)
            {
                if (e.Symbol?.HasSetAccess == true || e is AutoVarExpr || e is AliasExpr)
                {
                    // Handle writeBack
                    Expr t = IdExpr.Bound(conv.IndirectRefConversionTempLocal());
                    t = ArrayAccessExpr.Bound(t, ArgList.Bound(LiteralExpr.Bound(Constant.Create(i+1))), this);
                    var wc = Conversion(t, e.Datatype, BindOptions.Default);
                    if (wc.Exists)
                    {
                        Convert(ref t, e.Datatype, wc);
                        SymbolExtensions.AddExpr(ref wb, AssignExpr.Bound(e, t, BindOptions.Default));
                    }
                }
            }
        }
    }
}
