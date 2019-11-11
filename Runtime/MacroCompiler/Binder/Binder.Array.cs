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
        internal MemberSymbol BindArrayAccess(Expr self, Symbol symbol, ArgList args)
        {
            ConvertArrayBase(args);

            OverloadResult ovRes = null;

            var res = TryBindArrayAccess(self, symbol, args, ref ovRes, Options.Binding);

            if (res != null)
                return res;

            throw ArrayAccessBindError(self, symbol, args, ovRes);
        }

        internal void ConvertArrayBase(ArgList args)
        {
            if (!Options.ArrayZero)
            {
                for (int i = 0; i < args.Args.Count; i++)
                {
                    args.Args[i].Expr = BinaryExpr.Bound(args.Args[i].Expr, args.Args[i].Expr.Token, LiteralExpr.Bound(Constant.Create(1)), BinaryOperatorKind.Subtraction, false, Options.Binding);
                    var expr = args.Args[i].Expr;
                    Binder.Convert(ref expr, Compilation.Get(NativeType.Int32), BindOptions.Default);
                    args.Args[i].Expr = expr;

                }
            }
        }

        internal  MemberSymbol TryBindArrayAccess(Expr self, Symbol symbol, ArgList args, ref OverloadResult ovRes, BindOptions options)
        {
            bool isStatic = self == null;
            var matching = new List<OverloadResult>();

            if ((symbol as PropertySymbol)?.IsStatic == isStatic)
            {
                CheckArguments(symbol as PropertySymbol, (symbol as PropertySymbol).Parameters, args, ref ovRes, options);
            }
            else if ((symbol as SymbolList)?.HasProperty == true)
            {
                var properties = symbol as SymbolList;
                for (int i = 0; i < properties.Symbols.Count; i++)
                {
                    var p = properties.Symbols[i];
                    if ((p as PropertySymbol)?.IsStatic == isStatic)
                    {
                        CheckArguments(p as PropertySymbol, (p as PropertySymbol).Parameters, args, ref ovRes, options);
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

            return null;
        }
    }
}
