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
            bool isStatic = self == null;

            OverloadResult ovRes = null;

            if ((symbol as PropertySymbol)?.IsStatic == isStatic)
            {
                CheckArguments(symbol as PropertySymbol, (symbol as PropertySymbol).Parameters, args, ref ovRes);
            }
            else if ((symbol as SymbolList)?.HasProperty == true)
            {
                var properties = symbol as SymbolList;
                for (int i = 0; i < properties.Symbols.Count; i++)
                {
                    var p = properties.Symbols[i];
                    if ((p as PropertySymbol)?.IsStatic == isStatic)
                    {
                        CheckArguments(p as PropertySymbol, (p as PropertySymbol).Parameters, args, ref ovRes);
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
            return null;
        }
    }
}