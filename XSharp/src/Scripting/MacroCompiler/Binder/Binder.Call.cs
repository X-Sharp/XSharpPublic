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
                self = null;
            if (expr.Symbol is MemberSymbol)
            {
                if ((expr.Symbol as MemberSymbol)?.Member.MemberType == MemberTypes.Method)
                {
                    return expr.Symbol as MemberSymbol;
                }
            }
            if ((expr.Symbol as SymbolList)?.SymbolTypes.HasFlag(MemberTypes.Method) == true)
            {
                var methods = expr.Symbol as SymbolList;
                var valids = new System.Collections.BitArray(methods.Symbols.Count);
                MemberSymbol validMethod = null;
                int numValid = 0;
                for (int i = 0; i<methods.Symbols.Count; i++)
                {
                    var m = methods.Symbols[i];
                    if (m is MethodSymbol)
                    {
                        var method = (m as MethodSymbol).Method;
                        var parameters = method.GetParameters();
                        var returnparam = method.ReturnParameter;
                        if (parameters.Length == args.Args.Count)
                        {
                            bool v = true;
                            for (int p = 0; p<parameters.Length; p++)
                            {
                                if (parameters[p].ParameterType != args.Args[p].Expr.Datatype)
                                {
                                    v = false;
                                    break;
                                }
                            }
                            if (v)
                            {
                                valids[i] = true;
                                numValid += 1;
                                if (validMethod == null)
                                    validMethod = (MethodSymbol)m;
                            }
                        }
                    }
                }
                if (numValid == 1)
                    return validMethod;
            }
            return null;
        }
    }
}