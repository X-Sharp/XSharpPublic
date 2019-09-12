using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Reflection;

namespace XSharp.MacroCompiler
{
    internal static class SymbolExtensions
    {
        internal static bool IsUsualOrObject(this TypeSymbol s) => s.NativeType == NativeType.Usual || s.NativeType == NativeType.Object;

        internal static bool IsSubclassOf(this TypeSymbol ts, TypeSymbol tb) => ts.Type.IsSubclassOf(tb.Type);

        internal static bool IsMethodOrMethodGroup(this Symbol s)
        {
            if (s is MethodBaseSymbol)
                return true;
            if (s is SymbolList)
            {
                bool methods = true;
                foreach(var m in (s as SymbolList).Symbols)
                {
                    if (!(m is MethodBaseSymbol))
                    {
                        methods = false;
                        break;
                    }
                }
                return methods;
            }
            return false;
        }

        internal static bool HasMethods(this Symbol s) => (s is MethodBaseSymbol) || (s as SymbolList)?.HasMethodBase == true;

        internal static bool HasFunctions(this Symbol s)
        {
            if ((s as MethodSymbol)?.IsStatic == true)
                return true;
            if (s is SymbolList)
            {
                foreach (var m in (s as SymbolList).Symbols)
                {
                    if ((m as MethodSymbol)?.IsStatic == true)
                        return true;
                }
            }
            return false;
        }

        internal static bool IsInXSharpRuntime(this Assembly a) => a.GetName().Name.ToLower().StartsWith("xsharp.");

        internal static bool IsInXSharpRuntime(this MethodSymbol s) => s.Method.DeclaringType.Assembly.IsInXSharpRuntime();

        internal static Symbol UniqueIdent(this Symbol s)
        {
            if (s is SymbolList)
            {
                Symbol u = null;
                foreach (var m in (s as SymbolList).Symbols)
                {
                    if (!(m is MethodBaseSymbol) && !(m is TypeSymbol) && !(m is NamespaceSymbol))
                    {
                        if (u != null)
                            return null;
                        u = m;
                    }
                }
                return u;
            }
            return s;
        }

        internal static Symbol UniqueType(this Symbol s)
        {
            if (s is SymbolList)
            {
                Symbol u = null;
                foreach (var m in (s as SymbolList).Symbols)
                {
                    if (m is TypeSymbol)
                    {
                        if (u != null)
                            return s;
                        u = m;
                    }
                }
                return u;
            }
            return s;
        }

        internal static Symbol UniqueTypeOrNamespace(this Symbol s)
        {
            if (s is SymbolList)
            {
                Symbol u = null;
                foreach (var m in (s as SymbolList).Symbols)
                {
                    if (m is TypeSymbol || m is NamespaceSymbol)
                    {
                        if (u != null)
                            return s;
                        u = m;
                    }
                }
                return u;
            }
            return s;
        }

        internal static TypeSymbol Type(this Symbol s) => (s as TypedSymbol)?.Type;

        internal static string MemberName(this Symbol s)
        {
            return
                (s as MemberSymbol)?.FullName
                ?? (s as SymbolList)?.Symbols.First().MemberName();
        }

        internal static bool IsConstant(this Symbol s) => (s as Constant)?.Type.IsUsualOrObject() == false;
    }
}
