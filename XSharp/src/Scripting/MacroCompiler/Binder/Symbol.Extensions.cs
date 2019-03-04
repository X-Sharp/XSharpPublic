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
                        methods = false;
                }
                return methods;
            }
            return false;
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
