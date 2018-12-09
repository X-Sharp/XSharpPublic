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
        internal static TypeSymbol Type(this Symbol s) => (s as TypedSymbol)?.Type;
        internal static string MemberName(this Symbol s) => (s as MemberSymbol).Member.Name;
    }
}