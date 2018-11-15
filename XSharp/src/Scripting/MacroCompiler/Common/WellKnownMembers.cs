using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Reflection;
using System.Reflection.Emit;
using System.Diagnostics;

namespace XSharp.MacroCompiler
{
    internal enum WellKnownMembers
    {
        System_Decimal_Zero,
        System_String_Concat,
        System_String_Equals,
        System_String_op_Equality,
        System_String_op_Inequality,
        System_Object_Equals,
    }

    public static partial class Compilation
    {
        static string[] MemberNames =
        {
            "System.Decimal.Zero",
            "System.String.Concat",
            "System.String.Equals",
            "System.String.op_Equality",
            "System.String.op_Inequality",
            "System.Object.Equals",
        };

        static MemberSymbol[] WellKnownMemberSymbols;

        internal static void InitializeWellKnownMembers()
        {
            var memberSymbols = new MemberSymbol[MemberNames.Length];

            foreach (var m in (WellKnownMembers[])Enum.GetValues(typeof(WellKnownMembers)))
            {
                var name = MemberNames[(int)m];
                Debug.Assert(name.Replace('.', '_') == m.ToString());
                var s = Binder.Lookup(name);
                if (s is SymbolList)
                {
                    switch (m)
                    {
                        case WellKnownMembers.System_String_Concat:
                            s = (s as SymbolList).Symbols.Find(x => (x as MethodSymbol)?.Method.GetParameters().Length == 2 ? true : false);
                            Debug.Assert((s as MethodSymbol)?.Method.IsStatic == true);
                            break;
                        case WellKnownMembers.System_String_Equals:
                            s = (s as SymbolList).Symbols.Find(x =>
                                (x as MethodSymbol)?.Method.IsStatic == true
                                && (x as MethodSymbol)?.Method.GetParameters().Length == 2 
                                && (x as MethodSymbol)?.Method.GetParameters()[1].ParameterType == typeof(string)
                                ? true : false);
                            Debug.Assert((s as MethodSymbol)?.Method.IsStatic == true);
                            break;
                        case WellKnownMembers.System_Object_Equals:
                            s = (s as SymbolList).Symbols.Find(x => (x as MethodSymbol)?.Method.IsStatic == true ? true : false);
                            Debug.Assert((s as MethodSymbol)?.Method.GetParameters().Length == 2);
                            break;
                    }
                }
                Debug.Assert(s == null || s is MemberSymbol);
                memberSymbols[(int)m] = s as MemberSymbol;
                Debug.Assert(memberSymbols[(int)m] != null);
            }

            Interlocked.CompareExchange(ref WellKnownMemberSymbols, memberSymbols, null);
        }

        internal static MemberSymbol GetMember(WellKnownMembers kind)
        {
            Debug.Assert(WellKnownMemberSymbols != null);
            return WellKnownMemberSymbols[(int)kind];
        }
    }
}