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
    }

    public static partial class Compilation
    {
        static string[] MemberNames =
        {
            "System.Decimal.Zero",
        };

        static MemberSymbol[] WellKnownMemberSymbols;

        internal static void InitializeWellKnownMembers()
        {
            var memberSymbols = new MemberSymbol[MemberNames.Length];

            foreach (var m in (WellKnownMembers[])Enum.GetValues(typeof(WellKnownMembers)))
            {
                var name = MemberNames[(int)m];
                Debug.Assert(name.Replace('.', '_') == m.ToString());
                memberSymbols[(int)m] = Binder.Lookup(name) as MemberSymbol;
                Debug.Assert(memberSymbols[(int)m] != null);
            }

            Interlocked.CompareExchange(ref WellKnownMemberSymbols, memberSymbols, null);
        }

        internal static MemberSymbol GetMember(WellKnownMembers kind)
        {
            if (WellKnownMemberSymbols == null)
            {
                InitializeWellKnownMembers();
            }
            return WellKnownMemberSymbols[(int)kind];
        }
    }
}