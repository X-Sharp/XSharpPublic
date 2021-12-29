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
        System_String_Concat_Object,
        System_String_Equals,
        System_String_op_Equality,
        System_String_op_Inequality,
        System_String_CompareOrdinal,
        System_Object_Equals,
        System_Decimal_ctor,
        System_DateTime_ctor,
        System_Array_get_Length,
        System_IDisposable_Dispose,
        System_Threading_Monitor_Enter,
        System_Threading_Monitor_Exit,
        XSharp___Array_ctor,
        XSharp___Float_ctor,
        XSharp___Date_ctor,
        XSharp___Symbol_ctor,
        XSharp___Currency_ctor,
        XSharp___Binary_ctor,
        XSharp___Array___ArrayNew,
        XSharp_RT_Functions_POW,
        XSharp_RT_Functions___InternalSend,
        XSharp_RT_Functions_IVarGet,
        XSharp_RT_Functions_IVarPut,
        XSharp_RT_Functions___MemVarGet,
        XSharp_RT_Functions___MemVarPut,
        XSharp_RT_Functions___FieldGet,
        XSharp_RT_Functions___FieldSet,
        XSharp_RT_Functions___FieldGetWa,
        XSharp_RT_Functions___FieldSetWa,
        XSharp_RT_Functions___VarGet,
        XSharp_RT_Functions___VarPut,
        XSharp_Core_Functions_Instr,
        XSharp_RT_Functions___StringCompare,
        XSharp_RT_Functions___StringEquals,
        XSharp_RT_Functions___StringNotEquals,
        XSharp_RT_Functions___pushWorkarea,
        XSharp_RT_Functions___popWorkarea,
        XSharp_RT_Functions_Evaluate,
        XSharp_Core_Functions_Chr,
        XSharp_Internal_CompilerServices_EnterBeginSequence,
        XSharp_Internal_CompilerServices_ExitBeginSequence,
        XSharp_Error_WrapRawException,
        XSharp_RT_Functions___MemVarGetSafe,
        XSharp_RT_Functions___VarGetSafe,
        XSharp_VFP_Functions___FoxArrayAccess_1,
        XSharp_VFP_Functions___FoxArrayAccess_2,
    }

    public static partial class Compilation
    {
        static string[] MemberNames =
        {
            "System.Decimal.Zero",
            "System.String.Concat$(System.String,System.String)",
            "System.String.Concat$(System.Object,System.Object)",
            "System.String.Equals$(System.String,System.String)",
            "System.String.op_Equality$(System.String,System.String)",
            "System.String.op_Inequality$(System.String,System.String)",
            "System.String.CompareOrdinal$(System.String,System.String)",
            "System.Object.Equals$(System.Object,System.Object)",
            "System.Decimal.@ctor(System.Int32[])",
            "System.DateTime.@ctor(System.Int32,System.Int32,System.Int32,System.Int32,System.Int32,System.Int32)",
            "System.Array.get_Length",
            "System.IDisposable.Dispose",
            "System.Threading.Monitor.Enter$(System.Object,System.Boolean&)",
            "System.Threading.Monitor.Exit$(System.Object)",
            "XSharp.__Array.@ctor(XSharp.__Usual[])|Vulcan.__Array.@ctor(XSharp.__Usual[])",
            "XSharp.__Float.@ctor(System.Double,System.Int32,System.Int32)|Vulcan.__VOFloat.@ctor(System.Double,System.Int32,System.Int32)",
            "XSharp.__Date.@ctor(System.Int32,System.Int32,System.Int32)|Vulcan.__VODate.@ctor(System.Int32,System.Int32,System.Int32)",
            "XSharp.__Symbol.@ctor(System.String)|Vulcan.__Symbol.@ctor(System.String)",
            "XSharp.__Currency.@ctor(System.Decimal)",
            "XSharp.__Binary.@ctor(System.Byte[])",
            "XSharp.__Array.__ArrayNew$(System.Int32[])",
            XSharpQualifiedTypeNames.Functions+".POW|"+VulcanQualifiedTypeNames.Functions+".POW",
            XSharpQualifiedFunctionNames.InternalSend+"|"+VulcanQualifiedFunctionNames.InternalSend,
            XSharpQualifiedFunctionNames.IVarGet+"|"+VulcanQualifiedFunctionNames.IVarGet,
            XSharpQualifiedFunctionNames.IVarPut+"|"+VulcanQualifiedFunctionNames.IVarPut,
            XSharpQualifiedFunctionNames.MemVarGet+"|"+VulcanQualifiedFunctionNames.MemVarGet,
            XSharpQualifiedFunctionNames.MemVarPut+"|"+VulcanQualifiedFunctionNames.MemVarPut,
            XSharpQualifiedFunctionNames.FieldGet+"|"+VulcanQualifiedFunctionNames.FieldGet,
            XSharpQualifiedFunctionNames.FieldSet+"|"+VulcanQualifiedFunctionNames.FieldSet,
            XSharpQualifiedFunctionNames.FieldGetWa+"|"+VulcanQualifiedFunctionNames.FieldGetWa,
            XSharpQualifiedFunctionNames.FieldSetWa+"|"+VulcanQualifiedFunctionNames.FieldSetWa,
            XSharpQualifiedFunctionNames.VarGet+"|"+VulcanQualifiedFunctionNames.VarGet,
            XSharpQualifiedFunctionNames.VarPut+"|"+VulcanQualifiedFunctionNames.VarPut,
            XSharpQualifiedFunctionNames.InStr+"|"+VulcanQualifiedFunctionNames.InStr,
            XSharpQualifiedFunctionNames.StringCompare+"|"+VulcanQualifiedFunctionNames.StringCompare,
            XSharpQualifiedFunctionNames.StringEquals+"|"+VulcanQualifiedFunctionNames.StringEquals,
            XSharpQualifiedFunctionNames.StringNotEquals+"|"+VulcanQualifiedFunctionNames.StringNotEquals,
            XSharpQualifiedFunctionNames.PushWorkarea+"|"+VulcanQualifiedFunctionNames.PushWorkarea,
            XSharpQualifiedFunctionNames.PopWorkarea+"|"+VulcanQualifiedFunctionNames.PopWorkarea,
            XSharpQualifiedFunctionNames.Evaluate+"|"+VulcanQualifiedFunctionNames.Evaluate,
            XSharpQualifiedFunctionNames.Chr+"|"+VulcanQualifiedFunctionNames.Chr,
            XSharpQualifiedFunctionNames.EnterSequence+"|"+VulcanQualifiedFunctionNames.EnterSequence,
            XSharpQualifiedFunctionNames.ExitSequence+"|"+VulcanQualifiedFunctionNames.ExitSequence,
            XSharpQualifiedFunctionNames.WrapException+"|"+VulcanQualifiedFunctionNames.WrapException,
            XSharpQualifiedFunctionNames.MemVarGetSafe,
            XSharpQualifiedFunctionNames.VarGetSafe,
            XSharpQualifiedFunctionNames.FoxArrayAccess_1,
            XSharpQualifiedFunctionNames.FoxArrayAccess_2,
        };

        static MemberSymbol[] WellKnownMemberSymbols;

        static MemberSymbol FindMember(string names)
        {
            foreach (var proto in names.Split('|'))
            {
                var name = proto.Replace("$", "").Split('(').First();
                var s = Binder.LookupFullName(name.Replace("global::", "").Split('.').Select(n => n.Replace('@', '.')).ToArray());
                if (s == null)
                    continue;
                if (s is SymbolList)
                {
                    var isStatic = proto.Contains('$');
                    var args = proto.Replace(")", "").Split('(').Last().Split(',');
                    var argTypes = args.Select(x => Binder.LookupFullName(x) as TypeSymbol).ToArray();
                    s = (s as SymbolList).Symbols.Find(x => (x as MethodBaseSymbol)?.MethodBase.GetParameters().Length == args.Length
                       && (x as MethodBaseSymbol)?.MethodBase.IsStatic == isStatic
                       && (x as MethodBaseSymbol)?.MethodBase.GetParameters().All(y => args[y.Position] == "*" || y.ParameterType == argTypes[y.Position].Type) == true);
                    Debug.Assert(s is MethodBaseSymbol);
                }
                Debug.Assert(s is MemberSymbol);
                return s as MemberSymbol;
            }
            return null;
        }

        internal static void InitializeWellKnownMembers()
        {
            var memberSymbols = new MemberSymbol[MemberNames.Length];
            lock (MemberNames)
            {
                foreach (var m in (WellKnownMembers[])Enum.GetValues(typeof(WellKnownMembers)))
                {
                    var names = MemberNames[(int)m];
                    var flatname = names.Replace("global::", "").Replace('.', '_').Replace("`", "_T").Replace("$", "").Replace("@", "").Split('|', '(').First();
                    Debug.Assert(m.ToString().StartsWith(flatname));
                    memberSymbols[(int)m] = FindMember(names);
                    if (names.IndexOf("VFP") == -1 )
                    {
                        Debug.Assert(memberSymbols[(int)m] != null);
                    }
                    else if (XSharp.RuntimeState.Dialect == XSharpDialect.FoxPro)
                    {
                        Debug.Assert(memberSymbols[(int)m] != null);
                    }
                }

            }
            Interlocked.CompareExchange(ref WellKnownMemberSymbols, memberSymbols, null);
        }

        internal static MemberSymbol Get(WellKnownMembers kind)
        {
            Debug.Assert(WellKnownMemberSymbols != null);
            return WellKnownMemberSymbols[(int)kind];
        }

        // Useful for testing
        internal static bool Override(WellKnownMembers kind, string proto = null)
        {
            if (proto == null)
            {
                WellKnownMemberSymbols[(int)kind] = FindMember(MemberNames[(int)kind]);
                return true;
            }
            var name = proto.Replace("$", "").Split('(').First();
            var s = Binder.LookupName(name);
            if (s == null)
                return false;
            if (s is SymbolList)
            {
                var isStatic = proto.Contains('$');
                var args = proto.Replace(")", "").Split('(').Last().Split(',');
                var argTypes = args.Select(x => Binder.LookupFullName(x) as TypeSymbol).ToArray();
                s = (s as SymbolList).Symbols.Find(x => (x as MethodBaseSymbol)?.MethodBase.GetParameters().Length == args.Length
                   && (x as MethodBaseSymbol)?.MethodBase.IsStatic == isStatic
                   && (x as MethodBaseSymbol)?.MethodBase.GetParameters().All(y => args[y.Position] == "*" || y.ParameterType == argTypes[y.Position].Type) == true);
                if (!(s is MethodBaseSymbol))
                    return false;
            }
            if (!(s is MemberSymbol))
                return false;
            WellKnownMemberSymbols[(int)kind] = s as MemberSymbol;
            return true;
        }
    }
}
