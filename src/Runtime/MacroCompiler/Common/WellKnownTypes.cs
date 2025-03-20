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
    internal enum WellKnownTypes
    {
        System_ValueType,
        System_Int32,
        System_UInt32,
        System_Runtime_CompilerServices_IsConst,
        System_IDisposable,
        System_Collections_IEnumerator,
        System_Collections_Generic_IEnumerator_T1,
        System_Exception,
        ClipperCallingConventionAttribute,
        DefaultParameterValueAttribute,
        XSharp_Internal_VOStructAttribute,
        XSharp_Internal_ClassLibraryAttribute,
        XSharp_Internal_CompilerServices,
        XSharp_Internal_WrappedException,
        XSharp_Error,
        ImplicitNamespaceAttribute,
        XSharp_RT_Functions,
        XSharp_Codeblock,
        XSharp___Float,
        XSharp___Date,
        XSharp___Symbol,
        XSharp___Psz,
        XSharp___Usual,
        XSharp___Array,
        XSharp___ArrayBase_T1,
        XSharp___WinBool,
    }

    public static partial class Compilation
    {
        static string[] TypeNames =
        {
            "System.ValueType",
            "System.Int32",
            "System.UInt32",
            "System.Runtime.CompilerServices.IsConst",
            "System.IDisposable",
            "System.Collections.IEnumerator",
            "System.Collections.Generic.IEnumerator`1",
            "System.Exception",
            XSharpQualifiedTypeNames.ClipperCallingConvention ,
            XSharpQualifiedTypeNames.DefaultParameterAttribute,
            "XSharp.Internal.VOStructAttribute",
            "XSharp.Internal.ClassLibraryAttribute",
            "XSharp.Internal.CompilerServices",
            XSharpQualifiedTypeNames.WrappedException ,
            XSharpQualifiedTypeNames.Error ,
            "XSharp.ImplicitNamespaceAttribute",
            "XSharp.RT.Functions",
            "XSharp.Codeblock",
            "XSharp.__Float",
            "XSharp.__Date",
            "XSharp.__Symbol",
            "XSharp.__Psz",
            "XSharp.__Usual",
            "XSharp.__Array",
            "XSharp.__ArrayBase`1",
            "XSharp.__WinBool",
        };

        static TypeSymbol[] WellKnownTypeSymbols;

        internal static void InitializeWellKnownTypes()
        {
            var typeSymbols = new TypeSymbol[TypeNames.Length];

            foreach (var m in (WellKnownTypes[])Enum.GetValues(typeof(WellKnownTypes)))
            {
                var names = TypeNames[(int)m];
                Debug.Assert(names.Replace('.', '_').Replace("`","_T").Split('|').First().Contains(m.ToString()));
                foreach (var name in names.Split('|'))
                {
                    var s = Binder.LookupFullName(name);
                    if (s == null)
                        continue;
                    Debug.Assert(s is TypeSymbol);
                    typeSymbols[(int)m] = s as TypeSymbol;
                    break;
                }
                Debug.Assert(typeSymbols[(int)m] != null);
            }

            Interlocked.CompareExchange(ref WellKnownTypeSymbols, typeSymbols, null);
        }

        internal static TypeSymbol Get(WellKnownTypes kind)
        {
            Debug.Assert(WellKnownTypeSymbols != null);
            var result = WellKnownTypeSymbols[(int)kind];
            if (result == null)
                Compilation.Error(ErrorCode.TypeNotFound, kind);
            return result;
        }
    }
}
