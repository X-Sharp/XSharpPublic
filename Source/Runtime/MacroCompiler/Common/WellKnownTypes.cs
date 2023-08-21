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
            XSharpQualifiedTypeNames.ClipperCallingConvention + "|" + VulcanQualifiedTypeNames.ClipperCallingConvention,
            XSharpQualifiedTypeNames.DefaultParameterAttribute + "|" + VulcanQualifiedTypeNames.DefaultParameterAttribute,
            "XSharp.Internal.VOStructAttribute|Vulcan.Internal.VOStructAttribute",
            "XSharp.Internal.ClassLibraryAttribute|Vulcan.Internal.VulcanClassLibraryAttribute",
            "XSharp.Internal.CompilerServices|Vulcan.Internal.CompilerServices",
            XSharpQualifiedTypeNames.WrappedException + "|" + VulcanQualifiedTypeNames.WrappedException,
            XSharpQualifiedTypeNames.Error + "|" + VulcanQualifiedTypeNames.Error,
            "XSharp.ImplicitNamespaceAttribute|Vulcan.VulcanImplicitNamespaceAttribute",
            "XSharp.RT.Functions|VulcanRTFuncs.Functions",
            "XSharp.Codeblock|Vulcan.Codeblock",
            "XSharp.__Float|Vulcan.__VOFloat",
            "XSharp.__Date|Vulcan.__VODate",
            "XSharp.__Symbol|Vulcan.__Symbol",
            "XSharp.__Psz|Vulcan.__Psz",
            "XSharp.__Usual|Vulcan.__Usual",
            "XSharp.__Array|Vulcan.__Array",
            "XSharp.__ArrayBase`1",
            "XSharp.__WinBool|Vulcan.__WinBool",
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
