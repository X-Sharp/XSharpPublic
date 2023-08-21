//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

namespace Microsoft.CodeAnalysis
{
    internal static class XsHelpers
    {
        internal static string ReplaceTypeNames(string result)
        {
            result = result.Replace("void*", "ptr");
            if (result.IndexOf("XSharp.") >= 0)
            {
                result = result.Replace("XSharp.__Usual", "usual");
                result = result.Replace("XSharp.__Date", "date");
                result = result.Replace("XSharp.__Float", "float");
                result = result.Replace("XSharp.__ArrayBase", "array of");
                result = result.Replace("XSharp.__Array", "array");
                result = result.Replace("XSharp.__FoxArray", "foxarray");
                result = result.Replace("XSharp.__Currency", "currency");
                result = result.Replace("XSharp.__Binary", "binary");
                result = result.Replace("XSharp.__Symbol", "symbol");
                result = result.Replace("XSharp.__Psz", "psz");
                result = result.Replace("XSharp.Codeblock", "codeblock");
            }
            if (result.IndexOf("Vulcan.") >= 0)
            {
                result = result.Replace("Vulcan.__Usual", "usual");
                result = result.Replace("Vulcan.__VODate", "date");
                result = result.Replace("Vulcan.__VOFloat", "float");
                result = result.Replace("Vulcan.__Array", "array");
                result = result.Replace("Vulcan.__Symbol", "symbol");
                result = result.Replace("Vulcan.__Psz", "psz");
                result = result.Replace("Vulcan.Codeblock", "codeblock");
            }
            return result;
        }

        internal static string GetSpecialTypeName(INamedTypeSymbol symbol)
        {
            switch (symbol.SpecialType)
            {
                case SpecialType.System_Void:
                    return "void";
                case SpecialType.System_SByte:
                    return "sbyte";
                case SpecialType.System_Int16:
                    return "short";
                case SpecialType.System_Int32:
                    return "int";
                case SpecialType.System_Int64:
                    return "int64";
                case SpecialType.System_IntPtr when symbol.IsNativeIntegerType:
                    return "nint";
                case SpecialType.System_UIntPtr when symbol.IsNativeIntegerType:
                    return "nuint";
                case SpecialType.System_Byte:
                    return "byte";
                case SpecialType.System_UInt16:
                    return "word";
                case SpecialType.System_UInt32:
                    return "dword";
                case SpecialType.System_UInt64:
                    return "uint64";
                case SpecialType.System_Single:
                    return "real4";
                case SpecialType.System_Double:
                    return "real8";
                case SpecialType.System_Decimal:
                    return "decimal";
                case SpecialType.System_Char:
                    return "char";
                case SpecialType.System_Boolean:
                    return "logic";
                case SpecialType.System_String:
                    return "string";
                case SpecialType.System_Object:
                    return "object";
                default:
                    return null;
            }
        }
    }
}
