//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System;
using System.Linq;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.Symbols;
using Microsoft.CodeAnalysis;
using System.Collections.Concurrent;
using Microsoft.CodeAnalysis.CSharp.Symbols.Metadata.PE;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal static partial class TypeSymbolExtensions
    {
        private static readonly ConcurrentDictionary<string, XSharpTargetDLL> s_dictionary;
        static TypeSymbolExtensions()
        {
            s_dictionary = new(StringComparer.OrdinalIgnoreCase)
            {
                // XSharp
                {XSharpAssemblyNames.XSharpCore, XSharpTargetDLL.Core},
                {XSharpAssemblyNames.XSharpVO,XSharpTargetDLL.VO},
                {XSharpAssemblyNames.XSharpRT,XSharpTargetDLL.RT},
                {XSharpAssemblyNames.XSharpXPP,XSharpTargetDLL.XPP},
                {XSharpAssemblyNames.XSharpVFP,XSharpTargetDLL.VFP},
                {XSharpAssemblyNames.XSharpHarbour,XSharpTargetDLL.Harbour},
                {XSharpAssemblyNames.VoConsole,XSharpTargetDLL.VOConsoleClasses},
                {XSharpAssemblyNames.VoGui,XSharpTargetDLL.VOGuiClasses},
                {XSharpAssemblyNames.VoWin32,XSharpTargetDLL.VOWin32Api},
                {XSharpAssemblyNames.VoRdd,XSharpTargetDLL.VORDDClasses},
                {XSharpAssemblyNames.VoSql,XSharpTargetDLL.VOSQLClasses},
                {XSharpAssemblyNames.VoSystem,XSharpTargetDLL.VOSystemClasses},
                {XSharpAssemblyNames.VoReport,XSharpTargetDLL.VOReportClasses},
                {XSharpAssemblyNames.VoInet,XSharpTargetDLL.VOInternetClasses},
                // Vulcan
                {VulcanAssemblyNames.VulcanRT,XSharpTargetDLL.VulcanRT},
                {VulcanAssemblyNames.VulcanRTFuncs,XSharpTargetDLL.VulcanRTFuncs},
                {VulcanAssemblyNames.VulcanVoConsole,XSharpTargetDLL.VulcanVOConsoleClasses},
                {VulcanAssemblyNames.VulcanVoGui,XSharpTargetDLL.VulcanVOGuiClasses},
                {VulcanAssemblyNames.VulcanVoSystem,XSharpTargetDLL.VulcanVOSystemClasses},
                {VulcanAssemblyNames.VulcanVoRdd,XSharpTargetDLL.VulcanVORDDClasses},
                {VulcanAssemblyNames.VulcanVoSql,XSharpTargetDLL.VulcanVOSQLClasses},
                {VulcanAssemblyNames.VulcanVoInet,XSharpTargetDLL.VulcanVOInternetClasses},
                {VulcanAssemblyNames.VulcanVoWin32,XSharpTargetDLL.VulcanVOWin32Api}
            };
        }

        public static bool IsFunctionsClass(this TypeSymbol type)
        {
            if (type.Name.EndsWith(XSharpSpecialNames.FunctionsClass))
            {
                if (type is PENamedTypeSymbol petype)
                    return petype.HasCompilerGeneratedAttribute || petype.HasCompilerGlobalScopeAttribute;
                if (type is SourceNamedTypeSymbol sts)
                    return sts.HasCompilerGeneratedAttribute;
            }
            return false;
        }

        public static bool IsOurAttribute(this NamedTypeSymbol atype, string name)
        {
            if (atype is { })
            {
                if (atype.ContainingAssembly.IsRT())
                {
                    return XSharpString.Equals(atype.Name, name);
                }
            }
            return false;
        }
        internal static bool IsPossibleArrayIndex(this TypeSymbol type)
        {
            return IsUsualType(type) || (type is { } && type.IsObjectType()) || type.IsXNumericType();

        }

        internal static bool HasInstanceAttribute(this Symbol symbol)
        {
            return symbol is { } && symbol.GetAttributes().Any(a => a.AttributeClass.IsInstanceAttribute());
        }

        internal static bool IsInstanceAttribute(this TypeSymbol type)
        {
            return type is { } &&
                type.ContainingAssembly.IsRT() &&
                (type.Name == OurTypeNames.IsInstance || type.Name == OurTypeNames.IsVoInstance);
        }
        internal static bool IsUsualType(this TypeSymbol type)
        {
            return type is { } && type.Name == OurTypeNames.UsualType;
        }
        internal static bool IsNotUsualType(this TypeSymbol type)
        {
            return type is null || type.Name != OurTypeNames.UsualType;
        }
        public static MethodSymbol MethodSymbol(this IMethodSymbol sym)
        {
            if (sym is Symbols.PublicModel.MethodSymbol pms)
            {
                return pms.UnderlyingMethodSymbol;
            }
            else
            {
                return (MethodSymbol)sym;
            }
        }
        internal static bool IsSymbolType(this TypeSymbol type)
        {
            return type is { } && type.Name == OurTypeNames.SymbolType;
        }
        internal static bool IsNotSymbolType(this TypeSymbol type)
        {
            return type is null || type.Name != OurTypeNames.SymbolType;
        }
        internal static bool IsArrayType(this TypeSymbol type)
        {
            return type is { } && type.Name == OurTypeNames.ArrayType;
        }
        internal static bool IsArrayBaseType(this TypeSymbol type)
        {
            return type is { } && type.Name == OurTypeNames.ArrayBaseType;
        }
        internal static bool IsFloatType(this TypeSymbol type)
        {
            return type is { } && (type.Name == OurTypeNames.FloatType || type.Name == OurTypeNames.VnFloatType);
        }
        internal static SpecialType XsSpecialtype(this TypeSymbol type)
        {
            var result = type.GetSpecialTypeSafe();
            if (result == SpecialType.None)
            {
                if (type.IsFloatType())
                    result = SpecialType.System_Double;
                else if (type.IsCurrencyType())
                    result = SpecialType.System_Decimal;
            }
            return result;
        }
        internal static bool IsFoxArrayType(this TypeSymbol type)
        {
            return type is { } && type.Name == OurTypeNames.FoxArrayType;
        }
        internal static bool IsCurrencyType(this TypeSymbol type)
        {
            return type is { } && (type.Name == OurTypeNames.CurrencyType);
        }
        internal static bool IsBinaryType(this TypeSymbol type)
        {
            return type is { } && (type.Name == OurTypeNames.BinaryType);
        }
        internal static bool IsOurType(this TypeSymbol type)
        {
            if (type is { })
            {
                if (type.Name.Length > 0 && type.Name[0] == '_')
                {
                    switch (type.Name)
                    {
                        case OurTypeNames.ArrayType:
                        case OurTypeNames.ArrayBaseType:
                        case OurTypeNames.CodeBlockType:
                        case OurTypeNames.DateType:
                        case OurTypeNames.FloatType:
                        case OurTypeNames.SymbolType:
                        case OurTypeNames.PszType:
                        case OurTypeNames.UsualType:
                        case OurTypeNames.WinBoolType:
                        case OurTypeNames.WinDateType:
                        case OurTypeNames.CurrencyType:
                        case OurTypeNames.BinaryType:
                        case OurTypeNames.FoxArrayType:
                        case OurTypeNames.VnDateType:
                        case OurTypeNames.VnFloatType:
                            return true;
                    }
                }
                if (type is ArrayTypeSymbol ats)
                {
                    return ats.ElementType.IsOurType();
                }
            }
            return false;
        }

        internal static bool IsFractionalType(this TypeSymbol type)
        {
            if (type is null)
                return false;
            switch (type.GetSpecialTypeSafe())
            {
                case SpecialType.System_Decimal:
                case SpecialType.System_Double:
                case SpecialType.System_Single:
                    return true;
                default:
                    return type.IsFloatType() || type.IsCurrencyType();
            }
        }
        internal static bool IsXNumericType(this TypeSymbol type)
        {
            if (type is null)
                return false;
            if (type.IsIntegralType())
                return true;
            return type.IsFractionalType();
        }
        internal static bool IsCodeblockType(this TypeSymbol type)
        {
            return type is { } && type.Name == OurTypeNames.CodeBlockType;
        }
        internal static bool IsPszType(this TypeSymbol type)
        {
            return type is { } && type.Name == OurTypeNames.PszType;
        }
        internal static bool IsNotPszType(this TypeSymbol type)
        {
            return type is null || type.Name != OurTypeNames.PszType;
        }
        internal static bool IsWinBoolType(this TypeSymbol type)
        {
            return type is { } && type.Name == OurTypeNames.WinBoolType;
        }
        internal static bool IsWinDateType(this TypeSymbol type)
        {
            return type is { } && type.Name == OurTypeNames.WinDateType;
        }

        internal static bool IsDateType(this TypeSymbol type)
        {
            return type is { } && (type.Name == OurTypeNames.DateType || type.Name == OurTypeNames.VnDateType);
        }

        public static bool IsVoStructOrUnion(this TypeSymbol _type)
        {
            if (_type is { } && _type.OriginalDefinition is NamedTypeSymbol type)
            {
                if (type is SourceNamedTypeSymbol sourceType)
                {
                    return sourceType.IsSourceVoStructOrUnion;
                }
                if (type is PENamedTypeSymbol petype)
                {
                    return petype.IsVoStruct;
                }
            }
            return false;
        }

        public static int VoStructOrUnionSizeInBytes(this TypeSymbol _type)
        {
            if (_type is { } && _type.OriginalDefinition is NamedTypeSymbol type)
            {
                if (type is { } && type.Arity == 0 && !type.MangleName)
                {
                    if (type is SourceNamedTypeSymbol sourceType)
                    {
                        if (sourceType.IsSourceVoStructOrUnion)
                        {
                            return sourceType.VoStructSize;
                        }
                    }
                    else if (type is PENamedTypeSymbol petype && petype.IsVoStruct)
                    {
                        return petype.VoStructSizeInBytes;
                    }
                }
            }
            return 0;
        }

        public static int VoStructOrUnionLargestElementSizeInBytes(this TypeSymbol _type)
        {
            if (_type is { } && _type.OriginalDefinition is NamedTypeSymbol type)
            {
                if (type is { } && type.Arity == 0 && !type.MangleName)
                {
                    if (type is SourceNamedTypeSymbol sourceType)
                    {
                        if (sourceType.IsSourceVoStructOrUnion)
                        {
                            return sourceType.VoStructElementSize;
                        }
                    }
                    else if (type is PENamedTypeSymbol petype && petype.IsVoStruct)
                    {
                        return petype.VoStructLargestElementSize;
                    }
                }
            }
            return 0;
        }

        internal static int VoFixedBufferElementSizeInBytes(this TypeSymbol type, CSharpCompilation compilation)
        {
            int elementSize = type.SpecialType.FixedBufferElementSizeInBytes();
            if (elementSize == 0)
            {
                switch (type.Name)
                {
                    case OurTypeNames.PszType:
                        if (compilation?.Options.Platform == Platform.X86)
                            elementSize = 4;
                        else
                            elementSize = 8;
                        break;
                    case OurTypeNames.DateType:
                    case OurTypeNames.SymbolType:
                    case OurTypeNames.WinBoolType:
                    case OurTypeNames.WinDateType:
                        elementSize = 4;
                        break;
                    default:
                        if (type.SpecialType == SpecialType.System_IntPtr ||
                            type.SpecialType == SpecialType.System_UIntPtr ||
                            type.IsPointerType())
                            if (compilation?.Options.Platform == Platform.X86)
                                elementSize = 4;
                            else
                                elementSize = 8;
                        break;

                }
                if (elementSize == 0)
                {
                    elementSize = type.VoStructOrUnionSizeInBytes();
                }
            }
            return elementSize;
        }
        /// <summary>
        /// The assembly is one of our Dialect Specific Runtime DLLs
        /// </summary>
        /// <param name="asm"></param>
        /// <returns></returns>
        public static bool IsDialectSpecificDLL(this AssemblySymbol asm)
        {
            XSharpTargetDLL target = XSharpTargetDLL.Other;
            if (asm is { } && s_dictionary.TryGetValue(asm.Name, out target))
            {
                switch (target)
                {
                    case XSharpTargetDLL.VO:
                    case XSharpTargetDLL.XPP:
                    case XSharpTargetDLL.VFP:
                    case XSharpTargetDLL.Harbour:
                        return true;
                }
            }
            return false;
        }

        /// <summary>
        /// The assembly is one of our Runtime DLLs or one of the VO SDK DLLs(ours or Vulcans)
        /// </summary>
        /// <param name="asm"></param>
        /// <param name="wantedTarget"></param>
        /// <returns></returns>
        public static bool IsRTDLL(this AssemblySymbol asm, XSharpTargetDLL wantedTarget)
        {
            XSharpTargetDLL target = XSharpTargetDLL.Other;
            if (asm is { } && s_dictionary.TryGetValue(asm.Name, out target))
            {
                return target == wantedTarget;
            }
            return false;
        }
        /// <summary>
        /// The assembly is one of our Runtime DLLs(ours or Vulcans)
        /// </summary>
        /// <param name="target"></param>
        /// <returns></returns>
        public static bool IsRt(this XSharpTargetDLL target)
        {
            switch (target)
            {
                case XSharpTargetDLL.Core:
                case XSharpTargetDLL.RDD:
                case XSharpTargetDLL.RT:
                case XSharpTargetDLL.VO:
                case XSharpTargetDLL.Data:
                case XSharpTargetDLL.XPP:
                case XSharpTargetDLL.VFP:
                case XSharpTargetDLL.Harbour:
                case XSharpTargetDLL.RTDebugger:
                case XSharpTargetDLL.VulcanRT:
                case XSharpTargetDLL.VulcanRTFuncs:
                    return true;
            }
            return false;
        }
        /// <summary>
        /// The assembly is one of the VO SDK DLLs (ours or Vulcans)
        /// </summary>
        /// <param name="target"></param>
        /// <returns></returns>
        public static bool IsSdk(this XSharpTargetDLL target)
        {
            switch (target)
            {
                case XSharpTargetDLL.VOWin32Api:
                case XSharpTargetDLL.VOSystemClasses:
                case XSharpTargetDLL.VORDDClasses:
                case XSharpTargetDLL.VOSQLClasses:
                case XSharpTargetDLL.VOGuiClasses:
                case XSharpTargetDLL.VOInternetClasses:
                case XSharpTargetDLL.VOConsoleClasses:
                case XSharpTargetDLL.VOReportClasses:
                case XSharpTargetDLL.VulcanVOWin32Api:
                case XSharpTargetDLL.VulcanVOSystemClasses:
                case XSharpTargetDLL.VulcanVORDDClasses:
                case XSharpTargetDLL.VulcanVOSQLClasses:
                case XSharpTargetDLL.VulcanVOGuiClasses:
                case XSharpTargetDLL.VulcanVOInternetClasses:
                case XSharpTargetDLL.VulcanVOConsoleClasses:
                case XSharpTargetDLL.VulcanVOReportClasses:
                    return true;
            }
            return false;
        }
        public static bool IsRT(this AssemblySymbol _asm)
        {
            if (_asm is { } && s_dictionary.TryGetValue(_asm.Name, out var target))
            {
                return target.IsRt();
            }
            return false;
        }

        public static bool IsSdk(this AssemblySymbol _asm)
        {
            if (_asm is { } && s_dictionary.TryGetValue(_asm.Name, out var target))
            {
                return target.IsSdk();
            }

            return false;
        }

        public static bool HasLateBindingAttribute(this TypeSymbol type)
        {
            while (type is { })
            {
                var attrs = type.GetAttributes();
                foreach (var attr in attrs)
                {
                    var atype = attr.AttributeClass;
                    if (atype.IsOurAttribute(OurTypeNames.AllowLateBindingAttribute))
                        return true;
                }
                type = type.BaseTypeNoUseSiteDiagnostics;
            }
            return false;

        }

        private static MethodSymbol GetConstructor(this TypeSymbol type, TypeSymbol paramType)
        {
            return (MethodSymbol)type.GetMembers(".ctor").Where(m => m.GetParameterCount() == 1 &&
                                                TypeSymbol.Equals(m.GetParameterTypes()[0].Type, paramType)).FirstOrDefault();
        }

        public static bool HasMembers(this TypeSymbol type, string name)
        {
            TypeSymbol sym;
            sym = type;
            while (sym is { })
            {
                if (sym.GetMembers(name).Length > 0)
                    return true;
                sym = sym.BaseTypeNoUseSiteDiagnostics;
            }
            return false;
        }

        public static bool NeedAccessToLocals(this MethodSymbol method, out bool writeAccess)
        {
            var attrs = method.GetAttributes();
            writeAccess = false;
            foreach (var attr in attrs)
            {
                var atype = attr.AttributeClass;
                if (atype is { } && atype.Name == OurTypeNames.NeedAccessToLocals)
                {
                    if (attr.ConstructorArguments.Count() == 1)
                    {
                        writeAccess = attr.ConstructorArguments.First().DecodeValue<bool>(SpecialType.System_Boolean);
                    }
                    else
                    {
                        writeAccess = true;
                    }
                    return true;
                }
            }
            return false;
        }

        public static bool TypesChanged(this TypeSymbol type)
        {
            // Disabled for now because we found a recursive loop when compiling Xs2Ado:
            // With 2 types with each the [DefaultMember(..)] attribute
            // and an override in the subclass.
            // then the check for TypesChanged will go into an infinite loop because this gets called over and over again from
            // the member signature comparer
            if (type is null)
                return false;
            //var attrs = type.GetAttributes();
            //foreach (var attr in attrs)
            //{
            //    var atype = attr.AttributeClass;
            //    if (atype is { } && atype.Name == OurTypeNames.TypesChanged)
            //    {
            //        return true;
            //    }
            //}
            //var bt = type.BaseTypeNoUseSiteDiagnostics;
            //if (bt is { } && bt.SpecialType != SpecialType.System_Object)
            //    return bt.TypesChanged();
            return false;
        }

        public static string GetDisplayName(this TypeSymbol type)
        {
            string strType = type.ToString();
            switch (strType.ToLower())
            {
                case "xsharp.__date":
                case "vulcan.__vodate":
                    strType = "date";
                    break;
                case "xsharp.__float":
                case "vulcan.__vofloat":
                    strType = "float";
                    break;
                case "xsharp.__symbol":
                case "vulcan.__symbol":
                    strType = "symbol";
                    break;
                case "xsharp.__array":
                case "xsharp.__arraybase":
                case "vulcan.__array":
                    strType = "array";
                    break;
                case "xsharp.__psz":
                case "vulcan.__psz":
                    strType = "psz";
                    break;
                case "xsharp.__usual":
                case "vulcan.__usual":
                    strType = "usual";
                    break;
                case "xsharp.__currency":
                    strType = "currency";
                    break;
                case "xsharp.__binary":
                    strType = "binary";
                    break;
                default:
                    // just display the type                        
                    break;
            }
            return strType;
        }
    }
}
