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
            s_dictionary = new ConcurrentDictionary<string, XSharpTargetDLL>(XSharpString.Comparer);
        }

        public static bool IsFunctionsClass(this NamedTypeSymbol type)
        {
            if (type.IsStatic && type.Name.EndsWith(XSharpSpecialNames.FunctionsClass))
            {
                if (type is PENamedTypeSymbol || type is SourceNamedTypeSymbol)
                    return type.HasCompilerGeneratedAttribute;
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
            return IsUsualType(type) || (type is { } && type.IsObjectType()) || type.GetSpecialTypeSafe().IsNumericType();

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
            return type is { } && type.Name == OurTypeNames.ArrayBase;
        }
        internal static bool IsFloatType(this TypeSymbol type)
        {
            return type is { } && (type.Name == OurTypeNames.FloatType || type.Name == OurTypeNames.VnFloatType);
        }
        internal static bool IsFoxArrayType(this TypeSymbol type)
        {
            return type is { } && type.Name == OurTypeNames.FoxArrayType;
        }
        internal static bool IsCurrencyType(this TypeSymbol type)
        {
            return type is { } && (type.Name == OurTypeNames.CurrencyType);
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
            // TODO (nvk): there must be a better way!
            if (_type is { } && _type.OriginalDefinition is NamedTypeSymbol type)
            {
                if (type is SourceNamedTypeSymbol sourceType)
                {
                    return sourceType.IsSourceVoStructOrUnion;
                }
                if (type is Metadata.PE.PENamedTypeSymbol)
                {
                    if (type.Arity == 0 && !type.MangleName)
                    {
                        var attrs = type.GetAttributes();
                        foreach (var attr in attrs)
                        {
                            var atype = attr.AttributeClass;
                            if (atype.IsOurAttribute(OurTypeNames.VOStructAttribute))
                                return true;
                        }
                    }
                }
            }
            return false;
        }

        public static int VoStructOrUnionSizeInBytes(this TypeSymbol _type)
        {
            // TODO (nvk): there must be a better way!
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
                    else if (type is Metadata.PE.PENamedTypeSymbol)
                    {
                        var attrs = type.GetAttributes();
                        foreach (var attr in attrs)
                        {
                            var atype = attr.AttributeClass;
                            if (atype.IsOurAttribute(OurTypeNames.VOStructAttribute))
                            {
                                if (attr.ConstructorArguments != null)
                                {
                                    return attr.ConstructorArguments.First().DecodeValue<int>(SpecialType.System_Int32);
                                }
                                return 0;
                            }
                        }
                    }
                }
            }
            return 0;
        }

        public static int VoStructOrUnionLargestElementSizeInBytes(this TypeSymbol _type)
        {
            // TODO (nvk): there must be a better way!
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
                    else if (type is Metadata.PE.PENamedTypeSymbol)
                    {
                        var attrs = type.GetAttributes();
                        foreach (var attr in attrs)
                        {
                            var atype = attr.AttributeClass;
                            if (atype.IsOurAttribute(OurTypeNames.VOStructAttribute))
                            {
                                if (attr.ConstructorArguments != null)
                                {
                                    return attr.ConstructorArguments.Last().DecodeValue<int>(SpecialType.System_Int32);
                                }
                                return 0;
                            }
                        }
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


        public static bool IsRTDLL(this AssemblySymbol _asm, XSharpTargetDLL wantedTarget)
        {
            if (!s_dictionary.TryGetValue(_asm.Name, out var target))
            {
                IsRT(_asm);
                s_dictionary.TryGetValue(_asm.Name, out target);
            }
            return target == wantedTarget;
        }

        public static bool IsRT(this AssemblySymbol _asm)
        {
            if (_asm is null)  // prevent calling equals operator on AssemblySymbol
                return false;
            if (s_dictionary.TryGetValue(_asm.Name, out var target))
            {
                return target != XSharpTargetDLL.Other;
            }
            switch (_asm.Name.ToLower())
            {
                case XSharpAssemblyNames.XSharpCore:
                    s_dictionary.TryAdd(_asm.Name, XSharpTargetDLL.Core);
                    return true;
                case XSharpAssemblyNames.XSharpVO:
                    s_dictionary.TryAdd(_asm.Name, XSharpTargetDLL.VO);
                    return true;
                case XSharpAssemblyNames.XSharpRT:
                    s_dictionary.TryAdd(_asm.Name, XSharpTargetDLL.RT);
                    return true;
                case XSharpAssemblyNames.XSharpXPP:
                    s_dictionary.TryAdd(_asm.Name, XSharpTargetDLL.XPP);
                    return true;
                case XSharpAssemblyNames.XSharpVFP:
                    s_dictionary.TryAdd(_asm.Name, XSharpTargetDLL.VFP);
                    return true;
                case VulcanAssemblyNames.VulcanRT:
                    s_dictionary.TryAdd(_asm.Name, XSharpTargetDLL.VulcanRT);
                    return true;
                case VulcanAssemblyNames.VulcanRTFuncs:
                    s_dictionary.TryAdd(_asm.Name, XSharpTargetDLL.VulcanRTFuncs);
                    return true;
                default:
                    s_dictionary.TryAdd(_asm.Name, XSharpTargetDLL.Other);
                    return false;
            }
        }

          public static bool HasLateBindingAttribute(this TypeSymbol type)
        {
            while (type is { }) // prevent calling equals operator on TypeSymbol
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
