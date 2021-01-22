//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.Symbols;
using Microsoft.CodeAnalysis;
using System.Collections.Concurrent;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal static partial class TypeSymbolExtensions
    {
        private static ConcurrentDictionary<string, XSharpTargetDLL> dictionary;
        static TypeSymbolExtensions()
        {
            dictionary = new ConcurrentDictionary<string, XSharpTargetDLL>(XSharpString.Comparer);
        }

        public static bool IsOurAttribute(this NamedTypeSymbol atype, string name)
        {
            if (atype.IsNull())
                return false;
            if (atype.ContainingAssembly.IsRT())
            {
                return XSharpString.Equals(atype.Name, name);
            }
            return false;

        }
        internal static bool IsUsualType(this TypeSymbol type)
        {
            return !type.IsNull() && type.Name == OurTypeNames.UsualType;
        }
        internal static bool IsNotUsualType(this TypeSymbol type)
        {
            return !IsUsualType(type);
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
            return !type.IsNull() && type.Name == OurTypeNames.SymbolType;
        }
        
        internal static bool IsNotSymbolType(this TypeSymbol type)
        {
            return !IsSymbolType(type);
        }
        internal static bool IsArrayType(this TypeSymbol type)
        {
            return !type.IsNull() && type.Name == OurTypeNames.ArrayType;
        }
        internal static bool IsFloatType(this TypeSymbol type)
        {
            return !type.IsNull() && (type.Name == OurTypeNames.FloatType || type.Name == OurTypeNames.VnFloatType);
        }
        internal static bool IsCodeblockType(this TypeSymbol type)
        {
            return !type.IsNull() && type.Name == OurTypeNames.CodeBlockType;
        }
        internal static bool IsPszType(this TypeSymbol type)
        {
            return !type.IsNull() && type.Name == OurTypeNames.PszType;
        }
        internal static bool IsNotPszType(this TypeSymbol type)
        {
            return !IsPszType(type);
        }
        internal static bool IsWinBoolType(this TypeSymbol type)
        {
            return !type.IsNull() && type.Name == OurTypeNames.WinBoolType;
        }
        public static bool IsNull(this TypeSymbol type)
        {
            return type is null;
        }
        internal static bool IsDateType(this TypeSymbol type)
        {
            return !type.IsNull() && (type.Name == OurTypeNames.DateType || type.Name == OurTypeNames.VnDateType);
        }
        public static bool IsVoStructOrUnion(this TypeSymbol _type)
        {
            // TODO (nvk): there must be a better way!
            NamedTypeSymbol type = null;
            if (!_type.IsNull())
                type = _type.OriginalDefinition as NamedTypeSymbol;
            if (type is SourceNamedTypeSymbol)
            {
                return (type as SourceNamedTypeSymbol).IsSourceVoStructOrUnion ;
            }
            if (type is Metadata.PE.PENamedTypeSymbol)
            {
                if ((object)type != null && type.Arity == 0 && !type.MangleName)
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
            return false;
        }

        public static int VoStructOrUnionSizeInBytes(this TypeSymbol _type)
        {
            // TODO (nvk): there must be a better way!
            NamedTypeSymbol type = null;
            if (!_type.IsNull())
                type = _type.OriginalDefinition as NamedTypeSymbol;
            if (!type.IsNull() && type.Arity == 0 && !type.MangleName)
            {
                if (type is SourceNamedTypeSymbol)
                {
                    var sourceType = (SourceNamedTypeSymbol)type;
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
            return 0;
        }

        public static int VoStructOrUnionLargestElementSizeInBytes(this TypeSymbol _type)
        {
            // TODO (nvk): there must be a better way!
            NamedTypeSymbol type = null;
            if (!_type.IsNull())
                type = _type.OriginalDefinition as NamedTypeSymbol;
            if (!type.IsNull() && type.Arity == 0 && !type.MangleName)
            {
                if (type is SourceNamedTypeSymbol)
                {
                    var sourceType = (SourceNamedTypeSymbol)type;
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
            return 0;
        }

        internal static int VoFixedBufferElementSizeInBytes(this TypeSymbol type)
        {
            int elementSize = type.SpecialType.FixedBufferElementSizeInBytes();
            if (elementSize == 0)
            {
                if (type.IsPszType()|| type.IsPointerType()) 
                    elementSize = 4;
                else
                    elementSize = type.VoStructOrUnionSizeInBytes();
            }
            return elementSize;
        }


        public static bool IsRTDLL(this AssemblySymbol _asm, XSharpTargetDLL wantedTarget)
        {
            XSharpTargetDLL target;
            if (!dictionary.TryGetValue(_asm.Name, out target))
            {
                IsRT(_asm);
                dictionary.TryGetValue(_asm.Name, out target);
            }
            return target == wantedTarget;
        }

        public static bool IsRT(this AssemblySymbol _asm)
        {
            if ((object) _asm == null)  // cast to object to prevent calling equals operator on AssemblySymbol
                return false;
            XSharpTargetDLL target;
            if (dictionary.TryGetValue(_asm.Name, out target))
            {
                return target != XSharpTargetDLL.Other;
            }
            switch (_asm.Name.ToLower())
            {
                case XSharpAssemblyNames.XSharpCore:
                    dictionary.TryAdd(_asm.Name, XSharpTargetDLL.Core);
                    return true;
                case XSharpAssemblyNames.XSharpVO:
                    dictionary.TryAdd(_asm.Name, XSharpTargetDLL.VO);
                    return true;
                case XSharpAssemblyNames.XSharpRT:
                    dictionary.TryAdd(_asm.Name, XSharpTargetDLL.RT);
                    return true;
                case XSharpAssemblyNames.XSharpXPP:
                    dictionary.TryAdd(_asm.Name, XSharpTargetDLL.XPP);
                    return true;
                case XSharpAssemblyNames.XSharpVFP:
                    dictionary.TryAdd(_asm.Name, XSharpTargetDLL.VFP);
                    return true;
                case VulcanAssemblyNames.VulcanRT:
                    dictionary.TryAdd(_asm.Name, XSharpTargetDLL.VulcanRT);
                    return true;
                case VulcanAssemblyNames.VulcanRTFuncs:
                    dictionary.TryAdd(_asm.Name, XSharpTargetDLL.VulcanRTFuncs);
                    return true;
                default:
                    dictionary.TryAdd(_asm.Name, XSharpTargetDLL.Other);
                    return false;
            }
        }

        public static bool HasVODefaultParameter(this ParameterSymbol param)
        {
            if ((object)param != null) // cast to object to prevent calling equals operator on ParameterSymbol
            {
                var attrs = param.GetAttributes();
                foreach (var attr in attrs)
                {
                    var atype = attr.AttributeClass;
                    if (atype.IsOurAttribute(OurTypeNames.DefaultParameterAttribute))
                        return true;
                }
            }
            return false;
        }

        public static bool HasLateBindingAttribute(this TypeSymbol type)
        {
            while ((object)type != null) // cast to object to prevent calling equals operator on TypeSymbol
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

        public static ConstantValue GetVODefaultParameter(this ParameterSymbol param)
        {
            if ((object)param != null) // cast to object to prevent calling equals operator on ParameterSymbol
            {
                var attrs = param.GetAttributes();
                foreach (var attr in attrs)
                {
                    var atype = attr.AttributeClass;
                    if (atype.IsOurAttribute(OurTypeNames.DefaultParameterAttribute))
                    {
                        int desc = attr.CommonConstructorArguments[1].DecodeValue<int>(SpecialType.System_Int32);

                        var arg = attr.CommonConstructorArguments[0];
                        switch (desc)
                        {
                            case 0:
                                // normal .Net Object
                                // return value  or null
                                if (arg.Type != null && arg.Value != null)
                                {
                                    if (arg.Type.SpecialType == SpecialType.None)
                                    {
                                        // Enum type? can be casted to Int32
                                        return ConstantValue.Create(arg.Value, SpecialType.System_Int32);
                                    }
                                    else
                                        return ConstantValue.Create(arg.Value, arg.Type.SpecialType);
                                }
                                else
                                    return ConstantValue.Null;
                            case 1:
                                // NIL
                                return ConstantValue.Null;
                            case 2:     
                                // Date, value should be long of ticks. Return DateTime
                                DateTime dt = new DateTime((long)arg.Value);
                                return ConstantValue.Create(dt);
                            case 3:
                                // Symbol, value should be a string literal or null
                                if (arg.Value == null)
                                    return ConstantValue.Null;
                                else
                                    return ConstantValue.Create((string)arg.Value);
                            case 4:     
                                // Psz, value should be a string or null
                                if (arg.Value == null)
                                    return ConstantValue.Null;
                                else
                                    return ConstantValue.Create ((string)arg.Value);
                            case 5:     
                                // IntPtr, return value as IntPtr
                                if (arg.Value == null)
                                    return ConstantValue.Null;
                                else
                                {
                                    int i = arg.DecodeValue<int>(SpecialType.System_Int32);
                                    IntPtr p = new IntPtr(i);
                                    return ConstantValue.Create(p);
                                }
                            default:
                                return ConstantValue.Null;
                        }
                    }
                }
            }
            return ConstantValue.Bad;
        }

        public static bool HasMembers(this TypeSymbol type, string name)
        {
            TypeSymbol sym;
            sym = type;
            while (!sym.IsNull())
            {
                if (sym.GetMembers(name).Length > 0)
                    return true;
                sym = sym.BaseTypeNoUseSiteDiagnostics;
            }
            return false;
        }
        public static TypeWithAnnotations GetActualType(this ParameterSymbol parameter)
        {
            var type = parameter.Type;
            if (type.SpecialType == SpecialType.System_IntPtr)
            {
                var attrs = parameter.GetAttributes();
                foreach (var attr in attrs)
                {
                    var atype = attr.AttributeClass;
                    if (atype.IsOurAttribute(OurTypeNames.ActualTypeAttribute))
                    {
                        object t = attr.CommonConstructorArguments[0].DecodeValue<object>(SpecialType.None);
                        if (t is TypeSymbol)
                        {
                            type = (TypeSymbol)t;
                            break;
                        }
                    }
                }
            }
            return TypeWithAnnotations.Create(type);
        }
        public static bool NeedAccessToLocals(this MethodSymbol method)
        {
            var attrs = method.GetAttributes();
            foreach (var attr in attrs)
            {
                var atype = attr.AttributeClass;
                if (atype.Name == OurTypeNames.NeedAccessToLocals)
                {
                    return true;
                }
            }
            return false;
        }

        public static bool TypesChanged(this TypeSymbol type)
        {
            if (type.IsNull())
                return false;
            var attrs = type.GetAttributes();
            foreach (var attr in attrs)
            {
                var atype = attr.AttributeClass;
                if (atype.Name == OurTypeNames.TypesChanged)
                {
                    return true;
                }
            }
            var bt = type.BaseTypeNoUseSiteDiagnostics;
            if (!bt.IsNull() && bt.SpecialType != SpecialType.System_Object)
                return bt.TypesChanged();
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
    internal sealed partial class AnonymousTypeManager : CommonAnonymousTypeManager
    {
        /// <summary>
        /// Given a codeblock delegate provided constructs a codeblock type symbol.
        /// </summary>
        public NamedTypeSymbol ConstructCodeblockTypeSymbol(TypeSymbol[] codeblockParameters, Location location)
        {
            return new CodeblockTypePublicSymbol(this, codeblockParameters, location);
        }

        public NamedTypeSymbol GetCodeblockDelegateType(NamedTypeSymbol cbType)
        {
            return (NamedTypeSymbol)((CodeblockTypePublicSymbol)cbType).Properties[0].Type;
        }
        public NamedTypeSymbol CodeblockType
        {
            get { return this.Compilation.CodeBlockType(); }
        }

        public NamedTypeSymbol UsualType
        {
            get { return this.Compilation.UsualType(); }
        }
        public ArrayTypeSymbol UsualArrayType
        {
            get { return this.Compilation.CreateArrayTypeSymbol(this.Compilation.UsualType()); }
        }

        

    }
}
