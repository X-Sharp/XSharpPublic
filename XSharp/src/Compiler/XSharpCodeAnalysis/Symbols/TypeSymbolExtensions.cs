// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal static partial class TypeSymbolExtensions
    {

        private static readonly string[] s_VulcanNamespace = { "Vulcan", "" };
        private static readonly string[] s_VulcanInternalNamespace = { "Internal", "Vulcan", "" };

        public static bool IsVulcanType(this TypeSymbol _type, string TypeName)
        {
            // TODO (nvk): there must be a better way!
            NamedTypeSymbol type = null;
            if (_type != null)
                type = _type.OriginalDefinition as NamedTypeSymbol;
            return
                (object)type != null &&
                type.Arity == 0 &&
                !type.MangleName &&
                type.Name == TypeName &&
                CheckFullName(type.ContainingSymbol, s_VulcanNamespace);
        }
        public static bool IsCodeblock(this TypeSymbol _type)
        {
            return _type.IsVulcanType("Codeblock");
        }

        public static bool IsUsual(this TypeSymbol _type)
        {
            return _type.IsVulcanType("__Usual");
        }

        public static bool IsVoStructOrUnion(this TypeSymbol _type)
        {
            // TODO (nvk): there must be a better way!
            NamedTypeSymbol type = null;
            if (_type != null)
                type = _type.OriginalDefinition as NamedTypeSymbol;
            if ((object)type != null && type.Arity == 0 && !type.MangleName)
            {
                var attrs = type.GetAttributes();
                foreach (var attr in attrs)
                {
                    var atype = attr.AttributeClass;
                    if (atype.Name == "VOStructAttribute" && CheckFullName(atype.ContainingSymbol, s_VulcanInternalNamespace))
                        return true;
                }
            }
            if ((type as SourceNamedTypeSymbol)?.IsSourceVoStructOrUnion == true)
            {
                return true;
            }
            return false;
        }

        public static int VoStructOrUnionSizeInBytes(this TypeSymbol _type)
        {
            // TODO (nvk): there must be a better way!
            NamedTypeSymbol type = null;
            if (_type != null)
                type = _type.OriginalDefinition as NamedTypeSymbol;
            if ((object)type != null && type.Arity == 0 && !type.MangleName)
            {
                if (type is SourceNamedTypeSymbol)
                {
                    var sourceType = (SourceNamedTypeSymbol)type;
                    if (sourceType.IsSourceVoStructOrUnion)
                    {
                        return sourceType.VoStructSize;
                    }
                }
                else
                {
                    var attrs = type.GetAttributes();
                    foreach (var attr in attrs)
                    {
                        var atype = attr.AttributeClass;
                        if (atype.Name == "VOStructAttribute" && CheckFullName(atype.ContainingSymbol, s_VulcanInternalNamespace))
                        {
                            return attr.ConstructorArguments.FirstOrNullable()?.DecodeValue<int>(SpecialType.System_Int32) ?? 0;
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
            if (_type != null)
                type = _type.OriginalDefinition as NamedTypeSymbol;
            if ((object)type != null && type.Arity == 0 && !type.MangleName)
            {
                if (type is SourceNamedTypeSymbol)
                {
                    var sourceType = (SourceNamedTypeSymbol)type;
                    if (sourceType.IsSourceVoStructOrUnion)
                    {
                        return sourceType.VoStructElementSize;
                    }
                }
                else
                {
                    var attrs = type.GetAttributes();
                    foreach (var attr in attrs)
                    {
                        var atype = attr.AttributeClass;
                        if (atype.Name == "VOStructAttribute" && CheckFullName(atype.ContainingSymbol, s_VulcanInternalNamespace))
                        {
                            return attr.ConstructorArguments.LastOrNullable()?.DecodeValue<int>(SpecialType.System_Int32) ?? 0;
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
                if (type.IsPointerType())
                    elementSize = 4;
                else
                    elementSize = type.VoStructOrUnionSizeInBytes();
            }
            return elementSize;
        }

        public static bool IsVulcanRT(this AssemblySymbol _asm)
        {
            // TODO (nvk): there must be a better way!
            return
                (object)_asm != null &&
                (_asm.Name == "VulcanRTFuncs" || _asm.Name == "VulcanRT");
        }

        public static bool HasVODefaultParameter(this ParameterSymbol param)
        {
            if ((object)param != null)
            {
                var attrs = param.GetAttributes();
                foreach (var attr in attrs)
                {
                    var atype = attr.AttributeClass;
                    if (atype.Name == "DefaultParameterValueAttribute" && CheckFullName(atype.ContainingSymbol, s_VulcanInternalNamespace))
                        return true;
                }
            }
            return false;
        }

        public static ConstantValue GetVODefaultParameter(this ParameterSymbol param)
        {
            if ((object)param != null)
            {
                var attrs = param.GetAttributes();
                foreach (var attr in attrs)
                {
                    var atype = attr.AttributeClass;
                    if (atype.Name == "DefaultParameterValueAttribute" && CheckFullName(atype.ContainingSymbol, s_VulcanInternalNamespace))
                    {
                        int desc = attr.CommonConstructorArguments[1].DecodeValue<int>(SpecialType.System_Int32);
                        if (desc == 0)
                        {
                            var arg = attr.CommonConstructorArguments[0];
                            if (arg.Type != null && arg.Value != null)
                                return ConstantValue.Create(arg.Value, arg.Type.SpecialType);
                            else
                                return null;
                        }
                        else
                            return null;
                    }
                }
            }
            return ConstantValue.Bad;
        }
    }
}