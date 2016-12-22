// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.Symbols;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal static partial class TypeSymbolExtensions
    {

        private static readonly string[] s_VulcanNamespace = { "Vulcan", "" };

        public static bool IsVulcanRTAttribute(this NamedTypeSymbol atype, String name)
        {
            if (atype == null)
                return false;
            return (String.Equals(atype.Name, name, System.StringComparison.OrdinalIgnoreCase)
                && String.Equals(atype.ContainingAssembly.Name, "VulcanRT", System.StringComparison.OrdinalIgnoreCase));

        }
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
                        if (atype.IsVulcanRTAttribute( "VOStructAttribute"))
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
                else if (type is Metadata.PE.PENamedTypeSymbol)
                {
                    var attrs = type.GetAttributes();
                    foreach (var attr in attrs)
                    {
                        var atype = attr.AttributeClass;
                        if (atype.IsVulcanRTAttribute("VOStructAttribute"))
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
                else if (type is Metadata.PE.PENamedTypeSymbol)
                {
                    var attrs = type.GetAttributes();
                    foreach (var attr in attrs)
                    {
                        var atype = attr.AttributeClass;
                        if (atype.IsVulcanRTAttribute("VOStructAttribute"))
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
                    if (atype.IsVulcanRTAttribute("DefaultParameterValueAttribute"))
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
                    if (atype.IsVulcanRTAttribute("DefaultParameterValueAttribute"))
                    {
                        int desc = attr.CommonConstructorArguments[1].DecodeValue<int>(SpecialType.System_Int32);

                        var arg = attr.CommonConstructorArguments[0];
                        switch (desc)
                        {
                            case 0: 
                                // normal .Net Object
                                // return value  or null
                                if (arg.Type != null && arg.Value != null)
                                    return ConstantValue.Create(arg.Value, arg.Type.SpecialType);
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
        public NamedTypeSymbol Vulcan_Codeblock
        {
            get { return this.Compilation.GetWellKnownType(WellKnownType.Vulcan_Codeblock); }
        }

        public NamedTypeSymbol Vulcan_Usual
        {
            get { return this.Compilation.GetWellKnownType(WellKnownType.Vulcan___Usual); }
        }
    }

}