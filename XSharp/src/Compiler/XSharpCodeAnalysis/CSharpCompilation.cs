//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    public partial class CSharpCompilation
    {
        NamedTypeSymbol? _usualType = null;
        NamedTypeSymbol? _pszType = null;
        NamedTypeSymbol? _dateType = null;
        NamedTypeSymbol? _symbolType = null;
        NamedTypeSymbol? _floatType = null;
        NamedTypeSymbol? _arrayType = null;
        NamedTypeSymbol? _rtFuncsType = null;
        NamedTypeSymbol? _codeblockType = null;

        internal NamedTypeSymbol GetRtType(WellKnownType xsName, WellKnownType vulName)
        {
            return GetWellKnownType(Options.XSharpRuntime ? xsName : vulName);
        }
        #region cached types
        internal NamedTypeSymbol UsualType()
        {
            if (_usualType is null)
            {
                _usualType = GetRtType(WellKnownType.XSharp___Usual, WellKnownType.Vulcan___Usual);
            }
            return _usualType;
        }
        internal NamedTypeSymbol PszType()
        {
            if (_pszType is null)
            {
                _pszType = GetRtType(WellKnownType.XSharp___Psz, WellKnownType.Vulcan___Psz);
            }
            return _pszType;
        }
        internal NamedTypeSymbol DateType()
        {
            if (_dateType is null)
            {
                _dateType = GetRtType(WellKnownType.XSharp___Date, WellKnownType.Vulcan___VODate);
            }
            return _dateType;
        }
        internal NamedTypeSymbol SymbolType()
        {
            if (_symbolType is null)
            {
                _symbolType = GetRtType(WellKnownType.XSharp___Symbol, WellKnownType.Vulcan___Symbol);
            }
            return _symbolType;
        }
        internal NamedTypeSymbol FloatType()
        {
            if (_floatType is null)
            {
                _floatType = GetRtType(WellKnownType.XSharp___Float, WellKnownType.Vulcan___VOFloat);
            }
            return _floatType;
        }
        internal NamedTypeSymbol ArrayType()
        {
            if (_arrayType is null)
            {
                _arrayType = GetRtType(WellKnownType.XSharp___Array, WellKnownType.Vulcan___Array);
            }
            return _arrayType;
        }
        internal NamedTypeSymbol CodeBlockType()
        {
            if (_codeblockType is null)
            {
                _codeblockType = GetRtType(WellKnownType.XSharp_Codeblock, WellKnownType.Vulcan_Codeblock);
            }
            return _codeblockType;
        }
        internal NamedTypeSymbol RuntimeFunctionsType()
        {
            if (_rtFuncsType is null)
            {
                _rtFuncsType = GetRtType(WellKnownType.XSharp_RT_Functions, WellKnownType.VulcanRTFuncs_Functions);
            }
            return _rtFuncsType;
        }
        #endregion
        #region uncached types 
        internal NamedTypeSymbol ArrayBaseType()
        {
            return GetWellKnownType(WellKnownType.XSharp___ArrayBase_T1);
        }

        internal NamedTypeSymbol IndexerType()
        {
            return GetWellKnownType(WellKnownType.XSharp_IIndexer);
        }
        internal NamedTypeSymbol NamedIndexerType()
        {
            return GetWellKnownType(WellKnownType.XSharp_INamedIndexer);
        }
        internal NamedTypeSymbol IndexedPropertiesType()
        {
            return GetWellKnownType(WellKnownType.XSharp_IIndexedProperties);
        }
        internal NamedTypeSymbol ClassLibraryType()
        {
            return GetRtType(WellKnownType.XSharp_Internal_ClassLibraryAttribute, WellKnownType.Vulcan_Internal_VulcanClassLibraryAttribute);
        }
        internal NamedTypeSymbol ImplicitNamespaceType()
        {
            return GetRtType(WellKnownType.XSharp_ImplicitNamespaceAttribute, WellKnownType.Vulcan_VulcanImplicitNamespaceAttribute);
        }

        internal NamedTypeSymbol CompilerServicesType()
        {
            return GetRtType(WellKnownType.XSharp_Internal_CompilerServices,WellKnownType.Vulcan_Internal_CompilerServices);
        }
        internal NamedTypeSymbol VOStructAttributeType()
        {
            return GetRtType(WellKnownType.XSharp_Internal_VoStructAttribute,WellKnownType.Vulcan_Internal_VOStructAttribute);
        }
        #endregion
    }
}
