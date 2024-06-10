//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;
using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;



namespace Microsoft.CodeAnalysis.CSharp
{
    public partial class CSharpCompilation
    {
        NamedTypeSymbol? _usualType = null;
        NamedTypeSymbol? _pszType = null;
        NamedTypeSymbol? _dateType = null;
        NamedTypeSymbol? _symbolType = null;
        NamedTypeSymbol? _floatType = null;
        NamedTypeSymbol? _currencyType = null;
        NamedTypeSymbol? _arrayType = null;
        NamedTypeSymbol? _rtFuncsType = null;
        NamedTypeSymbol? _codeblockType = null;
        NamedTypeSymbol? _windateType = null;
        NamedTypeSymbol? _vfpFunctionsType = null;

        private void AddXSharpSyntaxTree(IEnumerable<SyntaxTree> trees, ref SyntaxAndDeclarationManager syntaxAndDeclarations)
        {
            if (!trees.IsEmpty() && !Options.HasDefaultTree && !this.IsSubmission)
            {
                SyntaxTree? def;
                var parseOptions = (CSharpParseOptions)trees.First().Options;
                var isApp = Options.OutputKind.IsApplication();
                var noMain = isApp && string.IsNullOrEmpty(Options.MainTypeName);
                if (Options.HasRuntime)
                {
                    if (noMain)
                    {
                        Options.MainTypeName = InternalSyntax.XSharpTreeTransformationRT.VOGlobalClassName(parseOptions);
                    }
                    def = InternalSyntax.XSharpTreeTransformationRT.DefaultRTSyntaxTree(trees, parseOptions);
                }
                else /* core dialect */
                {
                    if (noMain)
                    {
                        Options.MainTypeName = XSharpSpecialNames.FunctionsClass;
                    }
                    def = InternalSyntax.XSharpTreeTransformationCore.DefaultXSharpSyntaxTree(parseOptions);
                }
                syntaxAndDeclarations = syntaxAndDeclarations.AddSyntaxTrees(new[] { def });
                Options.HasDefaultTree = true;
            }
        }

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

        internal NamedTypeSymbol CurrrencyType()
        {
            if (_currencyType is null)
            {
                _currencyType = GetWellKnownType(WellKnownType.XSharp___Currency);
            }
            return _currencyType;
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
        internal NamedTypeSymbol WinDateType()
        {
            if (_windateType is null)
            {
                _windateType = GetRtType(WellKnownType.XSharp___WinDate, WellKnownType.XSharp___WinDate);
            }
            return _windateType;
        }
        internal NamedTypeSymbol RuntimeFunctionsType()
        {
            if (_rtFuncsType is null)
            {
                _rtFuncsType = GetRtType(WellKnownType.XSharp_RT_Functions, WellKnownType.VulcanRTFuncs_Functions);
            }
            return _rtFuncsType;
        }
        internal NamedTypeSymbol VFPFunctionsType()
        {
            if (_vfpFunctionsType is null)
            {
                _vfpFunctionsType = GetWellKnownType(WellKnownType.XSharp_VFP_Functions);
            }
            return _vfpFunctionsType;
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
