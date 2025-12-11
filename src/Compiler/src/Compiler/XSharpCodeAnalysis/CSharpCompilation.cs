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

        #region cached types
        internal NamedTypeSymbol UsualType()
        {
            if (_usualType is null)
            {
                _usualType = GetWellKnownType(WellKnownType.XSharp___Usual);
            }
            return _usualType;
        }
        internal NamedTypeSymbol PszType()
        {
            if (_pszType is null)
            {
                _pszType = GetWellKnownType(WellKnownType.XSharp___Psz);
            }
            return _pszType;
        }
        internal NamedTypeSymbol DateType()
        {
            if (_dateType is null)
            {
                _dateType = GetWellKnownType(WellKnownType.XSharp___Date);
            }
            return _dateType;
        }
        internal NamedTypeSymbol SymbolType()
        {
            if (_symbolType is null)
            {
                _symbolType = GetWellKnownType(WellKnownType.XSharp___Symbol);
            }
            return _symbolType;
        }
        internal NamedTypeSymbol FloatType()
        {
            if (_floatType is null)
            {
                _floatType = GetWellKnownType(WellKnownType.XSharp___Float);
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
                _arrayType = GetWellKnownType(WellKnownType.XSharp___Array);
            }
            return _arrayType;
        }
        internal NamedTypeSymbol CodeBlockType()
        {
            if (_codeblockType is null)
            {
                _codeblockType = GetWellKnownType(WellKnownType.XSharp_Codeblock);
            }
            return _codeblockType;
        }
        internal NamedTypeSymbol WinDateType()
        {
            if (_windateType is null)
            {
                _windateType = GetWellKnownType(WellKnownType.XSharp___WinDate);
            }
            return _windateType;
        }
        internal NamedTypeSymbol RuntimeFunctionsType()
        {
            if (_rtFuncsType is null)
            {
                _rtFuncsType = GetWellKnownType(WellKnownType.XSharp_RT_Functions);
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
            return GetWellKnownType(WellKnownType.XSharp_Internal_ClassLibraryAttribute);
        }
        internal NamedTypeSymbol ImplicitNamespaceType()
        {
            return GetWellKnownType(WellKnownType.XSharp_ImplicitNamespaceAttribute);
        }

        internal NamedTypeSymbol CompilerServicesType()
        {
            return GetWellKnownType(WellKnownType.XSharp_Internal_CompilerServices);
        }
        internal NamedTypeSymbol VOStructAttributeType()
        {
            return GetWellKnownType(WellKnownType.XSharp_Internal_VoStructAttribute);
        }
        #endregion
    }
}
