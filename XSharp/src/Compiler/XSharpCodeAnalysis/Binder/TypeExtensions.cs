//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Microsoft.CodeAnalysis.CSharp
{
    // Extension methods responsible for looking up 'our' types
    static internal class TypeExtensions
    {
        static internal NamedTypeSymbol PszType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___Psz);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___Psz);
            }
        }
        static internal NamedTypeSymbol UsualType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___Usual);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___Usual);
            }
        }
        static internal NamedTypeSymbol SymbolType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___Symbol);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___Symbol);
            }
        }

        static internal NamedTypeSymbol WinBoolType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___WinBool);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___WinBool);
            }
        }

        static internal NamedTypeSymbol DateType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___Date);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___VODate);
            }
        }

        static internal NamedTypeSymbol FloatType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___Float);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___VOFloat);
            }
        }
        static internal NamedTypeSymbol ArrayType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___Array);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___Array);
            }
        }
        static internal NamedTypeSymbol ArrayBaseType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp___ArrayBase_T1);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan___Array);
            }
        }

        static internal NamedTypeSymbol IndexerType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
                return compilation.GetWellKnownType(WellKnownType.XSharp_IIndexer);
            return null;
        }
        static internal NamedTypeSymbol NamedIndexerType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
                return compilation.GetWellKnownType(WellKnownType.XSharp_INamedIndexer);
            return null;
        }
        static internal NamedTypeSymbol IndexedPropertiesType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
                return compilation.GetWellKnownType(WellKnownType.XSharp_IIndexedProperties);
            return null;
        }


        static internal NamedTypeSymbol CodeBlockType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp_Codeblock);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan_Codeblock);
            }
        }
        static internal NamedTypeSymbol ClassLibraryType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp_Internal_ClassLibraryAttribute);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan_Internal_VulcanClassLibraryAttribute);
            }
        }
        static internal NamedTypeSymbol ImplicitNamespaceType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp_ImplicitNamespaceAttribute);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan_VulcanImplicitNamespaceAttribute);
            }
        }

        static internal NamedTypeSymbol RuntimeFunctionsType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp_RT_Functions);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.VulcanRTFuncs_Functions);
            }
        }
        static internal NamedTypeSymbol CompilerServicesType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp_Internal_CompilerServices);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan_Internal_CompilerServices);
            }
        }
        static internal NamedTypeSymbol VOStructAttributeType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp_Internal_VOStructAttribute);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan_Internal_VOStructAttribute);
            }
        }

        static internal bool IsXsCompilerGenerated(this Symbol symbol) 
        {
            if (symbol.Kind == SymbolKind.Local || symbol.Kind == SymbolKind.Parameter)
            {
                if (symbol.Name != null && symbol.Name.StartsWith("Xs$") )
                {
                    return true;
                }
            }
            if (symbol is SourceLocalSymbol local)
            {
                var syntax = local.GetDeclaratorSyntax();
                var vardecl = syntax.Parent as VariableDeclarationSyntax;
                if (vardecl != null && vardecl.XGenerated)
                    return true;
            }
            return false;
        }

        static internal bool IsValidVOUsualType(this TypeSymbol type, CSharpCompilation compilation)
        {
            switch (type.SpecialType)
            {
                case SpecialType.System_Int32:
                case SpecialType.System_Int64:
                case SpecialType.System_Boolean:
                case SpecialType.System_String:
                case SpecialType.System_IntPtr:
                case SpecialType.System_Decimal:
                case SpecialType.System_DateTime:
                case SpecialType.System_Object:
                    return true;
                
            }
            if (type == compilation.ArrayType())
                return true;
            if (type == compilation.CodeBlockType())
                return true;
            if (type == compilation.DateType())
                return true;
            if (type == compilation.FloatType())
                return true;
            if (type == compilation.SymbolType())
                return true;
            if (type == compilation.PszType())
                return true;
            return false;
        }
    }
}
