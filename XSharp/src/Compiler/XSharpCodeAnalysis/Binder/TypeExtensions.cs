/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

using Microsoft.CodeAnalysis.CSharp.Symbols;

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
                return compilation.GetWellKnownType(WellKnownType.XSharp___VODate);
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
                return compilation.GetWellKnownType(WellKnownType.XSharp___VOFloat);
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
                return compilation.GetWellKnownType(WellKnownType.XSharp_Internal_ImplicitNamespaceAttribute);
            }
            else
            {
                return compilation.GetWellKnownType(WellKnownType.Vulcan_VulcanImplicitNamespaceAttribute);
            }
        }

        static internal NamedTypeSymbol FunctionsType(this CSharpCompilation compilation)
        {
            if (compilation.Options.XSharpRuntime)
            {
                return compilation.GetWellKnownType(WellKnownType.XSharp_Functions);
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
    }
}
