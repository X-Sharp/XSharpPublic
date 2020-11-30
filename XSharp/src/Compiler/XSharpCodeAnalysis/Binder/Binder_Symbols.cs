//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class Binder
    {



        internal Symbol XSharpResolveEqualSymbols(Symbol first, Symbol second, ImmutableArray<Symbol> originalSymbols, CSharpSyntaxNode where, DiagnosticBag diagnostics)
        {
            CSDiagnosticInfo info;
            bool usefirst = false;
            if (first.IsFromCompilation(Compilation) && !second.IsFromCompilation(Compilation))
            {
                usefirst = true;
            }
            else if (second.IsFromCompilation(Compilation) && !first.IsFromCompilation(Compilation))
            {
                usefirst = false;
            }
            else if (first.Kind == second.Kind &&
                !string.Equals(first.Name, second.Name) &&
                string.Equals(first.Name, second.Name, StringComparison.OrdinalIgnoreCase))
            {
                usefirst = true;
            }
            else
            {
                usefirst = true;
            }
            if (usefirst)
            {
                info = GenerateWarning(first, second);
                diagnostics.Add(info, where.Location);
                return first;
            }
            else
            {
                info = GenerateWarning(second, first);
                diagnostics.Add(info, where.Location);
                return second;
            }


            CSDiagnosticInfo GenerateWarning(Symbol s1, Symbol s2)
            {
                return new CSDiagnosticInfo(ErrorCode.WRN_VulcanAmbiguous, originalSymbols,
                new object[] {
                        where,
                        s1.Kind.ToString(),
                        new FormattedSymbol(s1, SymbolDisplayFormat.CSharpErrorMessageFormat),
                        s1.ContainingAssembly.Name,
                        s2.Kind.ToString(),
                        new FormattedSymbol(s2, SymbolDisplayFormat.CSharpErrorMessageFormat),
                        s2.ContainingAssembly.Name
                });

            }
        }
    }
}
