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
            if (first.IsFromCompilation(Compilation) && !second.IsFromCompilation(Compilation))
            {
                info = new CSDiagnosticInfo(ErrorCode.WRN_VulcanAmbiguous, originalSymbols,
                    new object[] {
                                        where,
                                        first.Kind.ToString(),
                                        new FormattedSymbol(first, SymbolDisplayFormat.CSharpErrorMessageFormat),
                                        second.Kind.ToString(),
                                        new FormattedSymbol(second, SymbolDisplayFormat.CSharpErrorMessageFormat)
                    });
                diagnostics.Add(info, where.Location);
                return first;
            }
            else if (second.IsFromCompilation(Compilation) && !first.IsFromCompilation(Compilation))
            {
                info = new CSDiagnosticInfo(ErrorCode.WRN_VulcanAmbiguous, originalSymbols,
                    new object[] {
                                        where,
                                        first.Kind.ToString(),
                                        new FormattedSymbol(second, SymbolDisplayFormat.CSharpErrorMessageFormat),
                                        second.Kind.ToString(),
                                        new FormattedSymbol(first, SymbolDisplayFormat.CSharpErrorMessageFormat)
                                        });
                diagnostics.Add(info, where.Location);
                return second;
            }
            else if (first.Kind == second.Kind &&
                !string.Equals(first.Name, second.Name) &&
                string.Equals(first.Name, second.Name, StringComparison.OrdinalIgnoreCase))
            {
                // they only differ in case
                info = new CSDiagnosticInfo(ErrorCode.WRN_VulcanAmbiguous, originalSymbols,
                    new object[] {
                                    where,
                                    first.Kind.ToString(),
                                    new FormattedSymbol(first, SymbolDisplayFormat.CSharpErrorMessageFormat),
                                    second.Kind.ToString(),
                                    new FormattedSymbol(second, SymbolDisplayFormat.CSharpErrorMessageFormat)
                                    });
                diagnostics.Add(info, where.Location);
                return first;
            }
            else
            {
                info = new CSDiagnosticInfo(ErrorCode.WRN_VulcanAmbiguous, originalSymbols,
                    new object[] {
                                        where,
                                        first.Kind.ToString(),
                                        new FormattedSymbol(first, SymbolDisplayFormat.CSharpErrorMessageFormat),
                                        second.Kind.ToString(),
                                        new FormattedSymbol(second, SymbolDisplayFormat.CSharpErrorMessageFormat)
                                        });
                diagnostics.Add(info, where.Location);
                return first;

            }
        }
    }
}
