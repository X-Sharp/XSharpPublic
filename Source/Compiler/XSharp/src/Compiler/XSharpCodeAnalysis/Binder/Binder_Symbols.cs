//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class BoundCall
    {
        // This field is used to keep track of the original MemVarSymbol Property Access
        // for M->Name that gets changed to __MemVarGet("name")
        // so we have the BoundProperty available in the LocalRewriter for use when the variable gets
        // passed by reference in a call.
        internal BoundPropertyAccess? PropertyAccess { get; set; } = null;
    }
    internal partial class Binder
    {

        bool IsInsidePropertyAccess(string propName)
        {
            if (this.IsInMethodBody)
            {
                var binder = this.Next;
                while (binder is not InMethodBinder && binder != null)
                {
                    binder = binder.Next;
                }
                if (binder is InMethodBinder imb && imb.ContainingMemberOrLambda.IsAccessor())
                {
                    if (imb.ContainingMemberOrLambda is SourcePropertyAccessorSymbol spa)
                    {
                        var name = spa.AssociatedSymbol.Name;
                        if (string.Compare(name, propName, true) == 0)
                        {
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        bool PreferFirstOverSecond(Symbol first, Symbol second)
        {
            if (first.Kind != second.Kind)
            {
                if (first.Kind == SymbolKind.Field && second.Kind == SymbolKind.Property)
                {
                    if (first.DeclaredAccessibility <= Accessibility.Protected)
                    {
                        // when protected then we are inside the class itself.
                        if (first.HasInstanceAttribute() )
                        {
                            // inside the property we return the INSTANCE field
                            // otherwise we return the property
                            return IsInsidePropertyAccess(first.Name);
                        }
                        else
                        {
                            return true;
                        }
                    }
                }
                if (second.Kind == SymbolKind.Field && first.Kind == SymbolKind.Property)
                {
                    if (second.DeclaredAccessibility <= Accessibility.Protected)
                    {
                        // when protected then we are inside the class itself.
                        if (second.HasInstanceAttribute())
                        {
                            // inside the property we return the INSTANCE field
                            // otherwise we return the property
                            return !IsInsidePropertyAccess(second.Name);
                        }
                        else
                        {
                            return true;
                        }
                    }
                }
            }
            if (second.Kind == SymbolKind.NamedType)
            {
                if (first.Kind == SymbolKind.Local ||
                    first.Kind == SymbolKind.Parameter ||
                    first.Kind == SymbolKind.Property ||
                    first.Kind == SymbolKind.Event ||
                    first.Kind == SymbolKind.Field)
                {
                    return true;
                }
            }
            return false;
        }

        internal Symbol XSharpResolveEqualSymbols(Symbol first, Symbol second, ImmutableArray<Symbol> originalSymbols, CSharpSyntaxNode where, DiagnosticBag diagnostics)
        {
            CSDiagnosticInfo info;
            bool usefirst = false;
            if (PreferFirstOverSecond(first, second))
                return first;
            if (PreferFirstOverSecond(second, first))
                return second;
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
                var asm1 = s1.ContainingAssembly?.Name ?? "Unknown";
                var asm2 = s2.ContainingAssembly?.Name ?? "Unknown";
                return new CSDiagnosticInfo(ErrorCode.WRN_XSharpAmbiguous, originalSymbols,
                new object[] {
                        where,
                        s1.Kind.ToString(),
                        new FormattedSymbol(s1, SymbolDisplayFormat.CSharpErrorMessageFormat),
                        asm1,
                        s2.Kind.ToString(),
                        new FormattedSymbol(s2, SymbolDisplayFormat.CSharpErrorMessageFormat),
                        asm2
                });

            }
        }
        internal NamespaceOrTypeOrAliasSymbolWithAnnotations XsBindVoStructSymbol(ExpressionSyntax syntax, NamespaceOrTypeOrAliasSymbolWithAnnotations symbol, DiagnosticBag diagnostics)
        {
            if ((symbol.TypeWithAnnotations.Type)?.IsVoStructOrUnion() == true)
            {
                if (!syntax.XVoIsDecl)
                {
                    symbol = NamespaceOrTypeOrAliasSymbolWithAnnotations.CreateUnannotated(false, new PointerTypeSymbol(symbol.TypeWithAnnotations));
                }
            }
            else if (syntax.XVoIsDecl)
            {
                var _diagnosticInfo = diagnostics.Add(ErrorCode.ERR_BadSKknown, syntax.Location, syntax, symbol.NamespaceOrTypeSymbol.GetKindText(), "VOSTRUCT/UNION");
                var errsymbol = new ExtendedErrorTypeSymbol(GetContainingNamespaceOrType(symbol.Symbol), symbol.Symbol, LookupResultKind.NotATypeOrNamespace, _diagnosticInfo);
                symbol = NamespaceOrTypeOrAliasSymbolWithAnnotations.CreateUnannotated(false, errsymbol);
            }
            return symbol;
        }
        internal NamespaceOrTypeOrAliasSymbolWithAnnotations XsBindNamespaceOrTypeOrAliasSymbol(ExpressionSyntax syntax, DiagnosticBag diagnostics, ConsList<TypeSymbol> basesBeingResolved, bool suppressUseSiteDiagnostics, bool includeNameSpace = true)
        {
            switch (syntax.Kind())
            {

                case SyntaxKind.IdentifierName:
                    return BindNonGenericSimpleNamespaceOrTypeOrAliasSymbol((IdentifierNameSyntax)syntax, diagnostics, basesBeingResolved, suppressUseSiteDiagnostics, qualifierOpt: null, includeNameSpace: includeNameSpace);
                case SyntaxKind.GenericName:
                    return BindGenericSimpleNamespaceOrTypeOrAliasSymbol((GenericNameSyntax)syntax, diagnostics, basesBeingResolved, qualifierOpt: null, includeNameSpace: includeNameSpace);
                default:
                    return BindNamespaceOrTypeOrAliasSymbol(syntax, diagnostics, basesBeingResolved, suppressUseSiteDiagnostics);
            }
        }
    }
}

