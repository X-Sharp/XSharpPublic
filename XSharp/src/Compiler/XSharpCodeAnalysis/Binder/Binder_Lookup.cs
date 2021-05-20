//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis.Collections;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class Binder
    {
        /// <summary>
        /// Look for any symbols in scope with the given name and arity.
        /// </summary>
        /// <remarks>
        /// Makes a second attempt if the results are not viable, in order to produce more detailed failure information (symbols and diagnostics).
        /// </remarks>
        private Binder XSLookupSymbolsWithFallback(LookupResult result, string name, int arity, ref HashSet<DiagnosticInfo> useSiteDiagnostics, ConsList<TypeSymbol> basesBeingResolved = null, LookupOptions options = LookupOptions.Default)
        {
            Debug.Assert(options.AreValid());

            // don't create diagnosis instances unless lookup fails
            var binder = this.LookupSymbolsInternal(result, name, arity, basesBeingResolved, options, diagnose: false, useSiteDiagnostics: ref useSiteDiagnostics);
            FilterResults(result, options);
            if (result.Kind != LookupResultKind.Viable && result.Kind != LookupResultKind.Empty)
            {
                result.Clear();
                // retry to get diagnosis
                var otherBinder = this.LookupSymbolsInternal(result, name, arity, basesBeingResolved, options, diagnose: true, useSiteDiagnostics: ref useSiteDiagnostics);
                Debug.Assert(binder == otherBinder);
            }

            Debug.Assert(result.IsMultiViable || result.IsClear || result.Error != null);
            return binder;
        }
        private Binder XSLookupSymbolsInternal(
            LookupResult result, string name, int arity, ConsList<TypeSymbol> basesBeingResolved, LookupOptions options, bool diagnose, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            Debug.Assert(result.IsClear);
            Debug.Assert(options.AreValid());

            // X# looks for functions first
            //if (Compilation.Options.HasRuntime)
            {
                // check for function calls method calls outside the current class
                bool check = (options.HasFlag(LookupOptions.MustNotBeInstance) && !options.HasFlag(LookupOptions.MustNotBeMethod));
                if (check)
                {
                    var funcOptions = options;
                    funcOptions |= LookupOptions.MustBeInvocableIfMember;
                    Binder scope = this;
                    while (scope != null)
                    {
                        if (scope is InContainerBinder && scope.ContainingType is null) // at the namespace level, so outside of all types
                        {
                            scope.LookupSymbolsInSingleBinder(result, name, arity, basesBeingResolved, funcOptions, this, diagnose, ref useSiteDiagnostics);
                            FilterResults(result, options);
                            if (!result.IsClear)
                            {
                                break;
                            }
                        }
                        scope = scope.Next;
                    }
                }
            }
            LookupResult functionResults = LookupResult.GetInstance();

            if (!result.IsClear)
            {
                foreach (var symbol in result.Symbols)
                {
                    if (symbol is MethodSymbol)
                    {
                        var ms = symbol as MethodSymbol;
                        if (ms.IsStatic && ms.ContainingType.IsFunctionsClass())
                        {
                            SingleLookupResult single = new SingleLookupResult(LookupResultKind.Viable, ms, null);
                            functionResults.MergeEqual(single);
                        }
                    }
                }
                result.Clear();
            }
            Binder binder = null;
            LookupResult otherResults = LookupResult.GetInstance();
            for (var scope = this; scope != null && !result.IsMultiViable; scope = scope.Next)
            {
                if (binder != null)
                {
                    var tmp = LookupResult.GetInstance();
                    scope.LookupSymbolsInSingleBinder(tmp, name, arity, basesBeingResolved, options, this, diagnose, ref useSiteDiagnostics);
                    FilterResults(tmp, options);
                    result.MergeEqual(tmp);
                    tmp.Free();
                }
                else
                {
                    scope.LookupSymbolsInSingleBinder(result, name, arity, basesBeingResolved, options, this, diagnose, ref useSiteDiagnostics);
                    FilterResults(result, options);
                    if (!result.IsClear)
                    {
                        binder = scope;
                        otherResults.MergeEqual(result);
                    }
                }
            }
            if (!functionResults.IsClear && !otherResults.IsClear)
            {
                var func = functionResults.Symbols[0];
                Symbol meth = null;
                var includeMethods = !options.HasFlag(LookupOptions.MustNotBeMethod);
                foreach (var m in otherResults.Symbols)
                {
                    if (m.Kind == SymbolKind.Method )
                    {
                        if (includeMethods)
                        {
                            meth = m;
                            break;
                        }
                    }
                    else
                    {
                        if (!includeMethods)
                        {
                            meth = m;
                            break;
                        }
                    }
                }
                if (meth != null && !meth.IsStatic )
                {
                    var b = this;
                    while (b is not null && b is not InMethodBinder )
                    {
                        b = b.Next;
                    }
                    CSharpSyntaxNode syntax = null;
                    if (b is InMethodBinder mbinder)
                    {
                        syntax = mbinder.ContainingMemberOrLambda.GetNonNullSyntaxNode();
                    }
                    if (!Compilation.Options.HasOption(CompilerOption.EnforceSelf, syntax))
                    {
                        // Static method generate the error elsewhere
                        bool ignore = name.ToLower() == XSharpSpecialNames.funcToIgnore1 || name.ToLower() == XSharpSpecialNames.funcToIgnore2;
                        if (!ignore)
                        {
                            var args = new object[] { name, func, meth };
                            if (useSiteDiagnostics == null)
                            {
                                useSiteDiagnostics = new HashSet<DiagnosticInfo>();
                            }
                            useSiteDiagnostics.Add(new CSDiagnosticInfo(ErrorCode.WRN_FunctionsTakePrecedenceOverMethods, args));
                        }
                    }
                }
            }
            if (!functionResults.IsClear)
            {
                // compare the function results with the overall results found
                // create a list of functions and methods
                // function first and then the methods
                LookupResult mergedResults = LookupResult.GetInstance();
                mergedResults.MergeEqual(functionResults);
                // now add the symbols from result that do not exist 
                for (int j = 0; j < result.Symbols.Count; j++)
                {
                    var sym = result.Symbols[j];
                    var found = false;
                    for (int i = 0; i < mergedResults.Symbols.Count; i++)
                    {
                        if (sym == mergedResults.Symbols[i])
                        {
                            found = true;
                            break;
                        }
                    }
                    if (!found)
                    {
                        SingleLookupResult single = new SingleLookupResult(LookupResultKind.Viable, sym, null);
                        mergedResults.MergeEqual(single);
                    }
                }
                result.Clear();
                result.MergeEqual(mergedResults);
            }
            // C563 Make sure the error is generated for Inaccessible types.
            if (! result.IsClear && result.Kind == LookupResultKind.Inaccessible && result.Error != null )
            {
                // we only want to add this for internal fields (globals)
                if (result.Symbols[0].Kind == SymbolKind.Field)
                {
                    if (useSiteDiagnostics == null)
                    {
                        useSiteDiagnostics = new HashSet<DiagnosticInfo>();
                    }
                    useSiteDiagnostics.Add(result.Error);
                }
            }
            if (otherResults != null)
                otherResults.Free();
            if (functionResults != null)
                functionResults.Free();
            return binder;
        }

    }
}
