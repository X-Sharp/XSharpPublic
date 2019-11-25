//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.PooledObjects;
namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// This portion of the binder converts an <see cref="ExpressionSyntax"/> into a <see cref="BoundExpression"/>.
    /// </summary>
    internal partial class Binder
    {

        private void BindPCall(InvocationExpressionSyntax node, DiagnosticBag diagnostics, AnalyzedArguments analyzedArguments)
        {
            if (node.XPCall && node.Expression is GenericNameSyntax)
            {
                var gns = node.Expression as GenericNameSyntax;
                var arg = gns.TypeArgumentList.Arguments[0];
                var method = arg.ToFullString();
                bool pcallnative = method.IndexOf(XSharpSpecialNames.PCallNativePrefix, StringComparison.OrdinalIgnoreCase) >= 0;
                if (pcallnative)
                {
                    BindPCallNativeAndDelegate(node, analyzedArguments.Arguments, diagnostics, arg);
                }
                else
                {
                    BindPCallAndDelegate(node, analyzedArguments.Arguments, diagnostics, arg);
                }

            }

        }
        private string GetTypedPtrName(IXParseTree xNode)
        {
            // GLobals and Instance variables are all of type ClassvarContext
            if (xNode is XP.ClassvarContext && xNode.Parent is XP.ClassVarListContext)
            {
                var cvl = xNode.Parent as XP.ClassVarListContext;
                var pdtc = cvl.DataType as XP.PtrDatatypeContext;
                if (pdtc != null)
                    return pdtc.TypeName.GetText();

            }
            // Locals are of type LocalVarContext
            else if (xNode is XP.LocalvarContext)
            {
                var lvc = xNode as XP.LocalvarContext;
                var pdtc = lvc.DataType as XP.PtrDatatypeContext;
                if (pdtc != null)
                    return pdtc.TypeName.GetText();
            }
            return null;
        }
        private void BindPCallAndDelegate(InvocationExpressionSyntax node, ArrayBuilder<BoundExpression> args, 
            DiagnosticBag diagnostics, TypeSyntax type)
        {
            var XNode = node.XNode as XP.MethodCallContext;
            string method = XNode?.Expr.GetText(); 
            if (string.IsNullOrEmpty(method))
                method = "PCALL";
            if (!ValidatePCallArguments(node, args, diagnostics, method))
                return;
            var kind = args[0].Kind;
            if (kind != BoundKind.Local && kind != BoundKind.FieldAccess)
            {
                Error(diagnostics, ErrorCode.ERR_PCallFirstArgument, node, method, "typed function pointer");
                return ;
            }
            string methodName = null;
            // Note that this does not get the syntax of the argument itself
            // but the syntax of the place where the symbol (Global, Field or Local) that the argument points to was defined
            SyntaxReference syntaxref = null; 
            if (kind == BoundKind.FieldAccess)
            {
                var bfa = args[0] as BoundFieldAccess;  // Global or Field
                if (bfa != null && bfa.ExpressionSymbol.DeclaringSyntaxReferences.Length > 0)
                {
                    syntaxref = bfa.ExpressionSymbol.DeclaringSyntaxReferences[0] as SyntaxReference;
                }
            }
            else if (kind == BoundKind.Local)
            {
                var bl = args[0] as BoundLocal;         // Local
                if (bl != null && bl.LocalSymbol?.DeclaringSyntaxReferences.Length > 0)
                {
                    syntaxref = bl.LocalSymbol.DeclaringSyntaxReferences[0] as SyntaxReference;
                }
            }
            if (syntaxref != null)
            {
                CSharpSyntaxNode syntaxnode = syntaxref.GetSyntax() as CSharpSyntaxNode;
                var xNode = syntaxnode?.XNode;
                methodName = GetTypedPtrName(xNode);
            }

            if (methodName == null )
            {
                // first argument for pcall must be typed ptr
                Error(diagnostics, ErrorCode.ERR_PCallFirstArgument, node, method, "typed function pointer");
                return ;
            }
            var lookupResult = LookupResult.GetInstance();
            HashSet<DiagnosticInfo> useSiteDiagnostics = null;
            LookupOptions options = LookupOptions.AllMethodsOnArityZero;
            options |= LookupOptions.MustNotBeInstance;
            this.LookupSymbolsWithFallback(lookupResult, methodName, arity: 0, useSiteDiagnostics: ref useSiteDiagnostics, options: options);
            SourceMethodSymbol methodSym = null;
            if (lookupResult.IsClear)
            {
                // Cannot locate types pointer for pcall 
                Error(diagnostics, ErrorCode.ERR_PCallTypedPointerName, node, method, methodName);
                methodSym = null;
            }
            else if (lookupResult.IsMultiViable)
            {
                foreach (var symbol in lookupResult.Symbols)
                {
                    if (symbol.DeclaringCompilation == this.Compilation && symbol is SourceMethodSymbol)
                    {
                        methodSym = (SourceMethodSymbol) symbol;
                        break;
                    }
                }
            }
            else
            {
                methodSym = (SourceMethodSymbol)lookupResult.Symbols[0];
            }
            if (methodSym != null)
            {
                lookupResult.Clear();
                var ts = FindPCallDelegateType(type as IdentifierNameSyntax);
                if (ts != null && ts.IsDelegateType())
                {
                    SourceDelegateMethodSymbol delmeth = ts.DelegateInvokeMethod() as SourceDelegateMethodSymbol;
                    // clone the parameters from the methodSym
                    var builder = ArrayBuilder<ParameterSymbol>.GetInstance();
                    foreach (var par in methodSym.Parameters)
                    {
                        var parameter = new SourceSimpleParameterSymbol(
                            delmeth,
                            par.Type,
                            par.Ordinal,
                            par.RefKind,
                            par.Name,
                            par.Locations);
                        builder.Add(parameter);
                    }
                    delmeth.InitializeParameters(builder.ToImmutableAndFree());
                    delmeth.SetReturnType(methodSym.ReturnType);
                }
                else
                {
                    Error(diagnostics, ErrorCode.ERR_PCallResolveGeneratedDelegate, node, method, type.ToString());
                }
            }
            return;
        }
        private TypeSymbol FindPCallDelegateType(IdentifierNameSyntax type)
        {
            if (type == null)
                return null;
            var lookupResult = LookupResult.GetInstance();
            try
            {
                HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                LookupOptions options = LookupOptions.NamespacesOrTypesOnly;
                this.LookupSymbolsSimpleName(lookupResult, null, type.Identifier.Text, 0, null, options, false, ref useSiteDiagnostics);
                if (lookupResult.IsSingleViable)
                {
                    return lookupResult.Symbols[0] as TypeSymbol;
                }

                return null;
            }
            finally
            {
                lookupResult.Free();
            }
        }

        private bool ValidatePCallArguments(InvocationExpressionSyntax node, ArrayBuilder<BoundExpression> args,
            DiagnosticBag diagnostics, string method)
        {
            bool ok = args.Count == 1;
            if (ok  )
            {
                var argType = args[0].Type;
                ok = argType == Compilation.GetSpecialType(SpecialType.System_IntPtr);
                ok = ok | argType.IsVoidPointer();
                ok = ok | argType.IsPointerType();
            }
            if (!ok)
            {
                Error(diagnostics, ErrorCode.ERR_PCallFirstArgument, node, method, "pointer");
            }
            return ok;
        }

        private void BindPCallNativeAndDelegate(InvocationExpressionSyntax node, ArrayBuilder<BoundExpression> args,
            DiagnosticBag diagnostics, TypeSyntax type)
        {
            var XNode = node.XNode as XP.MethodCallContext;
            string method = XNode?.Expr.GetText() ;
            if (string.IsNullOrEmpty(method))
                method = "PCALLNATIVE";
            if (!ValidatePCallArguments(node, args, diagnostics, method))
                return;
            // Our parent is the invocation expression of the delegate
            AnalyzedArguments analyzedArguments = AnalyzedArguments.GetInstance();
            try
            {
                var ts = FindPCallDelegateType(type as IdentifierNameSyntax);
                if (ts != null && ts.IsDelegateType())
                {
                    SourceDelegateMethodSymbol delmeth = ts.DelegateInvokeMethod() as SourceDelegateMethodSymbol;
                    // create new parameters based on the parameters from out parent call
                    var invoke = node.Parent as InvocationExpressionSyntax;
                    var realargs = invoke.ArgumentList;
                    var delparams = ts.DelegateParameters();
                    BindArgumentsAndNames(realargs, diagnostics, analyzedArguments);
                    var builder = ArrayBuilder<ParameterSymbol>.GetInstance();
                    int i = 0;
                    foreach (var expr in analyzedArguments.Arguments)
                    {
                        var ptype = expr.Type;
                        if (ptype == null)
                            ptype = new PointerTypeSymbol(Compilation.GetSpecialType(SpecialType.System_Void));
                        var parameter = new SourceSimpleParameterSymbol(
                            delmeth,
                            ptype,
                            i,
                            delparams[i].RefKind,
                            delparams[i].Name,
                            delparams[i].Locations);
                        builder.Add(parameter);
                        i++;
                    }
                    delmeth.InitializeParameters(builder.ToImmutableAndFree());
                }
                else
                {
                    Error(diagnostics, ErrorCode.ERR_PCallResolveGeneratedDelegate, node, method, type.ToString());
                }

                return;
            }
            finally
            {
                analyzedArguments.Free();
            }
        }

        internal void RemoveNamespacesFromResult(LookupResult result)
        {
            var correctSymbols = ArrayBuilder<Symbol>.GetInstance();
            try
            {
                foreach (var s in result.Symbols)
                {
                    if (s.Kind != SymbolKind.Namespace)
                        correctSymbols.Add(s);
                }
                if (correctSymbols.Count != result.Symbols.Count)
                {
                    result.Clear();
                    result.Symbols.AddRange(correctSymbols);
                }
                return;
            }
            finally
            {
                correctSymbols.Free();
            }
        }

    }
}
