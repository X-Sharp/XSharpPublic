/*
   Copyright 2016 XSharp B.V.

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
using System.Linq;
using System.Reflection;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
using Antlr4.Runtime.Tree;

namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// This portion of the binder converts an <see cref="ExpressionSyntax"/> into a <see cref="BoundExpression"/>.
    /// </summary>
    internal partial class Binder
    {

        private void BindPCall(InvocationExpressionSyntax node, DiagnosticBag diagnostics, AnalyzedArguments analyzedArguments)
        {
            if (node.XPCall && node.Expression is QualifiedNameSyntax && ((QualifiedNameSyntax)node.Expression).Right is GenericNameSyntax)
            {
                var gns = ((QualifiedNameSyntax)node.Expression).Right as GenericNameSyntax;
                var arg = gns.TypeArgumentList.Arguments[0];
                var method = arg.ToFullString();
                bool pcall = method.IndexOf("$Pcall$", StringComparison.OrdinalIgnoreCase) >= 0;
                if (pcall)
                {
                    BindPCallAndDelegate(node, analyzedArguments.Arguments, diagnostics, arg);
                }
                else
                {
                    BindPCallNativeAndDelegate(node, analyzedArguments.Arguments, diagnostics, arg);
                }

            }

        }
        private string GetTypedPtrName(IParseTree xNode)
        {
            if (xNode is XP.ClassvarContext && xNode.Parent is XP.ClassVarListContext)
            {
                var cvl = xNode.Parent as XP.ClassVarListContext;
                var pdtc = cvl.DataType as XP.PtrDatatypeContext;
                if (pdtc != null)
                    return pdtc.TypeName.GetText();

            }
            return null;
        }
        private void BindPCallAndDelegate(InvocationExpressionSyntax node, ArrayBuilder<BoundExpression> args, 
            DiagnosticBag diagnostics, TypeSyntax type)
        {
            var XNode = node.XNode as XP.MethodCallContext;
            string method = XNode.Expr.GetText();
            if (!ValidatePCallArguments(node, args, diagnostics, method))
                return;
            var bfa = args[0] as BoundFieldAccess;
            if (bfa == null)
            {
                Error(diagnostics, ErrorCode.ERR_PCallFirstArgument, node, method, "global typed function pointer");
                return ;
            }
            
            string methodName = null;
            if (bfa.ExpressionSymbol.DeclaringSyntaxReferences.Length > 0)
            {
                var syntaxref = bfa.ExpressionSymbol.DeclaringSyntaxReferences[0] as SyntaxReference;
                if (syntaxref != null)
                {
                    CSharpSyntaxNode syntaxnode = syntaxref.GetSyntax() as CSharpSyntaxNode;
                    var xNode = syntaxnode?.XNode;
                    methodName = GetTypedPtrName(xNode);
                }
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
            HashSet<DiagnosticInfo> useSiteDiagnostics = null;
            LookupOptions options = LookupOptions.NamespacesOrTypesOnly;
            this.LookupSymbolsSimpleName(lookupResult, null, type.Identifier.Text, 0, null, options, false, ref useSiteDiagnostics);
            if (lookupResult.IsSingleViable)
            {
                return lookupResult.Symbols[0] as TypeSymbol;
            }
            return null;
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
            string method = XNode.Expr.GetText();
            if (!ValidatePCallArguments(node, args, diagnostics, method))
                return;
            // Our parent is the invocation expression of the delegate
            var ts = FindPCallDelegateType(type as IdentifierNameSyntax);
            if (ts != null && ts.IsDelegateType())
            {
                SourceDelegateMethodSymbol delmeth = ts.DelegateInvokeMethod() as SourceDelegateMethodSymbol;
                // create new parameters based on the parameters from out parent call
                var invoke = node.Parent as InvocationExpressionSyntax;
                var realargs = invoke.ArgumentList;
                AnalyzedArguments analyzedArguments = AnalyzedArguments.GetInstance();
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

        internal void RemoveNamespacesFromResult(LookupResult result)
        {
            var correctSymbols = ArrayBuilder<Symbol>.GetInstance();
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

    }
}