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
using System.Threading;
using Microsoft.CodeAnalysis.Collections;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.PooledObjects;
namespace Microsoft.CodeAnalysis.CSharp
{
    internal sealed partial class Imports
    {

        internal static void AddNs(UsingDirectiveSyntax usingDirective, NamespaceOrTypeSymbol ns, ArrayBuilder<NamespaceOrTypeAndUsingDirective> usings, PooledHashSet<NamespaceOrTypeSymbol> uniqueUsings)
        {
            if (!uniqueUsings.Contains(ns))
            {
                uniqueUsings.Add(ns);
                usings.Add(new NamespaceOrTypeAndUsingDirective(ns, usingDirective));
            }

        }
        internal static bool HandleXSharpImport(UsingDirectiveSyntax usingDirective, Binder usingsBinder, 
            ArrayBuilder<NamespaceOrTypeAndUsingDirective>  usings, PooledHashSet<NamespaceOrTypeSymbol> uniqueUsings, 
            ConsList<Symbol> basesBeingResolved, CSharpCompilation compilation)
        {
            // The usingDirective name contains spaces when it is nested and the GlobalClassName not , so we must eliminate them here
            // nvk: usingDirective.Name.ToString() ONLY has spaces if it is nested. This is not supposed to be nested, as it is "Functions" even for the non-core dialects !!!
            if (usingDirective.Name.ToString().EndsWith(XSharpSpecialNames.FunctionsClass))
            {
                var result = LookupResult.GetInstance();
                LookupOptions options = LookupOptions.AllNamedTypesOnArityZero;
                HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                usingsBinder.LookupSymbolsSimpleName(result, null, XSharpSpecialNames.FunctionsClass, 0, basesBeingResolved, options, false, useSiteDiagnostics: ref useSiteDiagnostics);
                foreach (var sym in result.Symbols)
                {
                    if (sym.Kind == SymbolKind.NamedType)
                    {
                        var ts = (NamedTypeSymbol)sym;
                        AddNs(usingDirective, ts, usings, uniqueUsings);
                    }
                }
                var opts = ((CSharpSyntaxTree)usingDirective.SyntaxTree).Options;
                if (opts.CommandLineArguments != null)
                {
                    string functionsClass = null;
                    if (compilation.Options.HasRuntime )
                    {
                        functionsClass = Syntax.InternalSyntax.XSharpTreeTransformationRT.VOGlobalClassName(opts);
                    }
                    else
                    {
                        functionsClass = Syntax.InternalSyntax.XSharpTreeTransformationCore.GlobalFunctionClassName(opts.TargetDLL);
                    }
                    if (!string.IsNullOrEmpty(functionsClass))
                    {
                        var declbinder = usingsBinder.WithAdditionalFlags(BinderFlags.SuppressConstraintChecks);
                        var diagnostics = DiagnosticBag.GetInstance();
                        var name = Syntax.InternalSyntax.XSharpTreeTransformationCore.ExtGenerateQualifiedName(functionsClass);
                        var imported = declbinder.BindNamespaceOrTypeSymbol(name, diagnostics, basesBeingResolved);
                        if (imported.Kind == SymbolKind.NamedType)
                        {
                            var importedType = (NamedTypeSymbol)imported;
                            AddNs(usingDirective, importedType, usings, uniqueUsings);
                        }
                    }
                }

                if (!compilation.ClassLibraryType().IsErrorType() &&
                    !compilation.ImplicitNamespaceType().IsErrorType())
                {
                    var declbinder = usingsBinder.WithAdditionalFlags(BinderFlags.SuppressConstraintChecks);
                    var diagnostics = DiagnosticBag.GetInstance();
                    string[] defNs;
                    if (compilation.Options.XSharpRuntime)
                        defNs = new string[]{ OurNameSpaces.XSharp}; 
                    else
                        defNs = new string[]{ OurNameSpaces.Vulcan}; 

                    foreach (var n in defNs)
                    {
                        var name = Syntax.InternalSyntax.XSharpTreeTransformationCore.ExtGenerateQualifiedName(n);
                        var imported = declbinder.BindNamespaceOrTypeSymbol(name, diagnostics, basesBeingResolved);
                        if (imported.Kind == SymbolKind.Namespace)
                        {
                            AddNs(usingDirective, imported, usings, uniqueUsings);
                        }
                        else if (imported.Kind == SymbolKind.NamedType)
                        {
                            var importedType = (NamedTypeSymbol)imported;
                            AddNs(usingDirective, importedType, usings, uniqueUsings);
                        }
                    }
                    var vcla = compilation.ClassLibraryType();
                    var vins = compilation.ImplicitNamespaceType();
                    var refMan = compilation.GetBoundReferenceManager();
                    foreach (var r in refMan.ReferencedAssemblies)
                    {
                        foreach (var attr in r.GetAttributes())
                        {
                            // Check for VulcanImplicitNameSpace attribute
                            if (attr.AttributeClass.ConstructedFrom == vins && compilation.Options.ImplicitNameSpace)
                            {
                                var args = attr.CommonConstructorArguments;
                                if (args != null && args.Length == 1)
                                {
                                    // only one argument, must be default namespace
                                    var defaultNamespace = args[0].Value.ToString();
                                    if (!string.IsNullOrEmpty(defaultNamespace))
                                    {
                                        var name = Syntax.InternalSyntax.XSharpTreeTransformationCore.ExtGenerateQualifiedName(defaultNamespace);
                                        var imported = declbinder.BindNamespaceOrTypeSymbol(name, diagnostics, basesBeingResolved);
                                        if (imported.Kind == SymbolKind.Namespace)
                                        {
                                            AddNs(usingDirective, imported, usings, uniqueUsings);
                                        }
                                    }
                                }
                            }
                            // Check for VulcanClasslibrary  attribute
                            else if (attr.AttributeClass.ConstructedFrom == vcla)
                            {
                                var args = attr.CommonConstructorArguments;
                                if (args != null && args.Length == 2)
                                {
                                    // first element is the Functions class
                                    var globalClassName = args[0].Value.ToString();
                                    if (!string.IsNullOrEmpty(globalClassName))
                                    {
                                        var name = Syntax.InternalSyntax.XSharpTreeTransformationCore.ExtGenerateQualifiedName(globalClassName);
                                        var imported = declbinder.BindNamespaceOrTypeSymbol(name, diagnostics, basesBeingResolved);
                                        if (imported.Kind == SymbolKind.NamedType)
                                        {
                                            var importedType = (NamedTypeSymbol)imported;
                                            AddNs(usingDirective, importedType, usings, uniqueUsings);
                                        }
                                    }
                                    // second element is the default namespace
                                    var defaultNamespace = args[1].Value.ToString();
                                    if (!string.IsNullOrEmpty(defaultNamespace) && compilation.Options.ImplicitNameSpace)
                                    {
                                        var name = Syntax.InternalSyntax.XSharpTreeTransformationCore.ExtGenerateQualifiedName(defaultNamespace);
                                        var imported = declbinder.BindNamespaceOrTypeSymbol(name, diagnostics, basesBeingResolved);
                                        if (imported.Kind == SymbolKind.Namespace)
                                        {
                                            AddNs(usingDirective, imported, usings, uniqueUsings);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                return true;
            }
            return false;
        }

    }
}
