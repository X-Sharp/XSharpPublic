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
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.Collections;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal sealed partial class Imports
    {
        internal static bool HandleVulcanImport(UsingDirectiveSyntax usingDirective, Binder usingsBinder, 
            ArrayBuilder<NamespaceOrTypeAndUsingDirective>  usings, PooledHashSet<NamespaceOrTypeSymbol> uniqueUsings, 
            ConsList<Symbol> basesBeingResolved, CSharpCompilation compilation)
        {
            // The usingDirective name contains spaces when it is nested and the GlobalClassName not , so we must eliminate them here
            // nvk: usingDirective.Name.ToString() ONLY has spaces if it is nested. This is not supposed to be nested, as it is "Functions" even for the non-core dialects !!!
            if (usingDirective.Name.ToString().EndsWith(XSharpSpecialNames.CoreFunctionsClass))
            {
                var result = LookupResult.GetInstance();
                LookupOptions options = LookupOptions.AllNamedTypesOnArityZero;
                HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                usingsBinder.LookupSymbolsSimpleName(result, null, XSharpSpecialNames.CoreFunctionsClass, 0, basesBeingResolved, options, false, useSiteDiagnostics: ref useSiteDiagnostics);
                foreach (var sym in result.Symbols)
                {
                    if (sym.Kind == SymbolKind.NamedType)
                    {
                        var ts = (NamedTypeSymbol)sym;
                        if (!uniqueUsings.Contains(ts))
                        {
                            uniqueUsings.Add(ts);
                            usings.Add(new NamespaceOrTypeAndUsingDirective(ts, usingDirective));
                        }
                    }
                }
                if (compilation.Options.IsDialectVO)
                {
                    var declbinder = usingsBinder.WithAdditionalFlags(BinderFlags.SuppressConstraintChecks);
                    var _diagnostics = DiagnosticBag.GetInstance();
                    var opts = ((CSharpSyntaxTree)usingDirective.SyntaxTree).Options;
                    if (opts.CommandLineArguments != null)
                    {
                        string n = Syntax.InternalSyntax.XSharpVOTreeTransformation.VOGlobalClassName(opts);
                        var _name = Syntax.InternalSyntax.XSharpTreeTransformation.ExtGenerateQualifiedName(n);
                        var _imported = declbinder.BindNamespaceOrTypeSymbol(_name, _diagnostics, basesBeingResolved);
                        if (_imported.Kind == SymbolKind.NamedType)
                        {
                            var importedType = (NamedTypeSymbol)_imported;
                            if (!uniqueUsings.Contains(importedType))
                            {
                                uniqueUsings.Add(importedType);
                                usings.Add(new NamespaceOrTypeAndUsingDirective(importedType, usingDirective));
                            }
                        }
                    }
                }
                if (!compilation.ClassLibraryType().IsErrorType() &&
                    !compilation.ImplicitNamespaceType().IsErrorType()
                    && !compilation.UsualType().IsErrorType())
                {
                    var declbinder = usingsBinder.WithAdditionalFlags(BinderFlags.SuppressConstraintChecks);
                    var _diagnostics = DiagnosticBag.GetInstance();
                    string[] defNs = { OurNameSpaces.Vulcan, OurNameSpaces.XSharp};
                    foreach (var n in defNs)
                    {
                        var _name = Syntax.InternalSyntax.XSharpTreeTransformation.ExtGenerateQualifiedName(n);
                        var _imported = declbinder.BindNamespaceOrTypeSymbol(_name, _diagnostics, basesBeingResolved);
                        if (_imported.Kind == SymbolKind.Namespace)
                        {
                            if (!uniqueUsings.Contains(_imported))
                            {
                                uniqueUsings.Add(_imported);
                                usings.Add(new NamespaceOrTypeAndUsingDirective(_imported, usingDirective));
                            }
                        }
                        else if (_imported.Kind == SymbolKind.NamedType)
                        {
                            var importedType = (NamedTypeSymbol)_imported;
                            if (!uniqueUsings.Contains(importedType))
                            {
                                uniqueUsings.Add(importedType);
                                usings.Add(new NamespaceOrTypeAndUsingDirective(importedType, usingDirective));
                            }
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
                                        var _name = Syntax.InternalSyntax.XSharpTreeTransformation.ExtGenerateQualifiedName(defaultNamespace);
                                        var _imported = declbinder.BindNamespaceOrTypeSymbol(_name, _diagnostics, basesBeingResolved);
                                        if (_imported.Kind == SymbolKind.Namespace)
                                        {
                                            if (!uniqueUsings.Contains(_imported))
                                            {
                                                uniqueUsings.Add(_imported);
                                                usings.Add(new NamespaceOrTypeAndUsingDirective(_imported, usingDirective));
                                            }
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
                                        var _name = Syntax.InternalSyntax.XSharpTreeTransformation.ExtGenerateQualifiedName(globalClassName);
                                        var _imported = declbinder.BindNamespaceOrTypeSymbol(_name, _diagnostics, basesBeingResolved);
                                        if (_imported.Kind == SymbolKind.NamedType)
                                        {
                                            var importedType = (NamedTypeSymbol)_imported;
                                            if (!uniqueUsings.Contains(importedType))
                                            {
                                                uniqueUsings.Add(importedType);
                                                usings.Add(new NamespaceOrTypeAndUsingDirective(importedType, usingDirective));
                                            }
                                        }
                                    }
                                    // second element is the default namespace
                                    var defaultNamespace = args[1].Value.ToString();
                                    if (!string.IsNullOrEmpty(defaultNamespace) && compilation.Options.ImplicitNameSpace)
                                    {
                                        var _name = Syntax.InternalSyntax.XSharpTreeTransformation.ExtGenerateQualifiedName(defaultNamespace);
                                        var _imported = declbinder.BindNamespaceOrTypeSymbol(_name, _diagnostics, basesBeingResolved);
                                        if (_imported.Kind == SymbolKind.Namespace)
                                        {
                                            if (!uniqueUsings.Contains(_imported))
                                            {
                                                uniqueUsings.Add(_imported);
                                                usings.Add(new NamespaceOrTypeAndUsingDirective(_imported, usingDirective));
                                            }
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
