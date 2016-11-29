// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using Microsoft.CodeAnalysis.CSharp.Emit;
namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class LocalRewriter
    {
        static IEnumerable<ISymbol> FindMembers(CSharpCompilation compilation, string name)
        {
            Func<string, bool> predicate = n => StringComparer.Ordinal.Equals(name, n);
            SymbolFilter filter = SymbolFilter.Member;
            return compilation.GetSymbolsWithName(predicate, filter);
        }

        private static BoundExpressionStatement ClearGlobal(CSharpCompilation compilation, CSharpSyntaxNode node, FieldSymbol field)
        {
            var lhs = new BoundFieldAccess(node, null, field, null) { WasCompilerGenerated = true };
            var rhs = new BoundDefaultOperator(node, field.Type) { WasCompilerGenerated = true };
            var op = new BoundAssignmentOperator(node, lhs, rhs, field.Type) { WasCompilerGenerated = true };
            var stmt = new BoundExpressionStatement(node, op) { WasCompilerGenerated = true };
            return stmt;
        }

        private static bool ClearGlobals(CSharpCompilation compilation, CSharpSyntaxNode node, TypeSymbol functionClass, List<BoundStatement> statements )
        {
            var members = functionClass.GetMembers();
            bool added = false;
            foreach (var member in members)
            {
                if (member.IsStatic && member.Kind == SymbolKind.Field)
                {
                    var field = member as FieldSymbol;
                    var fldtype = field.Type;
                    if (field.DeclaredAccessibility == Accessibility.Public
                        && fldtype.TypeKind == TypeKind.Class
                        && !field.IsReadOnly
                        && !field.IsConst
                        )
                    {
                        statements.Add(ClearGlobal(compilation, node, field));
                        added = true;
                    }
                }
            }
            return added;
        }

        private static void CreateMethodCall(CSharpCompilation compilation, CSharpSyntaxNode node, 
                                                MethodSymbol sym, List<BoundStatement> statements)
        {
            var args = ImmutableArray<BoundExpression>.Empty;
            var rettype = compilation.GetSpecialType(SpecialType.System_Void);
            var call = new BoundCall(node, null, sym,
                arguments: args,
                argumentNamesOpt: default(ImmutableArray<string>),
                argumentRefKindsOpt: default(ImmutableArray<RefKind>),
                isDelegateCall: false,
                expanded: false,
                invokedAsExtensionMethod: false,
                argsToParamsOpt: default(ImmutableArray<int>),
                resultKind: LookupResultKind.Viable,
                type: rettype,
                hasErrors: false)
            { WasCompilerGenerated = true };
            var stmt = new BoundExpressionStatement(node, call) { WasCompilerGenerated = true };
            statements.Add(stmt);

        }


        public static BoundStatement RewriteAppExit(
                 CSharpCompilation compilation,
                 MethodSymbol method,
                 int methodOrdinal,
                 NamedTypeSymbol containingType,
                 BoundStatement statement,
                 TypeCompilationState compilationState,
                 DiagnosticBag diagnostics)

        {
            if (method.Name != XSharpVOTreeTransformation.AppExit)
                return statement;
            var refMan = compilation.GetBoundReferenceManager();
            var vcla = compilation.GetWellKnownType(WellKnownType.Vulcan_Internal_VulcanClassLibraryAttribute);
            var newstatements = new List<BoundStatement>();

            foreach (var r in refMan.ReferencedAssemblies)
            {
                foreach (var attr in r.GetAttributes())
                {
                    if (attr.AttributeClass.ConstructedFrom == vcla)
                    {
                        var attargs = attr.CommonConstructorArguments;
                        if (attargs.Length == 2)
                        {
                            var functionsClassName = attargs[0].Value.ToString();
                            if (!string.IsNullOrEmpty(functionsClassName))
                            {
                                TypeSymbol type = r.GetTypeByMetadataName(functionsClassName) as TypeSymbol;
                                // If we can find the $Exit method then call that method
                                // Otherwise find the public globals and clear them from our code
                                var members = type.GetMembers(XSharpVOTreeTransformation.ExitProc);
                                if (members.Length > 0)
                                {
                                    foreach (MethodSymbol sym in members)
                                    {
                                        CreateMethodCall(compilation, statement.Syntax, sym, newstatements);
                                    }
                                }
                                else
                                {
                                    ClearGlobals(compilation, statement.Syntax, type, newstatements);
                                }

                            }
                        }
                    }
                }
            }
            // Now clear the globals in this assembly by calling $Exit

            var symbols = FindMembers(compilation, XSharpVOTreeTransformation.ExitProc);
            foreach (MethodSymbol sym in symbols)
            {
                CreateMethodCall(compilation, statement.Syntax, sym, newstatements);
            }
            newstatements.Add(new BoundReturnStatement(statement.Syntax, null));
            var oldbody = statement as BoundBlock;
            var newbody = oldbody.Update(oldbody.Locals, newstatements.ToImmutableArray<BoundStatement>());
            newbody.WasCompilerGenerated = true;
            return newbody;
        }
        public static BoundStatement RewriteAppInit(
            CSharpCompilation compilation,
            MethodSymbol method,
            int methodOrdinal,
            NamedTypeSymbol containingType,
            BoundStatement statement,
            TypeCompilationState compilationState,
            DiagnosticBag diagnostics)

        {
            if (method.Name != XSharpVOTreeTransformation.AppInit)
                return statement;
            var newstatements = new List<BoundStatement>();
            var oldbody = statement as BoundBlock;
            var trystmt = oldbody.Statements[0] as BoundTryStatement;
            var tryblock = trystmt.TryBlock;
            foreach (var stmt in tryblock.Statements)
                newstatements.Add(stmt);
            var vcla = compilation.GetWellKnownType(WellKnownType.Vulcan_Internal_VulcanClassLibraryAttribute);
            var refMan = compilation.GetBoundReferenceManager();
            var init1 = new List<ISymbol>();
            var init2 = new List<ISymbol>();
            var init3 = new List<ISymbol>();
            foreach (var r in refMan.ReferencedAssemblies)
            {
                foreach (var attr in r.GetAttributes())
                {
                    if (attr.AttributeClass.ConstructedFrom == vcla)
                    {
                        var attargs = attr.CommonConstructorArguments;
                        if (attargs.Length == 2)
                        {
                            var functionsClassName = attargs[0].Value.ToString();
                            if (!string.IsNullOrEmpty(functionsClassName))
                            {
                                var type = r.GetTypeByMetadataName(functionsClassName);
                                init1.AddRange(type.GetMembers(XSharpVOTreeTransformation.InitProc1));
                                init2.AddRange(type.GetMembers(XSharpVOTreeTransformation.InitProc2));
                                init3.AddRange(type.GetMembers(XSharpVOTreeTransformation.InitProc3));
                            }
                        }
                    }
                }
            }

            var members = FindMembers(compilation, XSharpVOTreeTransformation.InitProc1);
            init1.AddRange(members);
            members = FindMembers(compilation, XSharpVOTreeTransformation.InitProc2);
            init2.AddRange(members);
            members = FindMembers(compilation, XSharpVOTreeTransformation.InitProc3);
            init3.AddRange(members);
            // Now join all 3 lists to one list
            init1.AddRange(init2);
            init1.AddRange(init3);

            foreach (MethodSymbol sym in init1)
            {
                CreateMethodCall(compilation, statement.Syntax, sym, newstatements);
            }
            tryblock = tryblock.Update(tryblock.Locals, newstatements.ToImmutableArray<BoundStatement>());
            tryblock.WasCompilerGenerated = true;
            trystmt = trystmt.Update(tryblock, trystmt.CatchBlocks, trystmt.FinallyBlockOpt, trystmt.PreferFaultHandler);
            trystmt.WasCompilerGenerated = true;
            newstatements.Clear();
            newstatements.Add(trystmt);
            int i = 0;
            foreach (var stmt in oldbody.Statements)
            {
                if (i > 0)
                    newstatements.Add(stmt);
                ++i;
            }
            oldbody = oldbody.Update(oldbody.Locals, newstatements.ToImmutableArray<BoundStatement>());
            oldbody.WasCompilerGenerated = true;
            return oldbody;
        }


        public static BoundStatement RewriteExit(
                 CSharpCompilation compilation,
                 MethodSymbol method,
                 int methodOrdinal,
                 NamedTypeSymbol containingType,
                 BoundStatement statement,
                 TypeCompilationState compilationState,
                 DiagnosticBag diagnostics)

        {
            if (method.Name != XSharpVOTreeTransformation.ExitProc)
                return statement;
            var newstatements = new List<BoundStatement>();
            var oldbody = statement as BoundBlock;
            // Add exit procedures 
            foreach (var stmt in oldbody.Statements)
            {
                if (!(stmt is BoundSequencePointWithSpan))
                {
                    newstatements.Add(stmt);
                }
            }
            // Clear the globals in this assembly
            foreach (var tree in compilation.SyntaxTrees)
            {
                var root = tree.GetRoot();
                var cu = root.Green as CompilationUnitSyntax;
                if (cu?.Globals != null)
                {
                    foreach (var globaldecl in cu?.Globals)
                    {
                        // Each declaration may have 1 or more variables.
                        for (int i = 0; i < globaldecl.Declaration.Variables.Count; i++)
                        {
                            var global = globaldecl.Declaration.Variables[i];
                            var name = global.Identifier.ToString();
                            var members = FindMembers(compilation, name);
                            foreach (FieldSymbol field in members)
                            {
                                var fldtype = field.Type;
                                if ( (fldtype.TypeKind == TypeKind.Class ||
                                    fldtype == compilation.GetWellKnownType(WellKnownType.Vulcan___Usual))
                                    && !field.IsReadOnly
                                    && !field.IsConst
                                    && field.IsStatic
                                    && field.ContainingType.IsStatic
                                    && field.ContainingType.Name.StartsWith( "Functions", StringComparison.Ordinal) 
                                    )
                                {
                                    newstatements.Add(ClearGlobal(compilation, statement.Syntax, field));
                                }
                            }
                        }
                    }
                }
            }
            newstatements.Add(new BoundReturnStatement(statement.Syntax, null));
            var newbody = oldbody.Update(oldbody.Locals, newstatements.ToImmutableArray<BoundStatement>());
            return newbody;
        }

    }
}