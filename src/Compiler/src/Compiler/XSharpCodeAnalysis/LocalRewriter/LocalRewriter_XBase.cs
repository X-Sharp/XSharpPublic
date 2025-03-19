﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using Microsoft.CodeAnalysis.CSharp.Emit;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Symbols.Metadata.PE;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class LocalRewriter
    {

        internal BoundExpression MakePsz(BoundExpression value)
        {
            if (value.Type.IsPszType())
                return value;
            var psz = _compilation.PszType();
            if (value.Type.IsUsualType())
            {
                _factory.Syntax = value.Syntax;
                var op = getImplicitOperatorByReturnType(value.Type, psz);
                var result = _factory.StaticCall(value.Type, (MethodSymbol)op, value);
                result.WasCompilerGenerated = true;
                this._diagnostics.Add(ErrorCode.WRN_CompilerGeneratedPSZConversionGeneratesMemoryleak, value.Syntax.Location);
                return result;
            }
            var isString = value.Type is { } && value.Type.IsStringType();
            MethodSymbol ctor;
            if (isString)
            {
                ctor = Binder.FindConstructor(psz, 1, _compilation.GetSpecialType(SpecialType.System_String));
            }
            else
            {
                ctor = Binder.FindConstructor(psz, 1, _compilation.GetSpecialType(SpecialType.System_IntPtr));
            }
            if (ctor != null)
            {
                if (value.Type.SpecialType != SpecialType.System_IntPtr)
                {
                    value = new BoundConversion(
                        syntax: value.Syntax,
                        operand: value,
                        conversion: Conversion.Identity,
                        @checked: false,
                        explicitCastInCode: false,
                        constantValueOpt: null,
                        conversionGroupOpt: null,
                        type: ctor.Parameters[0].Type,
                        hasErrors: false);
                }
                return new BoundObjectCreationExpression(value.Syntax, ctor, new BoundExpression[] { value });
            }
            return value;
        }
        private static IEnumerable<ISymbol> FindMembers(CSharpCompilation compilation, string name)
        {
            Func<string, bool> predicate = n => StringComparer.Ordinal.Equals(name, n);
            SymbolFilter filter = SymbolFilter.Member;
            return compilation.GetSymbolsWithName(predicate, filter);
        }

        private static BoundExpressionStatement ClearGlobal(CSharpCompilation compilation, SyntaxNode node, FieldSymbol field)
        {
            var lhs = new BoundFieldAccess(node, null, field, null) { WasCompilerGenerated = true };
            var rhs = new BoundDefaultExpression(node, field.Type) { WasCompilerGenerated = true };
            var op = new BoundAssignmentOperator(node, lhs, rhs, field.Type) { WasCompilerGenerated = true };
            var stmt = new BoundExpressionStatement(node, op) { WasCompilerGenerated = true };
            return stmt;
        }

        private static bool ClearGlobals(CSharpCompilation compilation, SyntaxNode node, TypeSymbol functionClass, List<BoundStatement> statements )
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

        private static void CreateMethodCall(CSharpCompilation compilation, SyntaxNode node,
                                                MethodSymbol sym, List<BoundStatement> statements)
        {
            var args = ImmutableArray<BoundExpression>.Empty;
            var rettype = compilation.GetSpecialType(SpecialType.System_Void);
            var call = new BoundCall(syntax: node, receiverOpt: null,
                initialBindingReceiverIsSubjectToCloning: ThreeState.False,
                method: sym,
                arguments: args,
                argumentNamesOpt: default(ImmutableArray<string>),
                argumentRefKindsOpt: default(ImmutableArray<RefKind>),
                isDelegateCall: false,
                expanded: false,
                invokedAsExtensionMethod: false,
                argsToParamsOpt: default,
                defaultArguments: default,
                resultKind: LookupResultKind.Viable,
                type: rettype,
                hasErrors: false)
            { WasCompilerGenerated = true };
            var stmt = new BoundExpressionStatement(node, call) { WasCompilerGenerated = true };
            statements.Add(stmt);

        }


        internal static BoundStatement RewriteAppExit(
                 MethodSymbol method,
                 BoundStatement statement,
                 BindingDiagnosticBag diagnostics)

        {
            if (method.Name != XSharpSpecialNames.AppExit)
                return statement;
            var refMan = method.DeclaringCompilation.GetBoundReferenceManager();
            var vcla = method.DeclaringCompilation.ClassLibraryType();
            var newstatements = new List<BoundStatement>();

            foreach (var rkv in refMan.GetReferencedAssemblies())
            {
                var r = (AssemblySymbol)rkv.Value;
                foreach (var attr in r.GetAttributes())
                {
                    if (TypeSymbol.Equals(attr.AttributeClass.ConstructedFrom, vcla))
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
                                if (type is { })
                                {
                                    var members = type.GetMembers(XSharpSpecialNames.ExitProc);
                                    if (members.Length > 0)
                                    {
                                        foreach (Symbol sym in members)
                                        {
                                            CreateMethodCall(method.DeclaringCompilation, statement.Syntax, ((IMethodSymbol)sym.ISymbol).MethodSymbol(), newstatements);
                                        }
                                    }
                                    else
                                    {
                                        ClearGlobals(method.DeclaringCompilation, statement.Syntax, type, newstatements);
                                    }
                                }

                            }
                        }
                        break;
                    }
                }
            }
            // Now clear the globals in this assembly by calling $Exit

            var symbols = FindMembers(method.DeclaringCompilation, XSharpSpecialNames.ExitProc);
            foreach (IMethodSymbol sym in symbols)
            {
                CreateMethodCall(method.DeclaringCompilation, statement.Syntax, sym.MethodSymbol(), newstatements);
            }
            newstatements.Add(new BoundReturnStatement(statement.Syntax, RefKind.None, null, false));
            var oldbody = statement as BoundBlock;
            var newbody = oldbody.Update(oldbody.Locals, ImmutableArray<LocalFunctionSymbol>.Empty, oldbody.HasUnsafeModifier, oldbody.Instrumentation, newstatements.ToImmutableArray<BoundStatement>());
            return newbody;
        }

        private static BoundExpressionStatement vulcanruntimeStateAssign(SyntaxNode syntax, FieldSymbol field, bool value)
        {
            var bfa = new BoundFieldAccess(syntax, null, field, ConstantValue.NotAvailable) { WasCompilerGenerated = true };
            var lit = new BoundLiteral(syntax, ConstantValue.Create(value), field.Type) { WasCompilerGenerated = true };
            var ass = new BoundAssignmentOperator(syntax, bfa, lit, isRef: false, lit.Type) { WasCompilerGenerated = true };
            var stmt = new BoundExpressionStatement(syntax, ass) { WasCompilerGenerated = true };
            return stmt;
        }
        private static BoundExpressionStatement xsharpruntimeStateAssign(SyntaxNode syntax, PropertySymbol prop, object value)
        {
            var bpa = new BoundPropertyAccess(syntax, null, ThreeState.False, prop, LookupResultKind.Viable,prop.Type) { WasCompilerGenerated = true };
            BoundLiteral lit;
            if (value is bool)
                lit = new BoundLiteral(syntax, ConstantValue.Create(( bool) value), prop.Type) { WasCompilerGenerated = true };
            else
                lit = new BoundLiteral(syntax, ConstantValue.Create((int)value), prop.Type) { WasCompilerGenerated = true };

            var ass = new BoundAssignmentOperator(syntax, bpa, lit, isRef: false, lit.Type) { WasCompilerGenerated = true };
            var stmt = new BoundExpressionStatement(syntax, ass) { WasCompilerGenerated = true };
            return stmt;
        }

        internal static BoundStatement RewriteAppInit(
            MethodSymbol method,
            BoundStatement statement,
            BindingDiagnosticBag diagnostics)

        {
            if (method.Name != XSharpSpecialNames.AppInit )
                return statement;
            var newstatements = new List<BoundStatement>();
            var oldbody = statement as BoundBlock;
            var trystmt = oldbody.Statements[0] as BoundTryStatement;
            var tryblock = trystmt.TryBlock;
            foreach (var stmt in tryblock.Statements)
            {
                newstatements.Add(stmt);
            }
            // Generate RuntimeState field assignments when  the runtime supports the fields we expect
            var comp = method.DeclaringCompilation;
            var xc = comp.GetBoundReferenceManager().GetReferencedAssemblies().Where(x => x.Value.Name == "XSharp.Core");
            if (xc.Count() != 0)
            {
                var xsrt = (AssemblySymbol)xc.First().Value;
                var type = xsrt.GetTypeByMetadataName("XSharp.RuntimeState");
                if (type is { })
                {
                    string[] names = { XSharpSpecialNames.RTCompilerOptionOvf,
                                    XSharpSpecialNames.RTCompilerOptionVO11,
                                    XSharpSpecialNames.RTCompilerOptionVO13,
                                    XSharpSpecialNames.RTCompilerOptionFox2,
                                    XSharpSpecialNames.RTDialect,
                    };
                    object[] values = { comp.Options.CheckOverflow ,
                                        comp.Options.VOArithmeticConversions,
                                        comp.Options.VOStringComparisons,
                                        comp.Options.FoxArraySupport,
                                        comp.Options.Dialect};
                    for (int n = 0; n < names.Length; n++)
                    {
                        var mem = type.GetMembers(names[n]);
                        if (mem.Length == 1 && mem[0] is PropertySymbol prop)
                        {
                            newstatements.Add(xsharpruntimeStateAssign(oldbody.Syntax, prop, values[n]));
                        }
                    }
                }
            }

            var initstmts = GetInitStatements(method.DeclaringCompilation, statement,false);
            newstatements.AddRange(initstmts);
            tryblock = tryblock.Update(tryblock.Locals, ImmutableArray<LocalFunctionSymbol>.Empty, tryblock.HasUnsafeModifier, tryblock.Instrumentation, newstatements.ToImmutableArray<BoundStatement>());
            tryblock.WasCompilerGenerated = true;
            trystmt = trystmt.Update(tryblock, trystmt.CatchBlocks, trystmt.FinallyBlockOpt, trystmt.FinallyLabelOpt, trystmt.PreferFaultHandler);
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
            oldbody = oldbody.Update(oldbody.Locals, ImmutableArray<LocalFunctionSymbol>.Empty, oldbody.HasUnsafeModifier, oldbody.Instrumentation, newstatements.ToImmutableArray<BoundStatement>());
            oldbody.WasCompilerGenerated = true;
            return oldbody;
        }

        static List<BoundStatement> GetInitStatements(CSharpCompilation compilation, BoundStatement statement, bool localOnly)
        {
            var newstatements = new List<BoundStatement>();
            var vcla = compilation.ClassLibraryType();
            var refMan = compilation.GetBoundReferenceManager();
            var init1 = new List<ISymbol>();
            var init2 = new List<ISymbol>();
            var init3 = new List<ISymbol>();
            if (!localOnly)
            {
                foreach (var rkv in refMan.GetReferencedAssemblies())
                {
                    var r = (AssemblySymbol) rkv.Value;
                    foreach (var attr in r.GetAttributes())
                    {
                        if ( TypeSymbol.Equals(attr.AttributeClass.ConstructedFrom,vcla))
                        {
                            var attargs = attr.CommonConstructorArguments;
                            if (attargs.Length == 2)
                            {
                                var functionsClassName = attargs[0].Value.ToString();
                                if (!string.IsNullOrEmpty(functionsClassName))
                                {
                                    var type = r.GetTypeByMetadataName(functionsClassName);
                                    if (type is { })
                                    {
                                        foreach (var mem in type.GetMembers(XSharpSpecialNames.InitProc1))
                                        {
                                            init1.Add(mem.ISymbol);
                                        }
                                        foreach (var mem in type.GetMembers(XSharpSpecialNames.InitProc2))
                                        {
                                            init2.Add(mem.ISymbol);
                                        }
                                        foreach (var mem in type.GetMembers(XSharpSpecialNames.InitProc3))
                                        {
                                            init3.Add(mem.ISymbol);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            foreach (var mem in FindMembers(compilation, XSharpSpecialNames.InitProc1))
            {
                init1.Add(mem);
            }
            foreach (var mem in FindMembers(compilation, XSharpSpecialNames.InitProc2))
            {
                init2.Add(mem);
            }
            foreach (var mem in FindMembers(compilation, XSharpSpecialNames.InitProc3))
            {
                init3.Add(mem);
            }
            // Now join all 3 lists to one list
            init1.AddRange(init2);
            init1.AddRange(init3);

            foreach (IMethodSymbol sym in init1)
            {
                CreateMethodCall(compilation, statement.Syntax, sym.MethodSymbol(), newstatements);
            }
            return newstatements;
        }
        internal static BoundStatement RewriteRunInitProc(
            MethodSymbol method,
            BoundStatement statement,
            BindingDiagnosticBag diagnostics)

        {
            if ( method.Name != ReservedNames.RunInitProcs)
                return statement;
            var oldbody = statement as BoundBlock;
            var newstatements = new List<BoundStatement>();
            var initstmts = GetInitStatements(method.DeclaringCompilation, statement,true);
            newstatements.AddRange(initstmts);
            newstatements.Add(new BoundReturnStatement(statement.Syntax, RefKind.None, null, false) { WasCompilerGenerated = true } );
            oldbody = oldbody.Update(oldbody.Locals, ImmutableArray<LocalFunctionSymbol>.Empty, oldbody.HasUnsafeModifier, oldbody.Instrumentation, newstatements.ToImmutableArray<BoundStatement>());
            oldbody.WasCompilerGenerated = true;
            return oldbody;
        }



        internal static BoundStatement RewriteExit(
                 MethodSymbol method,
                 BoundStatement statement,
                 BindingDiagnosticBag diagnostics)

        {
            return statement;
            // Commented out for now because this kills GUIExit in the GUI classes
            //if (method.Name != XSharpVOTreeTransformation.ExitProc)
            //return statement;
            //var newstatements = new List<BoundStatement>();
            //var oldbody = statement as BoundBlock;
            //// Add exit procedures
            //foreach (var stmt in oldbody.Statements)
            //{
            //    if (!(stmt is BoundSequencePointWithSpan))
            //    {
            //        newstatements.Add(stmt);
            //    }
            //}
            //// Clear the globals in this assembly
            //foreach (var tree in compilation.SyntaxTrees)
            //{
            //    var root = tree.GetRoot();
            //    var cu = root.Green as CompilationUnitSyntax;
            //    if (cu?.Globals != null)
            //    {
            //        foreach (var globaldecl in cu?.Globals)
            //        {
            //            // Each declaration may have 1 or more variables.
            //            for (int i = 0; i < globaldecl.Declaration.Variables.Count; i++)
            //            {
            //                var global = globaldecl.Declaration.Variables[i];
            //                var name = global.Identifier.ToString();
            //                var members = FindMembers(compilation, name);
            //                foreach (FieldSymbol field in members)
            //                {
            //                    var fldtype = field.Type;
            //                    if ( (fldtype.TypeKind == TypeKind.Class ||
            //                        fldtype == compilation.GetWellKnownType(WellKnownType.Vulcan___Usual))
            //                        && !field.IsReadOnly
            //                        && !field.IsConst
            //                        && field.IsStatic
            //                        && field.ContainingType.IsStatic
            //                        && field.ContainingType.Name.StartsWith( "Functions", StringComparison.Ordinal)
            //                        )
            //                    {
            //                        newstatements.Add(ClearGlobal(compilation, statement.Syntax, field));
            //                    }
            //                }
            //            }
            //        }
            //    }
            //}
            //newstatements.Add(new BoundReturnStatement(statement.Syntax, null));
            //var newbody = oldbody.Update(oldbody.Locals, newstatements.ToImmutableArray<BoundStatement>());
            //return newbody;
        }

        internal static BoundStatement RemoveUnusedVars(
                 XSharpParser.MemberData data,
                 BoundStatement statement,
                 BindingDiagnosticBag diagnostics)
        {
            bool removeMemvars = data.HasMemVarLevel && !(data.HasMemVars || data.HasUndeclared);
            bool removePCount = data.HasClipperCallingConvention && data.UsesPCount == false;
            if (!removeMemvars && !removePCount)
            {
                return statement;
            }
            var body = statement as BoundBlock;
            if (body == null)
            {
                return statement;
            }
            LocalSymbol level = null;
            LocalSymbol pcount = null;
            var syms = new List<LocalSymbol>();
            foreach (var local in body.Locals)
            {
                if (local.Name == XSharpSpecialNames.PrivatesLevel && removeMemvars)
                {
                    level = local;
                }
                else if (local.Name == XSharpSpecialNames.ClipperPCount && removePCount)
                {
                    pcount = local;
                }
                else
                {
                    syms.Add(local);
                }
            }
            if (level == null && pcount == null)        // should not happen
                return body;
            var newStmts = new List<BoundStatement>();
            foreach (var stmt in body.Statements)
            {
                switch (stmt.Kind)
                {
                    case BoundKind.LocalDeclaration:
                        var decl = stmt as BoundLocalDeclaration;
                        if (decl.LocalSymbol == level)      // no need for local to store Level
                            continue;
                        if (decl.LocalSymbol == pcount)     // no need for local to store PCount
                            continue;
                        newStmts.Add(decl);
                        break;
                    case BoundKind.TryStatement:
                        if (level == null)
                        {
                            newStmts.Add(stmt);
                            continue;
                        }
                        var tryStmt = stmt as BoundTryStatement;
                        var finBlock = tryStmt.FinallyBlockOpt;
                        if (finBlock != null)
                        {
                            var newfinStatements = new List<BoundStatement>();
                            foreach (var finstmt in finBlock.Statements)
                            {
                                if (finstmt.Kind == BoundKind.ExpressionStatement)
                                {
                                    var exprStmt = finstmt as BoundExpressionStatement;
                                    if (exprStmt.Expression is BoundCall bc && bc.Method.Name == ReservedNames.MemVarRelease)
                                    {
                                        continue;
                                    }
                                }
                                newfinStatements.Add(finstmt);
                            }
                            if (newfinStatements.Count > 0  || tryStmt.CatchBlocks.Length > 0)
                            {
                                finBlock = finBlock.Update(finBlock.Locals, finBlock.LocalFunctions, finBlock.HasUnsafeModifier, finBlock.Instrumentation, newfinStatements.ToImmutableArray());
                                tryStmt = tryStmt.Update(tryStmt.TryBlock, tryStmt.CatchBlocks, finBlock, tryStmt.FinallyLabelOpt, tryStmt.PreferFaultHandler);
                                newStmts.Add(tryStmt);
                            }
                            else
                            {
                                // empty finally block and no Catch blocks, so we can remove the try completely
                                newStmts.Add(tryStmt.TryBlock);
                            }
                        }
                        else
                        {
                            newStmts.Add(stmt);
                        }
                        break;
                    default:
                        newStmts.Add(stmt);
                        break;
                }
            }
            body = body.Update(syms.ToImmutableArray(), body.LocalFunctions, body.HasUnsafeModifier, body.Instrumentation, newStmts.ToImmutableArray());
            return body;
        }
    }
}
