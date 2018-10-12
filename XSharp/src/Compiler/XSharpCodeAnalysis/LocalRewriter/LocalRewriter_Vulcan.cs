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
using System.Linq;
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

        public BoundExpression MakePsz(BoundExpression value)
        {
            var type = _compilation.PszType();
            if (value.Type == type)
                return value;
            bool isString;
            if (value.Type == _compilation.UsualType())
            {
                var op = getImplicitOperator(value.Type, type);
                var result = _factory.StaticCall(value.Type, (MethodSymbol)op, value);
                result.WasCompilerGenerated = true;
                this._diagnostics.Add(ErrorCode.WRN_CompilerGeneratedPSZConversionGeneratesMemoryleak, value.Syntax.Location);
                return result;
            }
            isString = value.Type.SpecialType == SpecialType.System_String;
            var ctors = type.Constructors;
            foreach (var ctor in ctors)
            {
                if (ctor.ParameterCount == 1) // there should only be constructors with one or zero parameters.
                {
                    bool found = false;
                    var partype = ctor.GetParameterTypes()[0];
                    if (isString && partype.SpecialType == SpecialType.System_String)
                    {
                        found = true;
                    }
                    if (! isString && partype.SpecialType == SpecialType.System_IntPtr)
                    {
                        found = true;
                        if (value.Type.SpecialType != SpecialType.System_IntPtr)
                        {
                            value = new BoundConversion(value.Syntax, value, Conversion.Identity, false, false, null, partype, false);
                        }
                        
                    }
                    if (found)
                    {
                        return new BoundObjectCreationExpression(value.Syntax, ctor, value);
                    }
                }
            }
            return value;
        }
        static IEnumerable<ISymbol> FindMembers(CSharpCompilation compilation, string name)
        {
            Func<string, bool> predicate = n => StringComparer.Ordinal.Equals(name, n);
            SymbolFilter filter = SymbolFilter.Member;
            return compilation.GetSymbolsWithName(predicate, filter);
        }

        private static BoundExpressionStatement ClearGlobal(CSharpCompilation compilation, SyntaxNode node, FieldSymbol field)
        {
            var lhs = new BoundFieldAccess(node, null, field, null) { WasCompilerGenerated = true };
            var rhs = new BoundDefaultOperator(node, field.Type) { WasCompilerGenerated = true };
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
                 MethodSymbol method,
                 BoundStatement statement,
                 DiagnosticBag diagnostics)

        {
            if (method.Name != XSharpSpecialNames.AppExit)
                return statement;
            var refMan = method.DeclaringCompilation.GetBoundReferenceManager();
            var vcla = method.DeclaringCompilation.ClassLibraryType();
            var newstatements = new List<BoundStatement>();

            foreach (var rkv in refMan.GetReferencedAssemblies())
            {
                var r = rkv.Value;
                foreach (var attr in r.GetAttributes())
                {
                    if ((Symbol)attr.AttributeClass.ConstructedFrom == vcla)
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
                                var members = type.GetMembers(XSharpSpecialNames.ExitProc);
                                if (members.Length > 0)
                                {
                                    foreach (MethodSymbol sym in members)
                                    {
                                        CreateMethodCall(method.DeclaringCompilation, statement.Syntax, sym, newstatements);
                                    }
                                }
                                else
                                {
                                    ClearGlobals(method.DeclaringCompilation, statement.Syntax, type, newstatements);
                                }

                            }
                        }
                        break;
                    }
                }
            }
            // Now clear the globals in this assembly by calling $Exit

            var symbols = FindMembers(method.DeclaringCompilation, XSharpSpecialNames.ExitProc);
            foreach (MethodSymbol sym in symbols)
            {
                CreateMethodCall(method.DeclaringCompilation, statement.Syntax, sym, newstatements);
            }
            newstatements.Add(new BoundReturnStatement(statement.Syntax, RefKind.None, null));
            var oldbody = statement as BoundBlock;
            var newbody = oldbody.Update(oldbody.Locals, ImmutableArray<LocalFunctionSymbol>.Empty, newstatements.ToImmutableArray<BoundStatement>());
            return newbody;
        }

        private static BoundExpressionStatement vulcanruntimeStateAssign(SyntaxNode syntax, FieldSymbol field, bool value, TypeSymbol type)
        {
            var bfa = new BoundFieldAccess(syntax, null, field, ConstantValue.NotAvailable) { WasCompilerGenerated = true };
            var lit = new BoundLiteral(syntax, ConstantValue.Create(value), type) { WasCompilerGenerated = true };
            var ass = new BoundAssignmentOperator(syntax, bfa, lit, RefKind.None, lit.Type) { WasCompilerGenerated = true };
            var stmt = new BoundExpressionStatement(syntax, ass) { WasCompilerGenerated = true };
            return stmt;
        }
        private static BoundExpressionStatement xsharpruntimeStateAssign(SyntaxNode syntax, PropertySymbol prop, bool value, TypeSymbol type)
        {
            var bpa = new BoundPropertyAccess(syntax, null, prop, LookupResultKind.Viable,type) { WasCompilerGenerated = true };
            var lit = new BoundLiteral(syntax, ConstantValue.Create(value), type) { WasCompilerGenerated = true };
            var ass = new BoundAssignmentOperator(syntax, bpa, lit, RefKind.None, lit.Type) { WasCompilerGenerated = true };
            var stmt = new BoundExpressionStatement(syntax, ass) { WasCompilerGenerated = true };
            return stmt;
        }

        public static BoundStatement RewriteAppInit(
            MethodSymbol method,
            BoundStatement statement,
            DiagnosticBag diagnostics)

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
            var vrt = comp.GetBoundReferenceManager().GetReferencedAssemblies().Where(x => x.Value.Name == "VulcanRT");
            if (vrt.Count() != 0)
            {
                var vulcanrt = vrt.First().Value;
                var type = vulcanrt.GetTypeByMetadataName("Vulcan.Runtime.State");
                if (type != null)
                {
                    var logic = comp.GetSpecialType(SpecialType.System_Boolean);
                    var mem = type.GetMembers("CompilerOptionFOvf");
                    if (mem.Length > 0 && mem[0].Kind == SymbolKind.Field)
                    {
                        newstatements.Add(vulcanruntimeStateAssign(oldbody.Syntax, mem[0] as FieldSymbol, comp.Options.CheckOverflow, logic));
                    }
                    mem = type.GetMembers("CompilerOptionOvf");
                    if (mem.Length > 0 && mem[0].Kind == SymbolKind.Field)
                    {
                        newstatements.Add(vulcanruntimeStateAssign(oldbody.Syntax, mem[0] as FieldSymbol, comp.Options.CheckOverflow, logic));
                    }
                    mem = type.GetMembers("CompilerOptionVO11");
                    if (mem.Length > 0 && mem[0].Kind == SymbolKind.Field)
                    {
                        newstatements.Add(vulcanruntimeStateAssign(oldbody.Syntax, mem[0] as FieldSymbol, comp.Options.vo11, logic));
                    }
                }
            }
            var xc = comp.GetBoundReferenceManager().GetReferencedAssemblies().Where(x => x.Value.Name == "XSharp.Core");
            if (xc.Count() != 0)
            {
                var xsrt = xc.First().Value;
                var type = xsrt.GetTypeByMetadataName("XSharp.RuntimeState");
                if (type != null)
                {
                    var logic = comp.GetSpecialType(SpecialType.System_Boolean);
                    var mem = type.GetMembers("CompilerOptionFOvf");
                    if (mem.Length > 0 && mem[0].Kind == SymbolKind.Property)
                    {
                        newstatements.Add(xsharpruntimeStateAssign(oldbody.Syntax, mem[0] as PropertySymbol, comp.Options.CheckOverflow, logic));
                    }
                    mem = type.GetMembers("CompilerOptionOvf");
                    if (mem.Length > 0 && mem[0].Kind == SymbolKind.Property)
                    {
                        newstatements.Add(xsharpruntimeStateAssign(oldbody.Syntax, mem[0] as PropertySymbol, comp.Options.CheckOverflow, logic));
                    }
                    mem = type.GetMembers("CompilerOptionVO11");
                    if (mem.Length > 0 && mem[0].Kind == SymbolKind.Property)
                    {
                        newstatements.Add(xsharpruntimeStateAssign(oldbody.Syntax, mem[0] as PropertySymbol, comp.Options.vo11, logic));
                    }
                    mem = type.GetMembers("CompilerOptionVO13");
                    if (mem.Length > 0 && mem[0].Kind == SymbolKind.Property)
                    {
                        newstatements.Add(xsharpruntimeStateAssign(oldbody.Syntax, mem[0] as PropertySymbol, comp.Options.vo13, logic));
                    }
                }
            }

            var initstmts = GetInitStatements(method.DeclaringCompilation, statement,false);
            newstatements.AddRange(initstmts);
            tryblock = tryblock.Update(tryblock.Locals, ImmutableArray<LocalFunctionSymbol>.Empty, newstatements.ToImmutableArray<BoundStatement>());
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
            oldbody = oldbody.Update(oldbody.Locals, ImmutableArray<LocalFunctionSymbol>.Empty, newstatements.ToImmutableArray<BoundStatement>());
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
            if (! localOnly)
            {
                foreach (var rkv in refMan.GetReferencedAssemblies())
                {
                    var r = rkv.Value;
                    foreach (var attr in r.GetAttributes())
                    {
                        if ((Symbol)attr.AttributeClass.ConstructedFrom == vcla)
                        {
                            var attargs = attr.CommonConstructorArguments;
                            if (attargs.Length == 2)
                            {
                                var functionsClassName = attargs[0].Value.ToString();
                                if (!string.IsNullOrEmpty(functionsClassName))
                                {
                                    var type = r.GetTypeByMetadataName(functionsClassName);
                                    init1.AddRange(type.GetMembers(XSharpSpecialNames.InitProc1));
                                    init2.AddRange(type.GetMembers(XSharpSpecialNames.InitProc2));
                                    init3.AddRange(type.GetMembers(XSharpSpecialNames.InitProc3));
                                }
                            }
                        }
                    }
                }
            }

            var members = FindMembers(compilation, XSharpSpecialNames.InitProc1);
            init1.AddRange(members);
            members = FindMembers(compilation, XSharpSpecialNames.InitProc2);
            init2.AddRange(members);
            members = FindMembers(compilation, XSharpSpecialNames.InitProc3);
            init3.AddRange(members);
            // Now join all 3 lists to one list
            init1.AddRange(init2);
            init1.AddRange(init3);

            foreach (MethodSymbol sym in init1)
            {
                CreateMethodCall(compilation, statement.Syntax, sym, newstatements);
            }
            return newstatements;
        }
        public static BoundStatement RewriteRunInitProc(
            MethodSymbol method,
            BoundStatement statement,
            DiagnosticBag diagnostics)

        {
            if ( method.Name != XSharpFunctionNames.RunInitProcs)
                return statement;
            var oldbody = statement as BoundBlock;
            var newstatements = new List<BoundStatement>();
            var initstmts = GetInitStatements(method.DeclaringCompilation, statement,true);
            newstatements.AddRange(initstmts);
            newstatements.Add(new BoundReturnStatement(statement.Syntax, RefKind.None, null) { WasCompilerGenerated = true } );
            oldbody = oldbody.Update(oldbody.Locals, ImmutableArray<LocalFunctionSymbol>.Empty, newstatements.ToImmutableArray<BoundStatement>());
            oldbody.WasCompilerGenerated = true;
            return oldbody;
        }



        public static BoundStatement RewriteExit(
                 MethodSymbol method,
                 BoundStatement statement,
                 DiagnosticBag diagnostics)

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

    }
}
