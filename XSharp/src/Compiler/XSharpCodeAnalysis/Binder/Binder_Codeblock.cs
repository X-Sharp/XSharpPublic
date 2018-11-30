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
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Roslyn.Utilities;
using System.Collections.Immutable;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class Binder
    {
        private BoundExpression BindCodeblock(SyntaxNode syntax, UnboundLambda unboundLambda, Conversion conversion, bool isCast, TypeSymbol destination, DiagnosticBag diagnostics)
        {
            if (!Compilation.Options.HasRuntime)
                return null;
            var isCodeblock = syntax.XIsCodeBlock;
            if (!isCodeblock)
            {
                isCodeblock = !destination.IsDelegateType() && !destination.IsExpressionTree();
            }
            if (!isCodeblock)
                return null;

            Conversion conv = Conversion.ImplicitReference;
            if (destination != Compilation.CodeBlockType() && !destination.IsObjectType())
            {
                HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                conv = Conversions.ClassifyConversionFromType(Compilation.CodeBlockType(), destination, ref useSiteDiagnostics);
                diagnostics.Add(syntax, useSiteDiagnostics);
            }
            if (Compilation.Options.HasRuntime)
            {
                Debug.Assert(destination == Compilation.CodeBlockType()|| conv.Exists);
            }
            if (!syntax.XIsCodeBlock && !Compilation.Options.MacroScript)
            {
                Error(diagnostics, ErrorCode.ERR_CodeblockWithLambdaSyntax, syntax);
            }

            AnonymousTypeManager manager = this.Compilation.AnonymousTypeManager;
            var delegateSignature = new TypeSymbol[unboundLambda.ParameterCount + 1];
            var usualType = this.Compilation.UsualType();
            for (int i = 0; i < delegateSignature.Length; i++)
            {
                delegateSignature[i] = usualType;
            }
            NamedTypeSymbol cbType = manager.ConstructCodeblockTypeSymbol(delegateSignature, syntax.Location);
            var delType = manager.GetCodeblockDelegateType(cbType);
            var _boundLambda = unboundLambda.Bind(delType);
            diagnostics.AddRange(_boundLambda.Diagnostics);
            var cbDel = new BoundConversion(
                syntax,
                _boundLambda,
                conversion,
                @checked: false,
                explicitCastInCode: false,
                constantValueOpt: ConstantValue.NotAvailable,
                type: delType)
            { WasCompilerGenerated = unboundLambda.WasCompilerGenerated };
            BoundExpression cbInst = new BoundAnonymousObjectCreationExpression(syntax,
                cbType.InstanceConstructors[0],
                new BoundExpression[] { cbDel }.ToImmutableArrayOrEmpty(),
                System.Collections.Immutable.ImmutableArray<BoundAnonymousPropertyDeclaration>.Empty, cbType)
            { WasCompilerGenerated = unboundLambda.WasCompilerGenerated }; ;
            if (conv != Conversion.ImplicitReference)
            {
                cbInst = new BoundConversion(syntax, cbInst, Conversion.ImplicitReference, false, false, ConstantValue.NotAvailable, Compilation.CodeBlockType())
                { WasCompilerGenerated = unboundLambda.WasCompilerGenerated }; ;
            }
            if (!conv.IsValid || (!isCast && conv.IsExplicit))
            {
                GenerateImplicitConversionError(diagnostics, syntax, conv, cbInst, destination);

                return new BoundConversion(
                    syntax,
                    cbInst,
                    conv,
                    false,
                    explicitCastInCode: isCast,
                    constantValueOpt: ConstantValue.NotAvailable,
                    type: destination,
                    hasErrors: true)
                { WasCompilerGenerated = unboundLambda.WasCompilerGenerated };
            }
            return new BoundConversion(
                syntax,
                cbInst,
                conv,
                false,
                explicitCastInCode: isCast,
                constantValueOpt: ConstantValue.NotAvailable,
                type: destination)
            { WasCompilerGenerated = unboundLambda.WasCompilerGenerated };
        }
        internal static BoundBlock FixCodeBlockProblems(LambdaSymbol lambdaSymbol, Binder lambdaBodyBinder, BoundBlock block, DiagnosticBag diagnostics)
        {
            // check for a Lambda that returns a USUAL
            var usualType = lambdaBodyBinder.Compilation.UsualType();
            if (lambdaSymbol.ReturnType != usualType)
                return block;
            // handle 2 problems:
            // 1) no statements, then add a return statement
            // 2) last statement is a void expression. Then the conversion to USUAL fails
            var count = block.Statements.Length;
            List<BoundStatement> newlist = new List<BoundStatement>();
            if (count == 0)
            {
                var result = new BoundDefaultExpression(block.Syntax, usualType);
                newlist.Add(new BoundReturnStatement(block.Syntax, RefKind.None, result));
                block = block.Update(block.Locals, ImmutableArray<LocalFunctionSymbol>.Empty, newlist.ToImmutableArray<BoundStatement>());
            }
            else
            {
                var stmt = block.Statements[count - 1];
                if (stmt is BoundReturnStatement && stmt.HasErrors)
                {
                    BoundExpression expr = (stmt as BoundReturnStatement).ExpressionOpt;
                    // when the last expression is a conversion to USUAL
                    // and there is an error, then this is most likely the conversion from 
                    // a void to USUAL. When that happens, then create an extra stmt in the body of the lambda
                    // store the return expression in an expression statement
                    // and return a NIL
                    if (expr is BoundConversion)
                    {
                        var boundConv = expr as BoundConversion;
                        var operand = boundConv.Operand;
                        if (boundConv.Type == usualType && operand.Type.SpecialType == SpecialType.System_Void)
                        {
                            diagnostics.Clear();
                            for (int i = 0; i < block.Statements.Length - 1; i++)
                            {
                                newlist.Add(block.Statements[i]);
                            }
                            newlist.Add(new BoundExpressionStatement(stmt.Syntax, operand));
                            var result = new BoundDefaultExpression(stmt.Syntax, usualType);
                            newlist.Add(new BoundReturnStatement(stmt.Syntax, RefKind.None, result));
                            block = new BoundBlock(block.Syntax, block.Locals, newlist.ToImmutableArray<BoundStatement>());
                        }
                    }
                }
            }
            return block;
        }

    }
}
