//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Roslyn.Utilities;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;

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
            if (destination.IsCodeblockType() && !destination.IsObjectType())
            {
                HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                conv = Conversions.ClassifyConversionFromType(Compilation.CodeBlockType(), destination, ref useSiteDiagnostics);
                diagnostics.Add(syntax, useSiteDiagnostics);
            }
            if (Compilation.Options.HasRuntime)
            {
                Debug.Assert(destination.IsCodeblockType()|| conv.Exists);
            }
            // Codeblocks are implemented as a special anonymous type (like CLASS {Name := "Robert"}
            // This type inherits from Codeblock and not from System.Object like the normal anonymous types
            // It gets the following members
            // 1) a Property for the lambda expression to evaluate
            // 2) a Property for the "original" string, so the codeblock can be shown in the debugger
            // 3) a Constructor
            // 4) an Eval method that overrides the eval method in the Codeblock class
            // The deletage (for 1) has a USUAL return value and USUAL parameters.
            // When there are 2 or more codeblocks with the same # of parameters then they share the delegate

            AnonymousTypeManager manager = this.Compilation.AnonymousTypeManager;
            var delegateSignature = new TypeSymbol[unboundLambda.ParameterCount + 1];
            var usualType = this.Compilation.UsualType();
            for (int i = 0; i < delegateSignature.Length; i++)
            {
                delegateSignature[i] = usualType;
            }
            // Create the delegate for the Lambda Property
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
                conversionGroupOpt: default,
                constantValueOpt: default,
                type: delType)
            { WasCompilerGenerated = unboundLambda.WasCompilerGenerated };
            // Construct the ToString()
            var cbSrc = new BoundLiteral(syntax, ConstantValue.Create(syntax.XCodeBlockSource), Compilation.GetSpecialType(SpecialType.System_String));
            // Instantiate the codeblock
            BoundExpression cbInst = new BoundAnonymousObjectCreationExpression(syntax,
                cbType.InstanceConstructors[0],
                new BoundExpression[] { cbDel, cbSrc }.ToImmutableArrayOrEmpty(),
                System.Collections.Immutable.ImmutableArray<BoundAnonymousPropertyDeclaration>.Empty, cbType)
            { WasCompilerGenerated = unboundLambda.WasCompilerGenerated }; 

            if (conv != Conversion.ImplicitReference)
            {
                cbInst = new BoundConversion(syntax,
                    cbInst,
                    Conversion.ImplicitReference,
                    @checked: false,
                    explicitCastInCode: false,
                    conversionGroupOpt: default,
                    constantValueOpt: default,
                    type: Compilation.CodeBlockType())
                { WasCompilerGenerated = unboundLambda.WasCompilerGenerated }; 
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
                    conversionGroupOpt: isCast ? new ConversionGroup(conv) : default,
                    constantValueOpt: default,
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
                conversionGroupOpt: new ConversionGroup(conv), 
                constantValueOpt: default,
                type: destination)
            { WasCompilerGenerated = unboundLambda.WasCompilerGenerated };
        }
        internal static BoundBlock FixCodeBlockProblems(LambdaSymbol lambdaSymbol, Binder lambdaBodyBinder, BoundBlock block, DiagnosticBag diagnostics)
        {
            // check for a Lambda that returns a USUAL
            if (lambdaSymbol.ReturnType.IsNotUsualType())
                return block;
            var usualType = lambdaBodyBinder.Compilation.UsualType();
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
                        if (boundConv.Type.IsUsualType() && operand.Type.SpecialType == SpecialType.System_Void)
                        {
                            var errors = diagnostics.ToReadOnly();
                            diagnostics.Clear();
                            foreach (var error in errors)
                            {
                                if (error.Code != (int)ErrorCode.ERR_NoImplicitConv &&
                                    error.Code != (int)ErrorCode.ERR_CantConvAnonMethReturns)
                                    diagnostics.Add(error);
                            }
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
