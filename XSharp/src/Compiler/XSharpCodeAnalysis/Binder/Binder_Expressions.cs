// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

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

namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// This portion of the binder converts an <see cref="ExpressionSyntax"/> into a <see cref="BoundExpression"/>.
    /// </summary>
    internal partial class Binder
    {
        private static InitializerExpressionSyntax s_constructInitializerFromArguments(AnalyzedArguments analyzedArguments)
        {
            var expressions = SeparatedSyntaxListBuilder<ExpressionSyntax>.Create();
            foreach (var arg in analyzedArguments.Arguments)
            {
                if (expressions.Count > 0)
                    expressions.AddSeparator(SyntaxFactory.MissingToken(SyntaxKind.CommaToken));
                expressions.Add(arg.Syntax as ExpressionSyntax);
            }
            return SyntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression,
                expressions);
        }

        private BoundExpression BindIndexerOrVulcanArrayAccess(ExpressionSyntax node, BoundExpression expr, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics)
        {
            if (Compilation.Options.IsDialectVO)
            {
                var arrayType = Compilation.GetWellKnownType(WellKnownType.Vulcan___Array);
                var usualType = Compilation.GetWellKnownType(WellKnownType.Vulcan___Usual);
                if (((NamedTypeSymbol)expr.Type).ConstructedFrom == usualType)
                {
                    expr = BindCastCore(node, expr, arrayType, wasCompilerGenerated: true, diagnostics: diagnostics);
                }
                if (((NamedTypeSymbol)expr.Type).ConstructedFrom == arrayType)
                {
                    ImmutableArray<BoundExpression> args;
                    if (!this.Compilation.Options.ArrayZero)
                    {
                        ArrayBuilder<BoundExpression> argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                        foreach (var arg in analyzedArguments.Arguments)
                        {
                            var left = arg;
                            var right = new BoundLiteral(arg.Syntax, ConstantValue.Create(1), arg.Type) { WasCompilerGenerated = true };
                            int compoundStringLength = 0;
                            var opKind = left.Type.SpecialType == SpecialType.System_Int32 ? BinaryOperatorKind.IntSubtraction
                                : left.Type.SpecialType == SpecialType.System_Int64 ? BinaryOperatorKind.LongSubtraction
                                : left.Type.SpecialType == SpecialType.System_UInt32 ? BinaryOperatorKind.UIntSubtraction
                                : BinaryOperatorKind.ULongSubtraction;
                            var resultConstant = FoldBinaryOperator(arg.Syntax, opKind, left, right, left.Type.SpecialType, diagnostics, ref compoundStringLength);
                            var sig = this.Compilation.builtInOperators.GetSignature(opKind);
                            argsBuilder.Add(new BoundBinaryOperator(arg.Syntax, BinaryOperatorKind.Subtraction,
                                left, right,
                                resultConstant,
                                sig.Method,
                                resultKind: LookupResultKind.Viable,
                                originalUserDefinedOperatorsOpt: ImmutableArray<MethodSymbol>.Empty,
                                type: arg.Type,
                                hasErrors: false)
                            { WasCompilerGenerated = true });
                        }
                        args = argsBuilder.ToImmutableAndFree();
                    }
                    else
                        args = analyzedArguments.Arguments.ToImmutable();
                    if (args.Count() > 1)
                    {
                        ArrayBuilder<BoundExpression> argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                        var exprs = SeparatedSyntaxListBuilder<ExpressionSyntax>.Create();
                        foreach (var arg in args)
                        {
                            if (exprs.Count > 0)
                                exprs.AddSeparator(SyntaxFactory.MissingToken(SyntaxKind.CommaToken));
                            exprs.Add(arg.Syntax as ExpressionSyntax);
                            argsBuilder.Add(BindCastCore(arg.Syntax as ExpressionSyntax, arg, Compilation.GetSpecialType(SpecialType.System_Int32), wasCompilerGenerated: true, diagnostics: diagnostics));
                        }
                        var initSyntax = SyntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression, exprs);
                        args = argsBuilder.ToImmutable();
                        argsBuilder.Clear();
                        argsBuilder.Add(BindArrayCreationWithInitializer(diagnostics,
                            creationSyntax: null,
                            initSyntax: initSyntax,
                            type: ArrayTypeSymbol.CreateCSharpArray(this.Compilation.Assembly, Compilation.GetSpecialType(SpecialType.System_Int32), ImmutableArray<CustomModifier>.Empty),
                            sizes: ImmutableArray<BoundExpression>.Empty,
                            boundInitExprOpt: args));
                        args = argsBuilder.ToImmutableAndFree();
                    }
                    return new BoundIndexerAccess(
                        syntax: node,
                        receiverOpt: expr,
                        indexer: analyzedArguments.Arguments.Count == 1 ? (arrayType as Symbols.Metadata.PE.PENamedTypeSymbol).VulcanArrayIndexerOne
                            : (Compilation.GetWellKnownType(WellKnownType.Vulcan___Array) as Symbols.Metadata.PE.PENamedTypeSymbol).VulcanArrayIndexerMany,
                        arguments: args,
                        argumentNamesOpt: default(ImmutableArray<string>),
                        argumentRefKindsOpt: default(ImmutableArray<RefKind>),
                        expanded: false,
                        argsToParamsOpt: default(ImmutableArray<int>),
                        type: usualType,
                        hasErrors: false)
                    { WasCompilerGenerated = true };
                }
            }

            return BindIndexerAccess(node, expr, analyzedArguments, diagnostics);
        }
        private bool CheckValidRefOmittedArguments(OverloadResolutionResult<MethodSymbol> result, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics)
        {
            for (int i = 0; i < analyzedArguments.Arguments.Count; i++)
            {
                if (analyzedArguments.RefKind(i) == RefKind.None && result.ValidResult.Member.Parameters[result.ValidResult.Result.ParameterFromArgument(i)].RefKind != RefKind.None)
                {
                    var arg = analyzedArguments.Arguments[i];
                    if (!CheckIsVariable(arg.Syntax, arg, BindValueKind.OutParameter, checkingReceiver: false, diagnostics: diagnostics))
                        return false;
                }
            }
            return true;
        }

        private bool CheckValidRefOmittedArguments(OverloadResolutionResult<PropertySymbol> result, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics)
        {
            for (int i = 0; i < analyzedArguments.Arguments.Count; i++)
            {
                if (analyzedArguments.RefKind(i) == RefKind.None && result.ValidResult.Member.Parameters[result.ValidResult.Result.ParameterFromArgument(i)].RefKind != RefKind.None)
                {
                    var arg = analyzedArguments.Arguments[i];
                    if (!CheckIsVariable(arg.Syntax, arg, BindValueKind.OutParameter, checkingReceiver: false, diagnostics: diagnostics))
                        return false;
                }
            }
            return true;
        }

    }
}