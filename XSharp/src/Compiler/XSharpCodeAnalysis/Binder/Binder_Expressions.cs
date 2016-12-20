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
using static LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

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

        private bool BindVulcanPointerDereference(CastExpressionSyntax node, TypeSymbol targetType, BoundExpression operand, 
            DiagnosticBag diagnostics, out BoundExpression expression)
        {
            // Type(pPointer) -> Dereference pointer
            if (node.XNode is PrimaryExpressionContext)
            {
                PrimaryExpressionContext pe = (PrimaryExpressionContext)node.XNode;
                if (pe.Expr is VoConversionExpressionContext && operand.Type.IsPointerType())
                {
                    // Dereference pointer
                    // Convert INT(<ptr>) to ((INT PTR) <ptr>)[0]
                    // No need to worry about /AZ. This has been handled already
                    var index = new BoundLiteral(node, ConstantValue.Create(0), Compilation.GetSpecialType(SpecialType.System_Int32)) { WasCompilerGenerated = true };
                    var ptrtype = new PointerTypeSymbol(targetType);
                    HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                    var newConv = Conversions.ClassifyConversionForCast(operand, ptrtype, ref useSiteDiagnostics);
                    var ptrconv = new BoundConversion(node, operand, newConv, true, true, null, ptrtype) { WasCompilerGenerated = true };
                    expression = new BoundPointerElementAccess(node, ptrconv, index, false, targetType) { WasCompilerGenerated = true };
                    return true;
                }
            }

            expression = null;
            return false;
        }
        private BoundExpression BindIndexerOrVulcanArrayAccess(ExpressionSyntax node, BoundExpression expr, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics)
        {
            if (Compilation.Options.IsDialectVO)
            {
                var arrayType = Compilation.GetWellKnownType(WellKnownType.Vulcan___Array);
                var usualType = Compilation.GetWellKnownType(WellKnownType.Vulcan___Usual);
                if (((NamedTypeSymbol)expr.Type).ConstructedFrom == usualType)
                {
                    // Index operator on USUAL then we convert the usual to an array first
                    expr = BindCastCore(node, expr, arrayType, wasCompilerGenerated: true, diagnostics: diagnostics);
                }
                if (((NamedTypeSymbol)expr.Type).ConstructedFrom == arrayType)
                {
                    ImmutableArray<BoundExpression> args;
                    ArrayBuilder<BoundExpression> argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                    foreach (var arg in analyzedArguments.Arguments)
                    {
                        var left = arg;
                        int compoundStringLength = 0;
                        BinaryOperatorKind opKind ;
                        // normalize the type
                        // we allow the following 'normal' types for the Index operator:
                        // int32, uint32, int64, uint64
                        // all other types (usual, float, real4, real8, decimal) are converted to int32
                        switch (left.Type.SpecialType)
                        {
                            case SpecialType.System_Int32:
                                opKind = BinaryOperatorKind.IntSubtraction;
                                break;
                            case SpecialType.System_UInt32:
                                opKind = BinaryOperatorKind.UIntSubtraction;
                                break;
                            case SpecialType.System_Int64:
                                opKind = BinaryOperatorKind.LongSubtraction;
                                break;
                            case SpecialType.System_UInt64:
                                opKind = BinaryOperatorKind.ULongSubtraction;
                                break;
                            default:
                                left = CreateConversion(left, Compilation.GetSpecialType(SpecialType.System_Int32), diagnostics);
                                opKind = BinaryOperatorKind.IntSubtraction;
                                break;
                        }
                        BoundExpression newarg;
                        if (!Compilation.Options.ArrayZero)
                        {
                            // when ! ArrayZero then subtract one from the index
                            var right = new BoundLiteral(arg.Syntax, ConstantValue.Create(1), arg.Type) { WasCompilerGenerated = true };
                            // when the argument is a literal then we may be able to fold the subtract expression.
                            var resultConstant = FoldBinaryOperator(arg.Syntax, opKind, left, right, left.Type.SpecialType, diagnostics, ref compoundStringLength);
                            var sig = this.Compilation.builtInOperators.GetSignature(opKind);
                            newarg = new BoundBinaryOperator(arg.Syntax, BinaryOperatorKind.Subtraction,
                                left, right,
                                resultConstant,
                                sig.Method,
                                resultKind: LookupResultKind.Viable,
                                originalUserDefinedOperatorsOpt: ImmutableArray<MethodSymbol>.Empty,
                                type: left.Type,
                                hasErrors: false)
                            { WasCompilerGenerated = true };
                        }
                        else
                        {
                            newarg = left;
                        }
                        argsBuilder.Add(newarg);
                    }
                    args = argsBuilder.ToImmutableAndFree();
                    if (args.Count() > 1)
                    {
                        // create a an array of ints and use that as the index for the array
                        // this will make sure that the proper GetIndex calls is chosen
                        argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
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

                    if (Compilation.Options.VOImplicitCastsAndConversions)
                    {
                        if (arg is BoundAddressOfOperator)
                        {
                            arg = (arg as BoundAddressOfOperator).Operand;
                            if (!analyzedArguments.RefKinds.Any())
                            {
                                for (int j = 0; j < analyzedArguments.Arguments.Count; j++)
                                    analyzedArguments.RefKinds.Add(i==j ? RefKind.Ref : RefKind.None);
                            }
                            else
                            {
                                analyzedArguments.RefKinds[i] = RefKind.Ref;
                            }
                            analyzedArguments.Arguments[i] = arg;
                        }
                    }

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

                    if (Compilation.Options.VOImplicitCastsAndConversions)
                    {
                        if (arg is BoundAddressOfOperator)
                        {
                            arg = (arg as BoundAddressOfOperator).Operand;
                        }
                        if (!analyzedArguments.RefKinds.Any())
                        {
                            for (int j = 0; j < analyzedArguments.Arguments.Count; j++)
                                analyzedArguments.RefKinds.Add(i == j ? RefKind.Ref : RefKind.None);
                        }
                        else
                        {
                            analyzedArguments.RefKinds[i] = RefKind.Ref;
                        }
                        analyzedArguments.Arguments[i] = arg;
                    }

                    if (!CheckIsVariable(arg.Syntax, arg, BindValueKind.OutParameter, checkingReceiver: false, diagnostics: diagnostics))
                        return false;
                }
            }
            return true;
        }
        private BoundExpression TryBindLateBoundCall(
            ExpressionSyntax node, 
            BoundExpression boundLeft, 
            TypeSymbol leftType, 
            SimpleNameSyntax right,
            bool invoked,
            bool indexed
            )
        {
            if (Compilation.Options.IsDialectVO && Compilation.Options.LateBinding && right.Kind() != SyntaxKind.GenericName)
            {
                string propName = right.Identifier.ValueText;
                if (leftType != null)
                {
                    bool earlyBound = propName == ".ctor";
                    bool isObject = leftType.IsObjectType();
                    bool isUsual = !isObject && leftType is NamedTypeSymbol
                        && ((NamedTypeSymbol)leftType).ConstructedFrom == Compilation.GetWellKnownType(WellKnownType.Vulcan___Usual);
                    // Late bound will only work for OBJECT or USUAL
                    if (isObject || isUsual)
                    {
                        if (isUsual)
                        {
                            earlyBound |= String.Compare(propName, "_NIL", StringComparison.OrdinalIgnoreCase) == 0;
                            //earlyBound |= String.Compare(propName, "Value", StringComparison.OrdinalIgnoreCase) == 0;
                        }
                        if (isObject)
                        {
                            earlyBound |= leftType.GetMembers(propName).Length > 0;
                        }
                        if (!earlyBound)
                        {
                            return new BoundDynamicMemberAccess(
                                syntax: node,
                                receiver: boundLeft,
                                typeArgumentsOpt: default(ImmutableArray<TypeSymbol>),
                                name: propName,
                                invoked: invoked,
                                indexed: indexed,
                                type: Compilation.GetWellKnownType(WellKnownType.Vulcan___Usual),
                                hasErrors: false);
                        }
                    }
                }
            }
            return null;
        }
        private BoundExpression CheckVulcanIndexedValue(BoundExpression expr, BindValueKind valueKind, DiagnosticBag diagnostics)
        {
            var originalexpr = expr;
            expr = CheckValue(expr, BindValueKind.RValue, diagnostics);
            if (expr.Kind == BoundKind.BadExpression)
            {
                // When a lookup of the Item property fails, then try again
                if (originalexpr.Kind == BoundKind.MethodGroup)
                {
                    var methodGroup = originalexpr as BoundMethodGroup;
                    if (string.Equals(methodGroup.Name, "Item", StringComparison.OrdinalIgnoreCase))
                    {
                        diagnostics.Clear();
                        expr = CheckValue(methodGroup.InstanceOpt, BindValueKind.RValue, diagnostics);
                    }
                }
            }
            return expr;

        }
        private bool BindStringToPsz(CSharpSyntaxNode syntax, ref BoundExpression source, TypeSymbol destination)
        {
            if (source.Type != null && source.Type.SpecialType == SpecialType.System_String &&
                Compilation.Options.IsDialectVO &&
                (destination == Compilation.GetWellKnownType(WellKnownType.Vulcan___Psz)
                || destination.IsVoidPointer()))
            {
                // Note this calls the constructor for __PSZ with a string.
                // The allocated pointer inside the PSZ is never freed by Vulcan !
                TypeSymbol psz = Compilation.GetWellKnownType(WellKnownType.Vulcan___Psz);
                MethodSymbol stringctor = null;
                var ctors = psz.GetMembers(".ctor");
                foreach (MethodSymbol ctor in ctors)
                {
                    var pars = ctor.GetParameters();
                    if (pars.Length == 1 && pars[0].Type.SpecialType == SpecialType.System_String)
                    {
                        stringctor = ctor;
                    }
                }

                if (stringctor != null)
                {
                    source = new BoundObjectCreationExpression(syntax, stringctor, new BoundExpression[] { source });
                    return true;
                }
            }
            return false;
        }
    }
}