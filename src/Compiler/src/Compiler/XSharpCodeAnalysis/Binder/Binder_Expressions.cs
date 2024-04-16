//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Syntax;
using static LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// This portion of the binder converts an <see cref="ExpressionSyntax"/> into a <see cref="BoundExpression"/>.
    /// </summary>
    internal partial class Binder
    {

        private bool BindVOPointerDereference(CastExpressionSyntax node, TypeWithAnnotations targetType, BoundExpression operand,
            DiagnosticBag diagnostics, out BoundExpression expression)
        {
            // Type(pPointer) -> Dereference pointer
            // Vulcan only allows this with pPointer is of type PTR (Void pointer)
            if (node.XNode is PrimaryExpressionContext pe)
            {
                if (pe.Expr is VoConversionExpressionContext && operand.Type.IsPointerType())
                {
                    var tType = targetType.Type;
                    // Dereference pointer
                    // only allowed when source is Void Ptr
                    // or source is <TargetType> PTR
                    // Convert INT(<ptr>) to ((INT PTR) <ptr>)[0]
                    // No need to worry about /AZ. This has been handled already
                    // make sure that PSZ(ptr) is not dereferenced !
                    bool canConvert = operand.Type.IsVoidPointer() && !tType.IsPszType();
                    if (!canConvert)
                    {
                        PointerTypeSymbol pt = operand.Type as PointerTypeSymbol;
                        canConvert = TypeSymbol.Equals(pt.PointedAtType, tType);
                    }
                    if (canConvert)
                    {
                        var index = new BoundLiteral(node, ConstantValue.Create(0), Compilation.GetSpecialType(SpecialType.System_Int32)) { WasCompilerGenerated = true };
                        var ptrtype = new PointerTypeSymbol(targetType);
                        HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                        //var newConv = Conversions.ClassifyConversionForCast(operand, ptrtype, ref useSiteDiagnostics);
                        var newConv = Conversions.ClassifyConversionFromExpression(operand, ptrtype, ref useSiteDiagnostics, forCast: true);
                        var ptrconv = new BoundConversion(node, operand, newConv, true, false,
                            conversionGroupOpt: null,
                            constantValueOpt: null,
                            type: ptrtype)
                        { WasCompilerGenerated = true };
                        expression = new BoundPointerElementAccess(node, ptrconv, index, false, tType) { WasCompilerGenerated = true };
                        return true;
                    }
                }
            }

            expression = null;
            return false;
        }

        internal BoundExpression Usual2Object(BoundExpression expression, SyntaxNode node, DiagnosticBag diagnostics)
        {
            if (expression.Type.IsUsualType())
            {
                expression = new BoundConversion(node, expression, Conversion.Special, false, false,
                    conversionGroupOpt: null,
                    constantValueOpt: null,
                    type: GetSpecialType(SpecialType.System_Object, diagnostics, node));
            }
            return expression;
        }

        public BoundExpression SubtractSystemIndex(BoundExpression index, DiagnosticBag diagnostics, bool checkZero = false)
        {
            var syntax = (CSharpSyntaxNode)index.Syntax;

            var kind = BinaryOperatorKind.Subtraction;
            var left = index;
            var leftType = left.Type;
            Debug.Assert(leftType.Equals(Compilation.GetWellKnownType(WellKnownType.System_Index)));

            var right = new BoundLiteral(syntax, ConstantValue.Create(1), index.Type) { WasCompilerGenerated = true };

            HashSet<DiagnosticInfo> useSiteDiagnostics = null;

            BoundExpression isFromEnd;
            {
                var symbol = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Index__get_IsFromEnd) as MethodSymbol;
                isFromEnd = new BoundCall(syntax, left, symbol, ImmutableArray<BoundExpression>.Empty, default, default, false, false, false, default, default, default, symbol.ReturnType) { WasCompilerGenerated = true };
            }

            BoundExpression leftValue;
            {
                var symbol = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Index__get_Value) as MethodSymbol;
                leftValue = new BoundCall(syntax, left, symbol, ImmutableArray<BoundExpression>.Empty, default, default, false, false, false, default, default, default, symbol.ReturnType) { WasCompilerGenerated = true };
            }

            var opKind = BinaryOperatorKind.IntSubtraction;
            var resultConstant = FoldBinaryOperator(syntax, opKind, leftValue, right, leftValue.Type.SpecialType, diagnostics);
            var sig = this.Compilation.builtInOperators.GetSignature(opKind);
            BoundExpression whenFalse = new BoundBinaryOperator(syntax, kind, leftValue, right, resultConstant, sig.Method,
                resultKind: LookupResultKind.Viable,
                originalUserDefinedOperatorsOpt: ImmutableArray<MethodSymbol>.Empty,
                type: index.Type,
                hasErrors: false)
            { WasCompilerGenerated = true };

            var whenTrue = index;

            if (checkZero)
            {
                BoundExpression isZero;
                {
                    var zeroValue = new BoundLiteral(syntax, ConstantValue.Create(0), Compilation.GetSpecialType(SpecialType.System_Int32));
                    var resultConstantZ = FoldBinaryOperator(syntax, BinaryOperatorKind.IntEqual, leftValue, zeroValue, leftValue.Type.SpecialType, diagnostics);
                    isZero = new BoundBinaryOperator(syntax,
                        BinaryOperatorKind.IntEqual,
                        leftValue,
                        zeroValue,
                        resultConstantZ,
                        null,
                        LookupResultKind.Viable,
                        ImmutableArray<MethodSymbol>.Empty,
                        Compilation.GetSpecialType(SpecialType.System_Boolean),
                        false);
                }
                var constantValueZ = FoldConditionalOperator(isZero, whenTrue, whenFalse);
                bool hasErrorsZ = constantValueZ?.IsBad == true;
                whenFalse = new BoundConditionalOperator(syntax, false, isZero, whenTrue, whenFalse, constantValueZ, null, false, leftType, hasErrorsZ) { WasCompilerGenerated = true };
            }

            TypeSymbol type = BestTypeInferrer.InferBestTypeForConditionalOperator(whenTrue, whenFalse, this.Conversions, out bool hadMultipleCandidates, ref useSiteDiagnostics);
            diagnostics.Add(syntax, useSiteDiagnostics);
            var constantValue = FoldConditionalOperator(isFromEnd, whenTrue, whenFalse);
            bool hasErrors = type?.IsErrorType() == true || constantValue?.IsBad == true;
            return new BoundConditionalOperator(syntax, false, isFromEnd, whenTrue, whenFalse, constantValue, null, false, type, hasErrors) { WasCompilerGenerated = true };
        }
        public BoundExpression SubtractSystemRange(BoundExpression range, DiagnosticBag diagnostics)
        {
            var syntax = (CSharpSyntaxNode)range.Syntax;

            var rangeType = Compilation.GetWellKnownType(WellKnownType.System_Range);
            Debug.Assert(range.Type.Equals(rangeType));

            BoundExpression start;
            {
                var symbol = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Range__get_Start) as MethodSymbol;
                start = new BoundCall(syntax, range, symbol, ImmutableArray<BoundExpression>.Empty, default, default, false, false, false, default, default, default, symbol.ReturnType) { WasCompilerGenerated = true };
            }

            BoundExpression end;
            {
                var symbol = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Range__get_End) as MethodSymbol;
                end = new BoundCall(syntax, range, symbol, ImmutableArray<BoundExpression>.Empty, default, default, false, false, false, default, default, default, symbol.ReturnType) { WasCompilerGenerated = true };
            }

            start = SubtractSystemIndex(start, diagnostics, checkZero: true);

            var symbolOpt = (MethodSymbol)GetWellKnownTypeMember(
                Compilation,
                WellKnownMember.System_Range__ctor,
                diagnostics,
                syntax: syntax);
            return new BoundRangeExpression(syntax, start, end, symbolOpt, rangeType) { WasCompilerGenerated = true };
        }
        private BoundExpression SubtractIndex(BoundExpression expr, DiagnosticBag diagnostics, SpecialType? specialTypeOpt = null)
        {
            if (expr is BoundFromEndIndexExpression)
            {
                return expr;
            }

            expr = BindToNaturalType(expr, diagnostics, false);
            if (expr.Type.Equals(Compilation.GetWellKnownType(WellKnownType.System_Index)))
            {
                return SubtractSystemIndex(expr, diagnostics);
            }
            if (expr.Type.Equals(Compilation.GetWellKnownType(WellKnownType.System_Range)))
            {
                return SubtractSystemRange(expr, diagnostics);
            }

            var type = expr.Type;
            var specialType = specialTypeOpt ?? type.SpecialType;
            var kind = specialType switch
            {
                SpecialType.System_Int32 => BinaryOperatorKind.IntSubtraction,
                SpecialType.System_Int64 => BinaryOperatorKind.LongSubtraction,
                SpecialType.System_UInt32 => BinaryOperatorKind.UIntSubtraction,
                _ => BinaryOperatorKind.ULongSubtraction
            };
            // normalize the type: all types are converted to int32
            if (expr.Type.SpecialType != specialType)
            {
                expr = CreateConversion(expr, Compilation.GetSpecialType(specialType), diagnostics);
                expr.WasCompilerGenerated = true;
                if (expr.HasErrors)
                {
                    Error(diagnostics, ErrorCode.ERR_CannotConvertArrayIndexAccess, expr.Syntax, type, Compilation.GetSpecialType(SpecialType.System_Int32));
                }
            }
            // Subtract one from the index
            var right = new BoundLiteral(expr.Syntax, ConstantValue.Create(1), expr.Type) { WasCompilerGenerated = true };
            // when the argument is a literal then we may be able to fold the subtract expression.
            var resultConstant = FoldBinaryOperator((CSharpSyntaxNode)expr.Syntax, kind, expr, right, expr.Type.SpecialType, diagnostics);
            var sig = this.Compilation.builtInOperators.GetSignature(kind);
            return new BoundBinaryOperator(expr.Syntax, BinaryOperatorKind.Subtraction,
                expr, right,
                resultConstant,
                sig.Method,
                resultKind: LookupResultKind.Viable,
                originalUserDefinedOperatorsOpt: ImmutableArray<MethodSymbol>.Empty,
                type: expr.Type,
                hasErrors: false)
            { WasCompilerGenerated = true };

        }

        private BoundExpression BindIndexerOrVOArrayAccess(ExpressionSyntax node, BoundExpression expr, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics)
        {
            if (Compilation.Options.HasRuntime)
            {
                var arrayType = Compilation.ArrayType();
                var usualType = Compilation.UsualType();
                var cf = ((NamedTypeSymbol)expr.Type).ConstructedFrom;
                if (cf.IsPszType())
                {
                    var zerobasedArray = Compilation.Options.HasOption(CompilerOption.ArrayZero, node);
                    if (Compilation.Options.Dialect == XSharpDialect.Vulcan)
                    {
                        zerobasedArray = true;
                    }
                    ArrayBuilder<BoundExpression> argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                    foreach (var arg in analyzedArguments.Arguments)
                    {
                        var specialType = Compilation.Options.XSharpRuntime ? SpecialType.System_Int32 : SpecialType.System_UInt32;
                        BoundExpression newarg = arg;
                        if (arg.Type.SpecialType != specialType)
                        {
                            newarg = CreateConversion(arg, Compilation.GetSpecialType(specialType), diagnostics);
                            newarg.WasCompilerGenerated = true;
                            if (newarg.HasErrors)
                            {
                                Error(diagnostics, ErrorCode.ERR_CannotConvertArrayIndexAccess, arg.Syntax, arg.Type, Compilation.GetSpecialType(SpecialType.System_Int32));
                            }
                        }
                        // in VO the indexer for a PSZ starts with 1. In Vulcan with 0.
                        // we assume that all other dialects are closer to VO
                        if (!zerobasedArray)
                        {
                            newarg = SubtractIndex(newarg, diagnostics, specialType);
                            newarg.WasCompilerGenerated = true;
                        }
                        argsBuilder.Add(newarg);
                    }
                    var newArgs = AnalyzedArguments.GetInstance();
                    newArgs.Arguments.AddRange(argsBuilder.ToImmutableAndFree());
                    return BindIndexerAccess(node, expr, newArgs, diagnostics);

                }
                var indexerType = Compilation.IndexerType();
                var namedIndexerType = Compilation.NamedIndexerType();
                var indexedPropsType = Compilation.IndexedPropertiesType();
                var arrayBaseType = Compilation.ArrayBaseType();
                bool numericParams = false;
                bool mustcast;
                HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                if (!TypeSymbol.Equals(cf, arrayType) && (cf.IsUsualType()
                    || TypeSymbol.Equals(cf.ConstructedFrom, arrayBaseType)
                    || cf.ImplementsInterface(indexedPropsType, ref useSiteDiagnostics)
                    || cf.ImplementsInterface(indexerType, ref useSiteDiagnostics)))
                {
                    // Index operator on USUAL then we convert the usual to an array or indexer first

                    if (Compilation.Options.XSharpRuntime)
                    {
                        if (analyzedArguments.Arguments.Count == 2 && analyzedArguments.Arguments[1].Type.IsStringType()
                            && cf.ImplementsInterface(namedIndexerType, ref useSiteDiagnostics))
                        {
                            cf = namedIndexerType;
                            numericParams = true;
                            mustcast = true;
                        }
                        else if (analyzedArguments.Arguments.Count == 1 && analyzedArguments.Arguments[0].Type.IsStringType()
                            && cf.ImplementsInterface(indexedPropsType, ref useSiteDiagnostics))
                        {
                            cf = indexedPropsType;
                            numericParams = false;
                            mustcast = true;
                        }
                        else if (cf.ImplementsInterface(indexerType, ref useSiteDiagnostics))
                        {
                            cf = indexerType;
                            numericParams = true;
                            mustcast = true;
                        }
                        else
                        {
                            numericParams = true;
                            mustcast = false;
                        }
                        if (mustcast)
                        {
                            expr = BindCastCore(node, expr, TypeWithAnnotations.Create(cf), wasCompilerGenerated: true, diagnostics: diagnostics);
                        }
                    }
                    else
                    {
                        expr = BindCastCore(node, expr, TypeWithAnnotations.Create(arrayType), wasCompilerGenerated: true, diagnostics: diagnostics);
                        cf = arrayType;
                        numericParams = true;
                    }
                }
                if (cf.IsArrayType() || numericParams)
                {
                    ImmutableArray<BoundExpression> args;
                    ArrayBuilder<BoundExpression> argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                    int argno = 0;
                    foreach (var arg in analyzedArguments.Arguments)
                    {
                        BoundExpression newarg;
                        ++argno;
                        bool mustBeNumeric = true;
                        if (Compilation.Options.XSharpRuntime && Equals(cf, namedIndexerType))
                        {
                            mustBeNumeric = argno == 1;
                        }
                        if (mustBeNumeric)
                        {
                            newarg = arg;
                            var specialType = SpecialType.System_Int32;
                            if (arg.Type.SpecialType != specialType)
                            {
                                newarg = CreateConversion(arg, Compilation.GetSpecialType(specialType), diagnostics);
                                newarg.WasCompilerGenerated = true;
                                if (newarg.HasErrors)
                                {
                                    Error(diagnostics, ErrorCode.ERR_CannotConvertArrayIndexAccess, arg.Syntax, arg.Type, Compilation.GetSpecialType(SpecialType.System_Int32));
                                }
                            }
                            if (!Compilation.Options.HasOption(CompilerOption.ArrayZero, node))
                            {
                                newarg = SubtractIndex(newarg, diagnostics, specialType);
                                newarg.WasCompilerGenerated = true;
                            }
                        }
                        else
                        {
                            newarg = arg;
                        }
                        argsBuilder.Add(newarg);
                    }
                    args = argsBuilder.ToImmutableAndFree();
                    if (Compilation.Options.XSharpRuntime)
                    {
                        analyzedArguments = AnalyzedArguments.GetInstance();
                        analyzedArguments.Arguments.AddRange(args);
                        var res = BindIndexerAccess(node, expr, analyzedArguments, diagnostics);
                        res.WasCompilerGenerated = true;
                        return res;
                    }
                    else
                    {
                        if (args.Length > 1)
                        {
                            // create a an array of ints and use that as the index for the array
                            // this will make sure that the proper GetIndex calls is chosen
                            var exprs = SeparatedSyntaxListBuilder<ExpressionSyntax>.Create();
                            argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                            var tint32 = TypeWithAnnotations.Create(Compilation.GetSpecialType(SpecialType.System_Int32));
                            foreach (var arg in args)
                            {
                                if (exprs.Count > 0)
                                    exprs.AddSeparator(SyntaxFactory.MissingToken(SyntaxKind.CommaToken));
                                exprs.Add(arg.Syntax as ExpressionSyntax);
                                argsBuilder.Add(BindCastCore(arg.Syntax as ExpressionSyntax, arg, tint32, wasCompilerGenerated: true, diagnostics: diagnostics));
                                args = argsBuilder.ToImmutable();
                            }
                            var initSyntax = SyntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression, exprs);
                            argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                            argsBuilder.Add(BindArrayCreationWithInitializer(diagnostics,
                                creationSyntax: null,
                                initSyntax: initSyntax,
                                type: ArrayTypeSymbol.CreateCSharpArray(this.Compilation.Assembly, tint32),
                                sizes: ImmutableArray<BoundExpression>.Empty,
                                boundInitExprOpt: args));
                            args = argsBuilder.ToImmutableAndFree();
                        }
                        PropertySymbol indexer;
                        // Select Array Indexer with the correct # of parameters
                        if (analyzedArguments.Arguments.Count == 1)
                        {
                            indexer = (arrayType as Symbols.Metadata.PE.PENamedTypeSymbol).XSharpArrayIndexerOne;
                        }
                        else
                        {
                            indexer = (arrayType as Symbols.Metadata.PE.PENamedTypeSymbol).XSharpArrayIndexerMany;
                        }
                        return new BoundIndexerAccess(
                            syntax: node,
                            receiverOpt: expr,
                            indexer: indexer,
                            arguments: args,
                            argumentNamesOpt: default,
                            argumentRefKindsOpt: default,
                            expanded: false,
                            argsToParamsOpt: default,
                            defaultArguments: default,
                            type: usualType,
                            hasErrors: false)
                        { WasCompilerGenerated = true };
                    }
                }
            }

            return BindIndexerAccess(node, expr, analyzedArguments, diagnostics);
        }
        private bool CheckValidRefOmittedArguments(OverloadResolutionResult<MethodSymbol> result, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics)
        {
            var member = result.ValidResult.Member;
            for (int i = 0; i < analyzedArguments.Arguments.Count; i++)
            {
                var parNumber = result.ValidResult.Result.ParameterFromArgument(i);
                var parRefKind = member.Parameters[parNumber].RefKind;
                var parType = member.Parameters[parNumber].Type;
                var arg = analyzedArguments.Arguments[i];
                var isBoundAddress = arg is BoundAddressOfOperator;
                var isRefKindMismatch = analyzedArguments.RefKind(i) == RefKind.None && parRefKind != RefKind.None;
                if (isRefKindMismatch || isBoundAddress)
                {
                    bool adjust = false;
                    if (Compilation.Options.HasOption(CompilerOption.ImplicitCastsAndConversions, arg.Syntax))
                    {
                        if (isBoundAddress)
                        {
                            // check to see if we really want to dereference
                            var baoo = (BoundAddressOfOperator)arg;
                            var isDecl = false;
                            if (baoo.Operand.Kind == BoundKind.Local)
                            {
                                var local = (BoundLocal)baoo.Operand;
                                var decl = local.LocalSymbol.DeclaringSyntaxReferences[0];
                                var decl1 = decl.GetSyntax() as CSharpSyntaxNode;
                                isDecl = decl1.XVoIsDecl;
                                // isDecl is true for local declared as
                                // LOCAL rect IS _winRect
                            }
                            if (isDecl)
                            {
                                // argument IS _winRect passed with @
                                adjust = false;
                            }
                            else if (isRefKindMismatch)
                            {
                                // parameter declared with REF and argument passed with @
                                adjust = true;
                                isRefKindMismatch = false;
                            }
                            else if (parType.IsPointerType())
                            {
                                // parameter is a PTR, AS VOSTRUCT or so and argument passed with @
                                adjust = false;
                            }
                            else
                            {
                                // argument is passed with @ but the parameter is neither a REF or a Pointer type.
                                adjust = true;
                            }
                            if (adjust)
                            {
                                arg = baoo.Operand;
                            }
                        }
                    }
                    if (isRefKindMismatch)
                    {
                        adjust = true;
                        Error(diagnostics, ErrorCode.WRN_AutomaticRefGeneration, arg.Syntax, i + 1, parRefKind);
                    }
                    if (arg is BoundLiteral bl && bl.IsLiteralNull())
                    {
                        adjust = false;
                    }
                    if (adjust)
                    {
                        if (!analyzedArguments.RefKinds.Any())
                        {
                            // Size the analyzedArguments list
                            for (int j = 0; j < analyzedArguments.Arguments.Count; j++)
                            {
                                analyzedArguments.RefKinds.Add(i == j ? parRefKind : RefKind.None);
                            }
                        }
                        else
                        {
                            analyzedArguments.RefKinds[i] = RefKind.Ref;
                        }
                        analyzedArguments.Arguments[i] = arg;
                        // check for correct type
                        if (!TypeSymbol.Equals(arg.Type, result.ValidResult.Member.Parameters[i].Type))
                        {
                            Error(diagnostics, ErrorCode.ERR_BadArgType, arg.Syntax, i + 1, arg.Type, member.Parameters[i].Type);
                        }
                    }
                }
                if (isRefKindMismatch && !CheckValueKind(arg.Syntax, arg, BindValueKind.RefOrOut, checkingReceiver: false, diagnostics: diagnostics))
                    return false;
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

                    if (Compilation.Options.HasOption(CompilerOption.ImplicitCastsAndConversions, arg.Syntax))
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

                    if (!CheckValueKind(arg.Syntax, arg, BindValueKind.RefOrOut, checkingReceiver: false, diagnostics: diagnostics))
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
            bool indexed,
            DiagnosticBag diagnostics
            )
        {
            if (Compilation.Options.LateBindingOrFox(node) && right.Kind() != SyntaxKind.GenericName && boundLeft.Kind != BoundKind.TypeExpression)
            {
                string propName = right.Identifier.ValueText;
                if (leftType is { })
                {
                    // when a class inherits from Object then SUPER:Foo() should not generate a late bind call.
                    // See https://github.com/X-Sharp/XSharpPublic/issues/1285
                    bool isSuper = boundLeft.Kind == BoundKind.BaseReference;
                    bool earlyBound = propName == WellKnownMemberNames.InstanceConstructorName;
                    bool isObject = leftType.IsObjectType() && !isSuper;
                    bool isUsual = false;
                    bool isArray = false;
                    NamedTypeSymbol usualType = Compilation.UsualType();
                    NamedTypeSymbol arrayType = Compilation.ArrayType();
                    if (!isObject)
                    {
                        if (leftType is NamedTypeSymbol nts)
                        {
                            isUsual = nts.ConstructedFrom.IsUsualType();
                            isArray = nts.ConstructedFrom.IsArrayType();
                        }
                    }
                    // Late bound will only work for OBJECT or USUAL
                    if (isObject || isUsual || isArray)
                    {
                        var returnType = Compilation.UsualType();
                        if (isArray)
                        {
                            // When method does not exist then do a late bound ASend()
                            if (Compilation.Options.Dialect.AllowASend())
                            {
                                var m = arrayType.GetMembers(propName);
                                earlyBound = m.Length > 0;
                                if (!earlyBound && Compilation.Options.XSharpRuntime)
                                {
                                    m = arrayType.BaseTypeNoUseSiteDiagnostics.GetMembers(propName);
                                    earlyBound = m.Length > 0;
                                }
                            }
                            else
                            {
                                earlyBound = true;
                            }
                        }
                        else if (isUsual)
                        {
                            // USUAL._NIL and USUAL.ToObject()
                            var m = usualType.GetMembers(propName);
                            if (m.Length > 0 && m[0].IsStatic)
                                earlyBound |= true;
                        }
                        else if (isObject)
                        {
                            earlyBound |= leftType.GetMembers(propName).Length > 0;
                        }
                        if (!earlyBound)
                        {
                            return new BoundDynamicMemberAccess(
                                syntax: node,
                                receiver: boundLeft,
                                typeArgumentsOpt: default,
                                name: propName,
                                invoked: invoked,
                                indexed: indexed,
                                type: returnType,
                                hasErrors: false);
                        }
                    }
                    if (!leftType.HasMembers(propName))
                    {
                        var method = node.Parent switch
                        {
                            InvocationExpressionSyntax => "NoMethod",
                            AssignmentExpressionSyntax => "NoIVarPut",
                            _ => "NoIVarGet",
                        };
                        if (leftType.HasMembers(method))
                        {
                            var returnType = Compilation.UsualType();
                            var result = new BoundDynamicMemberAccess(
                                    syntax: node,
                                    receiver: boundLeft,
                                    typeArgumentsOpt: default,
                                    name: propName,
                                    invoked: invoked,
                                    indexed: indexed,
                                    type: returnType,
                                    hasErrors: false);
                            var hidewWarning = Compilation.Options.Dialect.AllowLateBindingForTypesWithLateBindingAttribute() && leftType.HasLateBindingAttribute();
                            if (!hidewWarning)
                            {
                                // when FoxPro dialect and the type is marked with "allowLateBound" then no need for the warning
                                diagnostics.Add(ErrorCode.WRN_UndeclaredVariableLatebound, node.Location, leftType, propName, method);
                            }
                            return result;
                        }
                    }
                }
            }
            return null;
        }
        private BoundExpression CheckVOIndexedValue(BoundExpression expr, BindValueKind valueKind, DiagnosticBag diagnostics)
        {
            var originalexpr = expr;
            expr = CheckValue(expr, BindValueKind.RValue, diagnostics);
            if (expr.Kind == BoundKind.BadExpression)
            {
                // When a lookup of the Item property fails, then try again
                if (originalexpr.Kind == BoundKind.MethodGroup)
                {
                    var methodGroup = originalexpr as BoundMethodGroup;
                    if (XSharpString.Equals(methodGroup.Name, "Item"))
                    {
                        var newDiag = DiagnosticBag.GetInstance();
                        expr = CheckValue(methodGroup.InstanceOpt, BindValueKind.RValue, newDiag);
                        if (expr.Kind != BoundKind.BadExpression)
                        {
                            newDiag.Clear();
                            foreach (var error in diagnostics.AsEnumerable())
                            {
                                bool suppress = false;
                                var loc = error.Location;
                                if (loc.IsInSource)
                                {
                                    var start = loc.GetLineSpan().StartLinePosition;
                                    var curLine = expr.Syntax.Location.GetLineSpan().StartLinePosition;
                                    suppress = (start.Line == curLine.Line && error.Code == (int)ErrorCode.ERR_NoSuchMemberOrExtension);
                                }
                                if (!suppress)
                                    newDiag.Add(error);
                            }
                            diagnostics.Clear();
                            diagnostics.AddRange(newDiag);
                        }
                    }
                }
            }
            return expr;

        }

        private BoundExpression BindXsCast(CastExpressionSyntax node, TypeSymbol targetType, ref BoundExpression operand, DiagnosticBag diagnostics)
        {
            var pe = node.XNode as XSharpParser.PrimaryExpressionContext;
            if (pe.IsVoCast())
            {
                if (targetType.SpecialType == SpecialType.System_Object && !operand.Type.IsReferenceType && !pe.IsCastClass())
                {
                    diagnostics.Add(ErrorCode.ERR_NoExplicitCast, node.Location, operand.Type, targetType);
                }
                // LOGIC(_CAST, numeric)  => change conversion to <numeric> != 0
                if (targetType.SpecialType == SpecialType.System_Boolean && operand.Type.IsIntegralType())
                {
                    if (operand is BoundLiteral)
                    {
                        bool result = operand.ConstantValue.Int64Value != 0;
                        return new BoundLiteral(node, ConstantValue.Create(result), targetType);
                    }
                    var right = new BoundLiteral(node, ConstantValue.Create(0), Compilation.GetSpecialType(SpecialType.System_Int32));
                    return new BoundBinaryOperator(
                        syntax: node,
                        operatorKind: BinaryOperatorKind.NotEqual,
                        left: operand,
                        right: right,
                        constantValueOpt: null,
                        methodOpt: null,
                        resultKind: LookupResultKind.Viable,
                        type: targetType);
                }
            }
            if (targetType.IsVoidPointer() && operand.Type is { } && operand.Type.IsStringType())
            {
                if (pe.IsVoCast() || pe.IsVoConvert())
                {

                    // PTR(_Cast, SomeString) or PTR(SomeString)
                    // Convert to 
                    // PTR(_Cast, PSZ(_Cast, SomeString))
                    var pszType = Compilation.PszType();
                    HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                    var conversion = Conversions.ClassifyConversionFromExpression(operand, pszType, ref useSiteDiagnostics, true);
                    diagnostics.Add(node, useSiteDiagnostics);
                    diagnostics.Add(ErrorCode.WRN_CompilerGeneratedPSZConversionGeneratesMemoryleak, node.Location);
                    operand = new BoundConversion(node, operand, conversion, @checked: false, explicitCastInCode: true, null, null, pszType);
                    return null;
                }
            }
            if (BindVOPointerDereference(node, TypeWithAnnotations.Create(targetType), operand, diagnostics, out var expression))
            {
                return expression;
            }
            // WE do not want (USUAL) <object> to unbox the object !
            if (operand.Type?.SpecialType == SpecialType.System_Object && targetType.IsUsualType())
            {
                return operand;
            }
            return null;
        }

        private bool BindStringToPsz(CSharpSyntaxNode syntax, ref BoundExpression source, TypeSymbol destination, ref Conversion conversion, DiagnosticBag diagnostics)
        {
            NamedTypeSymbol psz = Compilation.PszType();
            if (source.Type is { } && source.Type.IsStringType() &&
                Compilation.Options.HasRuntime &&
                destination.IsPszType())
            {
                // Note this calls the constructor for __PSZ with a string.
                // The allocated pointer inside the PSZ is never freed by Vulcan and X# !
                MethodSymbol stringctor = FindConstructor(psz, 1, Compilation.GetSpecialType(SpecialType.System_String));

                if (stringctor != null)
                {
                    diagnostics.Add(ErrorCode.WRN_CompilerGeneratedPSZConversionGeneratesMemoryleak, syntax.Location);
                    source = new BoundObjectCreationExpression(syntax, stringctor, new BoundExpression[] { source });
                    return true;
                }
            }
            return false;
        }

        internal static MethodSymbol FindConstructor(NamedTypeSymbol type, int parameterCount, TypeSymbol p1, TypeSymbol p2 = null)
        {
            foreach (MethodSymbol ctor in type.Constructors)
            {
                if (ctor.ParameterCount == parameterCount)
                {
                    if (parameterCount == 1 && Equals(ctor.Parameters[0].Type, p1))
                    {
                        return ctor;
                    }
                    if (parameterCount == 2 && Equals(ctor.Parameters[0].Type, p1) && Equals(ctor.Parameters[1].Type, p2))
                    {
                        return ctor;
                    }
                }
            }
            return null;
        }
        /// <summary>
        /// Binds a simple identifier.
        /// </summary>
        private BoundExpression BindXSIdentifier(
            SimpleNameSyntax node,
            bool invoked,
            bool indexed,
            DiagnosticBag diagnostics,
            bool bindMethod,
            bool bindSafe = false
            )
        {
            // This method replaced the standard C# BindIdentifier
            // xBase has some different rules for binding
            // - for calls without object prefix we prefer static method calls over self method calls (In VO SELF: is mandatory)
            //   and we also prefer to find DEFINES over PROPERTIES
            // - when invoked = TRUE then we do not return local variables, with the exception of delegates
            // - when invoked = FALSE then we do not return methods, with the exception of assigning event handlers
            bool preferStatic = bindMethod;
            Debug.Assert(node != null);

            // If the syntax tree is ill-formed and the identifier is missing then we've already
            // given a parse error. Just return an error local and continue with analysis.
            if (node.IsMissing)
            {
                return BadExpression(node);
            }

            if (this.ContainingMemberOrLambda is LambdaSymbol ls && ls.ParameterCount == 1 && ls.Parameters[0].Name == "__this" && node.Identifier.ValueText != "__this" && node is IdentifierNameSyntax)
            {
                var syntax = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, SyntaxFactory.IdentifierName("__this"), SyntaxFactory.IdentifierName(node.Identifier.ValueText));
                DiagnosticBag loc_diagnostics = DiagnosticBag.GetInstance();
                BoundExpression e = BindMemberAccess(syntax, false, false, loc_diagnostics);
                bool valid = true;
                if (e is BoundMethodGroup m)
                {
                    valid = m.Methods.Count() > 0;
                }
                if (!loc_diagnostics.HasAnyErrors() && valid)
                {
                    syntax = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, SyntaxFactory.IdentifierName("__this"), node);
                    return BindMemberAccess(syntax, false, false, diagnostics);
                }
            }

            // A simple-name is either of the form I or of the form I<A1, ..., AK>, where I is a
            // single identifier and <A1, ..., AK> is an optional type-argument-list. When no
            // type-argument-list is specified, consider K to be zero. The simple-name is evaluated
            // and classified as follows:

            // If K is zero and the simple-name appears within a block and if the block's (or an
            // enclosing block's) local variable declaration space contains a local variable,
            // parameter or constant with name I, then the simple-name refers to that local
            // variable, parameter or constant and is classified as a variable or value.

            // If K is zero and the simple-name appears within the body of a generic method
            // declaration and if that declaration includes a type parameter with name I, then the
            // simple-name refers to that type parameter.

            BoundExpression expression = null;
            BoundExpression skippedExpression = null;

            // It's possible that the argument list is malformed; if so, do not attempt to bind it;
            // just use the null array.

            int arity = node.Arity;
            bool hasTypeArguments = arity > 0;

            SeparatedSyntaxList<TypeSyntax> typeArgumentList = node.Kind() == SyntaxKind.GenericName
                ? ((GenericNameSyntax)node).TypeArgumentList.Arguments
                : default(SeparatedSyntaxList<TypeSyntax>);

            Debug.Assert(arity == typeArgumentList.Count);

            var typeArguments = hasTypeArguments ? BindTypeArguments(typeArgumentList, diagnostics) : default;

            var lookupResult = LookupResult.GetInstance();
            LookupOptions options = LookupOptions.AllMethodsOnArityZero;
            if (invoked)
            {
                options |= LookupOptions.MustBeInvocableIfMember;
            }

            if (!IsInMethodBody && this.EnclosingNameofArgument == null)
            {
                Debug.Assert((options & LookupOptions.NamespacesOrTypesOnly) == 0);
                options |= LookupOptions.MustNotBeMethodTypeParameter;
            }
            // In the VO and Vulcan dialect you cannot call an instance method without SELF: prefix
            // and also not access a property without SELF: Prefix
            // So when there is a property (Access) and a DEFINE with the same name then the
            // system will use the define and not the property
            // so we check here if we are called from a memberaccessexpression with a colon separator
            // so String.Compare will use different lookup options as SELF:ToString()
            var originalOptions = options;
            var bCouldBeNameSpaceOrType = false;
            // Here we add XSharp Specific options
            if (!bindMethod)
            {
                options |= LookupOptions.MustNotBeMethod;
            }
            var aes = node.Parent as AssignmentExpressionSyntax;
            if (preferStatic)
            {
                bool instance = false;
                if (node.Parent is MemberAccessExpressionSyntax)
                {
                    instance = node.MustBeInstanceMemberAccess(false);
                }
                else if (aes is { } && aes.Left == node)
                {
                    instance = true;
                }
                if (!instance)
                    options |= LookupOptions.MustNotBeInstance;
            }
            else
            {
                if (node.Parent is MemberAccessExpressionSyntax)
                {
                    // Check for Messagebox.Show() which is class member access
                    // versions window:ToString() which is instance member access
                    if (!node.MustBeInstanceMemberAccess(true))
                    {
                        bCouldBeNameSpaceOrType = true;
                    }
                }
            }
            var name = node.Identifier.ValueText;
            HashSet<DiagnosticInfo> useSiteDiagnostics = null;
            var memvarorfield = name.IndexOf("->") > 0;
            // no need to look for our special names
            if (!memvarorfield)
            {
                this.LookupSymbolsWithFallback(lookupResult, name, arity: arity, useSiteDiagnostics: ref useSiteDiagnostics, options: options);
                if (bCouldBeNameSpaceOrType)
                {
                    // when there is a type and a local with the same name, then they will end up both in the resultSet
                    // the compiler will later resolve the right one.
                    var nsResult = LookupResult.GetInstance();
                    this.LookupSymbolsWithFallback(nsResult, name, arity: arity, useSiteDiagnostics: ref useSiteDiagnostics, options: LookupOptions.NamespacesOrTypesOnly);
                    if (!nsResult.IsClear)
                    {
                        lookupResult.MergeEqual(nsResult);
                    }
                }
                // when no field or local found then try to find defines
                if (lookupResult.IsClear && !bindMethod)
                {
                    this.LookupSymbolsWithFallback(lookupResult, name, arity: arity, useSiteDiagnostics: ref useSiteDiagnostics, options: options | LookupOptions.DefinesOnly);
                }
                if (preferStatic || lookupResult.IsClear)
                {
                    bool lookupAgain = false;
                    if (lookupResult.Kind == LookupResultKind.StaticInstanceMismatch)
                    {
                        // try again but now allow instance methods
                        if (!Compilation.Options.HasOption(CompilerOption.EnforceSelf, node))
                        {
                            lookupAgain = true;
                        }
                    }
                    if (lookupResult.Kind == LookupResultKind.Viable && invoked && lookupResult.Symbols.Count != 0)
                    {
                        Symbol s = lookupResult.Symbols[0];
                        if (s.Kind != SymbolKind.Method)
                            lookupAgain = true;
                    }
                    if (lookupAgain || lookupResult.IsClear)
                    {
                        // This uses the 'original' BindIdentifier lookup mechanism
                        options = originalOptions;
                        useSiteDiagnostics = null;
                        lookupResult.Clear();
                        this.LookupSymbolsWithFallback(lookupResult, name, arity: arity, useSiteDiagnostics: ref useSiteDiagnostics, options: options);
                    }
                }
                diagnostics.Add(node, useSiteDiagnostics);
                if (lookupResult.Kind != LookupResultKind.Empty)
                {
                    // have we detected an error with the current node?
                    bool isError = false;
                    bool wasError;
                    var members = ArrayBuilder<Symbol>.GetInstance();
                    Symbol symbol = null;
                    if (!invoked)
                    {
                        // not invoked, so prefer non-method symbols
                        members.AddRange(lookupResult.Symbols.Where(f => f.Kind != SymbolKind.Method));
                        if (members.Count == 1)
                        {
                            symbol = members[0];
                        }
                    }
                    if (symbol == null)
                    {
                        members.Clear();
                        symbol = GetSymbolOrMethodOrPropertyGroup(lookupResult, node, name, node.Arity, members, diagnostics, out wasError);  // reports diagnostics in result.
                    }
                    else
                    {
                        wasError = false;
                    }

                    isError |= wasError;

                    if (symbol is null)
                    {
                        Debug.Assert(members.Count > 0);
                        var receiver = SynthesizeMethodGroupReceiver(node, members);
                        expression = ConstructBoundMemberGroupAndReportOmittedTypeArguments(
                            node,
                            typeArgumentList,
                            typeArguments,
                            receiver,
                            name,
                            members,
                            lookupResult,
                            receiver != null ? BoundMethodGroupFlags.HasImplicitReceiver : BoundMethodGroupFlags.None,
                            isError,
                            diagnostics);
                        if (!bindMethod)
                        {
                            skippedExpression = expression;
                            expression = null;
                        }
                    }
                    else
                    {
                        bool isNamedType = (symbol.Kind == SymbolKind.NamedType) || (symbol.Kind == SymbolKind.ErrorType);

                        if (hasTypeArguments && isNamedType)
                        {
                            symbol = ConstructNamedTypeUnlessTypeArgumentOmitted(node, (NamedTypeSymbol)symbol, typeArgumentList, typeArguments, diagnostics);
                        }

                        expression = BindNonMethod(node, symbol, diagnostics, lookupResult.Kind, indexed: false, isError);

                        if (!isNamedType && (hasTypeArguments || node.Kind() == SyntaxKind.GenericName))
                        {
                            diagnostics.Add(ErrorCode.ERR_InvalidExprTerm, node.Location, node.XNode.GetText());
                            expression = new BoundBadExpression(
                                syntax: node,
                                resultKind: LookupResultKind.WrongArity,
                                symbols: ImmutableArray.Create<Symbol>(symbol),
                                childBoundNodes: ImmutableArray.Create<BoundExpression>(expression),
                                type: expression.Type,
                                hasErrors: true);
                        }
                    }

                    members.Free();
                }
            }

            // make sure that we do not resolve to a function call when we do not look for methods
            if (expression != null && !bindMethod && expression.Kind == BoundKind.MethodGroup && indexed)
            {
                expression = null;
            }

            if (expression != null && expression.Type is not null && expression.Type.IsErrorType())
            {
                if (expression.Type.ContainingAssembly != null && expression.Type.ContainingAssembly != Compilation.Assembly)
                {
                    Error(diagnostics, ErrorCode.ERR_NoTypeDef, node, expression.Type, expression.Type.ContainingAssembly);
                }
                return BadExpression(node);
            }

            // undeclared variables are allowed when the dialect supports memvars and memvars are enabled
            // or in the 'full' macro compiler

            bool isFoxMemberAccess = false;

            if (expression is BoundTypeExpression && node?.XNode is XSharpParserRuleContext xnode1)
            {
                // an expression like "M.Object = Something" should not resolve to the Object Type

                if (xnode1 is not AccessMemberContext)
                {
                    xnode1 = xnode1.Parent as XSharpParserRuleContext;
                }
                if (xnode1 is AccessMemberContext amc && amc.IsFox)
                {
                    isFoxMemberAccess = true;
                    if (amc.HasMPrefix)
                    {
                        expression = null;
                    }
                }
            }

            if (expression == null && node?.XNode is XSharpParserRuleContext xnode)
            {
                // Foxpro member access is transformed in 2 ways:
                // M.SomeVar is transformed to a SomeVar NameExpression node. The AccessMember context is then connected to SomeVar
                // The reason for this is that FoxPro also allows M. prefix for local variables.
                // Customer.SomeVar is transformed to a MemberAccess node with a Customer Expression and a SomeVar name. The AccessMember is then the XNode of the parent.
                if (xnode is not AccessMemberContext)
                {
                    xnode = xnode.Parent as XSharpParserRuleContext;
                }
                if (xnode is AccessMemberContext amc && amc.IsFox)
                {
                    isFoxMemberAccess = true;
                    if (amc.HasMPrefix)
                    {
                        memvarorfield = Compilation.Options.HasOption(CompilerOption.MemVars, node);
                        if (memvarorfield)
                        {
                            // convert "M.FieldName" to "Xs$Memvar->FieldName"
                            name = XSharpSpecialNames.MemVarPrefix + "->" + amc.FieldName;
                        }
                        else
                        {
                            // M. prefix and no support for Memvars, then the M. prefix is invalid and also not a workarea prefix.
                            isFoxMemberAccess = false;
                        }
                    }
                }
            }
            if (expression == null && (Compilation.Options.MacroScript || Compilation.Options.HasOption(CompilerOption.MemVars, node)
                || memvarorfield || isFoxMemberAccess))
            {
                var type = Compilation.RuntimeFunctionsType();
                bool declared = false;
                string alias = null;
                var get = GetCandidateMembers(type, ReservedNames.VarGet, LookupOptions.MustNotBeInstance, this);
                if (bindSafe)
                {
                    get = GetCandidateMembers(type, ReservedNames.VarGetSafe, LookupOptions.MustNotBeInstance, this);
                }
                var set = GetCandidateMembers(type, ReservedNames.VarPut, LookupOptions.MustNotBeInstance, this);
                if (memvarorfield)
                {
                    // this is either:
                    // alias->fieldname
                    // Xs$Memvar->Memvarname
                    // Xs$Field->Memvar
                    // or the left hand side of a Foxpro Customer.LastName syntax
                    //
                    var parts = name.Split(new string[] { "->" }, StringSplitOptions.None);
                    if (parts.Length == 2)
                    {
                        declared = true;
                        name = parts[1];
                        if (parts[0] == XSharpSpecialNames.MemVarPrefix)
                        {
                            if (bindSafe)
                            {
                                get = GetCandidateMembers(type, ReservedNames.MemVarGetSafe, LookupOptions.MustNotBeInstance, this);
                            }
                            else
                            {
                                get = GetCandidateMembers(type, ReservedNames.MemVarGet, LookupOptions.MustNotBeInstance, this);
                            }
                            set = GetCandidateMembers(type, ReservedNames.MemVarPut, LookupOptions.MustNotBeInstance, this);
                        }
                        else if (parts[0] == XSharpSpecialNames.FieldPrefix)
                        {
                            get = GetCandidateMembers(type, ReservedNames.FieldGet, LookupOptions.MustNotBeInstance, this);
                            set = GetCandidateMembers(type, ReservedNames.FieldSet, LookupOptions.MustNotBeInstance, this);
                        }
                        else
                        {
                            get = GetCandidateMembers(type, ReservedNames.FieldGetWa, LookupOptions.MustNotBeInstance, this);
                            set = GetCandidateMembers(type, ReservedNames.FieldSetWa, LookupOptions.MustNotBeInstance, this);
                            alias = parts[0];
                        }
                    }
                }
                var warning = ErrorCode.WRN_UndeclaredVariable;
                var undeclaredMemVar = Compilation.Options.HasOption(CompilerOption.UndeclaredMemVars, node);
                if (isFoxMemberAccess)
                {
                    if (undeclaredMemVar)
                    {
                        warning = ErrorCode.WRN_UndeclaredVariableOrCursor;
                    }
                    else
                    {
                        warning = ErrorCode.WRN_UndeclaredCursor;
                    }
                }
                if (node.Parent is InvocationExpressionSyntax && !Compilation.Options.HasOption(CompilerOption.FoxArraySupport, node))
                {
                    expression = null;
                }
                else if (Compilation.Options.HasOption(CompilerOption.UndeclaredMemVars, node) || declared || isFoxMemberAccess)
                {
                    if (get.Length == 1 && set.Length == 1 && get[0] is MethodSymbol getsym && set[0] is MethodSymbol setsym)
                    {
                        XsVariableSymbol ps;
                        var tUsual = TypeWithAnnotations.Create(Compilation.UsualType());
                        if (!string.IsNullOrEmpty(alias))
                        {
                            ps = new XsVariableSymbol(alias, name, getsym, setsym, tUsual);
                        }
                        else
                        {
                            ps = new XsVariableSymbol(name, getsym, setsym, tUsual);
                        }

                        expression = new BoundPropertyAccess(node, null, ps, LookupResultKind.Viable, Compilation.UsualType());
                        if (!Compilation.Options.MacroScript && !declared)
                        {
                            Error(diagnostics, warning, node.Location, name);

                            // find entity and set flag HasUndeclared
                            var parent = node.XNode as XSharpParserRuleContext;
                            while (parent != null)
                            {
                                if (parent is IMemberContext)
                                    break;
                                parent = parent.Parent as XSharpParserRuleContext;
                            }
                            if (parent is IMemberContext iec)
                            {
                                iec.Data.HasUndeclared = true;
                            }
                        }
                    }
                }
            }
            if (expression == null)
            {
                expression = skippedExpression;
            }
            if (expression == null)
            {
                // Otherwise, the simple-name is undefined and a compile-time error occurs.
                expression = BadExpression(node);
                if (lookupResult.Error != null)
                {
                    Error(diagnostics, lookupResult.Error, node);
                }
                else if (IsJoinRangeVariableInLeftKey(node))
                {
                    Error(diagnostics, ErrorCode.ERR_QueryOuterKey, node, name);
                }
                else if (IsInJoinRightKey(node))
                {
                    Error(diagnostics, ErrorCode.ERR_QueryInnerKey, node, name);
                }
                else
                {
                    Error(diagnostics, ErrorCode.ERR_NameNotInContext, node, name);
                }
            }

            lookupResult.Free();
            return expression;
        }

        private void FilterResults(LookupResult result, LookupOptions options)
        {
            bool noMethod = options.HasFlag(LookupOptions.MustNotBeMethod);
            bool onlyDef = options.HasFlag(LookupOptions.DefinesOnly);
            if ((noMethod || onlyDef) && !result.IsClear && result.Kind == LookupResultKind.Viable)
            {
                LookupResult tmp = LookupResult.GetInstance();
                foreach (var sym in result.Symbols)
                {
                    bool add = false;
                    switch (sym.Kind)
                    {
                        case SymbolKind.Field:
                        case SymbolKind.Property:
                            if (onlyDef)
                            {
                                if (sym.ContainingType.Name == XSharpSpecialNames.FunctionsClass)
                                {
                                    add = true;
                                }
                            }
                            else
                                add = true;
                            break;
                        case SymbolKind.Parameter:
                        case SymbolKind.Local:
                            add = true;
                            break;
                        case SymbolKind.Method:
                            add = !noMethod;
                            break;
                        default:
                            //add = true;
                            break;
                    }
                    if (add)
                    {
                        SingleLookupResult single = new SingleLookupResult(LookupResultKind.Viable, sym, null);
                        tmp.MergeEqual(single);
                    }
                }
                result.Clear();
                result.MergeEqual(tmp);
                tmp.Free();

            }
            return;
        }

        private static TypeWithAnnotations XsGetCorrespondingParameterType(ref MemberAnalysisResult result, ImmutableArray<TypeWithAnnotations> parameterTypes, int arg)
        {
            int paramNum = result.ParameterFromArgument(arg);
            TypeSymbol type = parameterTypes[paramNum].Type;
            if (paramNum == parameterTypes.Length - 1 && result.Kind == MemberResolutionKind.ApplicableInExpandedForm)
            {
                type = ((ArrayTypeSymbol)type).ElementType;
            }
            return TypeWithAnnotations.Create(type);
        }
        private BoundExpression XsFixPszArgumentProblems(BoundExpression argument, TypeWithAnnotations type, ref Conversion kind)
        {
            if (argument.Kind == BoundKind.Literal)
            {
                if (type.Type.IsPszType())
                {
                    var lit = argument as BoundLiteral;
                    if (lit.IsLiteralNull())
                    {
                        argument = new BoundLiteral(argument.Syntax, ConstantValue.Create(0), Compilation.GetSpecialType(SpecialType.System_Int32));
                        kind = Conversion.Identity;
                    }
                }
            }
            return argument;
        }
        private BoundExpression PszFromNull(BoundExpression expression)
        {
            var targetType = Compilation.PszType();
            return new BoundDefaultExpression(expression.Syntax, targetType);
        }
    }
    internal static class XsBoundExpressionExtensions
    {
        internal static bool MustBeInstanceMemberAccess(this SimpleNameSyntax node, bool mustBeLHS)
        {
            bool instance = false;
            if (node?.Parent is MemberAccessExpressionSyntax)
            {
                var xnode = node.Parent.XNode;
                if (xnode != null)
                {
                    if (xnode is ArrayElementContext aec)
                    {
                        xnode = aec.Expr;
                    }
                    if (xnode is AccessMemberContext amc && amc.Op.Text == ":")
                    {
                        instance = true;
                        if (mustBeLHS)
                        {
                            var par = node.Parent as MemberAccessExpressionSyntax;
                            instance = par.Expression == node;
                        }
                    }
                }
            }
            return instance;
        }
    }
    internal partial class BoundConversion
    {
        internal BoundExpression XOperand
        {
            get
            {
                if (this.Syntax.XGenerated)
                    return this.Operand;
                if (this.ExplicitCastInCode)
                    return this;
                return this.Operand;
            }
        }
    }
}
