//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System.Collections.Generic;
using System.Linq;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
namespace Microsoft.CodeAnalysis.CSharp
{

    internal partial class Binder
    {

        internal void XsCheckPsz2String(SyntaxNode node, BoundExpression op1, DiagnosticBag diagnostics)
        {
            var assignment = node as AssignmentExpressionSyntax;
            if (assignment.Right.XIsString2Psz)
            {
                if (op1 is BoundFieldAccess bfa)
                {
                    var symbol = bfa.FieldSymbol;
                    var decltype = symbol.ContainingType;
                    var ok = false;
                    if (decltype.IsVoStructOrUnion())
                    {
                        var receiver = bfa.ReceiverOpt;
                        if (receiver is BoundLocal local)
                        {
                            var localSymbol = local.LocalSymbol;
                            var decl = localSymbol.DeclaringSyntaxReferences;
                            if (decl.Length > 0)
                            {
                                var syntax = decl[0].GetSyntax() as CSharpSyntaxNode;
                                ok = syntax.XVoIsDecl;
                            }
                        }
                    }
                    if (!ok)
                    {
                        Error(diagnostics, ErrorCode.WRN_String2PszMustBeAssignedToLocal, assignment.Right);
                    }
                }
            }
        }

        internal BoundExpression XsBindFoxArrayAssign(SyntaxNode node, BoundExpression op1, BoundExpression op2, DiagnosticBag diagnostics)
        {
            // nothing to do for Variable Symbols
            bool needsWork = false;
            if (op1.Type.IsUsualType() || op1.Type.Name.Contains("__FoxArray"))
            {
                needsWork = true;
            }
            if (needsWork && op1 is BoundPropertyAccess bpa && bpa.PropertySymbol is XsVariableSymbol)
            {
                needsWork = false;
            }
            if (needsWork && (op1 is BoundObjectInitializerMember || op1 is BoundDynamicObjectInitializerMember))
            {
                needsWork = false;
            }
            if (needsWork && op2.Syntax.XNode is XP.FoxdimvarContext)
            {
                needsWork = false;
            }
            if (!needsWork)
            {
                return op2;
            }
            if (op1.Kind is BoundKind.IndexerAccess)
            {
                return op2;
            }
            var vfpfuncs = Compilation.GetWellKnownType(WellKnownType.XSharp_VFP_Functions);
            if (op1.Type.IsUsualType())
            {
                var syms1 = vfpfuncs.GetMembers(ReservedNames.FoxAssign);
                var args = new List<BoundExpression>();
                args.Add(CreateConversion(op1, Compilation.UsualType(), diagnostics));
                args.Add(CreateConversion(op2, Compilation.UsualType(), diagnostics));
                var call = new BoundCall(syntax: node,
                        receiverOpt: null,
                        method: (MethodSymbol)syms1[0],
                        arguments: args.ToImmutableArray(),
                        argumentNamesOpt: default,
                        argumentRefKindsOpt: default,
                        isDelegateCall: false,
                        expanded: false,
                        invokedAsExtensionMethod: false,
                        argsToParamsOpt: default,
                        defaultArguments: default,
                        resultKind: LookupResultKind.Viable,
                        type: Compilation.UsualType(),
                        hasErrors: false);
                op2 = call;
            }
            if (!op1.Type.IsUsualType())
            {
                var syms2 = vfpfuncs.GetMembers(ReservedNames.FoxFillArray);
                var args = new List<BoundExpression>();
                args.Add(CreateConversion(op1, Compilation.UsualType(), diagnostics));
                args.Add(CreateConversion(op2, Compilation.UsualType(), diagnostics));
                var call = new BoundCall(syntax: node,
                        receiverOpt: null,
                        method: (MethodSymbol)syms2[0],
                        arguments: args.ToImmutableArray(),
                        argumentNamesOpt: default,
                        argumentRefKindsOpt: default,
                        isDelegateCall: false,
                        expanded: false,
                        invokedAsExtensionMethod: false,
                        argsToParamsOpt: default,
                        defaultArguments: default,
                        resultKind: LookupResultKind.Viable,
                        type: Compilation.UsualType(),
                        hasErrors: false);
                op2 = call;
            }
            return op2;
        }
        internal BoundExpression XsBindUsualCollectionEnumerator(BoundExpression collection, DiagnosticBag diagnostics)
        {
            if (!collection.Type.IsUsualType())
                return collection;
            if (Compilation.Options.XSharpRuntime)
            {
                var rtfuncs = Compilation.GetWellKnownType(WellKnownType.XSharp_RT_Functions);
                var syms = rtfuncs.GetMembers(ReservedNames.UsualEnumerator);
                if (syms.Length == 1)
                {
                    var args = new List<BoundExpression>();
                    args.Add(collection);
                    var call = new BoundCall(syntax: collection.Syntax,
                        receiverOpt: null,
                        method: (MethodSymbol)syms[0],
                        arguments: args.ToImmutableArray(),
                        argumentNamesOpt: default,
                        argumentRefKindsOpt: default,
                        isDelegateCall: false,
                        expanded: false,
                        invokedAsExtensionMethod: false,
                        argsToParamsOpt: default,
                        defaultArguments: default,
                        resultKind: LookupResultKind.Viable,
                        type: Compilation.ArrayType(),
                        hasErrors: false);
                    return call;
                }
            }
            return CreateConversion(
                   syntax: collection.Syntax,
                   source: collection,
                   conversion: Conversion.ExplicitReference,
                   isCast: false,
                   conversionGroupOpt: null,
                   destination: Compilation.ArrayType(),
                   diagnostics: diagnostics);
        }

        BoundExpression XsCreateConversionNonIntegralNumeric(TypeSymbol targetType, BoundExpression expression, DiagnosticBag diagnostics, Conversion conversion)
        {
            if (Compilation.Options.HasOption(CompilerOption.Vo11, expression.Syntax)
                && expression.Type.IsXNumericType() && targetType.IsXNumericType())
            {
                // call Convert.To..() to round the result
                var mem = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Convert__ToInt32Double);
                switch (targetType.SpecialType)
                {
                    case SpecialType.System_UInt32:
                        mem = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Convert__ToUInt32Double);
                        break;
                    case SpecialType.System_Int16:
                        mem = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Convert__ToInt16Double);
                        break;
                    case SpecialType.System_UInt16:
                        mem = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Convert__ToUInt16Double);
                        break;
                    case SpecialType.System_Int64:
                        mem = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Convert__ToInt64Double);
                        break;
                    case SpecialType.System_UInt64:
                        mem = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Convert__ToUInt64Double);
                        break;
                    case SpecialType.System_Int32:
                    default:
                        break;
                }

                var args = new List<BoundExpression>();
                args.Add(expression);
                return new BoundCall(syntax: expression.Syntax,
                    receiverOpt: expression,
                    method: (MethodSymbol)mem,
                    arguments: args.ToImmutableArray(),
                    argumentNamesOpt: default,
                    argumentRefKindsOpt: default,
                    isDelegateCall: false,
                    expanded: false,
                    invokedAsExtensionMethod: false,
                    argsToParamsOpt: default,
                    defaultArguments: default,
                    resultKind: LookupResultKind.Viable,
                    type: targetType,
                    hasErrors: false);

            }
            else
            {
                return CreateXsConversion(expression, conversion, targetType, diagnostics);
            }

        }

        BoundExpression XsHandleNullPsz(TypeSymbol targetType, BoundExpression expression)
        {
            if (Compilation.Options.HasRuntime && targetType.IsPszType())
            {
                if (IsNullNode(expression))
                {
                    return PszFromNull(expression);
                }
            }
            return null;
        }

        BoundExpression CreateXsConversion(BoundExpression expression, Conversion conversion, TypeSymbol targetType, DiagnosticBag diagnostics)
        {
            var result = CreateConversion(syntax: expression.Syntax,
                                            source: expression,
                                            conversion: conversion,
                                            isCast: false,
                                            conversionGroupOpt: null,
                                            destination: targetType,
                                            diagnostics: diagnostics);
            result.WasCompilerGenerated = true;
            return result;
        }

        BoundExpression XsHandleExplicitConversion(TypeSymbol targetType, BoundExpression expression, DiagnosticBag diagnostics, Conversion conversion)
        {
            if (conversion.IsExplicit && !TypeSymbol.Equals(expression.Type, targetType))
            {
                if (expression.Syntax.XNode is AssignmentExpressionContext aec && aec.Op.Type == ASSIGN_EXP)
                {
                    return CreateXsConversion(expression, conversion, targetType, diagnostics);
                }
                if (expression.Syntax.XNode is BinaryExpressionContext bec && bec.Op.Type == EXP)
                {
                    return CreateXsConversion(expression, conversion, targetType, diagnostics);
                }
                if (expression.Kind == BoundKind.UnconvertedConditionalOperator)
                {
                    expression = BindToNaturalType(expression, diagnostics);
                }
                // silently convert integral types
                if (XsHasImplicitCast(expression, targetType, diagnostics))
                {
                    var sourceType = expression.Type;
                    BoundExpression result;
                    // Both integral, simple conversion
                    if (targetType.IsIntegralType() && sourceType.IsIntegralType())
                    {
                        return CreateXsConversion(expression, conversion, targetType, diagnostics);
                    }
                    if (Compilation.Options.HasRuntime && targetType.IsXNumericType() && sourceType.IsObjectType())
                    {
                        // To "silently" convert an object to a numeric we create a usual first and then rely
                        // on the conversion routines for the USUAL type
                        MethodSymbol ctor = FindConstructor(Compilation.UsualType(), 1, Compilation.GetSpecialType(SpecialType.System_Object));
                        expression = new BoundObjectCreationExpression(expression.Syntax, ctor, expression);
                        HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                        conversion = Conversions.ClassifyImplicitConversionFromExpression(expression, targetType, ref useSiteDiagnostics);
                        diagnostics.Add(expression.Syntax, useSiteDiagnostics);

                        return CreateXsConversion(expression, conversion, targetType, diagnostics);
                    }
                    if (targetType.IsXNumericType() && sourceType.IsUsualType() && Compilation.Options.HasRuntime)
                    {
                        return CreateXsConversion(expression, conversion, targetType, diagnostics);
                    }
                    else if (sourceType.IsXNumericType())
                    {
                        result = XsCreateConversionNonIntegralNumeric(targetType, expression, diagnostics, conversion);
                        result.WasCompilerGenerated = true;
                        return result;
                    }
                }
            }
            return null;
        }

        bool xsValueFitsIn(ConstantValue value, SpecialType specialType)
        {
            if (!value.IsIntegral)
                return false;
            if (value.SpecialType.IsSignedIntegralType())
            {
                var i64Value = value.Int64Value;
                return specialType switch
                {
                    SpecialType.System_SByte => i64Value <= sbyte.MaxValue && i64Value >= sbyte.MinValue,
                    SpecialType.System_Byte => i64Value <= byte.MaxValue && i64Value >= byte.MinValue,
                    SpecialType.System_Int16 => i64Value <= short.MaxValue && i64Value >= short.MinValue,
                    SpecialType.System_UInt16 => i64Value <= ushort.MaxValue && i64Value >= ushort.MinValue,
                    SpecialType.System_Int32 => i64Value <= int.MaxValue && i64Value >= int.MinValue,
                    SpecialType.System_UInt32 => i64Value <= uint.MaxValue && i64Value >= uint.MinValue,
                    SpecialType.System_UInt64 => i64Value > 0,
                    _ => true
                };
            }
            else
            {
                // unsigned
                var u64Value = value.UInt64Value;
                return specialType switch
                {
                    SpecialType.System_SByte => u64Value <= (ulong)sbyte.MaxValue,
                    SpecialType.System_Byte => u64Value <= byte.MaxValue,
                    SpecialType.System_Int16 => u64Value <= (ulong)short.MaxValue,
                    SpecialType.System_UInt16 => u64Value <= ushort.MaxValue,
                    SpecialType.System_Int32 => u64Value <= int.MaxValue,
                    SpecialType.System_UInt32 => u64Value <= uint.MaxValue,
                    SpecialType.System_Int64 => u64Value > long.MaxValue,
                    _ => true
                };
            }
        }

        bool XsLiteralIIfFitsInTarget(BoundConditionalOperator bco, TypeSymbol targetType)
        {
            var target = targetType.SpecialType;
            var trueConst = bco.Consequence.ConstantValue;
            var falseConst = bco.Alternative.ConstantValue;
            if (trueConst == null || falseConst == null)
            {
                return false;
            }
            if (target.IsIntegralType())
            {
                return xsValueFitsIn(trueConst, target) && xsValueFitsIn(falseConst, target);
            }
            return false;
        }

        void XsCheckConversionForAssignment(TypeSymbol targetType, ref BoundExpression expression, DiagnosticBag diagnostics, bool isDefaultParameter = false, bool isRefAssignment = false)
        {
            if (expression.Kind == BoundKind.UnboundLambda)
            {
                if (targetType.IsDelegateType())
                {
                    if (expression.Syntax.XIsCodeBlock && !Compilation.Options.MacroScript)
                    {
                        Error(diagnostics, ErrorCode.ERR_LamdaWithCodeblockSyntax, expression.Syntax, targetType);
                    }
                }
            }
            var vo4 = Compilation.Options.HasOption(CompilerOption.Vo4, expression.Syntax);
            var sourceType = expression.Type;
            var rhsType = expression.Type;
            if (rhsType is { } && !Equals(targetType, rhsType) &&
                targetType.SpecialType.IsIntegralType() &&
                rhsType.SpecialType.IsIntegralType())
            {
                bool ok = false;
                if (expression.ConstantValue != null)
                {
                    // warnings for literals that are too big are generated later
                    ok = true;
                }
                if (!ok)
                {
                    ok = Conversions.XsIsImplicitBinaryOperator(expression, targetType, this);
                }
                if (!ok)
                {
                    var sourceSize = sourceType.SpecialType.SizeInBytes();
                    var targetSize = targetType.SpecialType.SizeInBytes();

                    if (sourceSize > targetSize && expression is BoundBinaryOperator binop)
                    {
                        // determine size of smallest of the operands
                        sourceType = binop.LargestOperand(this.Compilation);
                        sourceSize = sourceType.SpecialType.SizeInBytes();
                    }
                    if (!Equals(sourceType, targetType)
                        && !expression.Syntax.HasErrors
                        && vo4
                        && !expression.Syntax.XContainsGeneratedExpression)
                    {
                        // Find sources that do not fit in the target
                        if (expression is BoundConditionalOperator bco && XsLiteralIIfFitsInTarget(bco, targetType))
                        {
                            return; // ok
                        }
                        else
                        {
                            var errorCode = LocalRewriter.DetermineConversionError(sourceType, targetType);
                            if (errorCode != ErrorCode.Void)
                            {
                                Error(diagnostics, errorCode, expression.Syntax, sourceType, targetType);
                            }
                        }
                    }
                }
            }
        }
    }
}

