//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System.Collections.Generic;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis.CSharp.Symbols;
namespace Microsoft.CodeAnalysis.CSharp
{

    internal partial class Binder
    {
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
            if (Compilation.Options.HasOption(CompilerOption.ArithmeticConversions, expression.Syntax)
                && expression.Type.SpecialType.IsNumericType() && targetType.SpecialType.IsNumericType())
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

        BoundExpression CreateXsConversion(BoundExpression expression, Conversion conversion, TypeSymbol targetType, DiagnosticBag diagnostics )
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
                    if (targetType.SpecialType.IsNumericType() &&
                        (sourceType.IsObjectType() || sourceType.IsUsualType()) &&
                        Compilation.Options.HasOption(CompilerOption.ArithmeticConversions, expression.Syntax))
                    {
                        return CreateXsConversion(expression, conversion, targetType, diagnostics);
                    }
                    else if (sourceType.SpecialType.IsNumericType())
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

        void XsCheckConversionForAssignment(TypeSymbol targetType, BoundExpression expression, DiagnosticBag diagnostics, bool isDefaultParameter = false, bool isRefAssignment = false)
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

            if (Compilation.Options.HasOption(CompilerOption.SignedUnsignedConversion, expression.Syntax))
                return;

            var rhsType = expression.Type;
            if (rhsType is { } && Equals(targetType, rhsType) &&
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
                    ok = Conversions.XsIsImplicitBinaryOperator(expression, targetType,this);
                }
                if (!ok)
                {
                    var sourceType = expression.Type;
                    var sourceSize = sourceType.SpecialType.SizeInBytes();
                    var targetSize = targetType.SpecialType.SizeInBytes();

                    if (sourceSize > targetSize && expression is BoundBinaryOperator binop)
                    {
                        // determine size of smallest of the operands
                        sourceType = binop.LargestOperand(this.Compilation);
                        sourceSize = sourceType.SpecialType.SizeInBytes();
                    }
                    if (!Equals(sourceType,targetType) && !expression.Syntax.HasErrors)
                    {
                        // Find sources that do not fit in the target
                        if (expression is BoundConditionalOperator bco && XsLiteralIIfFitsInTarget(bco, targetType))
                        {
                            return; // ok
                        }
                        else if (sourceSize > targetSize)
                        {
                            // Narrowing conversion from '{0}' to '{1}' may lead to loss of data or overflow errors
                            Error(diagnostics, ErrorCode.WRN_ConversionMayLeadToLossOfData, expression.Syntax, expression.Type, targetType);
                        }
                        else if (sourceSize == targetSize)
                        {
                            //Signed/unsigned conversions from '{0}' to '{1}' may lead to loss of data or overflow errors
                             Error(diagnostics, ErrorCode.WRN_SignedUnSignedConversion, expression.Syntax, expression.Type, targetType);
                        }
                        // lhs.Size < rhs.Size, only a problem when lhs is Signed and rhs is Unsiged
                        else if (sourceSize < targetSize && sourceType.SpecialType.IsSignedIntegralType() && !targetType.SpecialType.IsSignedIntegralType())
                        {
                            // Signed / unsigned conversions from '{0}' to '{1}' may lead to loss of data or overflow errors

                            Error(diagnostics, ErrorCode.WRN_SignedUnSignedConversion, expression.Syntax, expression.Type, targetType);
                        }
                    }
                }
            }
        }
    }
}

