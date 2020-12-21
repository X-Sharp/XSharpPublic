//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Generic;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis.CSharp.Symbols;
namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class Binder
    {
        BoundExpression XsCreateConversionNonIntegralNumeric(TypeSymbol targetType, BoundExpression expression, DiagnosticBag diagnostics, Conversion conversion)
        {
            if (Compilation.Options.HasOption(CompilerOption.ArithmeticConversions, expression.Syntax))
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
                return new BoundCall(expression.Syntax, expression, (MethodSymbol)mem,
                    args.ToImmutableArray(), default(ImmutableArray<string>), default(ImmutableArray<RefKind>), false, false,
                    false, default(ImmutableArray<int>), LookupResultKind.Viable, null, targetType);
            }
            else
            {
                // call (type) Expression to truncate the result
                return CreateConversion(expression.Syntax, expression, conversion, false, targetType, diagnostics);
            }

        }

        BoundExpression XsHandleNullPsz(TypeSymbol targetType, BoundExpression expression)
        {
            if (Compilation.Options.HasRuntime && targetType == Compilation.PszType())
            {
                if (IsNullNode(expression))
                {
                    return PszFromNull(expression);
                }
            }
            return null;
        }

        BoundExpression XsHandleExplicitConversion(TypeSymbol targetType, BoundExpression expression, DiagnosticBag diagnostics, Conversion conversion)
        {
            if (conversion.IsExplicit && expression.Type != targetType)
            {
                // silently convert integral types
                if (XsHasImplicitCast(expression , targetType, diagnostics) )
                {
                    BoundExpression result;
                    if (targetType.IsIntegralType() && expression.Type.IsIntegralType())
                    {
                        result=  CreateConversion(expression.Syntax, expression, conversion, false, targetType, diagnostics);
                    }
                    else
                    {
                        result = XsCreateConversionNonIntegralNumeric(targetType, expression, diagnostics, conversion);
                    }
                    result.WasCompilerGenerated = true;
                    return result;

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
                switch (specialType)
                {
                    case SpecialType.System_SByte:
                        return i64Value <= sbyte.MaxValue && i64Value >= sbyte.MinValue;
                    case SpecialType.System_Byte:
                        return i64Value <= byte.MaxValue && i64Value >= byte.MinValue;
                    case SpecialType.System_Int16:
                        return i64Value <= short.MaxValue && i64Value >= short.MinValue;
                    case SpecialType.System_UInt16:
                        return i64Value <= ushort.MaxValue && i64Value >= ushort.MinValue;
                    case SpecialType.System_Int32:
                        return i64Value <= int.MaxValue && i64Value >= int.MinValue;
                    case SpecialType.System_UInt32:
                        return i64Value <= uint.MaxValue && i64Value >= uint.MinValue;
                    case SpecialType.System_UInt64:
                        return i64Value > 0;
                }
                return true;
            }
            else
            {
                // unsigned
                var u64Value = value.UInt64Value;
                switch (specialType)
                {
                    case SpecialType.System_SByte:
                        return u64Value <= (ulong) sbyte.MaxValue ;
                    case SpecialType.System_Byte:
                        return u64Value <= byte.MaxValue ;
                    case SpecialType.System_Int16:
                        return u64Value <= (ulong)short.MaxValue;
                    case SpecialType.System_UInt16:
                        return u64Value <= ushort.MaxValue ;
                    case SpecialType.System_Int32:
                        return u64Value <= int.MaxValue ;
                    case SpecialType.System_UInt32:
                        return u64Value <= uint.MaxValue ;
                    case SpecialType.System_Int64:
                        return u64Value > long.MaxValue;
                }
                return true;
            }
        }

        bool XsLiteralIIfFitsInTarget(BoundConditionalOperator bco, TypeSymbol targetType)
        {
            var target = targetType.SpecialType;
            var trueConst = bco.Consequence.ConstantValue;
            var falseConst = bco.Alternative.ConstantValue;
            if (trueConst == null || falseConst== null)
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
            var st = targetType.GetSpecialTypeSafe();
            var rt = rhsType.GetSpecialTypeSafe();
            if (targetType != rhsType && st.IsIntegralType() && rt.IsIntegralType() )
            {
                bool ok = false;
                if (expression.ConstantValue != null)
                {
                    // warnings for literals that are too big are generated later
                    ok = true;
                }
                if (! ok)
                {
                    ok = Conversions.XsIsImplicitBinaryOperator(expression, targetType,this);
                }
                if (! ok)
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
                    if (sourceType != targetType && !expression.Syntax.HasErrors)
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

