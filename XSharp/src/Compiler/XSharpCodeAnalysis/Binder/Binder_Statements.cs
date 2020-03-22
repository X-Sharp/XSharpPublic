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
                if (CheckImplicitCast(expression.Type, targetType, expression.Syntax, diagnostics) || conversion.IsNullable)
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
            if (targetType != rhsType && 
                targetType.SpecialType.IsIntegralType() &&
                rhsType.SpecialType.IsIntegralType() 
                )
            {
                bool ok = false;
                if (expression.ConstantValue != null)
                {
                    if (rhsType.SpecialType.IsSignedIntegralType())
                    {
                        var value = expression.ConstantValue.Int64Value;
                        switch (targetType.SpecialType)
                        {
                            case SpecialType.System_SByte:
                                ok = value >= sbyte.MinValue && value <= sbyte.MaxValue;
                                break;
                            case SpecialType.System_Int16:
                                ok = value >= short.MinValue && value <= short.MaxValue;
                                break;
                            case SpecialType.System_Int32:
                                ok = value >= int.MinValue && value <= int.MaxValue;
                                break;
                            case SpecialType.System_Int64:
                                ok = true;
                                break;
                            case SpecialType.System_Byte:
                                ok = value >= 0 && value <= byte.MaxValue;
                                break;
                            case SpecialType.System_UInt16:
                                ok = value >= 0 && value <= ushort.MaxValue;
                                break;
                            case SpecialType.System_UInt32:
                                ok = value >= 0 && value <= uint.MaxValue;
                                break;
                            case SpecialType.System_UInt64:
                                ok = value >= 0;
                                break;
                            default:
                                ok = false;
                                break;
                        }
                        if (ok)
                            return;
                    }
                    else
                    {
                        var value = expression.ConstantValue.UInt64Value;
                        switch (targetType.SpecialType)
                        {
                            case SpecialType.System_SByte:
                                ok = value <= (ulong)sbyte.MaxValue;
                                break;
                            case SpecialType.System_Int16:
                                ok = value <= (ulong)short.MaxValue;
                                break;
                            case SpecialType.System_Int32:
                                ok = value <= int.MaxValue;
                                break;
                            case SpecialType.System_Byte:
                                ok = value <= byte.MaxValue;
                                break;
                            case SpecialType.System_UInt16:
                                ok = value <= ushort.MaxValue;
                                break;
                            case SpecialType.System_UInt32:
                                ok = value <= uint.MaxValue;
                                break;
                            case SpecialType.System_UInt64:
                                ok = true;
                                break;
                            case SpecialType.System_Int64:
                                ok = value < (ulong)long.MaxValue;
                                break;
                            default:
                                ok = false;
                                break;
                        }
                    }
                }
                if (! ok)
                {
                    var sourceType = expression.Type;
                    var sourceSize = sourceType.SpecialType.SizeInBytes();
                    var targetSize = targetType.SpecialType.SizeInBytes();
                    // Find sources that do not fit in the target
                    if (sourceSize > targetSize)
                    {
                        // Narrowing conversion from '{0}' to '{1}' may lead to loss of data or overflow errors
                        Error(diagnostics, ErrorCode.WRN_ConversionMayLeadToLossOfData, expression.Syntax, expression.Type, targetType);
                    }
                    else if (sourceSize == targetSize && expression.Type != targetType)
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

