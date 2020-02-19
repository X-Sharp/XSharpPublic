//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;

namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// This pass detects and reports diagnostics that do not affect lambda convertibility.
    /// This part of the partial class focuses on expression and operator warnings.
    /// </summary>
    internal sealed partial class DiagnosticsPass 
    {
        private void VOCheckIntegerToPointer(BoundConversion node)
        {
            var destType = node.Type;
            var srcType = node.Operand.Type;
            var platform = _compilation.Options.Platform;
            switch (platform)
            {
                case Platform.X86:
                    if (srcType.SpecialType.SizeInBytes() != 4)
                    {
                        Error(ErrorCode.ERR_CantCastPtrInPlatform, node,srcType.ToDisplayString(),"x86");
                    }
                    break;
                case Platform.X64:
                    if (srcType.SpecialType.SizeInBytes() != 8)
                    {
                        Error(ErrorCode.ERR_CantCastPtrInPlatform, node, srcType.ToDisplayString(), "x64");
                    }
                    break;
                default:
                    string name = platform.ToString();
                    if (platform == Platform.AnyCpu32BitPreferred)
                        name = "AnyCpu";
                    Error(ErrorCode.ERR_CantCastPtrInAnyCpu, node, srcType.ToDisplayString(),name);
                    break;
            }
        }
        private void VOCheckSignedUnsigned(BoundExpression expr1, BoundExpression expr2)
        {
            // These checks will only be performed when /vo4 Signed-Unsigned conversions is selected
            // That is the only situation where an implicitNumeric conversion is performed.
            // We Check for integral types only
            expr1 = StripImplicitCasts(expr1);
            expr2 = StripImplicitCasts(expr2);
            if ( _compilation.Options.VOSignedUnsignedConversion && expr1.Type != expr2.Type && expr1.Type.IsIntegralType() && expr2.Type.IsIntegralType())
            {
                bool ok = false;
                if (expr2.ConstantValue != null)
                {
                    if (expr2.ConstantValue.SpecialType.IsSignedIntegralType())
                    {
                        var value = expr2.ConstantValue.Int64Value;
                        switch (expr1.Type.SpecialType)
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
                                ok = value >= 0 ;
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
                        var value = expr2.ConstantValue.UInt64Value;
                        switch (expr1.Type.SpecialType)
                        {
                            case SpecialType.System_SByte:
                                ok = value <= (ulong)sbyte.MaxValue;
                                break;
                            case SpecialType.System_Int16:
                                ok = value <= (ulong) short.MaxValue;
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
                        if (ok)
                            return;
                    }
                }

                // Find sources that do not fit in the target
                if (expr1.Type.SpecialType.SizeInBytes() > expr2.Type.SpecialType.SizeInBytes())
                {
                    Error(ErrorCode.WRN_ConversionMayLeadToLossOfData, expr1, expr1.Type, expr2.Type);
                }
                else if (expr1.Type.SpecialType.SizeInBytes() == expr2.Type.SpecialType.SizeInBytes())
                {
                   Error(ErrorCode.WRN_SignedUnSignedConversion, expr1, expr1.Type, expr2.Type);
                }
                // Optype.Size < Type.Size, only a problem when Optype is Signed and Type is Unsiged
                else if (expr1.Type.SpecialType.IsSignedIntegralType() && !expr2.Type.SpecialType.IsSignedIntegralType())
                {
                    Error(ErrorCode.WRN_SignedUnSignedConversion, expr1, expr1.Type, expr2.Type);
                }
            }

        }
    }
}
