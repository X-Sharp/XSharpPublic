//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

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
        private void XsCheckCompoundAssignmentOperator(BoundCompoundAssignmentOperator node)
        {
            var syntax = node.Syntax;
            if (syntax == null || node.ConstantValue != null || node.WasCompilerGenerated || syntax.XGenerated)
                return;
            var leftType = node.Left.Type;
            var rightType = node.Right.Type;
            GenerateWarning(rightType, leftType, node);
        }

        private void GenerateWarning(TypeSymbol sourceType, TypeSymbol targetType, BoundNode node)
        {
            var syntax = node.Syntax;
            if (syntax.XWarning && !Equals(sourceType, targetType))
            {
                var vo4 = _compilation.Options.HasOption(CompilerOption.SignedUnsignedConversion, syntax);
                var vo11 = _compilation.Options.HasOption(CompilerOption.ArithmeticConversions, syntax);
                if (targetType.IsIntegralType() && sourceType.IsIntegralType())
                {
                    var srcsize = sourceType.SpecialType.SizeInBytes();
                    var trgsize = targetType.SpecialType.SizeInBytes();
                    if ((vo4 || vo11) && srcsize == trgsize)
                    {
                        Error(ErrorCode.WRN_SignedUnSignedConversion, node, sourceType, targetType);
                    }
                    else if (vo11 && srcsize > trgsize)
                    {
                        Error(ErrorCode.WRN_ConversionMayLeadToLossOfData, node, sourceType, targetType);
                    }
                }
                else if (vo11 && sourceType.IsNumericType() && targetType.IsNumericType())
                {
                    if (sourceType.IsFractionalType() && !targetType.IsFractionalType())
                    {
                        Error(ErrorCode.WRN_ConversionMayLeadToLossOfData, node, sourceType, targetType);
                    }
                }
            }
        }

        private void XsCheckConversion(BoundConversion node)
        {
            var syntax = node.Syntax;
            if (syntax == null || node.ConstantValue != null || node.WasCompilerGenerated ||
                syntax.XIsExplicitTypeCastInCode || syntax.XGenerated)
                return;
            var sourceType = node.Operand.Type;
            var targetType = node.Type;
            GenerateWarning(sourceType, targetType, node);
        }

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
    }
}
