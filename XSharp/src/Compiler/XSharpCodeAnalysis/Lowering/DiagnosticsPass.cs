/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

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
        private void VOCheckSignedUnsigned(BoundExpression expr, BoundConversion conversion)
        {
            // These checks will only be performed when /vo4 Signed-Unsigned conversions is selected
            // That is the only situation where an implicitNumeric conversion is performed.
            // We Check for integral types only
            if ( _compilation.Options.VOImplicitSignedUnsignedConversions &&
                conversion.Operand.Type != null && conversion.Type != null)
            {
                var type = conversion.Type;
                var opType = conversion.Operand.Type;
                var opKind = conversion.Operand.Kind;
                if (type.SpecialType.IsIntegralType() &&
                    opType.SpecialType.IsIntegralType() &&
                    opKind != BoundKind.Literal &&
                    opKind != BoundKind.UnaryOperator &&
                    expr.Syntax.Kind() != SyntaxKind.CastExpression)
                //  Unary operators will be handled in the next level

                {
                    // Find sources that do not fit in the target
                    if (opType.SpecialType.SizeInBytes() > type.SpecialType.SizeInBytes())
                    {
                        Error(ErrorCode.WRN_ConversionMayLeadToLossOfData, expr, conversion.Operand.Type, conversion.Type);
                    }
                    else if (opType.SpecialType.SizeInBytes() == type.SpecialType.SizeInBytes())
                    {
                        // Generate warning about signed / unsigned conversions
                        if (opType.SpecialType.IsSignedIntegralType() != type.SpecialType.IsSignedIntegralType())
                        {
                            Error(ErrorCode.WRN_SignedUnSignedConversion, expr, conversion.Operand.Type, conversion.Type);
                        }
                    }
                    // Optype.Size < Type.Size, only a problem when Optype is Signed and Type is Unsiged
                    else if (opType.SpecialType.IsSignedIntegralType() && !type.SpecialType.IsSignedIntegralType())
                    {
                        Error(ErrorCode.WRN_SignedUnSignedConversion, expr, conversion.Operand.Type, conversion.Type);
                    }
                }

            }
        }
    }
}
