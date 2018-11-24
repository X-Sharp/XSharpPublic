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

using Microsoft.CodeAnalysis.CSharp.Symbols;
using System.Collections.Generic;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using XP=LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
using System;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal sealed partial class OverloadResolution
    {
        private bool VOBetterFunctionMember<TMember>(
            MemberResolutionResult<TMember> m1,
            MemberResolutionResult<TMember> m2,
            ArrayBuilder<BoundExpression> arguments,
            out BetterResult result,
            out HashSet<DiagnosticInfo> useSiteDiagnostics
            )
            where TMember : Symbol
        {
            result = BetterResult.Neither;
            bool Ambiguous = false;
            // Prefer the member not declared in VulcanRT, if applicable
            useSiteDiagnostics = null;
            if (Compilation.Options.HasRuntime)
            {
                var asm1 = m1.Member.ContainingAssembly;
                var asm2 = m2.Member.ContainingAssembly;
                if (asm1 != asm2)
                {
                    // prefer XSharpCore over XSharpVO, so typed versions get preference over untyped versions
                    //if (asm1.IsXSharpCore() && asm2.IsXSharpVO())
                    //{
                    //    result = BetterResult.Left;
                    //    return true;
                    //}
                    // prefer non runtime over runtime to allow overriding built-in functions
                    if (asm1.IsRT() != asm2.IsRT())
                    {
                        if (asm1.IsRT())
                        {
                            result = BetterResult.Right;
                            return true;
                        }
                        else if (asm2.IsRT())
                        {
                            result = BetterResult.Left;
                            return true;
                        }
                    }
                    // prefer functions/method in the current assembly over external methods
                    if (asm1.IsFromCompilation(Compilation))
                    {
                        result = BetterResult.Left;
                        return true;
                    }
                    if (asm2.IsFromCompilation(Compilation))
                    {
                        result = BetterResult.Right;
                        return true;
                    }
                }
                if (m1.Member.HasClipperCallingConvention() != m2.Member.HasClipperCallingConvention())
                {
                    if (m1.Member.HasClipperCallingConvention())
                        result = BetterResult.Right;
                    else
                        result = BetterResult.Left;
                    return true;
                }
                if (m1.Member.GetParameterCount() == m2.Member.GetParameterCount())
                {
                    // In case of 2 methods with the same # of parameters 
                    // we have different / extended rules compared to C#
                    var parsLeft = m1.Member.GetParameters();
                    var parsRight = m2.Member.GetParameters();
                    var usualType = Compilation.UsualType();
                    var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
                    var len = parsLeft.Length;
                    if (arguments.Count < len)
                        len = arguments.Count;

                    bool equalLeft = true;
                    bool equalRight = true;
                    // check if all left types are equal
                    if (parsLeft.Length == arguments.Count)
                    {
                        for (int i = 0; i < len; i++)
                        {
                            var parLeft = parsLeft[i];
                            var arg = arguments[i];
                            if (parLeft.Type != arg.Type)
                            {
                                equalLeft = false;
                                break;
                            }
                        }
                    }
                    // check if all right types are equal
                    if (parsRight.Length == arguments.Count)
                    {
                        for (int i = 0; i < len; i++)
                        {
                            var parRight = parsRight[i];
                            var arg = arguments[i];
                            if (parRight.Type != arg.Type)
                            {
                                equalRight = false;
                                break;
                            }
                        }
                    }
                    // Only exit here when one of the two is better than the other
                    if (equalLeft && !equalRight)
                    {
                        result = BetterResult.Left;
                        return true;
                    }
                    if (equalRight && !equalLeft)
                    {
                        result = BetterResult.Right;
                        return true;
                    }
                    for (int i = 0; i < len; i++)
                    {
                        var parLeft = parsLeft[i];
                        var parRight = parsRight[i];
                        var refLeft = parLeft.RefKind;
                        var refRight = parRight.RefKind;
                        var arg = arguments[i];
                        bool argCanBeByRef = arg.Kind == BoundKind.AddressOfOperator;
                        var argType = arg.Type;
                        if (argCanBeByRef)
                        {
                            var bao = arg as BoundAddressOfOperator;
                            argType = bao.Operand.Type;
                        }

                        if (parLeft.Type != parRight.Type || refLeft != refRight)
                        {
                            // Prefer the method with a more specific parameter which is not an array type over USUAL
                            if (parLeft.Type == usualType && argType != usualType && !parRight.Type.IsArray())
                            {
                                result = BetterResult.Right;
                                return true;
                            }
                            if (parRight.Type == usualType && argType != usualType && !parLeft.Type.IsArray())
                            {
                                result = BetterResult.Left;
                                return true;
                            }
                            // Prefer the method with Object type over the one with Object[] type
                            if (parLeft.Type == objectType && parRight.Type.IsArray() && ((ArrayTypeSymbol)parRight.Type).ElementType == objectType)
                            {
                                result = BetterResult.Left;
                                return true;
                            }
                            if (parRight.Type == objectType && parLeft.Type.IsArray() && ((ArrayTypeSymbol)parLeft.Type).ElementType == objectType)
                            {
                                result = BetterResult.Right;
                                return true;
                            }
                            // Now check for REF parameters and possible REF arguments
                            if (argCanBeByRef)
                            {
                                var op = arg as BoundAddressOfOperator;
                                var opType = op?.Operand?.Type;
                                if (refLeft == RefKind.Ref && opType == parLeft.Type)
                                {
                                    result = BetterResult.Left;
                                    return true;
                                }
                                if (refRight == RefKind.Ref && opType == parRight.Type)
                                {
                                    result = BetterResult.Right;
                                    return true;
                                }
                                if (refLeft != refRight)
                                {
                                    if (refLeft == RefKind.Ref)
                                    {
                                        result = BetterResult.Left;
                                        return true;
                                    }
                                    if (refRight == RefKind.Ref)
                                    {
                                        result = BetterResult.Right;
                                        return true;
                                    }
                                }
                            }
                            if (refLeft != refRight)
                            {
                                if (parLeft.Type == argType && refLeft != RefKind.None && argCanBeByRef)
                                {
                                    result = BetterResult.Left;
                                    return true;
                                }
                                if (parRight.Type == argType && refRight != RefKind.None && argCanBeByRef)
                                {
                                    result = BetterResult.Right;
                                    return true;
                                }
                                if (parLeft.Type == argType && refLeft == RefKind.None && !argCanBeByRef)
                                {
                                    result = BetterResult.Left;
                                    return true;
                                }
                                if (parRight.Type == argType && refRight == RefKind.None && !argCanBeByRef)
                                {
                                    result = BetterResult.Right;
                                    return true;
                                }
                            }
                            // now fall back to original type (and not addressof type)
                            argType = arg.Type;
                            // Handle passing Enum values to methods that have a non enum parameter
                            if (argType?.TypeKind == TypeKind.Enum)
                            {
                                // First check if they have the enum type itself
                                if (argType == parLeft.Type)
                                {
                                    result = BetterResult.Left;
                                    return true;
                                }
                                if (argType == parRight.Type)
                                {
                                    result = BetterResult.Right;
                                    return true;
                                }
                                // Then check the underlying type
                                argType = argType.EnumUnderlyingType();
                                if (argType == parLeft.Type)
                                {
                                    result = BetterResult.Left;
                                    return true;
                                }
                                if (argType == parRight.Type)
                                {
                                    result = BetterResult.Right;
                                    return true;
                                }
                            }
                            if (argType == parLeft.Type)
                            {
                                result = BetterResult.Left;
                                return true;
                            }
                            if (argType == parRight.Type)
                            {
                                result = BetterResult.Right;
                                return true;
                            }
                            // VoFloat prefers overload with double over all other conversions
                            if (argType == Compilation.FloatType())
                            {
                                var doubleType = Compilation.GetSpecialType(SpecialType.System_Double);
                                if (parLeft.Type == doubleType)
                                {
                                    result = BetterResult.Left;
                                    return true;
                                }
                                if (parRight.Type == doubleType)
                                {
                                    result = BetterResult.Right;
                                    return true;
                                }
                            }
                            // handle case where argument is usual and the method is not usual
                            // prefer method with "native VO" parameter type
                            if (argType == Compilation.UsualType())
                            {
                                // no need to check if parleft or parright are usual that was checked above
                                if (parLeft.Type != parRight.Type)
                                {
                                    // is there an VO style conversion possible ?
                                    var leftConvert = parLeft.Type.IsValidVOUsualType(Compilation);
                                    var rightConvert = parRight.Type.IsValidVOUsualType(Compilation);
                                    if (leftConvert != rightConvert)
                                    {
                                        // One is a valid conversion, the other is not.
                                        if (leftConvert)
                                            result = BetterResult.Left;
                                        else
                                            result = BetterResult.Right;
                                        return true;
                                    }
                                }
                            }
                        }

                    }
                }
                // when both methods are in a functions class from different assemblies
                // pick the first one in the references list
                if (asm1 != asm2
                    && string.Equals(m1.Member.ContainingType.Name, XSharpSpecialNames.FunctionsClass, StringComparison.OrdinalIgnoreCase)
                    && string.Equals(m2.Member.ContainingType.Name, XSharpSpecialNames.FunctionsClass, StringComparison.OrdinalIgnoreCase))
                {
                    foreach (var reference in Compilation.ReferencedAssemblyNames)
                    {
                        if (reference.Name == asm1.Name)
                        {
                            result = BetterResult.Left;
                            Ambiguous = true;
                        }
                        if (reference.Name == asm2.Name)
                        {
                            result = BetterResult.Right;
                            Ambiguous = true;
                        }
                        if (Ambiguous)
                        {
                            TMember r1, r2;
                            if (result == BetterResult.Left)
                            {
                                r1 = m1.Member;
                                r2 = m2.Member;
                            }
                            else
                            {
                                r1 = m2.Member;
                                r2 = m1.Member;
                            }

                            var info = new CSDiagnosticInfo(ErrorCode.WRN_VulcanAmbiguous,
                                new object[] {
                                        r1.Name,
                                        new FormattedSymbol(r1, SymbolDisplayFormat.CSharpErrorMessageFormat),
                                        new FormattedSymbol(r2, SymbolDisplayFormat.CSharpErrorMessageFormat),
                                        r1.Kind.ToString()});
                            useSiteDiagnostics = new HashSet<DiagnosticInfo>();
                            useSiteDiagnostics.Add(info);
                            return true;
                        }
                    }
                }
            }
          

            // generate warning that function takes precedence over static method
            var func1 = m1.Member.ContainingType.Name.EndsWith("Functions");
            var func2 = m2.Member.ContainingType.Name.EndsWith("Functions");
            if (func1 && !func2)
            {
                result = BetterResult.Left;
                var info = new CSDiagnosticInfo(ErrorCode.WRN_FunctionsTakePrecedenceOverMethods,
                    new object[] {
                            m1.Member.Name,
                            new FormattedSymbol(m1.Member, SymbolDisplayFormat.CSharpErrorMessageFormat),
                            new FormattedSymbol(m2.Member, SymbolDisplayFormat.CSharpErrorMessageFormat),
                            m1.Member.Kind.ToString()});
                useSiteDiagnostics = new HashSet<DiagnosticInfo>();
                useSiteDiagnostics.Add(info);

            }
            if (func2 && !func1)
            {
                result = BetterResult.Right;
                var info = new CSDiagnosticInfo(ErrorCode.WRN_FunctionsTakePrecedenceOverMethods,
                    new object[] {
                            m2.Member.Name,
                            new FormattedSymbol(m2.Member, SymbolDisplayFormat.CSharpErrorMessageFormat),
                            new FormattedSymbol(m1.Member, SymbolDisplayFormat.CSharpErrorMessageFormat),
                            m2.Member.Kind.ToString()});
                useSiteDiagnostics = new HashSet<DiagnosticInfo>();
                useSiteDiagnostics.Add(info);

            }
            return false;
        }

        private BetterResult VoBetterOperator(BinaryOperatorSignature op1, BinaryOperatorSignature op2, BoundExpression left, BoundExpression right, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            // When the binary operators are equal we inspect the types
            if ((op1.Kind & BinaryOperatorKind.OpMask) == (op2.Kind & BinaryOperatorKind.OpMask))
            {
                if ((op1.Kind & BinaryOperatorKind.TypeMask) == BinaryOperatorKind.Float &&
                    (op2.Kind & BinaryOperatorKind.TypeMask) == BinaryOperatorKind.Double)
                {
                    // Lhs = real4, rhs = real8, choose real8

                    return BetterResult.Right;
                }
                if ((op1.Kind & BinaryOperatorKind.TypeMask) == BinaryOperatorKind.Double)
                {
                    // rhs = numeric, lhs = double choose double
                    switch (op2.Kind & BinaryOperatorKind.TypeMask)
                    {
                        case BinaryOperatorKind.Int:
                        case BinaryOperatorKind.UInt:
                        case BinaryOperatorKind.Long:
                        case BinaryOperatorKind.ULong:
                        case BinaryOperatorKind.Float:
                        case BinaryOperatorKind.Decimal:
                            return BetterResult.Left;
                    }
                }
                if ((op2.Kind & BinaryOperatorKind.TypeMask) == BinaryOperatorKind.Double)
                {
                    // lhs = numeric, rhs = double choose double
                    switch (op1.Kind & BinaryOperatorKind.TypeMask)
                    {
                        case BinaryOperatorKind.Int:
                        case BinaryOperatorKind.UInt:
                        case BinaryOperatorKind.Long:
                        case BinaryOperatorKind.ULong:
                        case BinaryOperatorKind.Float:
                        case BinaryOperatorKind.Decimal:
                            return BetterResult.Right;
                    }
                }
                if (left.Type != null && right.Type != null)
                {
                    bool enumL = left.Type.IsEnumType() || left.Type.IsNullableType() && left.Type.GetNullableUnderlyingType().IsEnumType();
                    bool enumR = right.Type.IsEnumType() || right.Type.IsNullableType() && right.Type.GetNullableUnderlyingType().IsEnumType();
                    if (enumL ^ enumR)
                    {
                        bool enum1 = (op1.LeftType.IsEnumType() || op1.LeftType.IsNullableType() && op1.LeftType.GetNullableUnderlyingType().IsEnumType())
                            && (op1.RightType.IsEnumType() || op1.RightType.IsNullableType() && op1.RightType.GetNullableUnderlyingType().IsEnumType());
                        bool enum2 = (op2.LeftType.IsEnumType() || op2.LeftType.IsNullableType() && op2.LeftType.GetNullableUnderlyingType().IsEnumType())
                            && (op2.RightType.IsEnumType() || op2.RightType.IsNullableType() && op2.RightType.GetNullableUnderlyingType().IsEnumType());
                        if (enum1 && !enum2)
                        {
                            return BetterResult.Left;
                        }
                        else if (!enum1 && enum2)
                            return BetterResult.Right;
                    }
                    // when /vo4 is enabled then we may end up having duplicate candidates
                    // we decide here which one takes precedence
                    if (Compilation.Options.VOSignedUnsignedConversion)
                    {
                        #region Integral Binary Operators
                        if (left.Type.IsIntegralType() && right.Type.IsIntegralType()
                            && op1.Kind.IsIntegral() && op2.Kind.IsIntegral())
                        {
                            // when both operands have integral types, choose the one that match the sign and or size 
                            // we check the lhs of the expression first
                            bool exprSigned = left.Type.SpecialType.IsSignedIntegralType();
                            bool op1Signed = op1.LeftType.SpecialType.IsSignedIntegralType();
                            bool op2Signed = op2.LeftType.SpecialType.IsSignedIntegralType();
                            int exprSize = left.Type.SpecialType.SizeInBytes();
                            int op1Size = op1.LeftType.SpecialType.SizeInBytes();
                            int op2Size = op2.LeftType.SpecialType.SizeInBytes();
                            // op1 matches sign and size and op2 does not
                            if ((exprSigned == op1Signed && exprSize == op1Size)
                                && (exprSigned != op2Signed || exprSize != op2Size))
                            {
                                return BetterResult.Left;
                            }
                            // op2 matches sign and size and op1 does not
                            if ((exprSigned != op1Signed || exprSize != op1Size)
                                && (exprSigned == op2Signed && exprSize == op2Size))
                            {
                                return BetterResult.Right;
                            }
                            // When we get here they both match or both do not match the sign and size
                            // now check the rhs of the expression, to see if this helps to decide
                            exprSigned = right.Type.SpecialType.IsSignedIntegralType();
                            exprSize = right.Type.SpecialType.SizeInBytes();
                            op1Signed = op1.RightType.SpecialType.IsSignedIntegralType();
                            op2Signed = op2.RightType.SpecialType.IsSignedIntegralType();
                            // when still undecided then choose the one where the size matches best
                            // op1 matches sign and size and op2 does not
                            if ((exprSigned == op1Signed && exprSize == op1Size)
                                && (exprSigned != op2Signed || exprSize != op2Size))
                            {
                                return BetterResult.Left;
                            }
                            // op2 matches sign and size and op1 does not
                            if ((exprSigned != op1Signed || exprSize != op1Size)
                                && (exprSigned == op2Signed && exprSize == op2Size))
                            {
                                return BetterResult.Right;
                            }
                            // still no match. Forget the size and check only on sign
                            exprSigned = left.Type.SpecialType.IsSignedIntegralType();
                            op1Signed = op1.LeftType.SpecialType.IsSignedIntegralType();
                            op2Signed = op2.LeftType.SpecialType.IsSignedIntegralType();
                            // op1 matches sign and op2 does not
                            if (exprSigned == op1Signed && exprSigned != op2Signed)
                            {
                                return BetterResult.Left;
                            }
                            // op2 matches sign and op1 does not
                            if (exprSigned != op1Signed && exprSigned == op2Signed)
                            {
                                return BetterResult.Right;
                            }
                            exprSigned = right.Type.SpecialType.IsSignedIntegralType();
                            op1Signed = op1.RightType.SpecialType.IsSignedIntegralType();
                            op2Signed = op2.RightType.SpecialType.IsSignedIntegralType();
                            // op1 matches sign and op2 does not
                            if (exprSigned == op1Signed && exprSigned != op2Signed)
                            {
                                return BetterResult.Left;
                            }
                            // op2 matches sign and op1 does not
                            if (exprSigned != op1Signed && exprSigned == op2Signed)
                            {
                                return BetterResult.Right;
                            }
                        }
                        #endregion
                    }

                    if ((left.Type.IsIntegralType() && right.Type.IsPointerType())
                        || left.Type.IsPointerType() && right.Type.IsIntegralType())
                    {
                        if (op1.LeftType.IsVoidPointer() && op1.RightType.IsVoidPointer())
                            return BetterResult.Left;
                        if (op2.LeftType.IsVoidPointer() && op2.RightType.IsVoidPointer())
                            return BetterResult.Right;
                    }

                }

            }
            // Solve Literal operations such as generated by ForNext statement
            if (right.Kind == BoundKind.Literal && op1.LeftType == left.Type)
            {
                if (left.Type.SpecialType.IsSignedIntegralType())     // When signed, always Ok
                    return BetterResult.Left;
                else if (left.Type.SpecialType.IsIntegralType())      // Unsigned integral, so check for overflow
                {

                    var constValue = ((BoundLiteral)right).ConstantValue;
                    if (constValue.IsIntegral && constValue.Int64Value >= 0)
                    {
                        return BetterResult.Left;
                    }
                }
                else // not integral, so most likely floating point
                {
                    return BetterResult.Left;
                }
            }
            if (left.Kind == BoundKind.Literal && op1.RightType == right.Type)
            {
                if (right.Type.SpecialType.IsSignedIntegralType())     // When signed, always Ok
                    return BetterResult.Left;
                else if (right.Type.SpecialType.IsIntegralType())      // Unsigned integral, so check for overflow
                {

                    var constValue = ((BoundLiteral)left).ConstantValue;
                    if (constValue.IsIntegral && constValue.Int64Value >= 0)
                    {
                        return BetterResult.Left;
                    }
                }
                else // not integral, so most likely floating point
                {
                    return BetterResult.Left;
                }
            }
            if (Compilation.Options.HasRuntime)
            {
                var usualType = Compilation.UsualType();
                if (left.Type != usualType)
                {
                    if (op1.RightType != usualType && op2.RightType == usualType)
                        return BetterResult.Left;
                    if (op2.RightType != usualType && op1.RightType == usualType)
                        return BetterResult.Right;
                }
                if (right.Type != usualType)
                {
                    if (op1.LeftType != usualType && op2.LeftType == usualType)
                        return BetterResult.Left;
                    if (op2.LeftType != usualType && op1.LeftType == usualType)
                        return BetterResult.Right;
                }
            }
            return BetterResult.Neither;
        }
        private bool VOStructBinaryOperatorComparison(BinaryOperatorKind kind, BoundExpression left, BoundExpression right, BinaryOperatorOverloadResolutionResult result)
        {
            if (left.Type == right.Type)
            {
                bool isVoStruct = false;
                if (left.Type.IsPointerType())
                {
                    var pt = left.Type as PointerTypeSymbol;
                    isVoStruct = pt.PointedAtType.IsVoStructOrUnion();
                }
                else
                {
                    isVoStruct = left.Type.IsVoStructOrUnion();
                }
                if (isVoStruct && (kind == BinaryOperatorKind.Equal || kind == BinaryOperatorKind.NotEqual))
                {
                    BinaryOperatorSignature sig = new BinaryOperatorSignature(kind, left.Type, right.Type, Compilation.GetSpecialType(SpecialType.System_Boolean));
                    BinaryOperatorAnalysisResult best = BinaryOperatorAnalysisResult.Applicable(sig, Conversion.Identity, Conversion.Identity);
                    result.Results.Clear();
                    result.Results.Add(best);
                    return true;
                }
            }
            return false;
        }
    }
    internal static class CastExtensionMethods
    {
        internal static bool IsVoCast(this XSharpParserRuleContext node)
        {
            if (node is XP.PrimaryExpressionContext)
            {
                var pec = node as XP.PrimaryExpressionContext;
                return pec.Expr is XP.VoCastExpressionContext;
            }
            return false;
        }
        internal static bool IsVoConvert(this XSharpParserRuleContext node)
        {
            if (node is XP.PrimaryExpressionContext)
            {
                var pec = node as XP.PrimaryExpressionContext;
                return pec.Expr is XP.VoConversionExpressionContext;
            }
            return false;
        }
        internal static bool IsCastClass(this XSharpParserRuleContext node)
        {
            return node.Start.Type == XSharpLexer.CASTCLASS;
        }
    }
}
