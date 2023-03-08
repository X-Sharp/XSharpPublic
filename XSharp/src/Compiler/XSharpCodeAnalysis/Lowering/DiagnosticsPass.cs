//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;

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
            if (syntax == null || node.WasCompilerGenerated)
                return;
            var leftType = node.Left.Type;
            var rightType = node.Right.Type;
            if (node.Right is BoundConversion bcv)
            {
                rightType = bcv.Operand.Type;
            }
            GenerateWarning(rightType, leftType, node);
        }

        private void GenerateWarning(TypeSymbol sourceType, TypeSymbol targetType, BoundNode node)
        {

            var syntax = node.Syntax;
            if (node.IsExpressionWithNumericConstant() || syntax.XIsExplicitTypeCastInCode)
            {
                node.DisableWarnings();
            }
            if (!syntax.XNoWarning && !syntax.XNoTypeWarning && !Equals(sourceType, targetType) && !syntax.XContainsGeneratedExpression
                && sourceType.IsXNumericType() && targetType.IsXNumericType())
            {
                var errCode = ErrorCode.Void;
                if (_compilation.Options.HasOption(CompilerOption.Vo4, syntax))
                {
                    // when converting from fractional to another type precision may be lost
                    errCode = LocalRewriter.DetermineConversionError(sourceType, targetType);
                }
                if (errCode != ErrorCode.Void)
                {
                    Error(errCode, node, sourceType, targetType);
                }
            }
        }
        private void XsCheckStaticMemberAccess(BoundNode node, XSharpParser.AccessMemberContext amc, Symbol symbol)
        {
            if (_compilation.Options.Dialect != XSharpDialect.XPP)
            {
                if (amc.Op.Type != XSharpLexer.DOT && amc.Op.Type != XSharpLexer.COLONCOLON && !node.HasErrors())
                {
                    Error(ErrorCode.ERR_ColonForStaticMember, node, symbol);
                }
            }
        }
        private void XsCheckInstanceMemberAccess(BoundNode node, XSharpParser.AccessMemberContext amc, Symbol symbol)
        {
            if (amc.Op.Type != XSharpLexer.COLON &&
                amc.Op.Type != XSharpLexer.COLONCOLON &&
                !node.HasErrors() && !_compilation.Options.HasOption(CompilerOption.AllowDotForInstanceMembers, node.Syntax))
            {
                Error(ErrorCode.ERR_DotForInstanceMember, node, symbol);
            }
        }

        private void XsVisitFieldAccess(BoundFieldAccess node)
        {
            if (node.Syntax?.XNode is XSharpParser.AccessMemberContext amc)
            {
                if (node.FieldSymbol.IsStatic)
                    XsCheckStaticMemberAccess(node, amc, node.FieldSymbol);
                else
                    XsCheckInstanceMemberAccess(node, amc, node.FieldSymbol);
            }
            return;
        }
        private void XsVisitPropertyAccess(BoundPropertyAccess node)
        {
            if (node.Syntax?.XNode is XSharpParser.AccessMemberContext amc)
            {
                if (node.PropertySymbol.IsStatic)
                    XsCheckStaticMemberAccess(node, amc, node.PropertySymbol);
                else
                    XsCheckInstanceMemberAccess(node, amc, node.PropertySymbol);
            }
            return;
        }
        private void XsVisitIndexerAccess(BoundIndexerAccess node)
        {
            if (node.Syntax?.XNode is XSharpParser.AccessMemberContext amc)
            {
                if (node.Indexer.IsStatic )
                    XsCheckStaticMemberAccess(node, amc, node.Indexer);
                else
                    XsCheckInstanceMemberAccess(node, amc, node.Indexer);
            }
            return;
        }
        private void XsVisitCall(BoundCall node)
        {
            if (node.Syntax?.XNode is XSharpParser.MethodCallContext mc && mc.Expr is XSharpParser.AccessMemberContext amc)
            {
                if (node.Method.IsStatic)
                    XsCheckStaticMemberAccess(node, amc, node.Method);
                else
                    XsCheckInstanceMemberAccess(node, amc, node.Method);
                return;
            }
        }
        public void XsVisitEventAssignmentOperator(BoundEventAssignmentOperator node)
        {
            if (node.Syntax?.XNode is XSharpParser.AssignmentExpressionContext aec && aec.Left is XSharpParser.AccessMemberContext amc)
            {
                if (node.Event.IsStatic)
                    XsCheckStaticMemberAccess(node, amc, node.Event);
                else
                    XsCheckInstanceMemberAccess(node, amc, node.Event);
            }
            return;
        }
        public void XsVisitEventAccess(BoundEventAccess node)
        {
            if (node.Syntax?.XNode is XSharpParser.AccessMemberContext amc)
            {
                if (node.EventSymbol.IsStatic )
                    XsCheckStaticMemberAccess(node, amc, node.EventSymbol);
                else
                    XsCheckInstanceMemberAccess(node, amc, node.EventSymbol);
            }
            return;
        }
        private void XsCheckConversion(BoundConversion node)
        {
            var syntax = node.Syntax;
            var sourceType = node.Operand.Type;
            var targetType = node.Type;
            if (syntax is BinaryExpressionSyntax binexp)
            {
                // determine original expression type
                // Roslyn will change a Int32 + UIn32
                // to an addition of 2 Int64 values
                // but we want the original UInt32 value for the warning message
                if (node.Operand is BoundBinaryOperator binop)
                {
                    var right = binop.Right;
                    if (right is BoundConversion bcv)
                    {
                        right = bcv.Operand;
                    }
                    sourceType = right.Type;
                }
            }
            if (sourceType is null || targetType is null)
                return;
            if (sourceType.IsIntegralType() && targetType.IsPointerType())
            {
                VOCheckIntegerToPointer(node);
            }
            if (syntax == null || syntax.XIsExplicitTypeCastInCode || node.WasCompilerGenerated
                || node.ConstantValueOpt != null)
                return;
            GenerateWarning(sourceType, targetType, node);
        }
        private void VOCheckIntegerToPointer(BoundConversion node)
        {
            var srcType = node.Operand.Type;
            var platform = _compilation.Options.Platform;
            if (srcType.IsPointerType())
                return;
            switch (platform)
            {
                case Platform.X86:
                    if (srcType.SpecialType.SizeInBytes() > 4)
                    {
                        Error(ErrorCode.ERR_CantCastPtrInPlatform, node, srcType.ToDisplayString(), "x86");
                    }
                    else if (node.Syntax.XIsExplicitTypeCastInCode && srcType.SpecialType.SizeInBytes() != 4)
                    {
                        Error(ErrorCode.ERR_CantCastPtrInPlatform, node, srcType.ToDisplayString(), "x86");
                    }
                    break;
                case Platform.X64:
                    if (srcType.SpecialType.SizeInBytes() > 8)
                    {
                        Error(ErrorCode.ERR_CantCastPtrInPlatform, node, srcType.ToDisplayString(), "x64");
                    }
                    else if (node.Syntax.XIsExplicitTypeCastInCode && srcType.SpecialType.SizeInBytes() != 8)
                    {
                        Error(ErrorCode.ERR_CantCastPtrInPlatform, node, srcType.ToDisplayString(), "x86");
                    }
                    break;
                default:
                    string name = platform.ToString();
                    if (platform == Platform.AnyCpu32BitPreferred)
                        name = "AnyCpu";
                    Error(ErrorCode.ERR_CantCastPtrInAnyCpu, node, srcType.ToDisplayString(), name);
                    break;
            }
        }
    }
}
