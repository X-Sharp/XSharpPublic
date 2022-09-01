//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

using System.Collections.Immutable;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Microsoft.CodeAnalysis.CSharp
{
    // Extension methods responsible for looking up 'our' types

    internal static class TypeExtensions
    {

        private class ConstantWalker : BoundTreeWalker
        {
            private bool hasNumericConstant = false;
            private bool hasStringConstant = false;
            internal bool HasNumericConstant => hasNumericConstant;
            internal bool HasStringConstant => hasStringConstant;
            public ConstantWalker() : base()
            {
            }

            public override BoundNode Visit(BoundNode node)
            {
                if (node is BoundCall || node is BoundDynamicInvocation || node is not BoundExpression)
                    return null;
                if (node is BoundExpression expr && expr.ConstantValue != null && !expr.ConstantValue.IsBad)
                {
                    if (expr.ConstantValue.SpecialType == SpecialType.System_String)
                        hasStringConstant = true;
                    if (expr.ConstantValue.SpecialType.IsNumericType())
                        hasNumericConstant = true;
                    return node;
                }
                return base.Visit(node);
            }
            protected override BoundExpression VisitExpressionWithoutStackGuard(BoundExpression expr)
            {
                return (BoundExpression)base.Visit(expr);
            }
        }

        /// <summary>
        /// Walk a an expression tree to detect if a constant element is involved.
        /// NOTE: this may not be complete !
        /// </summary>
        /// <param name="node"></param>
        /// <returns></returns>
        internal static bool
            IsExpressionWithNumericConstant(this BoundNode node)
        {
            switch (node)
            {
                case BoundLiteral:
                    return false;
                case BoundUnaryOperator unary:
                    if (unary.Operand is BoundLiteral)
                        return false;
                    break;
                case BoundExpression:
                    break;
                default:
                    return false;
            }
            var walker = new ConstantWalker();
            walker.Visit(node);
            return walker.HasNumericConstant;
        }


        private class DisableWarningsWalker : BoundTreeWalker
        {
            public DisableWarningsWalker() : base()
            {
            }

            public override BoundNode Visit(BoundNode node)
            {
                if (node?.Syntax != null)
                    node.Syntax.XNoWarning = true;
                return base.Visit(node);
            }
            protected override BoundExpression VisitExpressionWithoutStackGuard(BoundExpression expr)
            {
                return (BoundExpression)base.Visit(expr);
            }
        }

        internal static void DisableWarnings(this BoundNode node)
        {
            var walker = new DisableWarningsWalker();
            walker.Visit(node);
            return;
        }

        internal static bool IsXsCompilerGenerated(this Symbol symbol)
        {
            if (symbol.Kind == SymbolKind.Local || symbol.Kind == SymbolKind.Parameter)
            {
                if (symbol.Name != null && symbol.Name.StartsWith("Xs$") )
                {
                    return true;
                }
            }
            if (symbol is SourceLocalSymbol local)
            {
                var syntax = local.GetDeclaratorSyntax();
                var vardecl = syntax.Parent as VariableDeclarationSyntax;
                if (vardecl != null && vardecl.XGenerated)
                    return true;
            }
            return false;
        }

        internal static bool IsValidVOUsualType(this TypeSymbol type)
        {
            switch (type.SpecialType)
            {
                case SpecialType.System_Int32:
                case SpecialType.System_Int64:
                case SpecialType.System_Boolean:
                case SpecialType.System_String:
                case SpecialType.System_DateTime:
                case SpecialType.System_Object:
                case SpecialType.System_Double:
                case SpecialType.System_Single:
                case SpecialType.System_Byte:
                case SpecialType.System_IntPtr:
                case SpecialType.System_Decimal:
                case SpecialType.System_Int16:
                case SpecialType.System_UInt16:
                case SpecialType.System_UInt32:
                case SpecialType.System_UInt64:
                case SpecialType.System_SByte:
                    return true;
            }
            return type.IsOurType();
        }
        internal static TypeSymbol LargestOperand(this BoundBinaryOperator binop, Compilation compilation)
        {
            if (binop.OperatorKind.IsComparison() || !binop.Type.IsIntegralType())
                return binop.Type;

            var left = binop.Left;
            var right = binop.Right;
            if (left is BoundConversion lconv)
            {
                left = lconv.XOperand;
            }
            if (right is BoundConversion rconv)
            {
                right = rconv.XOperand;
            }
            var leftType = left.Type;
            var rightType = right.Type;
            if (left is BoundBinaryOperator binopl)
                leftType = binopl.LargestOperand(compilation);
            if (right is BoundBinaryOperator binopr)
                rightType = binopr.LargestOperand(compilation);
            if (left is BoundLiteral || left is BoundFieldAccess)
                leftType = left.ConstantType(compilation);
            if (right is BoundLiteral || right is BoundFieldAccess)
                rightType = right.ConstantType(compilation);

            var leftSize = leftType.SpecialType.SizeInBytes();
            var rightSize = rightType.SpecialType.SizeInBytes();
            if (leftSize >= rightSize)
                return leftType;
            return rightType;
        }

        internal static TypeSymbol ConstantType(this BoundExpression expression, Compilation compilation)
        {
            var type = expression.Type;
            if (expression.ConstantValue == null)
                return type;
            if (!expression.ConstantValue.IsIntegral)
                return type;
            var stype = type.SpecialType;
            if (type.SpecialType.IsSignedIntegralType())
            {
                var value = expression.ConstantValue.Int64Value;
                if (value == 0)
                {
                    stype = SpecialType.System_Byte;
                }
                else if (value < 0)
                {
                    if (value >= sbyte.MinValue)
                        stype = SpecialType.System_SByte;
                    else if (value >= short.MinValue)
                        stype = SpecialType.System_Int16;
                    else if (value >= int.MinValue)
                        stype = SpecialType.System_Int32;
                    else
                        stype = SpecialType.System_Int64;
                }
                // > 0
                else
                {
                    // prefer unsigned types when < 32 bits
                    if (value <= byte.MaxValue)
                        stype = SpecialType.System_Byte;
                    else if (value <= ushort.MaxValue)
                        stype = SpecialType.System_UInt16;
                    else if (value <= int.MaxValue)
                        stype = SpecialType.System_Int32;
                    else if (value <= uint.MaxValue)
                        stype = SpecialType.System_UInt32;
                    else
                        stype = SpecialType.System_Int64;
                }
            }
            else
            {
                // UnSigned
                var uvalue = expression.ConstantValue.UInt64Value;
                if (uvalue <= (ulong)sbyte.MaxValue)
                    stype = SpecialType.System_SByte;
                else if (uvalue <= (ulong)byte.MaxValue)
                    stype = SpecialType.System_Byte;
                else if (uvalue <= (ulong)short.MaxValue)
                    stype = SpecialType.System_Int16;
                else if (uvalue <= (ulong)ushort.MaxValue)
                    stype = SpecialType.System_UInt16;
                else if (uvalue <= (ulong)int.MaxValue)
                    stype = SpecialType.System_Int32;
                else if (uvalue <= (ulong)uint.MaxValue)
                    stype = SpecialType.System_UInt32;
                else if (uvalue <= (ulong)long.MaxValue)
                    stype = SpecialType.System_Int64;
                else
                    stype = SpecialType.System_UInt64;
            }
            var sym = ((CSharpCompilation)compilation).GetSpecialType(stype);
            return sym;
        }
    }
}
