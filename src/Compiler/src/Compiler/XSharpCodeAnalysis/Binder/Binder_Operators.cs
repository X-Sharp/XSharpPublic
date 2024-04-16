//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.PooledObjects;
namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class Binder
    {
        private enum VOOperatorType
        {
            None = 0,
            CompareString,
            SingleEqualsString,
            SingleEqualsUsual,
            NotEqualsUsual,
            SubtractString,
            UsualOther,
            Bitwise,
            PSZCompare,
            SymbolCompare,
            LogicCompare,
            DateCompare,
        }

        private bool XsHasImplicitCast(BoundExpression expression, TypeSymbol targetType, DiagnosticBag diagnostics)
        {
            var sourceType = expression.Type;
            var syntax = expression.Syntax;
            if (Equals(sourceType, targetType))
                return true;

            // do not silently cast one enum to another !
            if (sourceType.IsEnumType() || targetType.IsEnumType())
                return false;

            // do not throw an warnings for IIF() expressions with types that are too big
            // for the LHS of the assignment
            if (syntax.Kind() == SyntaxKind.ConditionalExpression && targetType.IsIntegralType() && sourceType.IsIntegralType())
            {
                return true;
            }
            if (Conversions.XsIsImplicitBinaryOperator(expression, targetType, this))
            {
                return true;
            }

            if (expression is BoundBinaryOperator binop)
            {
                sourceType = binop.LargestOperand(Compilation);
            }
            if (Equals(sourceType, targetType))
            {
                return true;
            }
            if (targetType.IsIntegralType() && sourceType.IsIntegralType())
            {
                // implicit conversions are allowed when /vo4 is active
                return Compilation.Options.HasOption(CompilerOption.Vo4, syntax);
            }
            if (sourceType.IsObjectType() && targetType.IsXNumericType())
            {
                return Compilation.Options.HasOption(CompilerOption.ImplicitCastsAndConversions, syntax);
            }
            return false;
        }

        private BoundExpression BindVOCompareString(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
            BoundExpression left, BoundExpression right)
        {
            MethodSymbol opMeth;
            TypeSymbol type;
            BoundCall opCall = null;
            var stringType = Compilation.GetSpecialType(SpecialType.System_String);
            if (left.Type.GetSpecialTypeSafe() != SpecialType.System_String)
            {
                left = CreateConversion(left, stringType, diagnostics);
            }
            if (right.Type.GetSpecialTypeSafe() != SpecialType.System_String)
            {
                right = CreateConversion(right, stringType, diagnostics);
            }

            if (Compilation.Options.HasRuntime && this.Compilation.Options.HasOption(CompilerOption.StringComparisons, node))
            {
                // VO Style String Comparison
                type = Compilation.RuntimeFunctionsType();
                string methodName = ReservedNames.StringCompare;
                var symbols = Binder.GetCandidateMembers(type, methodName, LookupOptions.MustNotBeInstance, this);
                if (symbols.Length == 1)
                {
                    opMeth = (MethodSymbol)symbols[0];
                    opCall = BoundCall.Synthesized(node, null, opMeth, left, right);
                }
                else
                {
                    Error(diagnostics, ErrorCode.ERR_FeatureNotAvailableInDialect, node, "String Compare method " + type.Name + "." + methodName, Compilation.Options.Dialect.ToString());
                }
            }
            else
            {
                // Standard String Comparison using .Net String Compare
                TryGetSpecialTypeMember(Compilation, SpecialMember.System_String__Compare, node, diagnostics, out opMeth);
                opCall = BoundCall.Synthesized(node, null, opMeth, left, right);
                opCall.WasCompilerGenerated = true;
            }
            var op = BindSimpleBinaryOperator(node, diagnostics, opCall,
                new BoundLiteral(node, ConstantValue.Create(0), GetSpecialType(SpecialType.System_Int32, diagnostics, node)));
            op.WasCompilerGenerated = true;
            var res = CreateConversion(op, Compilation.GetSpecialType(SpecialType.System_Boolean), diagnostics);
            res.WasCompilerGenerated = true;
            return res;
        }


        private BoundExpression BindVOSingleEqualsString(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
            BoundExpression left, BoundExpression right)
        {
            MethodSymbol opMeth = null;
            BoundExpression opCall = null;
            var type = Compilation.RuntimeFunctionsType();
            var methodName = ReservedNames.StringEquals;
            var symbols = Binder.GetCandidateMembers(type, methodName, LookupOptions.MustNotBeInstance, this);
            if (symbols.Length == 1)
            {
                opMeth = (MethodSymbol)symbols[0];
                var stringType = Compilation.GetSpecialType(SpecialType.System_String);
                if (!TypeSymbol.Equals(right.Type, stringType))
                {
                    right = CreateConversion(right, stringType, diagnostics);
                }
                if (!TypeSymbol.Equals(left.Type, stringType))
                {
                    left = CreateConversion(left, stringType, diagnostics);
                }
                opCall = BoundCall.Synthesized(node, null, opMeth, left, right);
                opCall.WasCompilerGenerated = true;
                opCall = CreateConversion(opCall, Compilation.GetSpecialType(SpecialType.System_Boolean), diagnostics);
                opCall.WasCompilerGenerated = true;
            }
            else
            {
                Error(diagnostics, ErrorCode.ERR_FeatureNotAvailableInDialect, node, "String Equals (=) method " + type.Name + "." + methodName, Compilation.Options.Dialect.ToString());
            }
            return opCall;
        }

        private BoundExpression BindVOUsualOther(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
            BoundExpression left, BoundExpression right)
        {
            var usualType = Compilation.UsualType();
            BoundExpression opCall = null;
            ImmutableArray<Symbol> symbols;
            if (node.OperatorToken.Kind() == SyntaxKind.MinusToken)
                symbols = Binder.GetCandidateMembers(usualType, WellKnownMemberNames.SubtractionOperatorName, LookupOptions.MustNotBeInstance, this);
            else
                symbols = Binder.GetCandidateMembers(usualType, WellKnownMemberNames.AdditionOperatorName, LookupOptions.MustNotBeInstance, this);
            if (symbols.Length == 1)
            {
                MethodSymbol opMeth = (MethodSymbol)symbols[0];
                if (right.Type.IsNotUsualType())
                {
                    right = CreateConversion(right, usualType, diagnostics);
                }
                if (left.Type.IsNotUsualType())
                {
                    left = CreateConversion(left, usualType, diagnostics);
                }
                opCall = BoundCall.Synthesized(node, null, opMeth, left, right);
            }
            else
            {
                Error(diagnostics, ErrorCode.ERR_FeatureNotAvailableInDialect, node, "Usual - Date operators", Compilation.Options.Dialect.ToString());
            }
            return opCall;

        }

        private bool IsNullNode(BoundExpression node)
        {
            if (node.Syntax?.XNode != null)
            {
                var xnode = node.Syntax.XNode as XSharpParser.LiteralExpressionContext;
                if (xnode == null && node.Syntax.XNode is XSharpParser.PrimaryExpressionContext)
                {
                    var pexp = node.Syntax.XNode as XSharpParser.PrimaryExpressionContext;
                    xnode = pexp.Expr as XSharpParser.LiteralExpressionContext;
                }
                if (xnode != null)
                {
                    switch (xnode.Literal.Token.Type)
                    {
                        case XSharpParser.NULL:
                        case XSharpParser.NULL_PTR:
                        case XSharpParser.NULL_PSZ:
                            return true;
                        case XSharpParser.INT_CONST:
                            return Convert.ToInt64(xnode.Literal.Token.Text) == 0;
                    }
                }
            }
            return false;
        }
        private BoundExpression BindVOPszCompare(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
                ref BoundExpression left, ref BoundExpression right)
        {
            var pszType = Compilation.PszType();
            if (right.Type.IsNotPszType())
            {
                if (IsNullNode(right))
                {
                    right = PszFromNull(right);
                }
                else
                {
                    right = CreateConversion(right, pszType, diagnostics);
                }
            }
            if (left.Type.IsNotPszType())
            {
                if (IsNullNode(left))
                {
                    left = PszFromNull(left);
                }
                else
                {
                    left = CreateConversion(left, pszType, diagnostics);
                }
            }
            return null;
        }
        private BoundExpression BindVOSymbolCompare(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
                ref BoundExpression left, ref BoundExpression right)
        {
            var symType = Compilation.SymbolType();
            if (left.Type.IsObjectType() || right.Type.IsObjectType())
            {
                if (right.Type.IsSymbolType())
                {
                    right = CreateConversion(right, Compilation.GetSpecialType(SpecialType.System_Object), diagnostics);
                }
                if (left.Type.IsSymbolType())
                {
                    left = CreateConversion(left, Compilation.GetSpecialType(SpecialType.System_Object), diagnostics);
                }
                return null;
            }
            if (right.Type.IsNotSymbolType())
            {
                right = CreateConversion(right, symType, diagnostics);
            }
            if (left.Type.IsNotSymbolType())
            {
                left = CreateConversion(left, symType, diagnostics);
            }
            return null;
        }

        private BoundExpression BindVODateCompare(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
        ref BoundExpression left, ref BoundExpression right)
        {
            var objType = Compilation.GetSpecialType(SpecialType.System_Object);
            var lhs = left.Type;
            var rhs = right.Type;
            if (lhs is null || lhs.IsObjectType())
            {
                Error(diagnostics, ErrorCode.ERR_BadBinaryOps, node, node.OperatorToken.Text, left.Display, right.Display);
                right = new BoundLiteral(right.Syntax, ConstantValue.Null, objType);
            }
            if (rhs is null || rhs.IsObjectType())
            {
                Error(diagnostics, ErrorCode.ERR_BadBinaryOps, node, node.OperatorToken.Text, left.Display, right.Display);
                left = new BoundLiteral(left.Syntax, ConstantValue.Null, objType);
            }
            return null;
        }


        private BoundExpression BindVOLogicCompare(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
                ref BoundExpression left, ref BoundExpression right)
        {
            // Convert logic compare to integer compare where TRUE = 1 and FALSE = 0
            var intType = Compilation.GetSpecialType(SpecialType.System_Int32);
            var lit0 = new BoundLiteral(node, ConstantValue.Create(0), intType);
            var lit1 = new BoundLiteral(node, ConstantValue.Create(1), intType);
            left = new BoundConditionalOperator(node, false, left, lit1, lit0, constantValueOpt: default, wasTargetTyped: false, type: intType, naturalTypeOpt: default);
            right = new BoundConditionalOperator(node, false, right, lit1, lit0, constantValueOpt: default, wasTargetTyped: false, type: intType, naturalTypeOpt: default);
            return null;
        }

        private BoundExpression BindVOSingleEqualsUsual(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
             BoundExpression left, BoundExpression right)
        {
            MethodSymbol opMeth = null;
            BoundExpression opCall = null;
            var usualType = Compilation.UsualType();
            var methodName = ReservedNames.InExactEquals;
            var symbols = Binder.GetCandidateMembers(usualType, methodName, LookupOptions.MustNotBeInstance, this);
            if (symbols.Length == 2)
            {
                // There should be 2 overloads in VulcanRTFuncs:
                // public static bool __InexactEquals(__Usual ul, string uR)
                // public static bool __InexactEquals(__Usual uL, __Usual uR)
                // Switch to overload with string when RHS = STRING
                opMeth = (MethodSymbol)symbols[0];
                if (right.Type.GetSpecialTypeSafe() == SpecialType.System_String)
                {
                    if (!TypeSymbol.Equals(right.Type, opMeth.Parameters[0].Type))
                        opMeth = (MethodSymbol)symbols[1];
                }
                else
                {
                    // When RHS != USUAL then switch
                    if (opMeth.Parameters[0].Type.IsNotUsualType())
                        opMeth = (MethodSymbol)symbols[1];
                    if (right.Type.IsNotUsualType())
                    {
                        right = CreateConversion(right, usualType, diagnostics);
                    }
                }
                if (left.Type.IsNotUsualType())
                {
                    left = CreateConversion(left, usualType, diagnostics);
                }

                opCall = BoundCall.Synthesized(node, null, opMeth, left, right);
            }
            else
            {
                Error(diagnostics, ErrorCode.ERR_FeatureNotAvailableInDialect, node, "Usual Equals (=) method " + usualType.Name + "." + methodName, Compilation.Options.Dialect.ToString());
            }
            return opCall;
        }

        private BoundExpression BindVONotEqualsUsual(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
            BoundExpression left, BoundExpression right)
        {
            MethodSymbol opMeth = null;
            BoundExpression opCall = null;
            var usualType = Compilation.UsualType();
            var methodName = ReservedNames.InExactNotEquals;
            var symbols = Binder.GetCandidateMembers(usualType, methodName, LookupOptions.MustNotBeInstance, this);
            if (symbols.Length == 2)
            {
                // There should be 2 overloads in VulcanRTFuncs:
                // public static bool __InexactNotEquals(__Usual ul, string uR)
                // public static bool __InexactNotEquals(__Usual uL, __Usual uR)
                // Switch to overload with string when RHS = STRING
                opMeth = (MethodSymbol)symbols[0];
                if (right.Type.GetSpecialTypeSafe() == SpecialType.System_String)
                {
                    if (!TypeSymbol.Equals(right.Type, opMeth.Parameters[0].Type))
                        opMeth = (MethodSymbol)symbols[1];
                }
                else
                {
                    // When RHS != USUAL then switch
                    if (opMeth.Parameters[0].Type.IsNotUsualType())
                        opMeth = (MethodSymbol)symbols[1];
                    if (right.Type.IsNotUsualType())
                    {
                        right = CreateConversion(right, usualType, diagnostics);
                    }
                }
                if (left.Type.IsNotUsualType())
                {
                    left = CreateConversion(left, usualType, diagnostics);
                }
                opCall = BoundCall.Synthesized(node, null, opMeth, left, right);
            }
            else
            {
                Error(diagnostics, ErrorCode.ERR_FeatureNotAvailableInDialect, node, "Usual NotEquals (!=) method " + usualType.Name + "." + methodName, Compilation.Options.Dialect.ToString());
            }
            return opCall;
        }

        private BoundExpression BindVOSubtractString(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
            BoundExpression left, BoundExpression right)
        {
            MethodSymbol opMeth = null;
            BoundExpression opCall = null;
            var type = Compilation.CompilerServicesType();
            var methodName = ReservedNames.StringSubtract;
            var symbols = Binder.GetCandidateMembers(type, methodName, LookupOptions.MustNotBeInstance, this);
            if (symbols.Length == 1)
            {
                opMeth = (MethodSymbol)symbols[0];
                var stringType = Compilation.GetSpecialType(SpecialType.System_String);
                if (left.Type.GetSpecialTypeSafe() != SpecialType.System_String)
                {
                    left = CreateConversion(left, stringType, diagnostics);
                }
                if (right.Type.GetSpecialTypeSafe() != SpecialType.System_String)
                {
                    right = CreateConversion(right, stringType, diagnostics);
                }
                opCall = BoundCall.Synthesized(node, null, opMeth, left, right);
            }
            else
            {
                Error(diagnostics, ErrorCode.ERR_FeatureNotAvailableInDialect, node, "String Subtract method " + type.Name + "." + methodName, Compilation.Options.Dialect.ToString());
            }
            return opCall;
        }

        private BoundExpression BindVOBinaryOperator(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
            ref BoundExpression left, ref BoundExpression right, VOOperatorType opType)
        {
            Debug.Assert(opType != VOOperatorType.None);
            left = BindToNaturalType(left, diagnostics, reportNoTargetType: false);
            right = BindToNaturalType(right, diagnostics, reportNoTargetType: false);
            switch (opType)
            {
                case VOOperatorType.SingleEqualsString:
                    return BindVOSingleEqualsString(node, diagnostics, left, right);
                case VOOperatorType.SingleEqualsUsual:
                    return BindVOSingleEqualsUsual(node, diagnostics, left, right);
                case VOOperatorType.NotEqualsUsual:
                    return BindVONotEqualsUsual(node, diagnostics, left, right);
                case VOOperatorType.SubtractString:
                    return BindVOSubtractString(node, diagnostics, left, right);
                case VOOperatorType.CompareString:
                    return BindVOCompareString(node, diagnostics, left, right);
                case VOOperatorType.UsualOther:
                    return BindVOUsualOther(node, diagnostics, left, right);
                case VOOperatorType.PSZCompare:
                    return BindVOPszCompare(node, diagnostics, ref left, ref right);
                case VOOperatorType.SymbolCompare:
                    return BindVOSymbolCompare(node, diagnostics, ref left, ref right);
                case VOOperatorType.LogicCompare:
                    return BindVOLogicCompare(node, diagnostics, ref left, ref right);
                case VOOperatorType.DateCompare:
                    return BindVODateCompare(node, diagnostics, ref left, ref right);
                case VOOperatorType.Bitwise:
                    break;

            }
            return null;
        }

        private VOOperatorType NeedsVOOperator(BinaryExpressionSyntax node, ref BoundExpression left,
            ref BoundExpression right, DiagnosticBag diagnostics)
        {
            // Check if a special XSharp binary operation is needed. This is needed when:
            //
            // Comparison  (>, >=, <, <=) operator and this.Compilation.Options.VOStringComparisons = true
            // Single Equals Operator and LHS and RHS are string                    // STRING = STRING
            // Single Equals Operator and LHS or RHS is USUAL                       // <any> = USUAL or USUAL = <any>
            // Not equals operator and LHS = USUAL and RHS is USUAL or STRING       // USUAL != STRING or USUAL != USUAL
            // Minus Operator and LHS and RHS is STRING                             // STRING - STRING
            // Minus Operator and LHS or  RHS is STRING and other side is USUAL     // STRING - USUAL or USUAL - STRING
            //
            VOOperatorType opType = VOOperatorType.None;
            XSharpParser.BinaryExpressionContext xnode;
            if (node.XNode is XSharpParser.CodeblockCodeContext)
                xnode = ((XSharpParser.CodeblockCodeContext)node.XNode).Expr as XSharpParser.BinaryExpressionContext;
            else
                xnode = node.XNode as XSharpParser.BinaryExpressionContext;

            TypeSymbol leftType = left.Type;
            TypeSymbol rightType = right.Type;
            var leftString = leftType.GetSpecialTypeSafe() == SpecialType.System_String;
            var rightString = rightType.GetSpecialTypeSafe() == SpecialType.System_String;
            // Convert __WinDate to __Date to make sure that the Date rules are applied
            // And do the same for __WinBool and System.Boolean
            if (Compilation.Options.HasRuntime)
            {
                if (leftType.IsWinDateType())
                {
                    left = CreateConversion(left, Compilation.DateType(), diagnostics);
                    leftType = left.Type;
                }
                else if (leftType.IsWinBoolType())
                {
                    left = CreateConversion(left, Compilation.GetSpecialType(SpecialType.System_Boolean), diagnostics);
                    leftType = left.Type;
                }

                if (rightType.IsWinDateType())
                {
                    right = CreateConversion(right, Compilation.DateType(), diagnostics);
                    rightType = right.Type;
                }
                else if (rightType.IsWinBoolType())
                {
                    right = CreateConversion(right, Compilation.GetSpecialType(SpecialType.System_Boolean), diagnostics);
                    rightType = right.Type;
                }
                else if (leftType.IsArrayType())
                {
                    if (rightType.IsArrayBaseType() && left.Kind == BoundKind.DefaultExpression)
                    {
                        left = new BoundLiteral(left.Syntax, ConstantValue.Null, null);
                        return opType;
                    }
                }
                else if (rightType.IsArrayType())
                {
                    if (leftType.IsArrayBaseType() && right.Kind == BoundKind.DefaultExpression)
                    {
                        right = new BoundLiteral(right.Syntax, ConstantValue.Null, null);
                        return opType;
                    }
                }
            }

            if (Compilation.Options.HasRuntime && xnode != null)
            {
                var leftObject = false;
                var rightObject = false;
                var leftUsual = false;
                var rightUsual = false;
                var leftSym = false;
                var rightSym = false;
                var leftPSZ = false;
                var rightPSZ = false;
                if (!leftString && leftType is { })
                {
                    if (leftType.IsObjectType())
                    {
                        leftObject = true;
                    }
                    else if (leftType.IsUsualType())
                    {
                        leftUsual = true;
                    }
                    else if (leftType.IsSymbolType())
                    {
                        leftSym = true;
                    }
                    else
                    {
                        leftPSZ = leftType.IsPszType();
                    }
                }
                if (!rightString && rightType is { })
                {
                    if (rightType.IsObjectType())
                    {
                        rightObject = true;
                    }
                    else if (rightType.IsUsualType())
                    {
                        rightUsual = true;
                    }
                    else if (rightType.IsSymbolType())
                    {
                        rightSym = true;
                    }
                    else
                    {
                        rightPSZ = rightType.IsPszType();
                    }
                }

                switch (xnode.Op.Type)
                {
                    case XSharpParser.EQ:
                        if (leftUsual || rightUsual)
                        {
                            opType = VOOperatorType.SingleEqualsUsual;
                            break;
                        }
                        if (leftString && rightString)
                        {
                            opType = VOOperatorType.SingleEqualsString;
                            break;
                        }
                        if (leftPSZ || rightPSZ)
                        {
                            opType = VOOperatorType.PSZCompare;
                            break;
                        }
                        if ((leftUsual || rightUsual) && (leftSym || rightSym))
                        {
                            opType = VOOperatorType.SymbolCompare;
                            break;
                        }
                        break;
                    case XSharpParser.EEQ:
                        if (leftPSZ || rightPSZ)
                        {
                            opType = VOOperatorType.PSZCompare;
                            break;
                        }
                        if ((leftUsual || rightUsual) && (leftSym || rightSym))
                        {
                            opType = VOOperatorType.SymbolCompare;
                            break;
                        }
                        if ((leftObject || rightObject) && (leftSym || rightSym))
                        {
                            opType = VOOperatorType.SymbolCompare;
                            break;
                        }

                        break;
                    case XSharpParser.NEQ:
                    case XSharpParser.NEQ2:
                        if (leftUsual || rightUsual)
                        {
                            opType = VOOperatorType.NotEqualsUsual;
                            break;
                        }
                        if (leftPSZ || rightPSZ)
                        {
                            opType = VOOperatorType.PSZCompare;
                            break;
                        }
                        break;
                    case XSharpParser.GT:
                    case XSharpParser.GTE:
                    case XSharpParser.LT:
                    case XSharpParser.LTE:
                        if (leftUsual || rightUsual)
                        {
                            // when LHS or RHS == USUAL then do not compare with CompareString
                            // but let the operator methods inside USUAL handle it.
                            opType = VOOperatorType.None;
                            break;
                        }
                        if (leftString && rightString)
                        {
                            // Convert to String.Compare or __StringCompare. Decide later
                            opType = VOOperatorType.CompareString;
                            break;
                        }
                        if (leftPSZ || rightPSZ)
                        {
                            opType = VOOperatorType.PSZCompare;
                            break;
                        }
                        if (leftType.GetSpecialTypeSafe() == SpecialType.System_Boolean &&
                            rightType.GetSpecialTypeSafe() == SpecialType.System_Boolean)
                        {
                            opType = VOOperatorType.LogicCompare;
                            break;
                        }
                        break;
                    case XSharpParser.MINUS:
                    case XSharpParser.PLUS:
                        //case XSharpParser.MULT:
                        //case XSharpParser.DIV:
                        if (xnode.Op.Type == XSharpParser.MINUS)
                        {
                            // String Subtract
                            // LHS    - RHS
                            // STRING - STRING
                            // STRING -- USUAL
                            // USUAL  - STRING
                            if (leftString && (rightString || rightUsual))
                            {
                                opType = VOOperatorType.SubtractString;
                                break;
                            }
                            if (leftUsual && rightString)
                            {
                                opType = VOOperatorType.SubtractString;
                                break;
                            }
                        }
                        if (opType == VOOperatorType.None)
                        {
                            // Add or Subtract USUAL with other type
                            // LHS   - RHS
                            // Usual - Date
                            // Date  - Usual
                            // Usual - Float
                            // Float - Usual
                            if (leftUsual && (rightType.IsDateType() || rightType.IsFloatType()))
                            {
                                opType = VOOperatorType.UsualOther;
                                break;
                            }
                            if (rightUsual && (leftType.IsDateType() || leftType.IsFloatType()))
                            {
                                opType = VOOperatorType.UsualOther;
                                break;
                            }
                        }
                        break;
                    default:
                        break;
                }
            }
            if (opType == VOOperatorType.None)
            {
                switch (node.Kind())
                {
                    case SyntaxKind.EqualsExpression:
                        if (leftType.IsDateType() || rightType.IsDateType())
                        {
                            if (leftType is null || rightType is null || leftType.IsObjectType() || rightType.IsObjectType())
                            {
                                opType = VOOperatorType.DateCompare;
                            }
                        }
                        break;
                    case SyntaxKind.GreaterThanExpression:
                    case SyntaxKind.GreaterThanOrEqualExpression:
                    case SyntaxKind.LessThanExpression:
                    case SyntaxKind.LessThanOrEqualExpression:
                        if (leftString && rightString)
                        {
                            // Make to String.Compare or __StringCompare. Decide later
                            opType = VOOperatorType.CompareString;
                        }
                        break;
                    case SyntaxKind.RightShiftExpression:
                    case SyntaxKind.LeftShiftExpression:
                    case SyntaxKind.RightShiftAssignmentExpression:
                    case SyntaxKind.LeftShiftAssignmentExpression:
                    case SyntaxKind.BitwiseAndExpression:
                    case SyntaxKind.BitwiseOrExpression:
                    case SyntaxKind.BitwiseNotExpression:
                        opType = VOOperatorType.Bitwise;
                        break;
                    case SyntaxKind.AddExpression:
                    case SyntaxKind.SubtractExpression:
                    case SyntaxKind.AddAssignmentExpression:
                    case SyntaxKind.SubtractAssignmentExpression:
                    case SyntaxKind.DivideAssignmentExpression:
                    case SyntaxKind.DivideExpression:
                    case SyntaxKind.MultiplyAssignmentExpression:
                    case SyntaxKind.MultiplyExpression:
                        if (leftType is { } && rightType is { })
                        {
                            if (!TypeSymbol.Equals(leftType, rightType) && leftType.IsIntegralType() && rightType.IsIntegralType())
                            {
                                if (xnode != null && xnode.Parent is XSharpParser.VodefineContext)
                                {
                                    // convert RHS to type of LHS inside a VODefine
                                    right = new BoundConversion(right.Syntax, right,
                                        Conversion.ImplicitNumeric,
                                        false,
                                        false,
                                        conversionGroupOpt: null,
                                        constantValueOpt: right.ConstantValue,
                                        type: leftType)
                                    { WasCompilerGenerated = true };
                                }

                            }
                        }
                        break;
                }
            }
            return opType;
        }
        private void AdjustVOUsualLogicOperands(BinaryExpressionSyntax node, ref BoundExpression left, ref BoundExpression right, DiagnosticBag diagnostics)
        {
            if (!Compilation.Options.HasRuntime)
                return;
            XSharpParser.BinaryExpressionContext xnode = null;
            if (node.XNode is XSharpParser.BinaryExpressionContext)
            {
                xnode = node.XNode as XSharpParser.BinaryExpressionContext;
            }
            else if (node.XNode is XSharpParser.CodeblockCodeContext)
            {
                var cbc = node.XNode as XSharpParser.CodeblockCodeContext;
                if (cbc.Expr is XSharpParser.BinaryExpressionContext)
                    xnode = cbc.Expr as XSharpParser.BinaryExpressionContext;
            }
            if (node.XNode is XSharpParser.ArrayElementContext actx)
            {
                xnode = actx.Expr as XSharpParser.BinaryExpressionContext;
            }
            if (xnode == null)  // this may happen for example for nodes generated in the transformation phase
                return;
            // check for Logic operations with Usual. If that is the case then add a conversion to the expression
            switch (xnode.Op.Type)
            {
                case XSharpParser.LOGIC_AND:
                case XSharpParser.LOGIC_OR:
                case XSharpParser.LOGIC_XOR:
                case XSharpParser.FOX_AND:
                case XSharpParser.FOX_OR:
                case XSharpParser.FOX_XOR:
                case XSharpParser.AND:
                case XSharpParser.OR:
                    var boolType = this.GetSpecialType(SpecialType.System_Boolean, diagnostics, node);
                    if (left.Type.IsUsualType())
                    {
                        left = CreateConversion(left, boolType, diagnostics);
                    }

                    if (right.Type.IsUsualType())
                    {
                        right = CreateConversion(right, boolType, diagnostics);
                    }
                    break;
            }
            return;
        }
        public BoundExpression RewriteIndexAccess(BoundExpression index, DiagnosticBag diagnostics)
        {
            var syntax = (CSharpSyntaxNode)index.Syntax;
            if (!index.HasAnyErrors && !this.Compilation.Options.HasOption(CompilerOption.ArrayZero, syntax) && !(index is BoundFromEndIndexExpression))
            {
                syntax.XGenerated = true;
                index = SubtractIndex(index, diagnostics);
            }
            return index;
        }

        public TypeSymbol VOGetType(BoundExpression expr)
        {
            if (expr.Kind == BoundKind.Literal)
            {
                var lit = expr as BoundLiteral;
                var xnode = lit?.Syntax.XNode as XSharpParser.PrimaryExpressionContext;
                var type = xnode?.Start?.Type;
                if (type == XSharpParser.INT_CONST || type == XSharpParser.REAL_CONST)
                {
                    string text = xnode.Start.Text;
                    if (text[0] == '$')
                    {
                        return Compilation.GetSpecialType(SpecialType.System_Decimal);
                    }
                    char last = text[text.Length - 1];
                    switch (last)
                    {
                        case 'L':
                            return Compilation.GetSpecialType(SpecialType.System_Int32);
                        case 'U':
                            return Compilation.GetSpecialType(SpecialType.System_UInt32);
                        case 'M':
                            return Compilation.GetSpecialType(SpecialType.System_Decimal);
                        case 'S':
                            return Compilation.GetSpecialType(SpecialType.System_Single);
                        case 'D':
                            return Compilation.GetSpecialType(SpecialType.System_Double);
                    }
                }
                return lit.ConstantType(Compilation);
            }
            else if (expr.Kind == BoundKind.UnaryOperator)
            {
                var unary = expr as BoundUnaryOperator;
                var type = VOGetType(unary.Operand);
                if (unary.OperatorKind.IsUnaryMinus())
                {
                    // see if we must change unsigned into signed
                    var sptype = type.GetSpecialTypeSafe();
                    type = sptype switch
                    {
                        SpecialType.System_Byte => Compilation.GetSpecialType(SpecialType.System_Int16),
                        SpecialType.System_UInt16 => Compilation.GetSpecialType(SpecialType.System_Int32),
                        SpecialType.System_UInt32 => Compilation.GetSpecialType(SpecialType.System_Int64),
                        _ => type
                    };
                }
                else if (unary.OperatorKind.HasFlag(UnaryOperatorKind.LogicalNegation))
                {
                    type = Compilation.GetSpecialType(SpecialType.System_Boolean);
                }
                return type;
            }
            else if (expr is BoundBinaryOperator binop)
            {
                return binop.LargestOperand(this.Compilation);
            }
            return expr.Type;
        }

        public BoundExpression BindXsAddressOfExpression(PrefixUnaryExpressionSyntax node, DiagnosticBag diagnostics)
        {
            // In vulcan when we have defined a structure like:
            // VOSTRUCT _WINWIN32_FIND_DATA
            //   MEMBER DIM cFileName[10] AS BYTE
            // This translates to
            // [StructLayout(LayoutKind.Sequential, Pack=8), VOStruct(10, 10)]
            //    public struct _WINWIN32_FIND_DATA
            //    {
            //        [FixedBuffer(typeof(byte), 10)]
            //        public <cFileName>e__FixedBuffer cFileName;
            //        // Nested Types
            //        [StructLayout(LayoutKind.Sequential, Size = 10), CompilerGenerated, UnsafeValueType]
            //        public struct $DIM_Array_cFileName
            //{
            //    public byte FixedElementField;
            //    }
            //}
            // The fixedBuffer is represented with a SourceFixedFieldSymbol
            // and the cFileName element is then accessed by reference:
            // cTemp := Psz2String(@pData:cFileName)
            // in C# we do not need the @ sign.
            // So when we detect that the Operand is a Field of the type SourceFixedFieldSymbol
            // we simply return the direct reference to the field without the AddressOf operator
            if (node.Operand is InvocationExpressionSyntax)
            {
                bool lAliasedExpression = false;
                if (node.Operand.XNode is XSharpParser.PrimaryExpressionContext pec &&
                   pec.Expr is XSharpParser.AliasedExpressionContext)
                {
                    lAliasedExpression = true;
                }
                if (lAliasedExpression)
                {
                    Error(diagnostics, ErrorCode.ERR_CannotTakeAddressOfAliasedExpression, node.Operand);
                }
                else
                {
                    Error(diagnostics, ErrorCode.ERR_CannotTakeAddressOfFunctionOrMethod, node.Operand);
                }
                return BadExpression(node);
            }

            var expr = this.BindExpression(node.Operand, diagnostics: diagnostics, invoked: false, indexed: false);

            if (expr.Kind == BoundKind.FieldAccess)
            {
                if (expr.ExpressionSymbol is SourceFixedFieldSymbol)
                {
                    return expr;
                }
                if (expr is BoundFieldAccess bfa)
                {
                    // Externally defined fixed Field. Could be a DIM field in a VoStruct class
                    if (bfa.FieldSymbol.IsFixedSizeBuffer)
                    {
                        var type = bfa.FieldSymbol.ContainingType;
                        if (type.IsVoStructOrUnion())
                        {
                            return expr;
                        }
                    }
                }

            }
            if (expr.Kind == BoundKind.ArrayAccess)
            {
                //translate @var[i]  to var[i]
                if (expr is BoundArrayAccess bac)
                {
                    var type = expr.Type;
                    if (bac.Expression.Kind == BoundKind.Local)
                    {
                        // when the local is declared with DIM then there is also a fixed pointer
                        // LOCAL DIM abTemp[100]   AS BYTE
                        // then we translate @abTemp[1] to abTemp$dim
                        var dimlocal = FindDimLocal((BoundLocal)bac.Expression);
                        if (dimlocal != null)
                        {
                            if (bac.Indices.Length == 1)
                            {
                                var index = bac.Indices[0];
                                var local = new BoundLocal(expr.Syntax, dimlocal, null, dimlocal.Type);
                                return new BoundBinaryOperator(bac.Syntax, BinaryOperatorKind.PointerAndIntAddition, local, index, null, null,
                                    LookupResultKind.Viable, default, dimlocal.Type);
                            }
                        }
                    }
                    if (bac.Expression.ExpressionSymbol is SourceLocalSymbol sls && type.IsVoStructOrUnion())
                    {
                        var syntaxes = sls.DeclaringSyntaxReferences;
                        if (syntaxes.Length > 0)
                        {
                            var syntaxNode = (CSharpSyntaxNode)syntaxes[0].GetSyntax();
                            if (syntaxNode.XNode is XSharpParser.LocalvarContext lvc && lvc.As.Type == XSharpParser.AS)
                            {
                                return expr;
                            }
                        }
                    }
                }
            }
            if (expr.Kind == BoundKind.Local)
            {
                // when the local is declared with DIM then there is also a fixed pointer
                // LOCAL DIM abTemp[100]   AS BYTE
                // then we translate @abTemp to abTemp$dim
                var local = (BoundLocal)expr;
                var dimlocal = FindDimLocal(local);
                if (dimlocal != null)
                {
                    return new BoundLocal(expr.Syntax, dimlocal, null, dimlocal.Type);
                }

                // only translate @name to @name[0] when not IsDecl
                if (expr.Type.IsArray())
                {
                    // convert from @expr to @expr[0]
                    var intType = Compilation.GetSpecialType(SpecialType.System_Int32);
                    var arrType = expr.Type as ArrayTypeSymbol;
                    var elType = arrType.ElementType;
                    var aindex = ArrayBuilder<BoundExpression>.GetInstance();
                    for (var i = 0; i < arrType.Rank; i++)
                    {
                        aindex.Add(new BoundLiteral(node, ConstantValue.Create(0), intType));
                    }
                    var bacc = new BoundArrayAccess(node.Operand, expr, aindex.ToImmutableAndFree(), elType, false);
                    TypeSymbol ptrType = new PointerTypeSymbol(TypeWithAnnotations.Create(elType));
                    return new BoundAddressOfOperator(node, bacc, false, ptrType, hasErrors: false);
                }
                var decl = local.LocalSymbol.DeclaringSyntaxReferences[0];
                if (decl is not null && decl.GetSyntax() is CSharpSyntaxNode csnode && csnode.XVoIsDecl)
                {
                    // local foo IS SomeType
                    // SomeFunc(@foo)
                    TypeSymbol ptrType = new PointerTypeSymbol(TypeWithAnnotations.Create(local.Type));
                    return new BoundAddressOfOperator(node, local, false, ptrType, hasErrors: false);
                }
            }
            return null;
        }
        private BoundExpression AdjustConstantType(BoundExpression expr, TypeSymbol type, DiagnosticBag diagnostics)
        {
            if (expr.Kind == BoundKind.Literal && xsValueFitsIn(expr.ConstantValue, type.SpecialType))
            {
                expr = CreateConversion(expr, type, diagnostics);
                expr.WasCompilerGenerated = true;
            }
            return expr;
        }
        public void VODetermineIIFTypes(ConditionalExpressionSyntax node, DiagnosticBag diagnostics,
            ref BoundExpression trueExpr, ref BoundExpression falseExpr)
        {
            // a combination of null and a value type is not allowed
            // in that case we replace the null with a default value.
            var trueNull = trueExpr?.Type is null;
            var falseNull = falseExpr?.Type is null;
            if (trueNull && falseNull)
            {
                return;
            }
            else if (trueNull && !falseExpr.Type.IsReferenceType)
            {
                if (Compilation.Options.HasRuntime)
                {
                    trueExpr = new BoundDefaultExpression(trueExpr.Syntax, Compilation.UsualType());
                    trueNull = false;
                }
            }
            else if (falseNull && !trueExpr.Type.IsReferenceType)
            {
                if (Compilation.Options.HasRuntime)
                {
                    falseExpr = new BoundDefaultExpression(falseExpr.Syntax, Compilation.UsualType());
                    falseNull = false;
                }
            }
            if (trueNull || falseNull)
            {
                // this happens with the combination of a null and a reference type.
                return;
            }
            // Determine underlying types. For literal numbers this may be Byte, Short, Int or Long
            TypeSymbol trueType = VOGetType(trueExpr);
            TypeSymbol falseType = VOGetType(falseExpr);
            if (!Equals(trueType, falseType) && trueType.IsIntegralType() && falseType.IsIntegralType())
            {
                // when one side is a literal and the other is not then try to cast to the non literal type
                if (trueExpr.ConstantValue != null && falseExpr.ConstantValue == null)
                {
                    trueExpr = AdjustConstantType(trueExpr, falseType, diagnostics);
                    trueType = trueExpr.Type;
                }
                if (falseExpr.ConstantValue != null && trueExpr.ConstantValue == null)
                {
                    falseExpr = AdjustConstantType(falseExpr, trueType, diagnostics);
                    falseType = falseExpr.Type;
                }
                // Determine the largest of the two integral types and scale up
                if (trueType.SpecialType.SizeInBytes() >= falseType.SpecialType.SizeInBytes())
                {
                    falseType = trueType;
                }
                else
                {
                    trueType = falseType;
                }
            }
            if (!Equals(trueType, falseType) && trueType.IsIntegralType() != falseType.IsIntegralType())
            {
                var trueFrac = trueType.IsFractionalType();
                var falseFrac = falseType.IsFractionalType();
                if (trueFrac)
                {
                    falseType = trueType;
                }
                else if (falseFrac)
                {
                    trueType = falseType;
                }
            }
            bool compatibleIIF = Compilation.Options.HasOption(CompilerOption.CompatibleIIF, node);
            if (!Equals(trueType, falseType) && Compilation.Options.HasRuntime)
            {
                // convert to usual when one of the two is a usual
                if (trueType.IsUsualType() || falseType.IsUsualType() || compatibleIIF)
                {
                    // convert to usual when Compatible IIF is activated
                    // or when one of the two types is USUAL
                    trueType = falseType = Compilation.UsualType();
                }
            }
            if (!Equals(trueType, falseType))
            {
                if (trueType.IsVoidPointer())
                {
                    if (falseType.GetSpecialTypeSafe() == SpecialType.System_IntPtr)
                    {
                        trueType = falseType;
                    }
                }
                else if (falseType.IsVoidPointer())
                {
                    if (trueType.GetSpecialTypeSafe() == SpecialType.System_IntPtr)
                    {
                        falseType = trueType;
                    }
                }
                else if (trueType.IsObjectType() || falseType.IsObjectType() || compatibleIIF)
                {
                    // convert to object when Compatible IIF is activated
                    // or when one of the two is object
                    // this will not happen for VO Dialect because that is handled above
                    trueType = falseType = Compilation.GetSpecialType(SpecialType.System_Object);
                }
                else
                {
                    Error(diagnostics, ErrorCode.ERR_InvalidQM, node, trueType, falseType);
                    trueType = falseType = Compilation.GetSpecialType(SpecialType.System_Object);
                }
            }
            if (!Equals(trueExpr.Type, trueType))
            {
                trueExpr = CreateConversion(trueExpr, trueType, diagnostics);
            }
            if (!Equals(falseExpr.Type, falseType))
            {
                falseExpr = CreateConversion(falseExpr, falseType, diagnostics);
            }
        }

        private LocalSymbol FindDimLocal(BoundLocal local)
        {
            var decl = local.LocalSymbol.DeclaringSyntaxReferences[0];
            if (decl is not null && decl.GetSyntax() is CSharpSyntaxNode csnode && csnode.XVoIsDim && this is not FixedStatementBinder)
            {

                var dimName = local.LocalSymbol.Name + XSharpSpecialNames.DimSuffix;
                LookupResult result = LookupResult.GetInstance();
                HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                var binder = this.LookupSymbolsInternal(result, dimName, 0, null, LookupOptions.Default, false, ref useSiteDiagnostics);
                var symbol = result.SingleSymbolOrDefault;
                return symbol as LocalSymbol;
            }
            return null;
        }
        private static object FoldXsUncheckedIntegralBinaryOperator(BinaryOperatorKind kind, ConstantValue valueLeft, ConstantValue valueRight, ref SpecialType resultType)
        {
            unchecked
            {
                Debug.Assert(valueLeft != null);
                Debug.Assert(valueRight != null);
                long result;
                ulong result2;

                switch (kind)
                {
                    case BinaryOperatorKind.IntAddition:
                        result = (long)valueLeft.Int32Value + (long)valueRight.Int32Value;
                        if (result <= Int32.MaxValue && result >= Int32.MinValue)
                            return (Int32)result;
                        resultType = SpecialType.System_Int64;
                        return result;
                    case BinaryOperatorKind.LongAddition:
                        return valueLeft.Int64Value + valueRight.Int64Value;
                    case BinaryOperatorKind.UIntAddition:
                        result2 = (ulong)valueLeft.UInt32Value + (ulong)valueRight.UInt32Value;
                        if (result2 <= UInt32.MaxValue && result2 >= UInt32.MinValue)
                            return (UInt32)result2;
                        resultType = SpecialType.System_UInt64;
                        return result2;
                    case BinaryOperatorKind.ULongAddition:
                        return valueLeft.UInt64Value + valueRight.UInt64Value;
                    case BinaryOperatorKind.IntSubtraction:
                        return valueLeft.Int32Value - valueRight.Int32Value;
                    case BinaryOperatorKind.LongSubtraction:
                        return valueLeft.Int64Value - valueRight.Int64Value;
                    case BinaryOperatorKind.UIntSubtraction:
                        return valueLeft.UInt32Value - valueRight.UInt32Value;
                    case BinaryOperatorKind.ULongSubtraction:
                        return valueLeft.UInt64Value - valueRight.UInt64Value;
                    case BinaryOperatorKind.IntMultiplication:
                        result = (long)valueLeft.Int32Value * (long)valueRight.Int32Value;
                        if (result <= int.MaxValue && result >= int.MinValue)
                            return (int)result;
                        resultType = SpecialType.System_Int64;
                        return result;
                    case BinaryOperatorKind.LongMultiplication:
                        return valueLeft.Int64Value * valueRight.Int64Value;
                    case BinaryOperatorKind.UIntMultiplication:
                        result2 = (ulong)valueLeft.UInt32Value * (ulong)valueRight.UInt32Value;
                        if (result2 <= uint.MaxValue && result2 >= uint.MinValue)
                            return (uint)result2;
                        resultType = SpecialType.System_UInt64;
                        return result2;
                    case BinaryOperatorKind.ULongMultiplication:
                        return valueLeft.UInt64Value * valueRight.UInt64Value;

                    // even in unchecked context division may overflow:
                    case BinaryOperatorKind.IntDivision:
                        if (valueLeft.Int32Value == int.MinValue && valueRight.Int32Value == -1)
                        {
                            return int.MinValue;
                        }

                        return valueLeft.Int32Value / valueRight.Int32Value;

                    case BinaryOperatorKind.LongDivision:
                        if (valueLeft.Int64Value == long.MinValue && valueRight.Int64Value == -1)
                        {
                            return long.MinValue;
                        }

                        return valueLeft.Int64Value / valueRight.Int64Value;
                }

                return null;
            }
        }
        ConstantValue XsConvertConstant(ConstantValue constant, SpecialType specialType)
        {
            unchecked
            {
                if (!constant.IsBad)
                {
                    return specialType switch
                    {
                        SpecialType.System_SByte => ConstantValue.Create((sbyte)constant.Int64Value),
                        SpecialType.System_Int16 => ConstantValue.Create((short)constant.Int64Value),
                        SpecialType.System_UInt16 => ConstantValue.Create((ushort)constant.Int64Value),
                        SpecialType.System_Int32 => ConstantValue.Create((int)constant.Int64Value),
                        SpecialType.System_UInt32 => ConstantValue.Create((uint)constant.Int64Value),
                        SpecialType.System_Int64 => ConstantValue.Create((long)constant.Int64Value),
                        SpecialType.System_UInt64 => ConstantValue.Create((ulong)constant.Int64Value),
                        _ => constant
                    };
                }
            }
            return constant;
        }

        BoundExpression XsHandleIntegralTypes(BinaryExpressionSyntax node, BoundBinaryOperator binaryOperator, TypeSymbol leftType, TypeSymbol rightType)
        {
            // This is the place where we will handle VO specific conversion
            // when the left and right types are equal we want a result of the same type.
            // also shift operations should return the left type: C277 ByteValue >> 2 should not return int but byte.
            var leftEquals = Equals(binaryOperator.Left.Type, leftType) || binaryOperator.Left.ConstantValue != null;
            var rightEquals = Equals(binaryOperator.Right.Type, rightType) || binaryOperator.Right.ConstantValue != null;

            BoundExpression result = binaryOperator;
            var kind = binaryOperator.OperatorKind;
            if (kind.IsComparison())
                return binaryOperator;
            var resultType = binaryOperator.Type;
            bool forceConvert = kind.IsShift() || (Equals(leftType, rightType) && !Equals(leftType, resultType));
            if (!forceConvert && leftType.SpecialType.SizeInBytes() == rightType.SpecialType.SizeInBytes())
            {
                // with 2 types of the same size and different SignedNess (DWORD and INT for example)
                // force the conversion to the type of the LHS
                // C711 u := DWORD + INT should become DWORD
                if (leftType.SpecialType.IsSignedIntegralType() != rightType.SpecialType.IsSignedIntegralType())
                    forceConvert = true;
            }
            if (binaryOperator.ConstantValue == null || forceConvert)
            {
                // SHORT(_CAST, expression) has been converted to a _AND() operation. In that case we want the type of the RHS of the operation.
                // the same is true for (WORD) -1
                var preferredType = leftType;
                if (binaryOperator.Left.ConstantValue != null)
                {
                    preferredType = rightType;
                }
                // we do not want to convert when the result is a folded constant that is too
                // large to fit into our destination type
                if (result.ConstantValue != null)
                {
                    forceConvert = XsConstantFitsInType(result, preferredType);
                }
                if (forceConvert && !Equals(resultType, preferredType))
                {
                    // make sure that the result fits in the type that we want
                    result = new BoundConversion(result.Syntax, result, Conversion.ImplicitNumeric, false, true,
                        conversionGroupOpt: null,
                        constantValueOpt: binaryOperator.ConstantValue,
                        type: preferredType);
                }
            }
            return result;
        }
        bool XsConstantFitsInType(BoundExpression result, TypeSymbol preferredType)
        {
            bool fits = true;
            var constant = result.ConstantValue;
            var resultType = result.Type;
            if (constant != null && !constant.IsBad)
            {
                if (resultType.SpecialType.IsSignedIntegralType())
                {
                    var i64 = constant.Int64Value;
                    switch (preferredType.SpecialType)
                    {
                        case SpecialType.System_Int32:
                            if (i64 > int.MaxValue)
                                fits = false;
                            else if (i64 < int.MinValue)
                                fits = false;
                            break;
                        case SpecialType.System_UInt32:
                            if (i64 > uint.MaxValue)
                                fits = false;
                            else if (i64 < uint.MinValue)
                                fits = false;
                            break;
                        case SpecialType.System_Int16:
                            if (i64 > short.MaxValue)
                                fits = false;
                            else if (i64 < short.MinValue)
                                fits = false;
                            break;
                        case SpecialType.System_UInt16:
                            if (i64 > ushort.MaxValue)
                                fits = false;
                            else if (i64 < ushort.MinValue)
                                fits = false;
                            break;
                        case SpecialType.System_SByte:
                            if (i64 > sbyte.MaxValue)
                                fits = false;
                            else if (i64 < sbyte.MinValue)
                                fits = false;
                            break;
                        case SpecialType.System_Byte:
                            if (i64 > byte.MaxValue)
                                fits = false;
                            else if (i64 < byte.MinValue)
                                fits = false;
                            break;
                        case SpecialType.System_UInt64:
                            if (i64 < 0)
                                fits = false;
                            break;

                    }

                }
                else if (resultType.SpecialType.IsUnsignedIntegralType())
                {
                    var u64 = constant.UInt64Value;
                    switch (preferredType.SpecialType)
                    {
                        case SpecialType.System_Int32:
                            if (u64 > (ulong)int.MaxValue)
                                fits = false;
                            break;
                        case SpecialType.System_UInt32:
                            if (u64 > uint.MaxValue)
                                fits = false;
                            break;
                        case SpecialType.System_Int16:
                            if (u64 > (ulong)short.MaxValue)
                                fits = false;
                            break;
                        case SpecialType.System_UInt16:
                            if (u64 > ushort.MaxValue)
                                fits = false;
                            break;
                        case SpecialType.System_SByte:
                            if (u64 > (ulong)sbyte.MaxValue)
                                fits = false;
                            break;
                        case SpecialType.System_Byte:
                            if (u64 > byte.MaxValue)
                                fits = false;
                            break;
                        case SpecialType.System_Int64:
                            if (u64 > long.MaxValue)
                                fits = false;
                            break;

                    }
                }
            }
            return fits;
        }
    }
}

