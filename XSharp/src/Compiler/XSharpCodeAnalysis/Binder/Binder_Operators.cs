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
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
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
            Cast,
            PSZCompare,
            SymbolCompare,
            LogicCompare
        }

        private bool CheckImplicitCast(TypeSymbol sourceType, TypeSymbol targetType, SyntaxNode syntax, DiagnosticBag diagnostics)
        {
            // do not throw an warnings for IIF() expressions with types that are too big
            // for the LHS of the assignment
            if (syntax.Kind() == SyntaxKind.ConditionalExpression)
            {
                return true;
            }
            if (targetType.IsIntegralType() && sourceType.IsIntegralType())
            {
                // warning assign to smaller type
                if (targetType.SpecialType.SizeInBytes() < sourceType.SpecialType.SizeInBytes())
                {
                    if (!Compilation.Options.VOImplicitCastsAndConversions)
                    { 
                        var distinguisher = new SymbolDistinguisher(this.Compilation, sourceType, targetType);
                        Error(diagnostics, ErrorCode.WRN_ImplicitCast, syntax, distinguisher.First, distinguisher.Second);
                    }
                }
                else if (targetType.SpecialType.IsSignedIntegralType() != sourceType.SpecialType.IsSignedIntegralType())
                {
                    if (!Compilation.Options.VOSignedUnsignedConversion)
                    { 
                        var distinguisher = new SymbolDistinguisher(this.Compilation, sourceType, targetType);
                        Error(diagnostics, ErrorCode.WRN_SignedUnSignedConversion, syntax, distinguisher.First, distinguisher.Second);
                    }

                }
            }
            return true;
        }
        private BoundExpression BindVOCompareString(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
            BoundExpression left, BoundExpression right, ref int compoundStringLength)
        {
            MethodSymbol opMeth = null;
            TypeSymbol type;
            BoundCall opCall = null;
            var stringType = Compilation.GetSpecialType(SpecialType.System_String);
            if (left.Type.SpecialType != SpecialType.System_String)
            {
                left = CreateConversion(left, stringType, diagnostics);
            }
            if (right.Type.SpecialType != SpecialType.System_String)
            {
                right = CreateConversion(right, stringType, diagnostics);
            }

            if (Compilation.Options.HasRuntime && this.Compilation.Options.VOStringComparisons)
            {
                // VO Style String Comparison
                type = Compilation.RuntimeFunctionsType();
                string methodName = XSharpFunctionNames.StringCompare ;
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
            }
            return BindSimpleBinaryOperator(node, diagnostics, opCall,
                new BoundLiteral(node, ConstantValue.Create((int)0), GetSpecialType(SpecialType.System_Int32, diagnostics, node)),
                ref compoundStringLength);
        }


        private BoundExpression BindVOSingleEqualsString(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
            BoundExpression left, BoundExpression right)
        {
            MethodSymbol opMeth = null;
            BoundExpression opCall = null;
            var type = Compilation.RuntimeFunctionsType();
            var methodName = XSharpFunctionNames.StringEquals; 
            var symbols = Binder.GetCandidateMembers(type, methodName, LookupOptions.MustNotBeInstance, this);
            if (symbols.Length == 1)
            {
                opMeth = (MethodSymbol)symbols[0];
                var stringType = Compilation.GetSpecialType(SpecialType.System_String);
                if (right.Type != stringType)
                {
                    right = CreateConversion(right, stringType, diagnostics);
                }
                if (left.Type != stringType)
                {
                    left = CreateConversion(left, stringType, diagnostics);
                }
                opCall = BoundCall.Synthesized(node, null, opMeth, left, right);
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
                symbols = Binder.GetCandidateMembers(usualType, "op_Subtraction", LookupOptions.MustNotBeInstance, this);
            else
                symbols = Binder.GetCandidateMembers(usualType, "op_Addition", LookupOptions.MustNotBeInstance, this);
            if (symbols.Length == 1)
            {
                MethodSymbol opMeth = (MethodSymbol)symbols[0];
                if (right.Type != usualType)
                {
                    right = CreateConversion(right, usualType, diagnostics);
                }
                if (left.Type != usualType)
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
                if (xnode ==null && node.Syntax.XNode is XSharpParser.PrimaryExpressionContext)
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
            if (right.Type != pszType)
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
            if (left.Type != pszType)
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
            if (right.Type != symType)
            {
                right = CreateConversion(right, symType, diagnostics);
            }
            if (left.Type != symType)
            {
                left = CreateConversion(left, symType, diagnostics);
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
            left = new BoundConditionalOperator(node, false, left, lit1, lit0, null, intType);
            right = new BoundConditionalOperator(node, false, right, lit1, lit0, null, intType);
            return null;
        }

        private BoundExpression BindVOSingleEqualsUsual(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
             BoundExpression left, BoundExpression right)
        {
            MethodSymbol opMeth = null;
            BoundExpression opCall = null;
            var usualType = Compilation.UsualType();
            var methodName = XSharpFunctionNames.InExactEquals  ;
            var symbols = Binder.GetCandidateMembers(usualType, methodName, LookupOptions.MustNotBeInstance, this);
            if (symbols.Length == 2)
            {
                // There should be 2 overloads in VulcanRTFuncs:
                // public static bool __InexactEquals(__Usual ul, string uR)
                // public static bool __InexactEquals(__Usual uL, __Usual uR)
                // Switch to overload with string when RHS = STRING
                opMeth = (MethodSymbol)symbols[0];
                if (right.Type?.SpecialType == SpecialType.System_String)
                {
                    if (right.Type != opMeth.Parameters[0].Type)
                        opMeth = (MethodSymbol)symbols[1];
                }
                else
                {
                    // When RHS != USUAL then switch
                    if (opMeth.Parameters[0].Type != usualType)
                        opMeth = (MethodSymbol)symbols[1];
                    if (right.Type != usualType)
                    {
                        right = CreateConversion(right, usualType, diagnostics);
                    }
                }
                if (left.Type != usualType)
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
            var methodName = XSharpFunctionNames.InExactNotEquals ;
            var symbols = Binder.GetCandidateMembers(usualType, methodName, LookupOptions.MustNotBeInstance, this);
            if (symbols.Length == 2)
            {
                // There should be 2 overloads in VulcanRTFuncs:
                // public static bool __InexactNotEquals(__Usual ul, string uR)
                // public static bool __InexactNotEquals(__Usual uL, __Usual uR)
                // Switch to overload with string when RHS = STRING
                opMeth = (MethodSymbol)symbols[0];
                if (right.Type?.SpecialType == SpecialType.System_String)
                {
                    if (right.Type != opMeth.Parameters[0].Type)
                        opMeth = (MethodSymbol)symbols[1];
                }
                else
                {
                    // When RHS != USUAL then switch
                    if (opMeth.Parameters[0].Type != usualType)
                        opMeth = (MethodSymbol)symbols[1];
                    if (right.Type != usualType)
                    {
                        right = CreateConversion(right, usualType, diagnostics);
                    }
                }
                if (left.Type != usualType)
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
            var methodName = XSharpFunctionNames.StringSubtract ;
            var symbols = Binder.GetCandidateMembers(type, methodName, LookupOptions.MustNotBeInstance, this);
            if (symbols.Length == 1)
            {
                opMeth = (MethodSymbol)symbols[0];
                var stringType = Compilation.GetSpecialType(SpecialType.System_String);
                if (left.Type != stringType)
                {
                    left = CreateConversion(left, stringType, diagnostics);
                }
                if (right.Type != stringType)
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
            ref BoundExpression left, ref BoundExpression right, ref int compoundStringLength, VOOperatorType opType)
        {
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
                    return BindVOCompareString(node, diagnostics, left, right, ref compoundStringLength);
                case VOOperatorType.UsualOther:
                    return BindVOUsualOther(node, diagnostics, left, right);
                case VOOperatorType.PSZCompare:
                    return BindVOPszCompare(node, diagnostics, ref left, ref right);
                case VOOperatorType.SymbolCompare:
                    return BindVOSymbolCompare(node, diagnostics, ref left, ref right);
                case VOOperatorType.LogicCompare:
                    return BindVOLogicCompare(node, diagnostics, ref left, ref right);

            }
            return null;
        }

        private VOOperatorType NeedsVOOperator(BinaryExpressionSyntax node, ref BoundExpression left, ref BoundExpression right)
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
            if (leftType == null || rightType == null)
            {
                // this is most likely an error that will be reported later by Roslyn
                return VOOperatorType.None;
            }
            var leftString = leftType.SpecialType == SpecialType.System_String;
            var rightString = rightType.SpecialType == SpecialType.System_String;

            if (Compilation.Options.HasRuntime && xnode != null)
            {
                var typeUsual = Compilation.UsualType();
                var typePSZ = Compilation.PszType();
                var typeSym = Compilation.SymbolType();
                var leftUsual = leftType == typeUsual;
                var rightUsual = rightType == typeUsual;
                var leftSym = leftType == typeSym;
                var rightSym = rightType == typeSym;
                var leftPSZ = leftType == typePSZ;
                var rightPSZ = rightType == typePSZ;

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
                        if ((leftUsual || rightUsual) && ( leftSym || rightSym))
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
                        if (leftString || rightString)
                        {
                            if (leftType?.SpecialType != SpecialType.System_Char && rightType?.SpecialType != SpecialType.System_Char)
                            {
                                // Convert to String.Compare or __StringCompare. Decide later
                                opType = VOOperatorType.CompareString;
                                break;
                            }
                        }
                        if (leftPSZ || rightPSZ)
                        {
                            opType = VOOperatorType.PSZCompare;
                            break;
                        }
                        if (leftType == Compilation.GetSpecialType(SpecialType.System_Boolean) &&
                            rightType == Compilation.GetSpecialType(SpecialType.System_Boolean))
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
                            if (leftString && (rightString || rightType == typeUsual))
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
                            var typeDate = Compilation.DateType();
                            var typeFloat = Compilation.FloatType();

                            // Add or Subtract USUAL with other type
                            // LHS   - RHS 
                            // Usual - Date
                            // Date  - Usual
                            // Usual - Float
                            // Float - Usual
                            if (leftUsual && (rightType == typeDate || rightType == typeFloat))
                            {
                                opType = VOOperatorType.UsualOther;
                                break;
                            }
                            if (rightUsual && (leftType == typeDate || leftType == typeFloat))
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
                    case SyntaxKind.GreaterThanExpression:
                    case SyntaxKind.GreaterThanOrEqualExpression:
                    case SyntaxKind.LessThanExpression:
                    case SyntaxKind.LessThanOrEqualExpression:
                        if (leftString || rightString)
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
                        opType = VOOperatorType.Cast;
                        break;
                    case SyntaxKind.AddExpression:
                    case SyntaxKind.SubtractExpression:
                    case SyntaxKind.AddAssignmentExpression:
                    case SyntaxKind.SubtractAssignmentExpression:
                    case SyntaxKind.DivideAssignmentExpression:
                    case SyntaxKind.DivideExpression:
                    case SyntaxKind.MultiplyAssignmentExpression:
                    case SyntaxKind.MultiplyExpression:
                        if (leftType != rightType && leftType.IsIntegralType() && rightType.IsIntegralType())
                        {
                           if (leftType.SpecialType.SizeInBytes() < 4)
                           {
                                // when adding a constant to a word or short
                                // cast the constant to the LHS
                                if (rightType.SpecialType.SizeInBytes() > leftType.SpecialType.SizeInBytes())
                                {
                                    if (right.ConstantValue != null)
                                    {
                                        right = new BoundConversion(right.Syntax, right, Conversion.ImplicitNumeric, false, false, right.ConstantValue, leftType) { WasCompilerGenerated = true };
                                    }
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
            if (xnode == null)  // this may happen for example for nodes generated in the transformation phase
                return;
            // check for Logic operations with Usual. If that is the case then add a conversion to the expression
            switch (xnode.Op.Type)
            {
                case XSharpParser.LOGIC_AND:
                case XSharpParser.LOGIC_OR:
                case XSharpParser.LOGIC_XOR:
                case XSharpParser.AND:
                case XSharpParser.OR:
                    var usualType = Compilation.UsualType();
                    var boolType = this.GetSpecialType(SpecialType.System_Boolean,diagnostics, node);
                    if (left.Type == usualType)
                    {
                        left = CreateConversion(left, boolType, diagnostics);
                    }

                    if (right.Type == usualType)
                    {
                        right = CreateConversion(right, boolType, diagnostics);
                    }
                    break;
            }
            return;
        }
        public BoundExpression RewriteIndexAccess(BoundExpression index, DiagnosticBag diagnostics)
        {
            if (!index.HasAnyErrors && !this.Compilation.Options.ArrayZero)
            {
                var kind = BinaryOperatorKind.Subtraction;
                var left = index;
                var right = new BoundLiteral(index.Syntax, ConstantValue.Create(1), index.Type) { WasCompilerGenerated = true };
                int compoundStringLength = 0;
                var leftType = left.Type;
                var opKind = leftType.SpecialType == SpecialType.System_Int32 ? BinaryOperatorKind.IntSubtraction
                    : leftType.SpecialType == SpecialType.System_Int64 ? BinaryOperatorKind.LongSubtraction
                    : leftType.SpecialType == SpecialType.System_UInt32 ? BinaryOperatorKind.UIntSubtraction
                    : BinaryOperatorKind.ULongSubtraction;
                var resultConstant = FoldBinaryOperator((CSharpSyntaxNode)index.Syntax, opKind, left, right, left.Type.SpecialType, diagnostics, ref compoundStringLength);
                var sig = this.Compilation.builtInOperators.GetSignature(opKind);
                index = new BoundBinaryOperator(index.Syntax, kind, left, right, resultConstant, sig.Method,
                    resultKind: LookupResultKind.Viable,
                    originalUserDefinedOperatorsOpt: ImmutableArray<MethodSymbol>.Empty,
                    type: index.Type,
                    hasErrors: false)
                { WasCompilerGenerated = true };
            }
            return index;
        }
        public TypeSymbol VOGetType(BoundExpression expr)
        {
            if (expr.Kind == BoundKind.Literal )
            {
                var lit = expr as BoundLiteral;
                var xnode = lit?.Syntax.XNode as XSharpParser.PrimaryExpressionContext;
                var type = xnode?.Start?.Type;
                if (type == XSharpParser.INT_CONST || type == XSharpParser.REAL_CONST)
                {
                    string text = xnode.Start.Text;
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
                // disable this for C642
                //if (lit.ConstantValue.Discriminator == ConstantValueTypeDiscriminator.Int32)
                //{
                //    var val = lit.ConstantValue.Int32Value;
                //    if (val >= Byte.MinValue && val <= Byte.MaxValue)
                //        return Compilation.GetSpecialType(SpecialType.System_Byte);
                //    else if (val >= Int16.MinValue && val <= Int16.MaxValue)
                //        return Compilation.GetSpecialType(SpecialType.System_Int16);
                //}
            }
            else if (expr.Kind ==BoundKind.UnaryOperator)
            {
                var unary = expr as BoundUnaryOperator;
                var type = VOGetType(unary.Operand);
                if (unary.OperatorKind.HasFlag(UnaryOperatorKind.UnaryMinus))
                {
                    // see if we must change unsigned into signed
                    if (type == Compilation.GetSpecialType(SpecialType.System_Byte))
                    {
                        type = Compilation.GetSpecialType(SpecialType.System_Int16);
                    }
                    else if (type == Compilation.GetSpecialType(SpecialType.System_UInt16))
                    {
                        type = Compilation.GetSpecialType(SpecialType.System_Int32);
                    }
                    else if (type == Compilation.GetSpecialType(SpecialType.System_UInt32))
                    {
                        type = Compilation.GetSpecialType(SpecialType.System_Int64);  
                    }
                }
                else if (unary.OperatorKind.HasFlag(UnaryOperatorKind.LogicalNegation ))
                {
                    type = Compilation.GetSpecialType(SpecialType.System_Boolean);
                }
                return type;
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
                Error(diagnostics, ErrorCode.ERR_CannotTakeAddressOfFunctionOrMethod, node.Operand);
            }

            var expr = this.BindExpression(node.Operand, diagnostics: diagnostics, invoked: false, indexed: false);

            if (expr.Kind == BoundKind.FieldAccess)
            {
                if (expr.ExpressionSymbol is SourceFixedFieldSymbol)
                {
                    return expr;
                }
                var bfa = expr as BoundFieldAccess;
                // Externally defined fixed Field. Could be a DIM field in a VoStruct class
                if (bfa.FieldSymbol.IsFixed)
                {
                    var type = bfa.FieldSymbol.ContainingType;
                    if (type.IsVoStructOrUnion())
                    {
                        return expr;
                    }
                }


            }
            if (expr.Kind == BoundKind.ArrayAccess)
            {
                //translate @var[i]  to var[i]
                var bac = expr as BoundArrayAccess;
                var type = expr.Type;
                if (bac.Expression.ExpressionSymbol is SourceLocalSymbol && type.IsVoStructOrUnion())
                {
                    var sls = bac.Expression.ExpressionSymbol as SourceLocalSymbol;
                    var syntaxes = sls.DeclaringSyntaxReferences;
                    if (syntaxes.Length > 0)
                    {
                        var syntaxNode = syntaxes[0].GetSyntax() as CSharpSyntaxNode;
                        var lvc = syntaxNode.XNode as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.LocalvarContext;
                        if (lvc.As.Type == LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.AS)
                        {
                            return expr;
                        }


                    }
                }
            }
            if (expr.Kind == BoundKind.Local)
            {
                var bl = expr as BoundLocal;
                // only translate @name to @name[0] when not IsDecl
                if (expr.Type.IsArray())
                {
                    var eltype = (expr.Type as ArrayTypeSymbol).ElementType;
                    // convert from @expr to @expr[0]
                    var intType = Compilation.GetSpecialType(SpecialType.System_Int32);
                    var arrType = expr.Type as ArrayTypeSymbol;
                    var elType = arrType.ElementType;
                    var aindex = ArrayBuilder<BoundExpression>.GetInstance();
                    for (int i = 0; i < arrType.Rank; i++)
                    {
                        aindex.Add(new BoundLiteral(node, ConstantValue.Create(0), intType));
                    }
                    var bacc = new BoundArrayAccess(node.Operand, expr, aindex.ToImmutableAndFree(), elType, false);
                    TypeSymbol ptrType = new PointerTypeSymbol(elType);
                    return new BoundAddressOfOperator(node, bacc, false, ptrType, hasErrors: false);
                }
            }
            return null;
        }
        public void VODetermineIIFTypes(ConditionalExpressionSyntax node, DiagnosticBag diagnostics,
            ref BoundExpression trueExpr, ref BoundExpression falseExpr, 
            ref TypeSymbol trueType, ref TypeSymbol falseType)
        {
            // do nothing when the types null
            if (trueType != null && falseType != null)
            {
                // Determine underlying types. For literal numbers this may be Byte, Short, Int or Long
                trueType = VOGetType(trueExpr);
                falseType = VOGetType(falseExpr);
                if (trueType != falseType && trueType.IsIntegralType() && falseType.IsIntegralType())
                {
                    // Determine the largest of the two integral types and scale up
                    if (trueType.SpecialType.SizeInBytes() > falseType.SpecialType.SizeInBytes())
                        falseType = trueType;
                    else
                        trueType = falseType;
                }

                if (trueType != falseType && Compilation.Options.HasRuntime)
                {
                    // convert to usual when one of the two is a usual
                    var usualType = Compilation.UsualType();
                    if (trueType == usualType)
                    {
                        falseType = trueType;
                        falseExpr = CreateConversion(falseExpr, usualType, diagnostics);
                    }
                    else if (falseType == usualType)
                    {
                        trueType = falseType;
                        trueExpr = CreateConversion(trueExpr, usualType, diagnostics);
                    }
                    else if (Compilation.Options.VOCompatibleIIF)
                    {
                        // convert to usual when Compatible IIF is activated
                        trueExpr = CreateConversion(trueExpr, usualType, diagnostics);
                        falseExpr = CreateConversion(falseExpr, usualType, diagnostics);
                        trueType = falseType = usualType;
                    }
                }
                if (trueType != falseType )
                {
                    if (trueType.IsVoidPointer())
                    {
                        if (falseType == Compilation.GetSpecialType(SpecialType.System_IntPtr))
                        {
                            trueExpr = CreateConversion(trueExpr, falseType, diagnostics);
                            trueType = falseType;
                        }
                    }
                    else if (falseType.IsVoidPointer())
                    {
                        if (trueType == Compilation.GetSpecialType(SpecialType.System_IntPtr))
                        {
                            falseExpr = CreateConversion(falseExpr, trueType, diagnostics);
                            falseType = trueType;
                        }
                    }
                    else if (Compilation.Options.VOCompatibleIIF)
                    {
                        // convert to object when Compatible IIF is activated
                        // this will not happen for VO Dialect because that is handled above
                        var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
                        trueExpr = CreateConversion(trueExpr, objectType, diagnostics);
                        falseExpr = CreateConversion(falseExpr, objectType, diagnostics);
                        trueType = falseType = objectType;
                    }

                }
            }
        }
        private static object FoldXsCheckedIntegralBinaryOperator(BinaryOperatorKind kind, ConstantValue valueLeft, ConstantValue valueRight, ref SpecialType resultType)
        {
            checked
            {
                Debug.Assert(valueLeft != null);
                Debug.Assert(valueRight != null);
                Int64 result;
                UInt64 result2;
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
                        result =  (long)valueLeft.Int32Value - (long)valueRight.Int32Value;
                        if (result <= Int32.MaxValue && result >= Int32.MinValue)
                            return (Int32)result;
                        resultType = SpecialType.System_Int64;
                        return result;

                    case BinaryOperatorKind.LongSubtraction:
                        return valueLeft.Int64Value - valueRight.Int64Value;
                    case BinaryOperatorKind.UIntSubtraction:
                        result2 = (ulong) valueLeft.UInt32Value - (ulong)valueRight.UInt32Value;
                        if (result2 <= UInt32.MaxValue && result2 >= UInt32.MinValue)
                            return (UInt32)result2;
                        resultType = SpecialType.System_UInt64;
                        return result2;
                    case BinaryOperatorKind.ULongSubtraction:
                        return valueLeft.UInt64Value - valueRight.UInt64Value;
                    case BinaryOperatorKind.IntMultiplication:
                        result = (long) valueLeft.Int32Value * (long)valueRight.Int32Value;
                        if (result <= Int32.MaxValue && result >= Int32.MinValue)
                            return (Int32)result;
                        resultType = SpecialType.System_Int64;
                        return result;

                    case BinaryOperatorKind.LongMultiplication:
                        return valueLeft.Int64Value * valueRight.Int64Value;
                    case BinaryOperatorKind.UIntMultiplication:
                        result2 = (ulong) valueLeft.UInt32Value * (ulong)valueRight.UInt32Value;
                        if (result2 <= UInt32.MaxValue && result2 >= UInt32.MinValue)
                            return (UInt32)result2;
                        resultType = SpecialType.System_UInt64;
                        return result2;
                    case BinaryOperatorKind.ULongMultiplication:
                        return valueLeft.UInt64Value * valueRight.UInt64Value;
                    case BinaryOperatorKind.IntDivision:
                        return valueLeft.Int32Value / valueRight.Int32Value;
                    case BinaryOperatorKind.LongDivision:
                        return valueLeft.Int64Value / valueRight.Int64Value;
                }

                return null;
            }
        }
        private static object FoldXsUncheckedIntegralBinaryOperator(BinaryOperatorKind kind, ConstantValue valueLeft, ConstantValue valueRight, ref SpecialType resultType)
        {
            unchecked
            {
                Debug.Assert(valueLeft != null);
                Debug.Assert(valueRight != null);
                Int64 result;
                UInt64 result2;

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
                        if (result <= Int32.MaxValue && result >= Int32.MinValue)
                            return (Int32)result;
                        resultType = SpecialType.System_Int64;
                        return result;
                    case BinaryOperatorKind.LongMultiplication:
                        return valueLeft.Int64Value * valueRight.Int64Value;
                    case BinaryOperatorKind.UIntMultiplication:
                        result2 = (ulong)valueLeft.UInt32Value * (ulong)valueRight.UInt32Value;
                        if (result2 <= UInt32.MaxValue && result2 >= UInt32.MinValue)
                            return (UInt32)result2;
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

    }
}

