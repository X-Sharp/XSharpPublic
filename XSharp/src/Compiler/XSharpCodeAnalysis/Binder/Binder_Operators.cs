using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class Binder
    {
        private enum VOOperatorType
        {
            None = 0,
            CompareString ,
            SingleEqualsString,
            SingleEqualsUsual,
            NotEqualsUsual,
            SubtractString
        }
        private BoundExpression BindVOCompareString(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
            BoundExpression left, BoundExpression right, ref int compoundStringLength)
        {
            MethodSymbol opMeth = null;
            TypeSymbol type;
            BoundCall opCall = null;
            
            if (Compilation.Options.IsDialectVO && this.Compilation.Options.VOStringComparisons )
            {
                // VO Style String Comparison
                type = this.GetWellKnownType(WellKnownType.VulcanRTFuncs_Functions, diagnostics, node);
                string methodName = "__StringCompare";
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
                type = this.GetSpecialType(SpecialType.System_String, diagnostics, node);
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
            var type = this.GetWellKnownType(WellKnownType.VulcanRTFuncs_Functions, diagnostics, node);
            var methodName = "__StringEquals";
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

        private BoundExpression BindVOSingleEqualsUsual(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
             BoundExpression left, BoundExpression right)
        {
            MethodSymbol opMeth = null;
            BoundExpression opCall = null;
            var usualType = this.GetWellKnownType(WellKnownType.Vulcan___Usual, diagnostics, node);
            var methodName = "__InexactEquals";
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
            var usualType = this.GetWellKnownType(WellKnownType.Vulcan___Usual, diagnostics, node);
            var methodName = "__InexactNotEquals";
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
            var type = this.GetWellKnownType(WellKnownType.Vulcan_Internal_CompilerServices, diagnostics, node);
            var methodName = "StringSubtract";
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
            BoundExpression left, BoundExpression right, ref int compoundStringLength, VOOperatorType opType)
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
            }
            return null;
        }

        private VOOperatorType NeedsVOOperator(BinaryExpressionSyntax node, BoundExpression left, BoundExpression right)
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
            VOOperatorType opType = VOOperatorType.None; ;
            var xnode = node.XNode as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.BinaryExpressionContext;
            if (xnode == null)  // this may happen for example for nodes generated in the transformation phase
                return opType;
            if (Compilation.Options.IsDialectVO )
            {
                var typeUsual = Compilation.GetWellKnownType(WellKnownType.Vulcan___Usual);
                TypeSymbol leftType = left.Type;
                TypeSymbol rightType = right.Type;
                switch (xnode.Op.Type)
                {
                    case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.EQ:
                        if (left.Type?.SpecialType == SpecialType.System_String && 
                            (right.Type?.SpecialType == SpecialType.System_String || right.Type == typeUsual))
                        {
                            opType = VOOperatorType.SingleEqualsString;
                        }
                        else if (leftType == typeUsual || rightType == typeUsual)
                        {
                            opType = VOOperatorType.SingleEqualsUsual;
                        }
                        break;
                    case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.NEQ:
                    case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.NEQ2:
                        if (leftType == typeUsual || rightType == typeUsual ) // || right.Type?.SpecialType == SpecialType.System_String))
                        {
                            opType = VOOperatorType.NotEqualsUsual;
                        }
                        break;
                    case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.GT:
                    case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.GTE:
                    case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.LT:
                    case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.LTE:
                        if (left.Type == typeUsual || right.Type == typeUsual)
                        {
                            // when LHS or RHS == USUAL then do not compare with CompareString
                            // but let the operator methods inside USUAL handle it.
                            opType = VOOperatorType.None;
                        }
                        else if (left.Type?.SpecialType == SpecialType.System_String || right.Type?.SpecialType == SpecialType.System_String)
                        {
                            // Make to String.Compare or __StringCompare. Decide later
                            opType = VOOperatorType.CompareString;
                        }
                        break;
                    case LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.MINUS:
                        if (left.Type?.SpecialType == SpecialType.System_String)
                        {
                            if (right.Type?.SpecialType == SpecialType.System_String || rightType == typeUsual)
                            {
                                opType = VOOperatorType.SubtractString;
                            }
                        }
                        if (leftType == typeUsual && right.Type?.SpecialType == SpecialType.System_String)
                        {
                            opType = VOOperatorType.SubtractString;
                        }
                        break;
                    default:
                        opType = VOOperatorType.None;
                        break;
                }
            }
            else 
            {
                switch (node.Kind())
                {
                    case SyntaxKind.GreaterThanExpression:
                    case SyntaxKind.GreaterThanOrEqualExpression:
                    case SyntaxKind.LessThanExpression:
                    case SyntaxKind.LessThanOrEqualExpression:
                        opType = VOOperatorType.CompareString;
                        break;
                    default:
                        opType = VOOperatorType.None;
                        break;
                }
            }
            return opType;
        }
    }
}

