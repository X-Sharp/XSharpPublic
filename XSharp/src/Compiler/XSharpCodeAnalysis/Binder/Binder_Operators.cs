using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
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
            UsualDate,
            Shift
        }
        private BoundExpression BindVOCompareString(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
            BoundExpression left, BoundExpression right, ref int compoundStringLength)
        {
            MethodSymbol opMeth = null;
            TypeSymbol type;
            BoundCall opCall = null;

            if (Compilation.Options.IsDialectVO && this.Compilation.Options.VOStringComparisons)
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

        private BoundExpression BindVOUsualDate(BinaryExpressionSyntax node, DiagnosticBag diagnostics,
            BoundExpression left, BoundExpression right)
        {
            var usualType = this.GetWellKnownType(WellKnownType.Vulcan___Usual, diagnostics, node);
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
                case VOOperatorType.UsualDate:
                    return BindVOUsualDate(node, diagnostics, left, right);
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
            VOOperatorType opType = VOOperatorType.None;
            var xnode = node.XNode as XSharpParser.BinaryExpressionContext;
            if (xnode == null)  // this may happen for example for nodes generated in the transformation phase
                return opType;
            if (Compilation.Options.IsDialectVO)
            {
                var typeUsual = Compilation.GetWellKnownType(WellKnownType.Vulcan___Usual);
                NamedTypeSymbol typeDate;
                TypeSymbol leftType = left.Type;
                TypeSymbol rightType = right.Type;
                switch (xnode.Op.Type)
                {
                    case XSharpParser.EQ:
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
                    case XSharpParser.NEQ:
                    case XSharpParser.NEQ2:
                        if (leftType == typeUsual || rightType == typeUsual) // || right.Type?.SpecialType == SpecialType.System_String))
                        {
                            opType = VOOperatorType.NotEqualsUsual;
                        }
                        break;
                    case XSharpParser.GT:
                    case XSharpParser.GTE:
                    case XSharpParser.LT:
                    case XSharpParser.LTE:
                        if (left.Type == typeUsual || right.Type == typeUsual)
                        {
                            // when LHS or RHS == USUAL then do not compare with CompareString
                            // but let the operator methods inside USUAL handle it.
                            opType = VOOperatorType.None;
                        }
                        else if (left.Type?.SpecialType == SpecialType.System_String || right.Type?.SpecialType == SpecialType.System_String)
                        {
                            // Convert to String.Compare or __StringCompare. Decide later
                            opType = VOOperatorType.CompareString;
                        }
                        break;
                    case XSharpParser.MINUS:
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
                        // usual - date
                        // date - usual
                        typeDate = Compilation.GetWellKnownType(WellKnownType.Vulcan___VODate);
                        if (leftType == typeUsual && right.Type == typeDate)
                            opType = VOOperatorType.UsualDate;
                        if (leftType == typeDate && right.Type == typeUsual)
                            opType = VOOperatorType.UsualDate;
                        break;
                    case XSharpParser.ADD:
                        // usual + date
                        // date + usual
                        typeDate = Compilation.GetWellKnownType(WellKnownType.Vulcan___VODate);
                        if (leftType == typeUsual && right.Type == typeDate)
                            opType = VOOperatorType.UsualDate;
                        if (leftType == typeDate && right.Type == typeUsual)
                            opType = VOOperatorType.UsualDate;
                        break;
                    default:
                        switch (node.Kind())
                        {
                            case SyntaxKind.RightShiftExpression:
                            case SyntaxKind.LeftShiftExpression:
                            case SyntaxKind.RightShiftAssignmentExpression:
                            case SyntaxKind.LeftShiftAssignmentExpression:
                                opType = VOOperatorType.Shift;
                                break;
                            default:
                                opType = VOOperatorType.None;
                                break;
                        }
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
                        if (left.Type?.SpecialType == SpecialType.System_String || right.Type?.SpecialType == SpecialType.System_String)
                        {
                            // Make to String.Compare or __StringCompare. Decide later
                            opType = VOOperatorType.CompareString;
                        }
                        break;
                    case SyntaxKind.RightShiftExpression:
                    case SyntaxKind.LeftShiftExpression:
                    case SyntaxKind.RightShiftAssignmentExpression:
                    case SyntaxKind.LeftShiftAssignmentExpression:
                        opType = VOOperatorType.Shift;
                        break;

                }

            }
            return opType;
        }
        private void AdjustVOUsualLogicOperands(BinaryExpressionSyntax node, ref BoundExpression left, ref BoundExpression right, DiagnosticBag diagnostics)
        {
            if (!Compilation.Options.IsDialectVO)
                return;
            var xnode = node.XNode as XSharpParser.BinaryExpressionContext;
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
                    var usualType = this.GetWellKnownType(WellKnownType.Vulcan___Usual, diagnostics, node);
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
                var opKind = left.Type.SpecialType == SpecialType.System_Int32 ? BinaryOperatorKind.IntSubtraction
                    : left.Type.SpecialType == SpecialType.System_Int64 ? BinaryOperatorKind.LongSubtraction
                    : left.Type.SpecialType == SpecialType.System_UInt32 ? BinaryOperatorKind.UIntSubtraction
                    : BinaryOperatorKind.ULongSubtraction;
                var resultConstant = FoldBinaryOperator(index.Syntax, opKind, left, right, left.Type.SpecialType, diagnostics, ref compoundStringLength);
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
            if (expr.Kind == BoundKind.Literal)
            {
                var lit = expr as BoundLiteral;
                if (lit.ConstantValue.Discriminator == ConstantValueTypeDiscriminator.Int32)
                {
                    var val = lit.ConstantValue.Int32Value;
                    if (val > Byte.MinValue && val < Byte.MaxValue)
                        return Compilation.GetSpecialType(SpecialType.System_Byte);
                    else if (val > Int16.MinValue && val < Int16.MaxValue)
                        return Compilation.GetSpecialType(SpecialType.System_Int16);
                }
            }
            else if (expr.Kind ==BoundKind.UnaryOperator)
            {
                var unary = expr as BoundUnaryOperator;
                return VOGetType(unary.Operand);
            }
            return expr.Type;
        }
        public void VODetermineIIFTypes(ConditionalExpressionSyntax node, DiagnosticBag diagnostics,
            ref BoundExpression trueExpr, ref BoundExpression falseExpr, 
            ref TypeSymbol trueType, ref TypeSymbol falseType)
        {
            // do nothing when the types are equal or null
            if (trueType != null && falseType != null && trueType != falseType)
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

                if (trueType != falseType && Compilation.Options.IsDialectVO)
                {
                    // convert to usual when one of the two is a usual
                    var usualType = GetWellKnownType(WellKnownType.Vulcan___Usual, diagnostics, node);
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
                if (trueType != falseType && Compilation.Options.VOCompatibleIIF)
                {
                    // convert to object when Compatible IIF is activated
                    // this will not happen for VO Dialect because that is handled above
                    var objectType = Compilation. GetSpecialType(SpecialType.System_Object);
                    trueExpr = CreateConversion(trueExpr, objectType, diagnostics);
                    falseExpr = CreateConversion(falseExpr, objectType, diagnostics);
                    trueType = falseType = objectType;
                }
            }
        }
    }
}