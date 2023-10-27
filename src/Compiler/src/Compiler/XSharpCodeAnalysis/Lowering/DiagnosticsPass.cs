//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// This pass detects and reports diagnostics that do not affect lambda convertibility.
    /// This part of the partial class focuses on expression and operator warnings.
    /// </summary>
    internal sealed partial class DiagnosticsPass
    {
        public void GenerateColonWarning(BoundNode node, Symbol symbol, IXParseTree xnode)
        {
            if (xnode is XSharpParser.AccessMemberContext || xnode is XSharpParser.DatatypeContext)
            {
                var text = xnode.GetText().Trim().Replace("::", ".");
                if (text.Contains(":"))
                {
                    Error(ErrorCode.ERR_ColonForTypeOrNs, node, symbol is TypeSymbol ? "Type" : "Namespace", text);
                }
            }
        }

        public override BoundNode VisitTypeExpression(BoundTypeExpression node)
        {
            if (!node.Type.IsFunctionsClass())
            {
                GenerateColonWarning(node, node.Type, node.Syntax.XNode);
            }
            return base.VisitTypeExpression(node);
        }
        public override BoundNode VisitNamespaceExpression(BoundNamespaceExpression node)
        {
            GenerateColonWarning(node, node.NamespaceSymbol, node.Syntax.XNode);
            return base.VisitNamespaceExpression(node);
        }
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
        private void XsCheckMemberAccess(BoundNode node, BoundNode receiver, XSharpParser.AccessMemberContext amc, TypeSymbol type, Symbol symbol)
        {
            var contType = symbol.ContainingType;
            // check for the type to makes sure that an assignment such as
            // CLASS TestClass
            // EXPORT Test1 := DateTime.Now AS DateTime
            // does not trigger an error on the Test1.
            // There is no receiver on Test1, and Roslyn (we?) associates the := DateTime.Now node (EqualsValue Clause) to the Test1 symbol
            if (!node.HasErrors())
            {
                var errorCode = ErrorCode.Unknown;
                switch (amc.Op.Type)
                {
                    case XSharpLexer.COLONCOLON:
                        if (_compilation.Options.Dialect != XSharpDialect.XPP && symbol.IsStatic)
                        {
                            // XPP allows static and instance, other dialects only instance
                            if (string.Compare(symbol.Name, amc.Name.GetText(), true) == 0)
                            {
                                errorCode = ErrorCode.ERR_ColonForStaticMember;
                            }
                        }
                        break;
                    case XSharpLexer.DOT:
                        if (symbol.IsStatic || contType.IsVoStructOrUnion() ||
                            _compilation.Options.HasOption(CompilerOption.AllowDotForInstanceMembers, node.Syntax)
                            )
                        {
                            ; // Ok
                        }
                        else if (string.Compare(symbol.Name, amc.Name.GetText(), true) == 0)
                        {
                            errorCode = ErrorCode.ERR_DotForInstanceMember;
                        }
                        break;
                    case XSharpLexer.COLON:
                        if (symbol.IsStatic && !contType.IsFunctionsClass()) // For late bound code where member access is redirected to function
                        {
                            if (string.Compare(symbol.Name, amc.Name.GetText(), true) == 0)
                            {
                                errorCode = ErrorCode.ERR_ColonForStaticMember;
                            }
                        }
                        break;
                    default:
                        RoslynDebug.Assert(false, $"Unexpected MemberAccess token {XSharpLexer.DefaultVocabulary.GetSymbolicName(amc.Op.Type)}  '{amc.Op.Text}'");
                        break;
                }
                if (errorCode != ErrorCode.Unknown)
                {
                    Error(errorCode, node, symbol, XSharpLexer.DefaultVocabulary.GetSymbolicName(amc.Op.Type), amc.Op.Text);
                }
            }
        }

        private void XsVisitFieldAccess(BoundFieldAccess node)
        {
            if (node.Syntax?.XNode is XSharpParser.AccessMemberContext amc)
            {
                XsCheckMemberAccess(node, node.ReceiverOpt, amc, node.Type, node.FieldSymbol);
            }
            return;
        }
        private void XsVisitPropertyAccess(BoundPropertyAccess node)
        {
            if (node.Syntax?.XNode is XSharpParser.AccessMemberContext amc)
            {
                XsCheckMemberAccess(node, node.ReceiverOpt, amc, node.Type, node.PropertySymbol);
            }
            return;
        }
        private void XsVisitIndexerAccess(BoundIndexerAccess node)
        {
            if (node.Syntax?.XNode is XSharpParser.AccessMemberContext amc)
            {
                XsCheckMemberAccess(node, node.ReceiverOpt, amc, node.Type, node.Indexer);
            }
            return;
        }
        private void XsVisitCall(BoundCall node)
        {
            if (node.Syntax?.XNode is XSharpParser.MethodCallContext mc && mc.Expr is XSharpParser.AccessMemberContext amc)
            {
                if (!node.Method.IsExtensionMethod && !node.Method.IsOperator())
                {
                    // For methods node.Type is the return type of the method
                    XsCheckMemberAccess(node, node.ReceiverOpt, amc, null, node.Method);
                }
            }
            if (node.Method.ParameterRefKinds != null && node.Syntax.XNode is XSharpParser.ICallContext icc)
            {
                var args = icc.Arguments;
                if (args != null)
                {
                    var refkinds = node.Method.ParameterRefKinds;
                    for (var i = 0; i < node.Method.ParameterCount; i++)
                    {
                        if (refkinds[i] == RefKind.Out && args._Args.Count > i)
                        {
                            var arg = args._Args[i];
                            if (arg.RefOut?.Type != XSharpLexer.OUT)
                            {
                                var argnode = node.Arguments[i];
                                Error(ErrorCode.WRN_AutomaticRefGeneration, argnode, i + 1, refkinds[i]);
                            }
                        }
                    }
                }
            }
        }
        public void XsVisitEventAssignmentOperator(BoundEventAssignmentOperator node)
        {
            if (node.Syntax?.XNode is XSharpParser.AssignmentExpressionContext aec &&
                aec.Left is XSharpParser.AccessMemberContext amc)
            {
                XsCheckMemberAccess(node, node.ReceiverOpt, amc, node.Type, node.Event);
            }
            return;
        }
        public void XsVisitEventAccess(BoundEventAccess node)
        {
            if (node.Syntax?.XNode is XSharpParser.AccessMemberContext amc)
            {
                XsCheckMemberAccess(node, node.ReceiverOpt, amc, node.Type, node.EventSymbol);
            }
            return;
        }
        private void XsCheckConversion(BoundConversion node)
        {
            var syntax = node.Syntax;
            var sourceType = node.Operand.Type;
            var targetType = node.Type;
            if (node.Conversion.Kind == ConversionKind.ImplicitUserDefined)
            {
                // Produce a warning when a NULL_PSZ is passed as argument to a function or method that expects a STRING

                if (node.Type.SpecialType == SpecialType.System_String &&
                    node.Operand.Type.IsPszType() &&
                    node.Syntax.XNode is XSharpParserRuleContext context &&
                    context.Start.Type == XSharpParser.NULL_PSZ)
                {
                    if (context.Parent is XSharpParser.NamedArgumentContext ||
                        context.Parent is XSharpParser.UnnamedArgumentContext)
                    {
                        Error(ErrorCode.WRN_NullPszForStringArgument, node);
                    }
                }
                if (node.Type.IsSymbolType() &&
                    node.Operand.Type.IsUsualType() &&
                    node.Syntax.XNode is XSharpParserRuleContext context2 &&
                    context2.Start.Type == XSharpParser.NIL)
                {
                    if (context2.Parent is XSharpParser.NamedArgumentContext ||
                        context2.Parent is XSharpParser.UnnamedArgumentContext||
                        context2.Parent is XSharpParser.AssignmentExpressionContext)
                    {
                        Error(ErrorCode.WRN_ConversionFromNilNotSupported, node, node.Type);
                    }
                }
            }
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
