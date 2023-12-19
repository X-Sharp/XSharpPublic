//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System.Collections.Immutable;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal sealed partial class LocalRewriter
    {
        private bool IsFoxAccessMember(BoundExpression loweredReceiver, IXParseTree xNode, out string areaName)
        {
            areaName = null;
            if (_compilation.Options.Dialect == XSharpDialect.FoxPro)
            {
                // only do this when not bound to a field/property in the current type
                // in that case BoundCall is usually a VarGet()
                if (loweredReceiver is BoundCall bc && bc.ReceiverOpt == null)
                {
                    if (xNode is XSharpParser.AccessMemberContext amc && amc.IsFox)
                    {
                        if (loweredReceiver is BoundCall && amc.Expr is XSharpParser.PrimaryExpressionContext pc
                            && pc.Expr is XSharpParser.NameExpressionContext)
                        {
                            areaName = amc.AreaName;
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        public BoundExpression MakeVODynamicGetMember(BoundExpression loweredReceiver, BoundDynamicMemberAccess node)
        {
            // check for FoxPro late access, such as Customer.LastName
            string name = node.Name;
            var syntax = loweredReceiver.Syntax;
            var allowLB = _compilation.Options.LateBindingOrFox(syntax);
            if (!allowLB || loweredReceiver.HasDynamicType())
                return null;
            _factory.Syntax = syntax;
            var nameExpr = _factory.Literal(name);
            if (IsFoxAccessMember(loweredReceiver, node.Syntax.XNode, out var areaName))
            {
                string method = ReservedNames.FieldGetWaUndeclared;
                var exprUndeclared = _factory.Literal(_compilation.Options.HasOption(CompilerOption.UndeclaredMemVars, syntax));
                var areaExpr = _factory.Literal(areaName);
                var expr = _factory.StaticCall(_compilation.RuntimeFunctionsType(), method, areaExpr, nameExpr, exprUndeclared);
                return expr;
            }
            var constructedFrom = ((NamedTypeSymbol)loweredReceiver.Type).ConstructedFrom;
            if (!allowLateBound(syntax, constructedFrom))
                return null;
            if (!_compilation.Options.HasOption(CompilerOption.LateBinding, syntax))
            {
                _diagnostics.Add(new CSDiagnostic(new CSDiagnosticInfo(ErrorCode.WRN_UndeclaredMember, constructedFrom, name, "property", "access"), loweredReceiver.Syntax.Location));
            }

            var usualType = _compilation.UsualType();
            if (TypeSymbol.Equals(constructedFrom, usualType))
            {
                loweredReceiver = _factory.StaticCall(usualType, ReservedNames.ToObject, loweredReceiver);
            }
            loweredReceiver = MakeConversionNode(loweredReceiver, _compilation.GetSpecialType(SpecialType.System_Object), false);
            return _factory.StaticCall(_compilation.RuntimeFunctionsType(), ReservedNames.IVarGet, loweredReceiver, nameExpr);
        }

        bool allowLateBound(SyntaxNode syntax, TypeSymbol type)
        {
            bool allowLB ;
            if (_compilation.Options.Dialect.AllowLateBindingForTypesWithLateBindingAttribute() && type.HasLateBindingAttribute() )
            {
                allowLB = true;
            }
            else
            {
                allowLB = _compilation.Options.HasOption(CompilerOption.LateBinding, syntax);
            }
            return allowLB;
        }

        public BoundExpression MakeVODynamicSetMember(BoundExpression loweredReceiver, BoundDynamicMemberAccess node, BoundExpression loweredValue)
        {
            string name = node.Name;
            var syntax = loweredReceiver.Syntax;
            _factory.Syntax = syntax;
            var allowLB = _compilation.Options.LateBindingOrFox(syntax);
            if (!allowLB || loweredReceiver.HasDynamicType())
                return null;
            var usualType = _compilation.UsualType();
            var value = loweredValue.Type is null ? new BoundDefaultExpression(syntax, usualType)
                : MakeConversionNode(loweredValue, usualType, false);
            var nameExpr = _factory.Literal(name);
            if (IsFoxAccessMember(loweredReceiver, node.Syntax.XNode, out var areaName))
            {
                string method = ReservedNames.FieldSetWaUndeclared;
                var exprUndeclared = _factory.Literal(_compilation.Options.HasOption(CompilerOption.UndeclaredMemVars, syntax));
                var areaExpr = _factory.Literal(areaName);
                var expr = _factory.StaticCall(_compilation.RuntimeFunctionsType(), method, areaExpr, nameExpr, value, exprUndeclared);
                return expr;
            }

            var constructedFrom = ((NamedTypeSymbol)loweredReceiver.Type).ConstructedFrom;
            if (!allowLateBound(syntax, constructedFrom))
            {
                return null;
            }
            if (!_compilation.Options.HasOption(CompilerOption.LateBinding, syntax))
            {
                _diagnostics.Add(new CSDiagnostic(new CSDiagnosticInfo(ErrorCode.WRN_UndeclaredMember, constructedFrom, name, "property", "assign"), loweredReceiver.Syntax.Location));
            }
            if (constructedFrom.IsUsualType())
            {
                loweredReceiver = _factory.StaticCall(usualType, ReservedNames.ToObject, loweredReceiver);
            }
            loweredReceiver = MakeConversionNode(loweredReceiver, _compilation.GetSpecialType(SpecialType.System_Object), false);
            return _factory.StaticCall(_compilation.RuntimeFunctionsType(), ReservedNames.IVarPut, loweredReceiver, nameExpr, value);


        }

        public BoundExpression MakeVODynamicInvokeMember(BoundExpression loweredReceiver, string name, BoundDynamicInvocation node, ImmutableArray<BoundExpression> args)
        {

            if (!allowLateBound(loweredReceiver.Syntax, loweredReceiver.Type))
                return null;
            if (loweredReceiver.HasDynamicType())
                return null;
            if (loweredReceiver.Type.IsArrayType())
            {
                if (_compilation.Options.Dialect.AllowASend())
                {
                    return MakeASend(loweredReceiver, name, args);
                }
                // This should not happen because we are not converting the method call to a dynamic call, but better safe than sorry.
                return null;
            }
            // for a method call the hierarchy is:
            // loweredReceiver = object
            // loweredReceiver.Parent = MemberAccess
            // loweredReceiver.Parent.Parent = InvocationExpression
            // loweredReceiver.Parent.Parent.Syntax.XNode = MethodCallContext
            //
            var syntax = node.Syntax;
            _factory.Syntax = syntax;
            var xnode = syntax.XNode as XSharpParser.MethodCallContext;
            if (!_compilation.Options.HasOption(CompilerOption.LateBinding, syntax))
            {
                _diagnostics.Add(new CSDiagnostic(new CSDiagnosticInfo(ErrorCode.WRN_UndeclaredMember, loweredReceiver.Type, name, "method", "call"), syntax.Location));
            }

            if (xnode != null && xnode.HasRefArguments)
            {
                return RewriteLateBoundCallWithRefParams(loweredReceiver, name, node, args);
            }

            var convArgs = new ArrayBuilder<BoundExpression>();
            var usualType = _compilation.UsualType();
            foreach (var a in args)
            {
                if (a.Kind == BoundKind.UnboundLambda)
                {
                    _diagnostics.Add(ErrorCode.ERR_LambdaConversionNotPossible, syntax.Location, usualType);
                }
                else if (a.Type is null && !a.Syntax.XIsCodeBlock)
                {
                    convArgs.Add(_factory.Default(usualType));
                }
                else
                {
                    convArgs.Add(MakeConversionNode(a, usualType, false));
                }
            }
            var aArgs = _factory.Array(usualType, convArgs.ToImmutableAndFree());
            // Note: Make sure the first parameter in __InternalSend() in the runtime is a USUAL!
            loweredReceiver = MakeConversionNode(loweredReceiver, usualType, false);
            return _factory.StaticCall(_compilation.RuntimeFunctionsType(), ReservedNames.InternalSend,
                    loweredReceiver,
                    _factory.Literal(name),
                    aArgs);

        }

        public BoundExpression MakeASend(BoundExpression loweredReceiver, string name, ImmutableArray<BoundExpression> args)
        {
            var convArgs = new ArrayBuilder<BoundExpression>();
            _factory.Syntax = loweredReceiver.Syntax;

            var usualType = _compilation.UsualType();
            foreach (var a in args)
            {
                if (a.Type is null)
                {
                    convArgs.Add(_factory.Default(usualType));
                }
                else
                {
                    convArgs.Add(MakeConversionNode(a, usualType, false));
                }
            }
            var aArgs = _factory.Array(usualType, convArgs.ToImmutableAndFree());
            loweredReceiver = MakeConversionNode(loweredReceiver, _compilation.ArrayType(), false);
            var expr = _factory.StaticCall(_compilation.RuntimeFunctionsType(), ReservedNames.ASend,
                    loweredReceiver,
                    _factory.Literal(name),
                    aArgs);
            _diagnostics.Add(new CSDiagnostic(new CSDiagnosticInfo(ErrorCode.WRN_ASend, name), loweredReceiver.Syntax.Location));
            return expr;
        }
    }
}
