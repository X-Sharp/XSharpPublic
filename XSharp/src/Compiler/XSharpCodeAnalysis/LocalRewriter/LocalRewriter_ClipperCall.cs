//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class LocalRewriter
    {

        private BoundExpression MakeRefUsual(BoundExpression expr)
        {
            var usualType = _compilation.UsualType();
            var ctors = usualType.GetMembers(".ctor");
            MethodSymbol ctor = null;
            foreach (var c in ctors)
            {
                if (c.GetParameterCount() == 2)
                {
                    var types = c.GetParameterTypes();
                    if (types[0] == usualType && types[1].SpecialType == SpecialType.System_Boolean)
                    {
                        ctor = (MethodSymbol)c;
                        break;
                    }
                }
            }
            if (ctor != null)
            {
                return new BoundObjectCreationExpression(expr.Syntax, ctor, null, expr, _makeLogic(expr.Syntax, true));
            }
            return expr;
        }

        private void checkRefKinds(ImmutableArray<BoundExpression> arguments, RefKind[] refKinds,
                                   ImmutableArray<BoundExpression>.Builder exprs,
                                   ImmutableArray<BoundExpression>.Builder args,
                                   ImmutableArray<LocalSymbol>.Builder temps,
                                   BoundLocal[] argBoundTemps,
                                   BoundPropertyAccess[] boundProperties,
                                   out bool addressOfChangedToRef)
        {
            exprs.Clear();
            addressOfChangedToRef = false;
            for (int i = 0; i < arguments.Length; i++)
            {
                // Check for @var when ImplicitCastsAndConversions is enabled (/vo7) and dialect allows AddressOf
                var a = arguments[i];
                if (a.Kind == BoundKind.AddressOfOperator && !_compilation.Options.Dialect.AddressOfIsAlwaysByRef() )
                {
                    if (_compilation.Options.HasOption(CompilerOption.ImplicitCastsAndConversions, a.Syntax))
                    {
                        var be = a as BoundAddressOfOperator;
                        a = be.Operand;
                        bool makeRef = true;
                        if (be.Operand is BoundLocal bl)
                        {
                            var decl = bl.LocalSymbol.DeclaringSyntaxReferences[0].GetSyntax();
                            if (decl?.XNode is XSharpParser.LocalvarContext lvc)
                            {
                                makeRef = lvc.As?.Type != XSharpParser.IS;
                            }
                        }
                        if (be.Operand is BoundFieldAccess bfa)
                        {
                            var decl = bfa.FieldSymbol.DeclaringSyntaxReferences[0].GetSyntax();
                            if (decl?.XNode is XSharpParser.ClassvarContext cvc)
                            {
                                var parent = cvc.Parent as XSharpParser.ClassVarListContext;
                                makeRef = parent.As?.Type != XSharpParser.IS;
                            }
                            if (decl?.XNode is XSharpParser.VostructmemberContext vmc)
                            {
                                makeRef = vmc.As?.Type != XSharpParser.IS;
                            }
                        }

                        if (makeRef)
                        { 
                            refKinds[i] = RefKind.Ref;
                            addressOfChangedToRef = true;
                        }
                    }
                }

                if (refKinds[i].IsWritableReference())
                {
                    bool normalArg = true;
                    while (a is BoundConversion c) a = c.Operand;
                    if (a is BoundPropertyAccess bp && bp.PropertySymbol is XsVariableSymbol)
                    {
                        // no need for conversion here because everything is a USUAL
                        boundProperties[i] = bp;
                        var newarg = VisitExpression(arguments[i]);
                        args.Add(MakeRefUsual(newarg));
                        normalArg = false;
                    }
                    else if ( a is BoundDynamicMemberAccess dma && dma.Syntax?.XNode is XSharpParser.AccessMemberContext amc && amc.IsFox)
                    {
                        ImmutableArray<Symbol> get, set;
                        var rtType = _compilation.RuntimeFunctionsType();
                        get = rtType.GetMembers(ReservedNames.FieldGetWaUndeclared);
                        set = rtType.GetMembers(ReservedNames.FieldSetWaUndeclared);
                        var varSym = new XsFoxMemberAccessSymbol(amc.AreaName, amc.FieldName, (MethodSymbol) get[0], (MethodSymbol)set[0], _compilation.UsualType());
                        if (get.Length > 0 && set.Length > 0)
                        {
                            boundProperties[i] = new BoundPropertyAccess(a.Syntax, null, varSym, LookupResultKind.Viable, varSym.Type);
                        }
                    }
                    if (normalArg)
                    {
                        a = VisitExpression(a);
                        LocalSymbol la = _factory.SynthesizedLocal(a.Type, refKind: refKinds[i]);
                        temps.Add(la);
                        BoundLocal bla = _factory.Local(la);
                        var lasgn = _factory.AssignmentExpression(bla, a, isRef: true);
                        exprs.Add(VisitAssignmentOperator(lasgn, true));
                        var newarg = MakeConversionNode(bla, arguments[i].Type, false);
                        args.Add(MakeRefUsual(newarg));
                        argBoundTemps[i] = bla;
                    }
                }
                else
                {
                    args.Add(VisitExpression(arguments[i]));
                }
            }
        }

        private void writeRefBack(ImmutableArray<BoundExpression> arguments, RefKind[] refKinds,
            BoundExpression boundPars, BoundLocal[] argBoundTemps, BoundPropertyAccess[] boundProperties, ImmutableArray<BoundExpression>.Builder exprs)
        {
            exprs.Clear();
            for (int i = 0; i < arguments.Length; i++)
            {
                if (refKinds[i].IsWritableReference())
                {
                    BoundExpression idx = _factory.Literal(i);
                    var elem = _factory.ArrayAccess(boundPars, ImmutableArray.Create(idx));
                    if (boundProperties[i] != null)
                    {
                        if (boundProperties[i].PropertySymbol is XsFoxMemberAccessSymbol foxAccess)
                        {
                            // fore now handle special FoxPro member assign here
                            var arg = arguments[i].Syntax;
                            var method = foxAccess.SetMethod;
                            var alias = _makeString(arg, foxAccess.Alias);
                            var field = _makeString(arg, foxAccess.Name);
                            var undecl = _makeLogic(arg, _compilation.Options.HasOption(CompilerOption.UndeclaredMemVars, arguments[0].Syntax));
                            var args = ImmutableArray.Create<BoundExpression>(alias, field, elem, undecl);
                            var call = new BoundCall(arg, null, method, arguments: args,
                                argumentNamesOpt: default,
                                argumentRefKindsOpt: default,
                                expanded: false,
                                argsToParamsOpt: default,
                                binderOpt: null,
                                isDelegateCall: false,
                                invokedAsExtensionMethod: false,
                                resultKind: LookupResultKind.Viable,
                                type: method.ReturnType,
                                hasErrors: false);
                            exprs.Add(call);
                        }
                        else
                        {
                            // no needs to convert. The param is a usual and the property as well
                            BoundExpression ass = _factory.AssignmentExpression(boundProperties[i], elem);
                            ass = VisitExpression(ass);
                            exprs.Add(ass);
                        }
                    }
                    else
                    {
                        var a = argBoundTemps[i];
                        var conv = MakeConversionNode(elem, a.Type, false);
                        var asgn = _factory.AssignmentExpression(a, conv);
                        exprs.Add(asgn);
                    }
                }
            }

        }

        internal BoundExpression RewriteLateBoundCallWithRefParams(BoundExpression loweredReceiver, string name, BoundDynamicInvocation node, ImmutableArray<BoundExpression> arguments)
        {
            
            var convArgs = new ArrayBuilder<BoundExpression>();
            var usualType = _compilation.UsualType();
            foreach (var a in arguments)
            {

                if (a.Type == null && !a.Syntax.XIsCodeBlock)
                    convArgs.Add(new BoundDefaultExpression(a.Syntax, usualType));
                else
                    convArgs.Add(MakeConversionNode(a, usualType, false));
            }
            var aArgs = _factory.Array(usualType, convArgs.ToImmutableAndFree());

             // Note: Make sure the first parameter in __InternalSend() in the runtime is a USUAL!
            var expr = _factory.StaticCall(_compilation.RuntimeFunctionsType(), ReservedNames.InternalSend,
                        MakeConversionNode(loweredReceiver, usualType, false),
                        new BoundLiteral(loweredReceiver.Syntax, ConstantValue.Create(name), _compilation.GetSpecialType(SpecialType.System_String)),
                        aArgs);

            if (expr is BoundCall bc && bc.Arguments.Length == 3)       // object, name, param array
            {
                // we can't use the code for early bound code because we our param array is already processed.

                var temps = ImmutableArray.CreateBuilder<LocalSymbol>();
                var exprs = ImmutableArray.CreateBuilder<BoundExpression>();
                var args = ImmutableArray.CreateBuilder<BoundExpression>(arguments.Length);
                var argBoundTemps = new BoundLocal[arguments.Length];
                var refKinds = new RefKind[arguments.Length];
                var properties = new BoundPropertyAccess[arguments.Length];      // for xsVariableSymbols
                var argumentRefKindsOpt = node.ArgumentRefKindsOpt;
                for (int i = 0; i < arguments.Length; i++)
                {
                    var r = (!argumentRefKindsOpt.IsDefaultOrEmpty && i < argumentRefKindsOpt.Length) ? argumentRefKindsOpt[i] : RefKind.None;
                    refKinds[i] = r;
                }

                // keep track of the locals that need to be assigned back
                checkRefKinds(arguments, refKinds, exprs, args, temps, argBoundTemps, properties, out var addressOfChangedToRef);
                var preExprs = exprs.ToImmutable();
                exprs.Clear();

                // the usual array is in the 3rd parameter to the bound call
                var actualargs = bc.Arguments;
                var argsNode = actualargs[2] as BoundArrayCreation;

                if (addressOfChangedToRef)
                {
                    var initializer = argsNode.InitializerOpt;
                    var expressions = initializer.Initializers;
                    for (int i = 0; i < expressions.Length; i++)
                    {
                        // note that an expression like @USUAL will be replaced already in the Initializers array
                        // with a call to an implicit operator. So we take the original argument here.
                        var e = expressions[i];
                        var a = arguments[i];
                        if (a is BoundAddressOfOperator bop)
                        {
                            exprs.Add(bop.Operand);
                        }
                        else
                        {
                            exprs.Add(e);
                        }
                    }
                    initializer = initializer.Update(exprs.ToImmutable());
                    argsNode = argsNode.Update(argsNode.Bounds, initializer, argsNode.Type);
                }

                LocalSymbol parsTemp = _factory.SynthesizedLocal(argsNode.Type);
                BoundExpression boundPars = _factory.Local(parsTemp);
                temps.Add(parsTemp);
                BoundExpression parsAssignment = _factory.AssignmentExpression(boundPars, argsNode);

                // local temp 
                // __InternalSend( oObject, cName, temp = paramArray)

                var rewrittenArgs = ImmutableArray.CreateBuilder<BoundExpression>();
                // replace last argument node with assignment node
                rewrittenArgs.Add(actualargs[0]);
                rewrittenArgs.Add(actualargs[1]);
                rewrittenArgs.Add(parsAssignment);
                bc = bc.Update(bc.ReceiverOpt, bc.Method, rewrittenArgs.ToImmutableArray());



                // result = __InternalSend(oObject, methodName, params)
                LocalSymbol callTemp = _factory.SynthesizedLocal(bc.Type);
                temps.Add(callTemp);
                BoundLocal boundCallTemp = _factory.Local(callTemp);
                BoundExpression callAssignment = _factory.AssignmentExpression(boundCallTemp, bc);

                // generate statements that assign param array elements back to the locals

                writeRefBack(arguments, refKinds, boundPars, argBoundTemps, properties, exprs);
                var postExprs = exprs.ToImmutable();
                exprs.Clear();

                exprs.AddRange(preExprs);
                exprs.Add(callAssignment);
                exprs.AddRange(postExprs);

                // the sequence now contains:
                // save vars
                // temp = paramArray
                // result = __INternalSend()
                // assign array elements from temp back to local vars
                // return result
                return new BoundSequence(node.Syntax, temps.ToImmutable(), exprs.ToImmutableArray(), boundCallTemp, node.Type);

            }
            return expr;

        }

   
        internal BoundNode VisitCtorCallClipperConvention(BoundObjectCreationExpression node)
        {
            if (node.Arguments.Length > 0 && node.Syntax is ObjectCreationExpressionSyntax oces)
            {
                var xnode = (oces.XNode as XSharpParser.PrimaryExpressionContext)?.Expr as XSharpParser.CtorCallContext;
                if (xnode == null || !xnode.HasRefArguments)
                    return null;
                if (!node.Constructor.HasClipperCallingConvention())
                    return null;
            }
            else
                return null;

            var argumentRefKindsOpt = node.ArgumentRefKindsOpt;
            ImmutableArray<LocalSymbol> temps;
            ImmutableArray<BoundExpression> preExprs;
            ImmutableArray<BoundExpression> postExprs;
            var rewrittenArguments = MakeClipperCallArguments(node.Syntax, node.Arguments, node.Constructor, node.Expanded, node.ArgsToParamsOpt,
                                                                ref argumentRefKindsOpt, out temps, false, out preExprs, out postExprs);

            LocalSymbol objTemp = _factory.SynthesizedLocal(node.Type);
            BoundLocal boundObjTemp = _factory.Local(objTemp);
            BoundExpression rewrittenObjectCreation = node.UpdateArgumentsAndInitializer(rewrittenArguments, argumentRefKindsOpt, newInitializerExpression: null, changeTypeOpt: node.Constructor.ContainingType);
            if (node.Type.IsInterfaceType())
            {
                Debug.Assert(rewrittenObjectCreation.Type == ((NamedTypeSymbol)node.Type).ComImportCoClass);
                rewrittenObjectCreation = MakeConversionNode(rewrittenObjectCreation, node.Type, false, false);
            }
            BoundExpression objectAssignment = _factory.AssignmentExpression(boundObjTemp, rewrittenObjectCreation);
            var exprs = preExprs.Add(objectAssignment).AddRange(postExprs);

            rewrittenObjectCreation = new BoundSequence(node.Syntax, temps.Add(objTemp), exprs, boundObjTemp, node.Type);
            if (node.InitializerExpressionOpt == null || node.InitializerExpressionOpt.HasErrors)
            {
                return rewrittenObjectCreation;
            }
            return MakeObjectCreationWithInitializer(node.Syntax, rewrittenObjectCreation, node.InitializerExpressionOpt, node.Type);
        }

        internal BoundNode VisitCallClipperConvention(BoundCall node)
        {
            // some generated nodes do not have an invocation expression syntax node
            if (node.Arguments.Length > 0 && node.Syntax is InvocationExpressionSyntax ies)
            {
                var mcall = ies.XNode as XSharpParser.MethodCallContext;
                var dostmt = ies.XNode as XSharpParser.DoStmtContext;
                if (mcall == null && dostmt == null)
                    return null;
                if (mcall != null && !mcall.HasRefArguments)
                    return null;
                if (dostmt != null && !dostmt.HasRefArguments)
                    return null;

                if (!node.Method.HasClipperCallingConvention())
                    return null;
            }
            else
                return null;

            BoundExpression rewrittenReceiver = VisitExpression(node.ReceiverOpt);

            var argumentRefKindsOpt = node.ArgumentRefKindsOpt;
            ImmutableArray<LocalSymbol> temps;
            ImmutableArray<BoundExpression> preExprs;
            ImmutableArray<BoundExpression> postExprs;
            ImmutableArray<BoundExpression> exprs;
            var rewrittenArguments = MakeClipperCallArguments(node.Syntax, node.Arguments, node.Method, node.Expanded, node.ArgsToParamsOpt,
                                                            ref argumentRefKindsOpt, out temps, node.InvokedAsExtensionMethod, out preExprs, out postExprs);
            var mustassign = true;
            var nodeType = node.Type;
            if (nodeType.SpecialType == SpecialType.System_Void)
            {
                mustassign = false;
                nodeType = _compilation.GetSpecialType(SpecialType.System_Int32);
            }

            LocalSymbol callTemp = _factory.SynthesizedLocal(nodeType);
            BoundLocal boundCallTemp = _factory.Local(callTemp);
            var call = MakeCall(node, node.Syntax, rewrittenReceiver, node.Method, rewrittenArguments, argumentRefKindsOpt, node.InvokedAsExtensionMethod, node.ResultKind, node.Type);
            if (mustassign)
            {
                BoundExpression callAssignment = _factory.AssignmentExpression(boundCallTemp, call);
                exprs = preExprs.Add(callAssignment).AddRange(postExprs);
            }
            else
            {
                exprs = preExprs.Add(call).AddRange(postExprs);

            }

        
            return new BoundSequence(node.Syntax, temps.Add(callTemp), exprs, boundCallTemp, node.Type);
        }

        internal ImmutableArray<BoundExpression> MakeClipperCallArguments(SyntaxNode syntax, ImmutableArray<BoundExpression> arguments, MethodSymbol method, bool expanded, ImmutableArray<int> argsToParamsOpt,
            ref ImmutableArray<RefKind> argumentRefKindsOpt, out ImmutableArray<LocalSymbol> temps, bool invokeAsExtensionMethod, out ImmutableArray<BoundExpression> preExprs, out ImmutableArray<BoundExpression> postExprs)
        {
            var argTemps = ImmutableArray.CreateBuilder<LocalSymbol>();
             
            var exprs = ImmutableArray.CreateBuilder<BoundExpression>();
            var args = ImmutableArray.CreateBuilder<BoundExpression>(arguments.Length);
            var argBoundTemps = new BoundLocal[arguments.Length];
            var refKinds = new RefKind[arguments.Length];
            var properties = new BoundPropertyAccess[arguments.Length];      // for xsVariableSymbols

            for (int i = 0; i < arguments.Length; i++)
            {
                var r = (!argumentRefKindsOpt.IsDefaultOrEmpty && i < argumentRefKindsOpt.Length) ? argumentRefKindsOpt[i] : RefKind.None;
                refKinds[i] = r;
            }

            checkRefKinds(arguments, refKinds, exprs, args, argTemps, argBoundTemps, properties, out var addressOfChangedToRef);

            var rewrittenArgumentRefKindsOpt = argumentRefKindsOpt;
            var rewrittenArguments = args.ToImmutable();
            ImmutableArray<LocalSymbol> aTemps;
            rewrittenArguments = MakeArguments(syntax, rewrittenArguments, method, method, expanded, argsToParamsOpt, ref rewrittenArgumentRefKindsOpt, out aTemps, invokeAsExtensionMethod);
            argTemps.AddRange(aTemps);

            var argsNode = rewrittenArguments[rewrittenArguments.Length - 1];
            LocalSymbol parsTemp = _factory.SynthesizedLocal(argsNode.Type);
            BoundExpression boundPars = _factory.Local(parsTemp);
            BoundExpression parsAssignment = _factory.AssignmentExpression(boundPars, argsNode);
            rewrittenArguments = rewrittenArguments.RemoveAt(rewrittenArguments.Length - 1).Add(parsAssignment);
            argTemps.Add(parsTemp);

            preExprs = exprs.ToImmutable();

            writeRefBack(arguments, refKinds, boundPars, argBoundTemps, properties, exprs);

            argumentRefKindsOpt = rewrittenArgumentRefKindsOpt;
            temps = argTemps.ToImmutable();
            postExprs = exprs.ToImmutable();
            return rewrittenArguments;
        }
    }
}

