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
using System.Linq;
using System.Reflection;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using static LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// This portion of the binder converts an <see cref="ExpressionSyntax"/> into a <see cref="BoundExpression"/>.
    /// </summary>
    internal partial class Binder
    {
        private static InitializerExpressionSyntax s_constructInitializerFromArguments(AnalyzedArguments analyzedArguments)
        {
            var expressions = SeparatedSyntaxListBuilder<ExpressionSyntax>.Create();
            foreach (var arg in analyzedArguments.Arguments)
            {
                if (expressions.Count > 0)
                    expressions.AddSeparator(SyntaxFactory.MissingToken(SyntaxKind.CommaToken));
                expressions.Add(arg.Syntax as ExpressionSyntax);
            }
            return SyntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression,
                expressions);
        }

        private bool BindVulcanPointerDereference(CastExpressionSyntax node, TypeSymbol targetType, BoundExpression operand, 
            DiagnosticBag diagnostics, out BoundExpression expression)
        {
            // Type(pPointer) -> Dereference pointer
            // Vulcan only allows this with pPointer is of tyope PTR (Void pointer)
            if (node.XNode is PrimaryExpressionContext)
            {
                PrimaryExpressionContext pe = (PrimaryExpressionContext)node.XNode;
                if (pe.Expr is VoConversionExpressionContext && operand.Type.IsPointerType())
                {
                    // Dereference pointer
                    // only allowed when source is Void Ptr
                    // or source is <TargetType> PTR
                    // Convert INT(<ptr>) to ((INT PTR) <ptr>)[0]
                    // No need to worry about /AZ. This has been handled already
                    // make sure that PSZ(ptr) is not dereferenced !
                    bool canConvert = operand.Type.IsVoidPointer() && targetType != Compilation.PszType();
                    if ( ! canConvert)
                    {
                        PointerTypeSymbol pt = operand.Type as PointerTypeSymbol;
                        canConvert = pt.PointedAtType == targetType;
                    }
                    if (canConvert)
                    {
                        var index = new BoundLiteral(node, ConstantValue.Create(0), Compilation.GetSpecialType(SpecialType.System_Int32)) { WasCompilerGenerated = true };
                        var ptrtype = new PointerTypeSymbol(targetType);
                        HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                        //var newConv = Conversions.ClassifyConversionForCast(operand, ptrtype, ref useSiteDiagnostics);
                        var newConv = Conversions.ClassifyConversionFromExpression(operand, ptrtype, ref useSiteDiagnostics, forCast: true);
                        var ptrconv = new BoundConversion(node, operand, newConv, true, true, null, ptrtype) { WasCompilerGenerated = true };
                        expression = new BoundPointerElementAccess(node, ptrconv, index, false, targetType) { WasCompilerGenerated = true };
                        return true;
                    }
                }
            }

            expression = null;
            return false;
        }

        private BoundExpression SubtractIndex(BoundExpression expr, DiagnosticBag diagnostics)
        {
            BinaryOperatorKind opKind;
            int compoundStringLength = 0;
            var type = expr.Type;
            
            // normalize the type
            // we allow the following 'normal' types for the Index operator:
            // int32, uint32, int64, uint64
            // all other types (usual, float, real4, real8, decimal) are converted to int32
            switch (type.SpecialType)
            {
                case SpecialType.System_Int32:
                    opKind = BinaryOperatorKind.IntSubtraction;
                    break;
                case SpecialType.System_UInt32:
                    opKind = BinaryOperatorKind.UIntSubtraction;
                    break;
                case SpecialType.System_Int64:
                    opKind = BinaryOperatorKind.LongSubtraction;
                    break;
                case SpecialType.System_UInt64:
                    opKind = BinaryOperatorKind.ULongSubtraction;
                    break;
                default:
                    expr= CreateConversion(expr, Compilation.GetSpecialType(SpecialType.System_Int32), diagnostics);
                    if (expr.HasErrors)
                    {
                        Error(diagnostics, ErrorCode.ERR_CannotConvertArrayIndexAccess, expr.Syntax, type, Compilation.GetSpecialType(SpecialType.System_Int32));
                    }
                    opKind = BinaryOperatorKind.IntSubtraction;
                    break;
            }
            // Subtract one from the index
            var right = new BoundLiteral(expr.Syntax, ConstantValue.Create(1), expr.Type) { WasCompilerGenerated = true };
            // when the argument is a literal then we may be able to fold the subtract expression.
            var resultConstant = FoldBinaryOperator((CSharpSyntaxNode)expr.Syntax, opKind, expr, right, expr.Type.SpecialType, diagnostics, ref compoundStringLength);
            var sig = this.Compilation.builtInOperators.GetSignature(opKind);
            return new BoundBinaryOperator(expr.Syntax, BinaryOperatorKind.Subtraction,
                expr, right,
                resultConstant,
                sig.Method,
                resultKind: LookupResultKind.Viable,
                originalUserDefinedOperatorsOpt: ImmutableArray<MethodSymbol>.Empty,
                type: expr.Type,
                hasErrors: false)
            { WasCompilerGenerated = true };

        }

        private BoundExpression BindIndexerOrVulcanArrayAccess(ExpressionSyntax node, BoundExpression expr, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics)
        {
            if (Compilation.Options.IsDialectVO)
            {
                var arrayType = Compilation.ArrayType();
                var usualType = Compilation.UsualType();
                var pszType = Compilation.PszType();
                var cf = ((NamedTypeSymbol)expr.Type).ConstructedFrom;
                // in VO the indexer for a PSZ starts with 1. In Vulcan with 0.
                // So this is NOT compatible with Vulcan
                if (cf == pszType && !Compilation.Options.ArrayZero && Compilation.Options.Dialect == XSharpDialect.VO )
                {
                    ArrayBuilder<BoundExpression> argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                    foreach (var arg in analyzedArguments.Arguments)
                    {
                        BoundExpression newarg ;
                        if (!Compilation.Options.ArrayZero)
                        {
                            newarg = SubtractIndex(arg, diagnostics);
                        }
                        else
                        {
                            newarg = CreateConversion(arg, Compilation.GetSpecialType(SpecialType.System_Int32), diagnostics);
                            if (newarg.HasErrors)
                            {
                                Error(diagnostics, ErrorCode.ERR_CannotConvertArrayIndexAccess, arg.Syntax, arg.Type, Compilation.GetSpecialType(SpecialType.System_Int32));
                            }
                        }
                        argsBuilder.Add(newarg);
                    }
                    var newArgs = AnalyzedArguments.GetInstance();
                    newArgs.Arguments.AddRange(argsBuilder.ToImmutableAndFree());
                    return BindIndexerAccess(node, expr, newArgs, diagnostics);

                }
                if (cf == usualType)
                {
                    // Index operator on USUAL then we convert the usual to an array first
                    expr = BindCastCore(node, expr, arrayType, wasCompilerGenerated: true, diagnostics: diagnostics);
                    cf = arrayType;
                }
                if (cf == arrayType)
                {
                    ImmutableArray<BoundExpression> args;
                    ArrayBuilder<BoundExpression> argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                    foreach (var arg in analyzedArguments.Arguments)
                    {
                        BoundExpression newarg;
                        if (!Compilation.Options.ArrayZero)
                        {
                            newarg = SubtractIndex(arg, diagnostics);
                        }
                        else
                        {
                            newarg = CreateConversion(arg, Compilation.GetSpecialType(SpecialType.System_Int32), diagnostics);
                            if (newarg.HasErrors)
                            {
                                Error(diagnostics, ErrorCode.ERR_CannotConvertArrayIndexAccess, arg.Syntax, arg.Type, Compilation.GetSpecialType(SpecialType.System_Int32));
                            }
                        }
                        argsBuilder.Add(newarg);
                    }
                    args = argsBuilder.ToImmutableAndFree();
                    if (Compilation.Options.XSharpRuntime)
                    {
                        analyzedArguments = AnalyzedArguments.GetInstance();
                        analyzedArguments.Arguments.AddRange(args);
                        return BindIndexerAccess(node, expr, analyzedArguments, diagnostics);
                    }
                    else
                    {
                        if (args.Length > 1)
                        {
                            // create a an array of ints and use that as the index for the array
                            // this will make sure that the proper GetIndex calls is chosen
                            var exprs = SeparatedSyntaxListBuilder<ExpressionSyntax>.Create();
                            argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                            foreach (var arg in args)
                            {
                                if (exprs.Count > 0)
                                    exprs.AddSeparator(SyntaxFactory.MissingToken(SyntaxKind.CommaToken));
                                exprs.Add(arg.Syntax as ExpressionSyntax);
                                argsBuilder.Add(BindCastCore(arg.Syntax as ExpressionSyntax, arg, Compilation.GetSpecialType(SpecialType.System_Int32), wasCompilerGenerated: true, diagnostics: diagnostics));
                                args = argsBuilder.ToImmutable();
                            }
                            var initSyntax = SyntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression, exprs);
                            argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                            argsBuilder.Add(BindArrayCreationWithInitializer(diagnostics,
                                creationSyntax: null,
                                initSyntax: initSyntax,
                                type: ArrayTypeSymbol.CreateCSharpArray(this.Compilation.Assembly, Compilation.GetSpecialType(SpecialType.System_Int32), ImmutableArray<CustomModifier>.Empty),
                                sizes: ImmutableArray<BoundExpression>.Empty,
                                boundInitExprOpt: args));
                            args = argsBuilder.ToImmutableAndFree();
                        }
                        PropertySymbol indexer;
                        // Select Array Indexer with the correct # of parameters
                        if (analyzedArguments.Arguments.Count == 1)
                        {
                            indexer = (arrayType as Symbols.Metadata.PE.PENamedTypeSymbol).VulcanArrayIndexerOne;
                        }
                        else
                        {
                            indexer = (arrayType as Symbols.Metadata.PE.PENamedTypeSymbol).VulcanArrayIndexerMany;
                        }
                        return new BoundIndexerAccess(
                            syntax: node,
                            receiverOpt: expr,
                            indexer: indexer,
                            arguments: args,
                            argumentNamesOpt: default(ImmutableArray<string>),
                            argumentRefKindsOpt: default(ImmutableArray<RefKind>),
                            expanded: false,
                            argsToParamsOpt: default(ImmutableArray<int>),
                            type: usualType,
                            hasErrors: false)
                        { WasCompilerGenerated = true };
                    }
                }
            }

            return BindIndexerAccess(node, expr, analyzedArguments, diagnostics);
        }
        private bool CheckValidRefOmittedArguments(OverloadResolutionResult<MethodSymbol> result, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics)
        {
            for (int i = 0; i < analyzedArguments.Arguments.Count; i++)
            {
                if (analyzedArguments.RefKind(i) == RefKind.None && result.ValidResult.Member.Parameters[result.ValidResult.Result.ParameterFromArgument(i)].RefKind != RefKind.None)
                {
                    var arg = analyzedArguments.Arguments[i];

                    if (Compilation.Options.VOImplicitCastsAndConversions)
                    {
                        if (arg is BoundAddressOfOperator)
                        {
                            arg = (arg as BoundAddressOfOperator).Operand;
                            if (!analyzedArguments.RefKinds.Any())
                            {
                                for (int j = 0; j < analyzedArguments.Arguments.Count; j++)
                                    analyzedArguments.RefKinds.Add(i==j ? RefKind.Ref : RefKind.None);
                            }
                            else
                            {
                                analyzedArguments.RefKinds[i] = RefKind.Ref;
                            }
                            analyzedArguments.Arguments[i] = arg;
                            // check for correct type
                            if (arg.Type != result.ValidResult.Member.ParameterTypes[i])
                            {
                                Error(diagnostics,ErrorCode.ERR_BadArgType, arg.Syntax, i+1, arg.Type, result.ValidResult.Member.ParameterTypes[i]);
                            }
                        }
                    }

                    if (!CheckIsVariable(arg.Syntax, arg, BindValueKind.RefOrOut, checkingReceiver: false, diagnostics: diagnostics))
                        return false;
                }
            }
            return true;
        }

        private bool CheckValidRefOmittedArguments(OverloadResolutionResult<PropertySymbol> result, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics)
        {
            for (int i = 0; i < analyzedArguments.Arguments.Count; i++)
            {
                if (analyzedArguments.RefKind(i) == RefKind.None && result.ValidResult.Member.Parameters[result.ValidResult.Result.ParameterFromArgument(i)].RefKind != RefKind.None)
                {
                    var arg = analyzedArguments.Arguments[i];

                    if (Compilation.Options.VOImplicitCastsAndConversions)
                    {
                        if (arg is BoundAddressOfOperator)
                        {
                            arg = (arg as BoundAddressOfOperator).Operand;
                        }
                        if (!analyzedArguments.RefKinds.Any())
                        {
                            for (int j = 0; j < analyzedArguments.Arguments.Count; j++)
                                analyzedArguments.RefKinds.Add(i == j ? RefKind.Ref : RefKind.None);
                        }
                        else
                        {
                            analyzedArguments.RefKinds[i] = RefKind.Ref;
                        }
                        analyzedArguments.Arguments[i] = arg;
                    }

                    if (!CheckIsVariable(arg.Syntax, arg, BindValueKind.RefOrOut, checkingReceiver: false, diagnostics: diagnostics))
                        return false;
                }
            }
            return true;
        }
        private BoundExpression TryBindLateBoundCall(
            ExpressionSyntax node, 
            BoundExpression boundLeft, 
            TypeSymbol leftType, 
            SimpleNameSyntax right,
            bool invoked,
            bool indexed
            )
        {
            if (Compilation.Options.IsDialectVO && Compilation.Options.LateBinding && right.Kind() != SyntaxKind.GenericName)
            {
                string propName = right.Identifier.ValueText;
                if (leftType != null)
                {
                    bool earlyBound = propName == ".ctor";
                    bool isObject = leftType.IsObjectType();
                    bool isUsual = false;
                    bool isArray = false;
                    if (! isObject)
                    {
                        if (leftType is NamedTypeSymbol)
                        {
                            var nts = leftType as NamedTypeSymbol;
                            isUsual = nts.ConstructedFrom == Compilation.UsualType();
                            isArray = nts.ConstructedFrom == Compilation.ArrayType();
                        }
                    }
                    // Late bound will only work for OBJECT or USUAL
                    if (isObject || isUsual || isArray)
                    {
                        var returnType = Compilation.UsualType();
                        if (isArray)
                        {
                            // When method does not exist then do a late bound ASend()
                            var m = Compilation.ArrayType().GetMembers(propName);
                            earlyBound = m.Length > 0;
                        }
                        else if (isUsual)
                        {
                            earlyBound |= String.Compare(propName, "_NIL", StringComparison.OrdinalIgnoreCase) == 0;
                            //earlyBound |= String.Compare(propName, "Value", StringComparison.OrdinalIgnoreCase) == 0;
                        }
                        else if (isObject)
                        {
                            earlyBound |= leftType.GetMembers(propName).Length > 0;
                        }
                        if (!earlyBound)
                        {
                            return new BoundDynamicMemberAccess(
                                syntax: node,
                                receiver: boundLeft,
                                typeArgumentsOpt: default(ImmutableArray<TypeSymbol>),
                                name: propName,
                                invoked: invoked,
                                indexed: indexed,
                                type: returnType,
                                hasErrors: false);
                        }
                    }
                }
            }
            return null;
        }
        private BoundExpression CheckVulcanIndexedValue(BoundExpression expr, BindValueKind valueKind, DiagnosticBag diagnostics)
        {
            var originalexpr = expr;
            expr = CheckValue(expr, BindValueKind.RValue, diagnostics);
            if (expr.Kind == BoundKind.BadExpression)
            {
                // When a lookup of the Item property fails, then try again
                if (originalexpr.Kind == BoundKind.MethodGroup)
                {
                    var methodGroup = originalexpr as BoundMethodGroup;
                    if (string.Equals(methodGroup.Name, "Item", StringComparison.OrdinalIgnoreCase))
                    {
                        var newDiag = DiagnosticBag.GetInstance();
                        expr = CheckValue(methodGroup.InstanceOpt, BindValueKind.RValue, newDiag);
                        if (expr.Kind != BoundKind.BadExpression)
                        {
                            diagnostics.Clear();
                        }
                    }
                }
            }
            return expr;

        }
        private bool BindStringToPsz(CSharpSyntaxNode syntax, ref BoundExpression source, TypeSymbol destination)
        {
            if (source.Type != null && source.Type.SpecialType == SpecialType.System_String &&
                Compilation.Options.IsDialectVO &&
                (destination == Compilation.PszType()
                || destination.IsVoidPointer()))
            {
                // Note this calls the constructor for __PSZ with a string.
                // The allocated pointer inside the PSZ is never freed by Vulcan !
                TypeSymbol psz = Compilation.PszType();
                MethodSymbol stringctor = null;
                var ctors = psz.GetMembers(".ctor");
                foreach (MethodSymbol ctor in ctors)
                {
                    var pars = ctor.GetParameters();
                    if (pars.Length == 1 && pars[0].Type.SpecialType == SpecialType.System_String)
                    {
                        stringctor = ctor;
                    }
                }

                if (stringctor != null)
                {
                    source = new BoundObjectCreationExpression(syntax, stringctor, new BoundExpression[] { source });
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Binds a simple identifier.
        /// </summary>
        private BoundExpression BindXSIdentifier(
            SimpleNameSyntax node,
            bool invoked,
            DiagnosticBag diagnostics,
            bool bindMethod
            )
        {
            // This method replaced the standard C# BindIdentifier
            // xBase has some different rules for binding
            // - for calls without object prefix we prefer static method calls over self method calls (In VO SELF: is mandatory)
            //   and we also prefer to find DEFINES over PROPERTIES
            // - when invoked = TRUE then we do not return local variables, with the exception of delegates
            // - when invoked = FALSE then we do not return methods, with the exception of assigning event handlers

            bool preferStatic = bindMethod && Compilation.Options.IsDialectVO;

            Debug.Assert(node != null);

            // If the syntax tree is ill-formed and the identifier is missing then we've already
            // given a parse error. Just return an error local and continue with analysis.
            if (node.IsMissing)
            {
                return BadExpression(node);
            }

            // A simple-name is either of the form I or of the form I<A1, ..., AK>, where I is a
            // single identifier and <A1, ..., AK> is an optional type-argument-list. When no
            // type-argument-list is specified, consider K to be zero. The simple-name is evaluated
            // and classified as follows:

            // If K is zero and the simple-name appears within a block and if the block's (or an
            // enclosing block's) local variable declaration space contains a local variable,
            // parameter or constant with name I, then the simple-name refers to that local
            // variable, parameter or constant and is classified as a variable or value.

            // If K is zero and the simple-name appears within the body of a generic method
            // declaration and if that declaration includes a type parameter with name I, then the
            // simple-name refers to that type parameter.

            BoundExpression expression;

            // It's possible that the argument list is malformed; if so, do not attempt to bind it;
            // just use the null array.

            int arity = node.Arity;
            bool hasTypeArguments = arity > 0;

            SeparatedSyntaxList<TypeSyntax> typeArgumentList = node.Kind() == SyntaxKind.GenericName
                ? ((GenericNameSyntax)node).TypeArgumentList.Arguments
                : default(SeparatedSyntaxList<TypeSyntax>);

            Debug.Assert(arity == typeArgumentList.Count);

            var typeArguments = hasTypeArguments ?
                BindTypeArguments(typeArgumentList, diagnostics) :
                default(ImmutableArray<TypeSymbol>);

            var lookupResult = LookupResult.GetInstance();
            LookupOptions options = LookupOptions.AllMethodsOnArityZero;
            if (invoked)
            {
                options |= LookupOptions.MustBeInvocableIfMember;
            }

            if (!IsInMethodBody && this.EnclosingNameofArgument == null)
            {
                Debug.Assert((options & LookupOptions.NamespacesOrTypesOnly) == 0);
                options |= LookupOptions.MustNotBeMethodTypeParameter;
            }
            // In the VO and Vulcan dialect you cannot call an instance method without SELF: prefix
            // and also not access a property without SELF: Prefix
            // So when there is a property (Access) and a DEFINE with the same name then the
            // system will use the define and not the property
            // so we check here if we are called from a memberaccessexpression with a colon separator
            // so String.Compare will use different lookup options as SELF:ToString()
            var originalOptions = options;

            // Here we add XSharp Specific options
            if (!bindMethod)
            {
                options |= LookupOptions.MustNotBeMethod;
            }

            if (preferStatic)
            {
                bool instance = false;
                if (node.Parent is MemberAccessExpressionSyntax)
                {
                    instance = node.IsInstanceMemberAccess(false);
                }
                else if (node.Parent is AssignmentExpressionSyntax)
                {
                    var aes = node.Parent as AssignmentExpressionSyntax;
                    if (aes.Left == node)
                    {
                        instance = true;
                    }
                }
                if (!instance)
                    options |= LookupOptions.MustNotBeInstance;
                
            }
            else
            {
                if (node.Parent is MemberAccessExpressionSyntax)
                {
                    // Check for Messagebax.Show() which is class member access
                    // versions window:ToString() which is instance member access
                    if (!node.IsInstanceMemberAccess(true))
                    { 
                        options = LookupOptions.NamespacesOrTypesOnly;
                    }
                }
            }

            var name = node.Identifier.ValueText;
            HashSet<DiagnosticInfo> useSiteDiagnostics = null;

            if (lookupResult.IsClear)
            {
                this.LookupSymbolsWithFallback(lookupResult, name, arity: arity, useSiteDiagnostics: ref useSiteDiagnostics, options: options);
            }
            // when no field or local found then try to find defines
            if (lookupResult.IsClear && !bindMethod)
            {
                this.LookupSymbolsWithFallback(lookupResult, name, arity: arity, useSiteDiagnostics: ref useSiteDiagnostics, options: options | LookupOptions.DefinesOnly);
            }
            if (preferStatic || lookupResult.IsClear)
            {
                bool lookupAgain = false;
                if (lookupResult.Kind == LookupResultKind.StaticInstanceMismatch)
                {
                    // try again but now allow instance methods
                    lookupAgain = true;
                }
                if (lookupResult.Kind == LookupResultKind.Viable && invoked && lookupResult.Symbols.Count != 0)
                {
                    Symbol s = lookupResult.Symbols[0];
                    if (s.Kind != SymbolKind.Method)
                        lookupAgain = true;
                }
                if (lookupAgain || lookupResult.IsClear)
                {
                    // This uses the 'original' BindIdentifier lookup mechanism
                    options = originalOptions;
                    useSiteDiagnostics = null;
                    lookupResult.Clear();
                    this.LookupSymbolsWithFallback(lookupResult, name, arity: arity, useSiteDiagnostics: ref useSiteDiagnostics, options: options);
                }
            }

            diagnostics.Add(node, useSiteDiagnostics);

            if (lookupResult.Kind != LookupResultKind.Empty)
            {
                // have we detected an error with the current node?
                bool isError = false;
                bool wasError;
                var members = ArrayBuilder<Symbol>.GetInstance();
                Symbol symbol = null;
                if (!invoked)
                {
                    // not invoked, so prefer non-method symbols
                    members.AddRange(lookupResult.Symbols.Where(f => f.Kind != SymbolKind.Method));
                    if (members.Count == 1)
                    {
                        symbol = members[0];
                    }
                }
                if (symbol == null)
                {
                    members.Clear();
                    symbol = GetSymbolOrMethodOrPropertyGroup(lookupResult, node, name, node.Arity, members, diagnostics, out wasError);  // reports diagnostics in result.
                }
                else
                {
                    wasError = false;
                }

                isError |= wasError;

                if ((object)symbol == null)
                {
                    Debug.Assert(members.Count > 0);

                    var receiver = SynthesizeMethodGroupReceiver(node, members);
                    expression = ConstructBoundMemberGroupAndReportOmittedTypeArguments(
                        node,
                        typeArgumentList,
                        typeArguments,
                        receiver,
                        name,
                        members,
                        lookupResult,
                        receiver != null ? BoundMethodGroupFlags.HasImplicitReceiver : BoundMethodGroupFlags.None,
                        isError,
                        diagnostics);
                }
                else
                {
                    bool isNamedType = (symbol.Kind == SymbolKind.NamedType) || (symbol.Kind == SymbolKind.ErrorType);

                    if (hasTypeArguments && isNamedType)
                    {
                        symbol = ConstructNamedTypeUnlessTypeArgumentOmitted(node, (NamedTypeSymbol)symbol, typeArgumentList, typeArguments, diagnostics);
                    }

                    expression = BindNonMethod(node, symbol, diagnostics, lookupResult.Kind, isError);

                    if (!isNamedType && (hasTypeArguments || node.Kind() == SyntaxKind.GenericName))
                    {
                        Debug.Assert(isError); // Should have been reported by GetSymbolOrMethodOrPropertyGroup.
                        expression = new BoundBadExpression(
                            syntax: node,
                            resultKind: LookupResultKind.WrongArity,
                            symbols: ImmutableArray.Create<Symbol>(symbol),
                            childBoundNodes: ImmutableArray.Create<BoundNode>(expression),
                            type: expression.Type,
                            hasErrors: isError);
                    }
                }

                members.Free();
            }
            else
            {
                // Otherwise, the simple-name is undefined and a compile-time error occurs.
                expression = BadExpression(node);
                if (lookupResult.Error != null)
                {
                    Error(diagnostics, lookupResult.Error, node);
                }
                else if (IsJoinRangeVariableInLeftKey(node))
                {
                    Error(diagnostics, ErrorCode.ERR_QueryOuterKey, node, name);
                }
                else if (IsInJoinRightKey(node))
                {
                    Error(diagnostics, ErrorCode.ERR_QueryInnerKey, node, name);
                }
                else
                {
                    Error(diagnostics, ErrorCode.ERR_NameNotInContext, node, name);
                }
            }

            lookupResult.Free();
            return expression;
        }

        private void FilterResults(LookupResult result, LookupOptions options)
        {
            bool noMethod = options.HasFlag(LookupOptions.MustNotBeMethod);
            bool onlyDef = options.HasFlag(LookupOptions.DefinesOnly);
            if ((noMethod || onlyDef) && ! result.IsClear && result.Kind == LookupResultKind.Viable)
            {
                LookupResult tmp = LookupResult.GetInstance();
                foreach (var sym in result.Symbols)
                {
                    bool add = false;
                    switch (sym.Kind)
                    {
                        case SymbolKind.Field:
                            if (onlyDef)
                            {
                                if (sym.ContainingType.Name == XSharpSpecialNames.CoreFunctionsClass)
                                {
                                    add = true;
                                }
                            }
                            else
                                add = true;
                            break;
                        case SymbolKind.Parameter:
                        case SymbolKind.Local:
                            add = true;
                            break;
                        case SymbolKind.Method:
                        case SymbolKind.Property:
                            add = !noMethod && !onlyDef;
                            break;
                        default:
                            //add = true;
                            break;
                    }
                    if (add)
                    {
                        SingleLookupResult single = new SingleLookupResult(LookupResultKind.Viable, sym, null);
                        tmp.MergeEqual(single);
                    }

                }
                result.Clear();
                result.MergeEqual(tmp);
                tmp.Free();

            }
            return;
        }

        private static TypeSymbol XsGetCorrespondingParameterType(ref MemberAnalysisResult result, ImmutableArray<TypeSymbol> parameterTypes, int arg)
        {
            int paramNum = result.ParameterFromArgument(arg);
            var type =
                (paramNum == parameterTypes.Length - 1 && result.Kind == MemberResolutionKind.ApplicableInExpandedForm) ?
                ((ArrayTypeSymbol)parameterTypes[paramNum]).ElementType :
                parameterTypes[paramNum];
            return type;
        }
        private BoundExpression XsFixPszArgumentProblems (BoundExpression argument, TypeSymbol type, ref Conversion kind)
        {
            if (argument.Kind == BoundKind.Literal)
            {
                if (type == Compilation.PszType())
                {
                    var lit = argument as BoundLiteral;
                    if (lit.IsLiteralNull())
                    {
                        argument = new BoundLiteral(argument.Syntax, ConstantValue.Create(0), Compilation.GetSpecialType(SpecialType.System_Int32));
                        kind = Conversion.Identity;
                    }
                }
            }
            return argument;
        }
        private BoundExpression PszFromNull(BoundExpression expression)
        {
            var targetType = Compilation.PszType();
            return new BoundDefaultOperator(expression.Syntax, targetType);
        }
    }
    internal static class XsBoundExpressionExtensions
    {
        internal static bool IsInstanceMemberAccess(this SimpleNameSyntax node, bool mustBeLHS)
        {
            bool instance = false;
            if (node?.Parent is MemberAccessExpressionSyntax)
            {
                var xnode = node.Parent.XNode;
                if (xnode != null)
                { 
                    if (xnode is ArrayElementContext)
                    {
                        xnode = ((ArrayElementContext)xnode).Expr;
                    }
                    var amc = xnode as AccessMemberContext;
                    if (amc != null && amc.Op.Text == ":")
                    {
                        instance = true;
                        if (mustBeLHS)
                        {
                            var par = node.Parent as MemberAccessExpressionSyntax;
                            instance = par.Expression == node;
                        }
                    }
                }
            }
            return instance;
        }
    }
}