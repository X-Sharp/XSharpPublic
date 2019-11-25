//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


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
using Microsoft.CodeAnalysis.PooledObjects;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

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

        private bool BindVOPointerDereference(CastExpressionSyntax node, TypeSymbol targetType, BoundExpression operand, 
            DiagnosticBag diagnostics, out BoundExpression expression)
        {
            // Type(pPointer) -> Dereference pointer
            // Vulcan only allows this with pPointer is of type PTR (Void pointer)
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

        private BoundExpression SubtractIndex(BoundExpression expr, DiagnosticBag diagnostics, SpecialType specialType)
        {
            int compoundStringLength = 0;
            var type = expr.Type;
            var kind = BinaryOperatorKind.IntSubtraction;
            if (!specialType.IsSignedIntegralType())
            {
                kind = BinaryOperatorKind.UIntSubtraction;
            }
            // normalize the type: all types are converted to int32
            if (expr.Type.SpecialType != specialType)
            {
                expr = CreateConversion(expr, Compilation.GetSpecialType(specialType), diagnostics);
                if (expr.HasErrors)
                {
                    Error(diagnostics, ErrorCode.ERR_CannotConvertArrayIndexAccess, expr.Syntax, type, Compilation.GetSpecialType(SpecialType.System_Int32));
                }
            }
            // Subtract one from the index
            var right = new BoundLiteral(expr.Syntax, ConstantValue.Create(1), expr.Type) { WasCompilerGenerated = true };
            // when the argument is a literal then we may be able to fold the subtract expression.
            var resultConstant = FoldBinaryOperator((CSharpSyntaxNode)expr.Syntax, kind, expr, right, expr.Type.SpecialType, diagnostics, ref compoundStringLength);
            var sig = this.Compilation.builtInOperators.GetSignature(kind);
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

        private BoundExpression BindIndexerOrVOArrayAccess(ExpressionSyntax node, BoundExpression expr, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics)
        {
            if (Compilation.Options.HasRuntime)
            {
                var arrayType = Compilation.ArrayType();
                var usualType = Compilation.UsualType();
                var pszType = Compilation.PszType();
                var cf = ((NamedTypeSymbol)expr.Type).ConstructedFrom;
                if (cf == pszType )
                {
                    ArrayBuilder<BoundExpression> argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                    foreach (var arg in analyzedArguments.Arguments)
                    {
                        var specialType = SpecialType.System_UInt32;
                        if (Compilation.Options.XSharpRuntime)
                        {
                            specialType = SpecialType.System_Int32;
                        }
                        BoundExpression newarg = arg ;
                        if (arg.Type.SpecialType != specialType)
                        {
                            newarg = CreateConversion(arg, Compilation.GetSpecialType(specialType), diagnostics);
                            if (newarg.HasErrors)
                            {
                                Error(diagnostics, ErrorCode.ERR_CannotConvertArrayIndexAccess, arg.Syntax, arg.Type, Compilation.GetSpecialType(SpecialType.System_Int32));
                            }
                        }
                        // in VO the indexer for a PSZ starts with 1. In Vulcan with 0.
                        // we assume that all other dialects are closer to VO
                        if (Compilation.Options.Dialect != XSharpDialect.Vulcan)
                        { 
                            newarg = SubtractIndex(newarg, diagnostics,specialType);
                        }
                        argsBuilder.Add(newarg);
                    }
                    var newArgs = AnalyzedArguments.GetInstance();
                    newArgs.Arguments.AddRange(argsBuilder.ToImmutableAndFree());
                    return BindIndexerAccess(node, expr, newArgs, diagnostics);

                }
                var indexerType = Compilation.IndexerType();
                var namedIndexerType = Compilation.NamedIndexerType();
                var indexedPropsType = Compilation.IndexedPropertiesType();
                var arrayBaseType = Compilation.ArrayBaseType();
                bool numericParams = false;
                bool mustcast = false;
                HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                if (cf != arrayType && (cf == usualType
                    || cf.ConstructedFrom == arrayBaseType
                    || cf.ImplementsInterface(indexedPropsType, ref useSiteDiagnostics)
                    || cf.ImplementsInterface(indexerType, ref useSiteDiagnostics)))
                {
                    // Index operator on USUAL then we convert the usual to an array or indexer first

                    if (Compilation.Options.XSharpRuntime)
                    {
                        if (analyzedArguments.Arguments.Count == 2 && analyzedArguments.Arguments[1].Type.IsStringType()
                            && cf.ImplementsInterface(namedIndexerType, ref useSiteDiagnostics))
                        {
                            cf = namedIndexerType;
                            numericParams = true;
                            mustcast = true;
                        }
                        else if (analyzedArguments.Arguments.Count == 1 && analyzedArguments.Arguments[0].Type.IsStringType()
                            && cf.ImplementsInterface(indexedPropsType, ref useSiteDiagnostics))
                        {
                            cf = indexedPropsType;
                            numericParams = false;
                            mustcast = true;
                        }
                        else if ( cf.ImplementsInterface(indexerType, ref useSiteDiagnostics))
                        {
                            cf = indexerType;
                            numericParams = true;
                            mustcast = true;
                        }
                        else
                        {
                            numericParams = true;
                            mustcast = false;
                        }
                        if (mustcast)
                        {
                            expr = BindCastCore(node, expr, cf, wasCompilerGenerated: true, diagnostics: diagnostics);
                        }
                    }
                    else
                    {
                        expr = BindCastCore(node, expr, arrayType, wasCompilerGenerated: true, diagnostics: diagnostics);
                        cf = arrayType;
                        numericParams = true;
                    }
                }
                if (cf == arrayType || numericParams ) 
                {
                    ImmutableArray<BoundExpression> args;
                    ArrayBuilder<BoundExpression> argsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                    int argno = 0;
                    foreach (var arg in analyzedArguments.Arguments)
                    {
                        BoundExpression newarg;
                        bool mustBeNumeric = false;
                        ++argno;
                        mustBeNumeric = true;
                        if (Compilation.Options.XSharpRuntime  && cf == namedIndexerType)
                        { 
                            mustBeNumeric = argno == 1;
                        }
                        if (mustBeNumeric)
                        {
                            newarg = arg;
                            var specialType = SpecialType.System_Int32;
                            if (arg.Type.SpecialType != specialType)
                            {
                                newarg = CreateConversion(arg, Compilation.GetSpecialType(specialType), diagnostics);
                                if (newarg.HasErrors)
                                {
                                    Error(diagnostics, ErrorCode.ERR_CannotConvertArrayIndexAccess, arg.Syntax, arg.Type, Compilation.GetSpecialType(SpecialType.System_Int32));
                                }
                            }
                            if (!Compilation.Options.ArrayZero)
                            {
                                newarg = SubtractIndex(newarg, diagnostics, specialType);
                            }
                        }
                        else
                        {
                            newarg = arg;
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
                            binderOpt: this,
                            useSetterForDefaultArgumentGeneration:false ,
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
            var member = result.ValidResult.Member;
            
            for (int i = 0; i < analyzedArguments.Arguments.Count; i++)
            {
                var parNumber = result.ValidResult.Result.ParameterFromArgument(i);
                var parRefKind = member.Parameters[parNumber].RefKind;

                if (analyzedArguments.RefKind(i) == RefKind.None &&  parRefKind != RefKind.None)
                {
                    var arg = analyzedArguments.Arguments[i];

                    if (Compilation.Options.VOImplicitCastsAndConversions)
                    {
                        bool adjust = false;
                        if (arg is BoundAddressOfOperator)
                        {
                            arg = (arg as BoundAddressOfOperator).Operand;
                            adjust = true;
                        }
                        else if (arg is BoundLiteral bl && bl.IsLiteralNull())
                        {
                            adjust = false;
                        }
                        else
                        {
                            adjust = true;
                            Error(diagnostics, ErrorCode.WRN_AutomaticRefGeneration, arg.Syntax, i + 1, parRefKind);
                        }
                        if (adjust)
                        {
                            if (!analyzedArguments.RefKinds.Any())
                            {
                                // Size the analyzedArguments list
                                for (int j = 0; j < analyzedArguments.Arguments.Count; j++)
                                {
                                    analyzedArguments.RefKinds.Add(i == j ? parRefKind : RefKind.None);
                                }
                            }
                            else
                            {
                                analyzedArguments.RefKinds[i] = RefKind.Ref;
                            }
                            analyzedArguments.Arguments[i] = arg;
                            // check for correct type
                            if (arg.Type != result.ValidResult.Member.ParameterTypes[i])
                            {
                                Error(diagnostics, ErrorCode.ERR_BadArgType, arg.Syntax, i + 1, arg.Type, member.ParameterTypes[i]);
                            }

                        }
                    }
                    
                    if (!CheckValueKind(arg.Syntax, arg, BindValueKind.RefOrOut, checkingReceiver: false, diagnostics: diagnostics))
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

                    if (!CheckValueKind(arg.Syntax, arg, BindValueKind.RefOrOut, checkingReceiver: false, diagnostics: diagnostics))
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
            if (Compilation.Options.HasRuntime && Compilation.Options.LateBinding && right.Kind() != SyntaxKind.GenericName)
            {
                string propName = right.Identifier.ValueText;
                if (leftType != null)
                {
                    bool earlyBound = propName == ".ctor";
                    bool isObject = leftType.IsObjectType();
                    bool isUsual = false;
                    bool isArray = false;
                    NamedTypeSymbol usualType = Compilation.UsualType();
                    NamedTypeSymbol arrayType = Compilation.ArrayType();
                    if (! isObject)
                    {
                        if (leftType is NamedTypeSymbol)
                        {
                            var nts = leftType as NamedTypeSymbol;
                            isUsual = nts.ConstructedFrom == usualType;
                            isArray = nts.ConstructedFrom == arrayType;
                        }
                    }
                    // Late bound will only work for OBJECT or USUAL
                    if (isObject || isUsual || isArray)
                    {
                        var returnType = Compilation.UsualType();
                        if (isArray )
                        {
                            // When method does not exist then do a late bound ASend()
                            if (Compilation.Options.Dialect.AllowASend())
                            {
                                var m = arrayType.GetMembers(propName);
                                earlyBound = m.Length > 0;
                                if (! earlyBound && Compilation.Options.XSharpRuntime)
                                {
                                    m = arrayType.BaseTypeNoUseSiteDiagnostics.GetMembers(propName);
                                    earlyBound = m.Length > 0;
                                }
                            }
                            else
                            {
                                earlyBound = true;
                            }
                        }
                        else if (isUsual)
                        {
                            // USUAL._NIL and USUAL.ToObject()
                            var m = usualType.GetMembers(propName);
                            if (m.Length > 0 && m[0].IsStatic)
                                earlyBound |= true;
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
        private BoundExpression CheckVOIndexedValue(BoundExpression expr, BindValueKind valueKind, DiagnosticBag diagnostics)
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
        private bool BindStringToPsz(CSharpSyntaxNode syntax, ref BoundExpression source, TypeSymbol destination,DiagnosticBag diagnostics)
        {
            TypeSymbol psz = Compilation.PszType();
            if (source.Type != null && source.Type.SpecialType == SpecialType.System_String &&
                Compilation.Options.HasRuntime &&
                (destination == psz || destination.IsVoidPointer()))
            {
                // Note this calls the constructor for __PSZ with a string.
                // The allocated pointer inside the PSZ is never freed by Vulcan and X# !
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
                    diagnostics.Add(ErrorCode.WRN_CompilerGeneratedPSZConversionGeneratesMemoryleak, syntax.Location);
                    source = new BoundObjectCreationExpression(syntax, stringctor, binderOpt: null, new BoundExpression[] { source });
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

            bool preferStatic = bindMethod;

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

            BoundExpression expression = null;

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
            var nsOrTypesFirst = false;
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
                    // Check for Messagebox.Show() which is class member access
                    // versions window:ToString() which is instance member access
                    if (!node.IsInstanceMemberAccess(true))
                    {
                        nsOrTypesFirst = true;
                    }
                }
            }
            var name = node.Identifier.ValueText;
            HashSet<DiagnosticInfo> useSiteDiagnostics = null;
            var memvarorfield = name.IndexOf("->") >0;
            // no need to look for our special names
            if (!memvarorfield)
            {
                if (nsOrTypesFirst)
                {
                    this.LookupSymbolsWithFallback(lookupResult, name, arity: arity, useSiteDiagnostics: ref useSiteDiagnostics, options: LookupOptions.NamespacesOrTypesOnly);
                }
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

                        expression = BindNonMethod(node, symbol, diagnostics, lookupResult.Kind, indexed: false, isError);

                        if (!isNamedType && (hasTypeArguments || node.Kind() == SyntaxKind.GenericName))
                        {
                            diagnostics.Add(ErrorCode.ERR_InvalidExprTerm, node.Location, node.XNode.GetText());
                            expression = new BoundBadExpression(
                                syntax: node,
                                resultKind: LookupResultKind.WrongArity,
                                symbols: ImmutableArray.Create<Symbol>(symbol),
                                childBoundNodes: ImmutableArray.Create<BoundExpression>(expression),
                                type: expression.Type,
                                hasErrors: true);
                        }
                    }

                    members.Free();
                }
            }
            // undeclared variables are allowed when the dialect supports memvars and memvars are enabled
            // or in the 'full' macro compiler
            if (expression == null && (Compilation.Options.MacroScript || Compilation.Options.SupportsMemvars || memvarorfield))
            {
                var type = Compilation.RuntimeFunctionsType();
                bool declared = false;
                string alias = null;
                var get = GetCandidateMembers(type, ReservedNames.VarGet, LookupOptions.MustNotBeInstance, this);
                var set = GetCandidateMembers(type, ReservedNames.VarPut, LookupOptions.MustNotBeInstance, this);
                if (memvarorfield)
                {
                    // this is either:
                    // alias->fieldname
                    // Xs$Memvar->Memvarname
                    // Xs$Field->Memvar
                    var parts = name.Split(new string[] { "->" }, StringSplitOptions.None);
                    if (parts.Length == 2)
                    {
                        declared = true;
                        name = parts[1];
                        if (parts[0] == XSharpSpecialNames.MemVarPrefix)
                        {
                            get = GetCandidateMembers(type, ReservedNames.MemVarGet, LookupOptions.MustNotBeInstance, this);
                            set = GetCandidateMembers(type, ReservedNames.MemVarPut, LookupOptions.MustNotBeInstance, this);

                        }
                        else if (parts[0] == XSharpSpecialNames.FieldPrefix)
                        {
                            get = GetCandidateMembers(type, ReservedNames.FieldGet, LookupOptions.MustNotBeInstance, this);
                            set = GetCandidateMembers(type, ReservedNames.FieldPut, LookupOptions.MustNotBeInstance, this);
                        }
                        else
                        {
                            get = GetCandidateMembers(type, ReservedNames.FieldGetWa, LookupOptions.MustNotBeInstance, this);
                            set = GetCandidateMembers(type, ReservedNames.FieldPutWa, LookupOptions.MustNotBeInstance, this);
                            alias = parts[0];
                        }
                    }
                }

                if (Compilation.Options.UndeclaredLocalVars || declared)
                {
                    if (get.Length == 1 && set.Length == 1 && get[0] is MethodSymbol && set[0] is MethodSymbol)
                    {
                        XsVariableSymbol ps;
                        if (!String.IsNullOrEmpty(alias))
                        {
                            ps = new XsVariableSymbol(alias, name, (MethodSymbol)get[0], (MethodSymbol)set[0], Compilation.UsualType());
                        }
                        else
                        {
                            ps = new XsVariableSymbol(name, (MethodSymbol)get[0], (MethodSymbol)set[0], Compilation.UsualType());
                        }

                        expression = new BoundPropertyAccess(node, null, ps, LookupResultKind.Viable, Compilation.UsualType());
                        if (!Compilation.Options.MacroScript && !declared)
                        {
                            Error(diagnostics, ErrorCode.WRN_UndeclaredVariable, node.Location, name);

                            // find entity and set flag HasUndeclared
                            var parent = node.XNode as XSharpParserRuleContext;
                            while (parent != null)
                            {
                                if (parent is IEntityContext)
                                    break;
                                parent = parent.Parent as XSharpParserRuleContext;
                            }
                            if (parent is IEntityContext iec)
                            {
                                iec.Data.HasUndeclared = true;
                            }
                        }

                    }
                }
            }
            if (expression == null)
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
                                if (sym.ContainingType.Name == XSharpSpecialNames.FunctionsClass)
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
            return new BoundDefaultExpression(expression.Syntax, targetType);
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
