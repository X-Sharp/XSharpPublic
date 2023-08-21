//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.PooledObjects;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal sealed partial class OverloadResolution
    {
        private MemberResolutionResult<TMember> XsCheckMemberResolution<TMember>(bool allowUnexpandedForm,
            MemberResolutionResult<TMember> normalResult, TMember leastOverriddenMember, TMember member, AnalyzedArguments arguments)
             where TMember : Symbol
        {
            // when calling a USUAL[] function we allow 1 usual param and wrap it as params as well
            // REF USUAL is not allowed
            if (allowUnexpandedForm && normalResult.Result.IsValid && IsValidParams(leastOverriddenMember) && Compilation.Options.HasRuntime)
            {
                // Find Params argument
                BoundExpression paramsArg = null;
                var parameters = member.GetParameters();
                for (int arg = 0; arg < arguments.Arguments.Count; ++arg)
                {
                    int parm = normalResult.Result.ParameterFromArgument(arg);
                    if (parm >= parameters.Length)
                        continue;
                    var parameter = parameters[parm];
                    if (parameter.IsParams)
                    {
                        paramsArg = arguments.Argument(arg);
                    }
                }

                // If params arg is USUAL prefer the expanded form
                if (paramsArg != null)
                {
                    if (paramsArg.Type.IsUsualType())
                    {
                        if (arguments.RefKinds.Count == 0 || arguments.RefKinds[0] == RefKind.None)
                        {
                            // We have seen an example where the customer is mixing different versions of the Vulcan runtime.
                            // when we set allowUnexpandedForm to false then strange errors will happen later.
                            if (!member.HasUseSiteError)
                            {
                                normalResult = default;
                            }
                        }
                    }
                    // make sure that the null parameter is not passed for the ClipperArgs array but as first value in the array
                    else if (paramsArg.IsLiteralNull() && member.HasClipperCallingConvention())
                    {
                        arguments.Arguments[0] = new BoundDefaultExpression(paramsArg.Syntax, Compilation.ObjectType);
                        normalResult = default;
                    }
                }
            }
            return normalResult;
        }

        private MemberAnalysisResult XsAddConstructorToCandidateSet(MemberAnalysisResult result, MethodSymbol constructor,
            AnalyzedArguments arguments, bool completeResults, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            if (result.IsValid && IsValidParams(constructor) && Compilation.Options.HasRuntime)
            {
                // Find Params argument
                BoundExpression paramsArg = null;
                var parameters = constructor.Parameters;
                for (int arg = 0; arg < arguments.Arguments.Count; ++arg)
                {
                    int parm = result.ParameterFromArgument(arg);
                    if (parm >= parameters.Length)
                        continue;
                    var parameter = parameters[parm];
                    if (parameter.IsParams)
                    {
                        paramsArg = arguments.Argument(arg);
                    }
                }

                // If params arg is USUAL prefer the expanded form
                if (paramsArg != null && paramsArg.Type.IsUsualType())
                {
                    if (arguments.RefKinds.Count == 0 || arguments.RefKinds[0] == RefKind.None)
                    {
                        if (!constructor.HasUseSiteError)
                        {
                            var expandedResult = IsConstructorApplicableInExpandedForm(constructor, arguments, completeResults, ref useSiteDiagnostics);
                            if (expandedResult.IsValid || completeResults)
                            {
                                result = expandedResult;
                            }
                        }
                    }
                }
            }
            return result;
        }

        private Conversion XsIsApplicable(Symbol candidate, AnalyzedArguments arguments, ref BoundExpression argument,
            ImmutableArray<int> argsToParameters, int argumentPosition, EffectiveParameters parameters, bool completeResults,
            ref RefKind argumentRefKind, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            RefKind parameterRefKind = parameters.ParameterRefKinds.IsDefault ? RefKind.None : parameters.ParameterRefKinds[argumentPosition];
            bool literalNullForRefParameter = false;
            bool implicitCastsAndConversions = Compilation.Options.HasOption(CompilerOption.ImplicitCastsAndConversions, argument.Syntax);
            Conversion conversion = Conversion.NoConversion;
            if (implicitCastsAndConversions)
            {
                // C590 Allow NULL as argument for REF parameters
                var paramRefKinds = (candidate is MethodSymbol) ? (candidate as MethodSymbol).ParameterRefKinds
                    : (candidate is PropertySymbol) ? (candidate as PropertySymbol).ParameterRefKinds
                    : default(ImmutableArray<RefKind>);
                RefKind realParamRefKind = paramRefKinds.IsDefault ? RefKind.None : paramRefKinds[argsToParameters.IsDefault ? argumentPosition : argsToParameters[argumentPosition]];
                if (realParamRefKind == RefKind.Ref && argument.Kind == BoundKind.Literal && ((BoundLiteral)argument).IsLiteralNull())
                {
                    literalNullForRefParameter = true;
                }
                else
                {
                    if (argument is BoundAddressOfOperator baoo)
                    {
                        var argType = baoo.Operand.Type;
                        var parType = parameters.ParameterTypes[argumentPosition].Type;
                        var argIsPtr = argType.IsPointerType() ||
                            argType.IsVoStructOrUnion() ||
                            argType.IsPszType() ||
                            argType.SpecialType == SpecialType.System_IntPtr;
                        if (!argIsPtr)
                        {
                            var parIsPtr = parType.IsPointerType() ||
                                parType.IsVoStructOrUnion() ||
                                parType.IsPszType() ||
                                parType.SpecialType == SpecialType.System_IntPtr;
                            if (realParamRefKind != RefKind.None && argumentRefKind == RefKind.None)
                            {
                                // pass value @foo to function/method that is declared as BAR (n REF Something)
                                argument = baoo.Operand;
                            }
                            else if (!parIsPtr)
                            {
                                var xNode = argument.Syntax.XNode;
                                var isParams = candidate is MethodSymbol ms && ms.IsParams();
                                if (!isParams && xNode.Parent is not XP.QoutStmtContext)
                                {
                                    // pass value @foo to function/method that is declared as BAR (n AS Something)
                                    argument = baoo.Operand;
                                    argumentRefKind = RefKind.Ref;
                                    if (completeResults)
                                    {
                                        arguments.SetRefKind(argumentPosition, argumentRefKind);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (parameterRefKind == RefKind.Out && argumentRefKind == RefKind.Ref)
            {
                // pass variable with REF to function/method that expects OUT (Vulcan did not have OUT)
                argumentRefKind = parameterRefKind;
                arguments.SetRefKind(argumentPosition, argumentRefKind);
                useSiteDiagnostics = new HashSet<DiagnosticInfo>();
                var info = new CSDiagnosticInfo(ErrorCode.WRN_ArgumentRefParameterOut,
                                                new object[] { argumentPosition + 1, parameterRefKind.ToParameterDisplayString() });
                useSiteDiagnostics = new HashSet<DiagnosticInfo>();
                useSiteDiagnostics.Add(info);
            }
            if (parameterRefKind.IsByRef() && argumentRefKind == RefKind.None)
            {
                argumentRefKind = parameterRefKind;
                arguments.SetRefKind(argumentPosition, argumentRefKind);
                if (!implicitCastsAndConversions)
                {
                    useSiteDiagnostics = new HashSet<DiagnosticInfo>();
                    var info = new CSDiagnosticInfo(ErrorCode.ERR_BadArgExtraRef,
                                                    new object[] { argumentPosition + 1, argumentRefKind.ToParameterDisplayString() });
                    useSiteDiagnostics = new HashSet<DiagnosticInfo>();
                    useSiteDiagnostics.Add(info);
                }
            }

            if (literalNullForRefParameter)
            {
                conversion = Conversion.NullLiteral;
            }

            if (implicitCastsAndConversions && argumentRefKind == RefKind.None &&
                argument is BoundAddressOfOperator &&
                candidate.EndsWithUsualParams())
            {
                argumentRefKind = RefKind.Ref;
            }
            return conversion;
        }

        private static BetterResult PreferMostDerived<TMember>(MemberResolutionResult<TMember> m1, MemberResolutionResult<TMember> m2,
            ref HashSet<DiagnosticInfo> useSiteDiagnostics, bool equalLeft, bool equalRight) where TMember : Symbol
        {
            var t1 = m1.Member.ContainingType;
            var t2 = m2.Member.ContainingType;

            if (equalLeft == equalRight)
            {
                if (t1.SpecialType != SpecialType.System_Object && t2.SpecialType == SpecialType.System_Object)
                    return BetterResult.Left;
                if (t1.SpecialType == SpecialType.System_Object && t2.SpecialType != SpecialType.System_Object)
                    return BetterResult.Right;

                if (t1.IsInterfaceType() && t2.IsInterfaceType())
                {
                    if (t1.AllInterfacesWithDefinitionUseSiteDiagnostics(ref useSiteDiagnostics).Contains((NamedTypeSymbol)t2))
                        return BetterResult.Left;
                    if (t2.AllInterfacesWithDefinitionUseSiteDiagnostics(ref useSiteDiagnostics).Contains((NamedTypeSymbol)t1))
                        return BetterResult.Right;
                }
                else if (t1.IsClassType() && t2.IsClassType())
                {
                    if (t1.IsDerivedFrom(t2, TypeCompareKind.ConsiderEverything, useSiteDiagnostics: ref useSiteDiagnostics))
                        return BetterResult.Left;
                    if (t2.IsDerivedFrom(t1, TypeCompareKind.ConsiderEverything, useSiteDiagnostics: ref useSiteDiagnostics))
                        return BetterResult.Right;
                }
            }
            return BetterResult.Neither;
        }

        private bool TypeEquals(TypeSymbol parType, TypeSymbol argType, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            if (parType is { } && argType is { })
            {
                if (Equals(parType, argType))
                    return true;
                if (parType.IsInterfaceType() && argType.ImplementsInterface(parType, ref useSiteDiagnostics))
                    return true;
            }
            return false;
        }

        private BetterResult DetermineRTAssemblyPriority(AssemblySymbol asm1, AssemblySymbol asm2)
        {
            // prefer overload in dialect specific assembly over generic assemblies
            if (asm1.IsDialectSpecificDLL())
            {
                if (asm2.IsRTDLL(XSharpTargetDLL.Core) ||
                    asm2.IsRTDLL(XSharpTargetDLL.RT))
                {
                    return BetterResult.Left;
                }
            }
            if (asm2.IsDialectSpecificDLL())
            {
                if (asm1.IsRTDLL(XSharpTargetDLL.Core) ||
                    asm1.IsRTDLL(XSharpTargetDLL.RT))
                {
                    return BetterResult.Right;
                }
            }
            return BetterResult.Neither;
        }

        private bool checkMatchingParameters(ImmutableArray<ParameterSymbol> pars, ArrayBuilder<BoundExpression> arguments,
            ref int score, ref HashSet<DiagnosticInfo> useSiteDiagnostics, out bool hasArrayPar, out bool hasArrayBasePar)
        {
            var equals = true;
            hasArrayBasePar = hasArrayPar = false;
            int len = pars.Length;
            if (arguments.Count < len)
            {
                len = arguments.Count;
            }
            for (int i = 0; i < len; i++)
            {
                var parType = pars[i].Type;
                hasArrayBasePar |= parType.IsArrayBaseType();
                hasArrayPar |= parType.IsArrayType();
                var arg = arguments[i];
                var argType = arguments[i].Type;
                if (argType is not { })
                    continue;
                if (parType.IsNullableType() && !argType.IsNullableType())
                {
                    parType = parType.GetNullableUnderlyingType();
                }
                var isConst = arg.ConstantValue != null;
                var matchScore = isConst ? 90 : 100;
                if (TypeEquals(parType, argType, ref useSiteDiagnostics))
                {
                    score += matchScore;
                    continue;
                }
                if (parType.TypeKind == TypeKind.Enum)
                {
                    parType = parType.GetEnumUnderlyingType();
                }
                if (argType.TypeKind == TypeKind.Enum)
                {
                    argType = argType.GetEnumUnderlyingType();
                }

                if (parType.IsObjectType() || parType.IsUsualType())
                {
                    // usual argument prefers object or usual parameter type
                    if (argType.IsUsualType())
                    {
                        score += 100;
                    }
                    else if (argType.IsValidVOUsualType())
                    {
                        score += 55;
                    }
                    else if (parType.IsObjectType())
                    {
                        // give a slight preference to usual parameters
                        score -= 1;
                    }
                }
                else if (argType.IsUsualType())
                {
                    // Prefer overload with object variable
                    // Usual is never const
                    if (parType.IsObjectType())
                    {
                        score += 60;
                    }
                    else if (parType.IsValidVOUsualType())
                    {
                        score += 55;
                    }
                    // otherwise we have no preference, since we do not know what's in the usual
                    else
                    {
                        score += 50;
                    }
                }
                else if (argType.IsDerivedFrom(parType, TypeCompareKind.ConsiderEverything, ref useSiteDiagnostics))
                {
                    // determine depth of inheritance
                    // so we will take the most derived
                    var baseType = argType;
                    var depth = isConst ? 25 : 35;
                    while (baseType is { } && !TypeEquals(baseType, parType, ref useSiteDiagnostics))
                    {
                        depth -= 1;
                        baseType = baseType.BaseTypeNoUseSiteDiagnostics;
                    }
                    score += depth;
                }
                else if (parType.IsFloatType() || argType.IsFloatType())
                {
                    // they are NOT both Float
                    if (parType.IsFloatType() && argType.SpecialType == SpecialType.System_Double)
                    {
                        // treat this as matching
                        score += matchScore;
                    }
                    else if (argType.IsFloatType() && parType.SpecialType == SpecialType.System_Double)
                    {
                        // treat this as matching
                        score += matchScore;
                    }
                    else
                    {
                        score += isConst ? 70 : 80;
                    }
                }
                else if (parType.IsCurrencyType() || argType.IsCurrencyType())
                {
                    // they are NOT both Currency
                    if (parType.IsCurrencyType() && argType.SpecialType == SpecialType.System_Decimal)
                    {
                        // treat this as matching
                        score += matchScore;
                    }
                    else if (argType.IsCurrencyType() && parType.SpecialType == SpecialType.System_Decimal)
                    {
                        // treat this as matching
                        score += matchScore;
                    }
                    else
                    {
                        score += isConst ? 70 : 80;
                    }
                }
                else if (argType.IsXNumericType() && parType.IsXNumericType())
                {
                    if (argType.IsIntegralType() && parType.IsIntegralType())
                    {
                        score += isConst ? 35 : 40;
                    }
                    else if (argType.IsFractionalType() && parType.IsFractionalType())
                    {
                        if (argType.IsFloatType() && parType.SpecialType == SpecialType.System_Double)
                        {
                            score += isConst ? 30 : 35;
                        }
                        else
                        {
                            score += isConst ? 25 : 30;
                        }
                    }
                    else if (parType.IsFractionalType())
                    {
                        // argument is then integral type
                        score += isConst ? 20 : 25;
                    }
                    else
                    {
                        // argument is fractional, parameter integral
                        score += isConst ? 15 : 20;
                    }
                }
                else
                {
                    equals = false;
                }
            }
            return equals;
        }

        private string GetSignature(Symbol sym)
        {
            var pars = sym.GetParameters();
            var returnType = sym.GetTypeOrReturnType();
            var result = new System.Text.StringBuilder();
            foreach (var p in pars)
            {
                var type = p.Type;
                result.Append(type.GetDisplayName());
                result.Append(";");
            }
            return result.ToString();
        }

        private BetterResult MatchUsualParameters(Symbol m1, Symbol m2, ArrayBuilder<BoundExpression> arguments)
        {
            var hasUsualArg = arguments.Any(a => a.Type.IsUsualType());
            if (hasUsualArg)
            {
                // when one of the arguments is a usual then
                // prefer the overload that has one or more usual arguments
                var leftUsual = m1.GetParameters().Any(p => p.Type.IsUsualType());
                var rightUsual = m2.GetParameters().Any(p => p.Type.IsUsualType());
                if (leftUsual != rightUsual)
                {
                    if (leftUsual)
                        return BetterResult.Left;
                    else
                        return BetterResult.Right;
                }
            }
            return BetterResult.Neither;
        }
        private BetterResult MatchRefParameters(Symbol m1, Symbol m2, ArrayBuilder<BoundExpression> arguments, out HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            useSiteDiagnostics = null;
            var parsLeft = m1.GetParameters();
            var parsRight = m2.GetParameters();
            var leftHasRef = parsLeft.Any(p => p.RefKind.IsByRef());
            var rightHasRef = parsRight.Any(p => p.RefKind.IsByRef());
            if (!leftHasRef && !rightHasRef)
                return BetterResult.Neither;
            var len = parsLeft.Length;
            if (arguments.Count < len)
                len = arguments.Count;
            for (int i = 0; i < len; i++)
            {
                var parLeft = parsLeft[i];
                var parRight = parsRight[i];
                var refLeft = parLeft.RefKind;
                var refRight = parRight.RefKind;
                var arg = arguments[i];
                bool argCanBeByRef = arg.Kind == BoundKind.AddressOfOperator;
                var argType = arg.Type;
                var leftType = parLeft.Type;
                var rightType = parRight.Type;
                if (argCanBeByRef)
                {
                    var bao = arg as BoundAddressOfOperator;
                    argType = bao.Operand.Type;
                }

                if (!Equals(leftType, rightType) || refLeft != refRight)
                {
                    // Prefer the method with a more specific parameter which is not an array type over USUAL
                    if (leftType.IsUsualType() && argType.IsNotUsualType() && !rightType.IsArray())
                    {
                        return BetterResult.Right;
                    }
                    if (rightType.IsUsualType() && argType.IsNotUsualType() && !leftType.IsArray())
                    {
                        return BetterResult.Left;
                    }
                    // Prefer the method with Object type over the one with Object[] type
                    if (leftType.IsObjectType() && rightType.IsArray() && ((ArrayTypeSymbol)rightType).ElementType.IsObjectType())
                    {
                        return BetterResult.Left;
                    }
                    if (rightType.IsObjectType() && leftType.IsArray() && ((ArrayTypeSymbol)leftType).ElementType.IsObjectType())
                    {
                        return BetterResult.Right;
                    }
                    // Now check for REF parameters and possible REF arguments
                    if (argCanBeByRef)
                    {
                        var op = arg as BoundAddressOfOperator;
                        var opType = op?.Operand?.Type;
                        if (refLeft == RefKind.Ref && Equals(opType, leftType))
                        {
                            return BetterResult.Left;
                        }
                        if (refRight == RefKind.Ref && Equals(opType, rightType))
                        {
                            return BetterResult.Right;
                        }
                        if (refLeft != refRight)
                        {
                            if (refLeft == RefKind.Ref)
                            {
                                return BetterResult.Left;
                            }
                            if (refRight == RefKind.Ref)
                            {
                                return BetterResult.Right;
                            }
                        }
                    }
                    if (refLeft != refRight)
                    {
                        if (TypeEquals(leftType, argType, ref useSiteDiagnostics) && refLeft != RefKind.None && argCanBeByRef)
                        {
                            return BetterResult.Left;
                        }
                        if (TypeEquals(rightType, argType, ref useSiteDiagnostics) && refRight != RefKind.None && argCanBeByRef)
                        {
                            return BetterResult.Right;
                        }
                        if (TypeEquals(leftType, argType, ref useSiteDiagnostics) && refLeft == RefKind.None && !argCanBeByRef)
                        {
                            return BetterResult.Left;
                        }
                        if (TypeEquals(rightType, argType, ref useSiteDiagnostics) && refRight == RefKind.None && !argCanBeByRef)
                        {
                            return BetterResult.Right;
                        }
                    }
                }
            }
            return BetterResult.Neither;
        }


        private BetterResult XsBetterFunctionMemberImpl<TMember>(
            MemberResolutionResult<TMember> m1,
            MemberResolutionResult<TMember> m2,
            ArrayBuilder<BoundExpression> arguments,
            out HashSet<DiagnosticInfo> useSiteDiagnostics
            )
            where TMember : Symbol
        {
            useSiteDiagnostics = null;
            BetterResult result = BetterResult.Neither;
            var parsLeft = m1.Member.GetParameters();
            var parsRight = m2.Member.GetParameters();
            var type1 = m1.Member.ContainingType;
            var type2 = m2.Member.ContainingType;

            var func1 = m1.Member.IsStatic && type1.IsFunctionsClass();
            var func2 = m2.Member.IsStatic && type2.IsFunctionsClass();

            int leftScore = 0;
            int rightScore = 0;
            bool equalLeft = checkMatchingParameters(parsLeft, arguments, ref leftScore, ref useSiteDiagnostics, out var hasArrayParL, out var hasArrayBaseParL);
            bool equalRight = checkMatchingParameters(parsRight, arguments, ref rightScore, ref useSiteDiagnostics, out var hasArrayParR, out var hasArrayBaseParR);
            if (equalLeft && equalRight)
            {
                if (hasArrayParL && hasArrayBaseParR)
                    return BetterResult.Left;
                if (hasArrayParR && hasArrayBaseParL)
                    return BetterResult.Right;

            }
            bool Ambiguous = false;
            var asm1 = m1.Member.ContainingAssembly;
            var asm2 = m2.Member.ContainingAssembly;
            var rt1 = asm1.IsRT();
            var rt2 = asm2.IsRT();
            bool bothRT = rt1 && rt2;

            var sig1 = GetSignature(m1.Member);
            var sig2 = GetSignature(m2.Member);
            var sdk1 = asm1.IsSdk();
            var sdk2 = asm2.IsSdk();
            if (Compilation.Options.HasRuntime)
            {
                if (sig1 == sig2)
                {
                    if (type1.IsDerivedFrom(type2, TypeCompareKind.ConsiderEverything, ref useSiteDiagnostics))
                    {
                        return BetterResult.Left;
                    }
                    if (type2.IsDerivedFrom(type1, TypeCompareKind.ConsiderEverything, ref useSiteDiagnostics))
                    {
                        return BetterResult.Right;
                    }
                }
                if (asm1 != asm2 && sig1 == sig2)
                {
                    // prefer non runtime over runtime to allow customers to override built-in functions
                    // we ignore the prototype of the function here
                    // This means that even when the function in XSharp.Core or XSharp.RT matches better
                    // then that function will still not be chosen.
                    // we generate no warning when there are identical signature
                    // assume that they know what they are doing
                    if (rt1 != rt2)
                    {
                        if (rt1)
                        {
                            return BetterResult.Right;
                        }
                        else //  (rt2)
                        {
                            return BetterResult.Left;
                        }
                    }
                }
                if (rt1 && rt2)
                {
                    // when function of same name in 2 runtime DLLs then determine
                    // the priority
                    result = DetermineRTAssemblyPriority(asm1, asm2);
                    if (result != BetterResult.Neither)
                        return result;
                }

                if (!Equals(type1, type2) && sig1 == sig2)
                {
                    result = PreferMostDerived(m1, m2, ref useSiteDiagnostics, equalLeft, equalRight);
                    if (result != BetterResult.Neither)
                        return result;
                }
                if (asm1 != asm2)
                {
                    var sys1 = rt2 && asm1.Name.StartsWith("System", StringComparison.OrdinalIgnoreCase);
                    var sys2 = rt1 && asm2.Name.StartsWith("System", StringComparison.OrdinalIgnoreCase);
                    // prefer our assembly or other assembly that is not a runtime and not a system assembly over runtime
                    // the system check is needed because XSharp.RT has some Linq methods for Float and Currency
                    // that need to have preference over the linq methods for real4, real8 and decimal.
                    if (asm1.IsFromCompilation(Compilation) || (rt2 && !rt1 && !sys1))
                    {
                        result = BetterResult.Left;
                        if (leftScore <= rightScore)
                            useSiteDiagnostics = GenerateWarning(bothRT, m1.Member, m2.Member, ErrorCode.WRN_XSharpAmbiguous);
                        return result;
                    }
                    if (asm2.IsFromCompilation(Compilation) || (rt1 && !rt2 && !sys2))
                    {
                        result = BetterResult.Right;
                        if (rightScore <= leftScore)
                            useSiteDiagnostics = GenerateWarning(bothRT, m2.Member, m1.Member, ErrorCode.WRN_XSharpAmbiguous);
                        return result;
                    }
                }
                var m1Clipper = m1.Member.HasClipperCallingConvention();
                var m2Clipper = m2.Member.HasClipperCallingConvention();
                if (m1Clipper != m2Clipper)
                {
                    if (m1Clipper)
                    {
                        result = BetterResult.Right;
                        useSiteDiagnostics = GenerateWarning(bothRT, m2.Member, m1.Member, ErrorCode.WRN_XSharpAmbiguous);
                    }
                    else
                    {
                        result = BetterResult.Left;
                        useSiteDiagnostics = GenerateWarning(bothRT, m1.Member, m2.Member, ErrorCode.WRN_XSharpAmbiguous);
                    }
                    return result;
                }

                // prefer __Array params over __ArrayBase<> params
                if (leftScore != rightScore)
                {
                    if (leftScore > rightScore)
                        return BetterResult.Left;
                    else
                        return BetterResult.Right;
                }
                if (sig1 == sig2)
                {
                    // when a function is in one of the SDK dlls choose the other
                    if (sdk1 || sdk2)
                    {
                        return sdk1 ? BetterResult.Left : BetterResult.Right;
                    }
                }
                if (m1.Member.GetParameterCount() == m2.Member.GetParameterCount())
                {
                    // we have different / extended rules compared to C#
                    // Check for REF parameters and possible REF arguments
                    // now fall back to original type (and not addressof type)
                    result = MatchRefParameters(m1.Member, m2.Member, arguments, out useSiteDiagnostics);
                    if (result != BetterResult.Neither)
                    {
                        return result;
                    }
                    result = MatchUsualParameters(m1.Member, m2.Member, arguments);
                    if (result != BetterResult.Neither)
                    {
                        return result;
                    }

                    // check if all left and types are equal. The score is the # of matching types
                    // Only exit here when one of the two is better than the other
                    if (equalLeft != equalRight)
                    {
                        if (equalLeft)
                            return BetterResult.Left;
                        else
                            return BetterResult.Right;
                    }

                    if (leftScore != rightScore)
                    {
                        if (leftScore > rightScore)
                            return BetterResult.Left;
                        else
                            return BetterResult.Right;
                    }
                }
                else
                {
                    // parameter counts are different for m1 and m2
                    // choose the one where the parameter count matches the #help of arguments
                    if (m1.Member.GetParameterCount() == arguments.Count)
                    {
                        return BetterResult.Left;
                    }
                    if (m2.Member.GetParameterCount() == arguments.Count)
                    {
                        return BetterResult.Right;
                    }
                    // both methods have a different # of arguments
                }
                // when both methods are in a functions class from different assemblies
                // pick the first one in the references list
                //
                if (asm1 != asm2
                    && XSharpString.Equals(m1.Member.ContainingType.Name, XSharpSpecialNames.FunctionsClass)
                    && XSharpString.Equals(m2.Member.ContainingType.Name, XSharpSpecialNames.FunctionsClass))
                {
                    foreach (var reference in Compilation.ReferencedAssemblyNames)
                    {
                        if (reference.Name == asm1.Name)
                        {
                            result = BetterResult.Left;
                            Ambiguous = true;
                        }
                        if (reference.Name == asm2.Name)
                        {
                            result = BetterResult.Right;
                            Ambiguous = true;
                        }
                        if (Ambiguous)
                        {
                            TMember r1, r2;
                            if (result == BetterResult.Left)
                            {
                                r1 = m1.Member;
                                r2 = m2.Member;
                            }
                            else
                            {
                                r1 = m2.Member;
                                r2 = m1.Member;
                            }

                            useSiteDiagnostics = GenerateWarning(bothRT, r1, r2, ErrorCode.WRN_XSharpAmbiguous);
                            return result;
                        }
                    }
                }
            }
            // generate warning that function takes precedence over static method
            if (func1 && !func2)
            {
                result = BetterResult.Left;
                useSiteDiagnostics = GenerateWarning(bothRT, m1.Member, m2.Member, ErrorCode.WRN_FunctionsTakePrecedenceOverMethods);
                return result;
            }
            else if (func2 && !func1)
            {
                result = BetterResult.Right;
                useSiteDiagnostics = GenerateWarning(bothRT, m2.Member, m1.Member, ErrorCode.WRN_FunctionsTakePrecedenceOverMethods);
                return result;
            }
            if (!Equals(type1, type2))
            {
                result = PreferMostDerived(m1, m2, ref useSiteDiagnostics, equalLeft, equalRight);
                if (result != BetterResult.Neither)
                    return result;
            }
            return BetterResult.Neither;
        }
        HashSet<DiagnosticInfo> GenerateWarning(bool bothRT, Symbol r1, Symbol r2, ErrorCode warning)
        {
            if (!bothRT)
            {
                var diag = new HashSet<DiagnosticInfo>();
                var info = new CSDiagnosticInfo(warning,
                        new object[] {
                            r1.Name,
                            r1.Kind.ToString(),
                            new FormattedSymbol(r1, SymbolDisplayFormat.CSharpErrorMessageFormat),
                            r1.ContainingAssembly.Name,
                            r2.Kind.ToString(),
                            new FormattedSymbol(r2, SymbolDisplayFormat.CSharpErrorMessageFormat),
                            r2.ContainingAssembly.Name,
                        });
                diag.Add(info);
                return diag;
            }
            return null;
        }

        /// <summary>
        /// This function tries to decide which of 2 overloads needs to be picked.
        /// The logic is VERY complicated and fragile
        /// In the code below m1 is called Left and m2 is called Right (to match the return BetterLeft and BetterRight)
        /// </summary>
        /// <returns></returns>
        private BetterResult XsBetterFunctionMember<TMember>(MemberResolutionResult<TMember> m1,
            MemberResolutionResult<TMember> m2,
            ArrayBuilder<BoundExpression> arguments,
            out HashSet<DiagnosticInfo> useSiteDiagnostics
            ) where TMember : Symbol
        {
            // Prefer the member not declared in VulcanRT, if applicable
            useSiteDiagnostics = null;
            var parsLeft = m1.Member.GetParameters();
            var parsRight = m2.Member.GetParameters();
            var type1 = m1.Member.ContainingType;
            var type2 = m2.Member.ContainingType;

            // we only want to use "our" logic when:
            // a function is involved
            // when one of the parameters or arguments is one or our types or an array of one of our types
            // When one of the arguments is an enum (we allow more implicit conversions)
            // are there any other situations?
            var func1 = m1.Member.IsStatic && type1.IsFunctionsClass();
            var func2 = m2.Member.IsStatic && type2.IsFunctionsClass();
            var useOurLogic = func1 || func2 ||
                parsLeft.Any((p) => p.Type.IsOurType() || p.RefKind.IsByRef()) ||
                parsRight.Any((p) => p.Type.IsOurType() || p.RefKind.IsByRef()) ||
                arguments.Any((a) => a.Type.IsOurType()) ||
                arguments.Any((a) => a.IsLiteralNull()) ||
                arguments.Any((a) => a.Type is { } && a.Type.IsEnumType());
            if (useOurLogic)
            {
                return XsBetterFunctionMemberImpl(m1, m2, arguments, out useSiteDiagnostics);
            }
            return BetterResult.Neither;
        }

        private BetterResult XsPreferMostDerived<TMember>(ArrayBuilder<BoundExpression> arguments,
            MemberResolutionResult<TMember> m1,
            MemberResolutionResult<TMember> m2,
            ref HashSet<DiagnosticInfo> useSiteDiagnostics)
            where TMember : Symbol
        {
            var parsLeft = m1.Member.GetParameters();
            var parsRight = m2.Member.GetParameters();
            int score = 0;
            bool equalLeft = checkMatchingParameters(parsLeft, arguments, ref score, ref useSiteDiagnostics, out _, out _);
            bool equalRight = checkMatchingParameters(parsRight, arguments, ref score, ref useSiteDiagnostics, out _, out _);
            return PreferMostDerived(m1, m2, ref useSiteDiagnostics, equalLeft, equalRight);
        }
        private BetterResult VoBetterOperator(BinaryOperatorSignature op1, BinaryOperatorSignature op2, BoundExpression left, BoundExpression right, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            // When the binary operators are equal we inspect the types
            if ((op1.Kind & BinaryOperatorKind.OpMask) == (op2.Kind & BinaryOperatorKind.OpMask))
            {
                if ((op1.Kind & BinaryOperatorKind.TypeMask) == BinaryOperatorKind.Float &&
                    (op2.Kind & BinaryOperatorKind.TypeMask) == BinaryOperatorKind.Double)
                {
                    // Lhs = real4, rhs = real8, choose real8

                    return BetterResult.Right;
                }
                if ((op1.Kind & BinaryOperatorKind.TypeMask) == BinaryOperatorKind.Double)
                {
                    // rhs = numeric, lhs = double choose double
                    switch (op2.Kind & BinaryOperatorKind.TypeMask)
                    {
                        case BinaryOperatorKind.Int:
                        case BinaryOperatorKind.UInt:
                        case BinaryOperatorKind.Long:
                        case BinaryOperatorKind.ULong:
                        case BinaryOperatorKind.Float:
                        case BinaryOperatorKind.Decimal:
                            return BetterResult.Left;
                    }
                }
                if ((op2.Kind & BinaryOperatorKind.TypeMask) == BinaryOperatorKind.Double)
                {
                    // lhs = numeric, rhs = double choose double
                    switch (op1.Kind & BinaryOperatorKind.TypeMask)
                    {
                        case BinaryOperatorKind.Int:
                        case BinaryOperatorKind.UInt:
                        case BinaryOperatorKind.Long:
                        case BinaryOperatorKind.ULong:
                        case BinaryOperatorKind.Float:
                        case BinaryOperatorKind.Decimal:
                            return BetterResult.Right;
                    }
                }
                if (!object.ReferenceEquals(left.Type, null) && !object.ReferenceEquals(right.Type, null))
                {
                    bool enumL = left.Type.IsEnumType() || left.Type.IsNullableType() && left.Type.GetNullableUnderlyingType().IsEnumType();
                    bool enumR = right.Type.IsEnumType() || right.Type.IsNullableType() && right.Type.GetNullableUnderlyingType().IsEnumType();
                    if (enumL ^ enumR)
                    {
                        bool enum1 = (op1.LeftType.IsEnumType() || op1.LeftType.IsNullableType() && op1.LeftType.GetNullableUnderlyingType().IsEnumType())
                            && (op1.RightType.IsEnumType() || op1.RightType.IsNullableType() && op1.RightType.GetNullableUnderlyingType().IsEnumType());
                        bool enum2 = (op2.LeftType.IsEnumType() || op2.LeftType.IsNullableType() && op2.LeftType.GetNullableUnderlyingType().IsEnumType())
                            && (op2.RightType.IsEnumType() || op2.RightType.IsNullableType() && op2.RightType.GetNullableUnderlyingType().IsEnumType());
                        if (enum1 && !enum2)
                        {
                            return BetterResult.Left;
                        }
                        else if (!enum1 && enum2)
                            return BetterResult.Right;
                    }
                    // when /vo4 or /vo11 is enabled then we may end up having duplicate candidates
                    // we decide here which one takes precedence
                    if (Compilation.Options.HasOption(CompilerOption.Vo4, left.Syntax) || // vo4
                        Compilation.Options.HasOption(CompilerOption.Vo11, left.Syntax)) // vo11
                    {
                        #region Integral Binary Operators
                        if (left.Type.IsIntegralType() && right.Type.IsIntegralType()
                            && op1.Kind.IsIntegral() && op2.Kind.IsIntegral())
                        {
                            // when both operands have integral types, choose the one that match the sign and or size
                            // we check the lhs of the expression first
                            bool exprSigned = left.Type.SpecialType.IsSignedIntegralType();
                            bool op1Signed = op1.LeftType.SpecialType.IsSignedIntegralType();
                            bool op2Signed = op2.LeftType.SpecialType.IsSignedIntegralType();
                            int exprSize = left.Type.SpecialType.SizeInBytes();
                            int op1Size = op1.LeftType.SpecialType.SizeInBytes();
                            int op2Size = op2.LeftType.SpecialType.SizeInBytes();
                            // op1 matches sign and size and op2 does not
                            if ((exprSigned == op1Signed && exprSize == op1Size)
                                && (exprSigned != op2Signed || exprSize != op2Size))
                            {
                                return BetterResult.Left;
                            }
                            // op2 matches sign and size and op1 does not
                            if ((exprSigned != op1Signed || exprSize != op1Size)
                                && (exprSigned == op2Signed && exprSize == op2Size))
                            {
                                return BetterResult.Right;
                            }
                            // When we get here they both match or both do not match the sign and size
                            // now check the rhs of the expression, to see if this helps to decide
                            exprSigned = right.Type.SpecialType.IsSignedIntegralType();
                            exprSize = right.Type.SpecialType.SizeInBytes();
                            op1Signed = op1.RightType.SpecialType.IsSignedIntegralType();
                            op2Signed = op2.RightType.SpecialType.IsSignedIntegralType();
                            // when still undecided then choose the one where the size matches best
                            // op1 matches sign and size and op2 does not
                            if ((exprSigned == op1Signed && exprSize == op1Size)
                                && (exprSigned != op2Signed || exprSize != op2Size))
                            {
                                return BetterResult.Left;
                            }
                            // op2 matches sign and size and op1 does not
                            if ((exprSigned != op1Signed || exprSize != op1Size)
                                && (exprSigned == op2Signed && exprSize == op2Size))
                            {
                                return BetterResult.Right;
                            }
                            // still no match. Forget the size and check only on sign
                            exprSigned = left.Type.SpecialType.IsSignedIntegralType();
                            op1Signed = op1.LeftType.SpecialType.IsSignedIntegralType();
                            op2Signed = op2.LeftType.SpecialType.IsSignedIntegralType();
                            // op1 matches sign and op2 does not
                            if (exprSigned == op1Signed && exprSigned != op2Signed)
                            {
                                return BetterResult.Left;
                            }
                            // op2 matches sign and op1 does not
                            if (exprSigned != op1Signed && exprSigned == op2Signed)
                            {
                                return BetterResult.Right;
                            }
                            exprSigned = right.Type.SpecialType.IsSignedIntegralType();
                            op1Signed = op1.RightType.SpecialType.IsSignedIntegralType();
                            op2Signed = op2.RightType.SpecialType.IsSignedIntegralType();
                            // op1 matches sign and op2 does not
                            if (exprSigned == op1Signed && exprSigned != op2Signed)
                            {
                                return BetterResult.Left;
                            }
                            // op2 matches sign and op1 does not
                            if (exprSigned != op1Signed && exprSigned == op2Signed)
                            {
                                return BetterResult.Right;
                            }
                        }
                        #endregion
                    }

                    if ((left.Type.IsIntegralType() && right.Type.IsPointerType())
                        || left.Type.IsPointerType() && right.Type.IsIntegralType())
                    {
                        if (op1.LeftType.IsVoidPointer() && op1.RightType.IsVoidPointer())
                            return BetterResult.Left;
                        if (op2.LeftType.IsVoidPointer() && op2.RightType.IsVoidPointer())
                            return BetterResult.Right;
                    }
                    // Prefer Date over DateTime, because when one of the two is date then we know that we can't compare the time parts
                    if (left.Type.SpecialType == SpecialType.System_DateTime && right.Type.IsDateType())
                        return BetterResult.Right;
                    if (right.Type.SpecialType == SpecialType.System_DateTime && left.Type.IsDateType())
                        return BetterResult.Left;
                }
            }
            // Solve Literal operations such as generated by ForNext statement
            var literal = right.Kind == BoundKind.Literal;
            if (!literal && right.Kind == BoundKind.UnaryOperator)
            {
                var unop = (BoundUnaryOperator)right;
                literal = unop.Operand.Kind == BoundKind.Literal;
            }
            if (literal && Equals(op1.LeftType, left.Type))
            {
                if (left.Type.SpecialType.IsSignedIntegralType())     // When signed, always Ok
                    return BetterResult.Left;
                else if (left.Type.SpecialType.IsIntegralType())      // Unsigned integral, so check for overflow
                {

                    var constValue = ((BoundLiteral)right).ConstantValue;
                    if (constValue.IsIntegral && constValue.Int64Value >= 0)
                    {
                        return BetterResult.Left;
                    }
                }
                else // not integral, so most likely floating point
                {
                    return BetterResult.Left;
                }
            }
            literal = left.Kind == BoundKind.Literal;
            if (!literal && left.Kind == BoundKind.UnaryOperator)
            {
                var unop = (BoundUnaryOperator)left;
                literal = unop.Operand.Kind == BoundKind.Literal;
            }
            if (literal && Equals(op1.RightType, right.Type))
            {
                if (right.Type.SpecialType.IsSignedIntegralType())     // When signed, always Ok
                    return BetterResult.Left;
                else if (right.Type.SpecialType.IsIntegralType())      // Unsigned integral, so check for overflow
                {

                    var constValue = ((BoundLiteral)left).ConstantValue;
                    if (constValue.IsIntegral && constValue.Int64Value >= 0)
                    {
                        return BetterResult.Left;
                    }
                }
                else // not integral, so most likely floating point
                {
                    return BetterResult.Left;
                }
            }
            if (Compilation.Options.HasRuntime)
            {
                if (left.Type.IsNotUsualType())
                {
                    if (op1.RightType.IsNotUsualType() && op2.RightType.IsUsualType())
                        return BetterResult.Left;
                    if (op2.RightType.IsNotUsualType() && op1.RightType.IsUsualType())
                        return BetterResult.Right;
                }
                if (right.Type.IsNotUsualType())
                {
                    if (op1.LeftType.IsNotUsualType() && op2.LeftType.IsUsualType())
                        return BetterResult.Left;
                    if (op2.LeftType.IsNotUsualType() && op1.LeftType.IsUsualType())
                        return BetterResult.Right;
                }
            }
            return BetterResult.Neither;
        }
        private bool VOStructBinaryOperatorComparison(BinaryOperatorKind kind, BoundExpression left, BoundExpression right, BinaryOperatorOverloadResolutionResult result)
        {
            if (Equals(left.Type, right.Type))
            {
                bool isVoStruct;
                if (left.Type.IsPointerType())
                {
                    var pt = left.Type as PointerTypeSymbol;
                    isVoStruct = pt.PointedAtType.IsVoStructOrUnion();
                }
                else
                {
                    isVoStruct = left.Type.IsVoStructOrUnion();
                }
                if (isVoStruct && (kind == BinaryOperatorKind.Equal || kind == BinaryOperatorKind.NotEqual))
                {
                    BinaryOperatorSignature sig = new BinaryOperatorSignature(kind, left.Type, right.Type, Compilation.GetSpecialType(SpecialType.System_Boolean));
                    BinaryOperatorAnalysisResult best = BinaryOperatorAnalysisResult.Applicable(sig, Conversion.Identity, Conversion.Identity);
                    result.Results.Clear();
                    result.Results.Add(best);
                    return true;
                }
            }
            return false;
        }
    }
    internal static class CastExtensionMethods
    {
        internal static bool IsVoCast(this XSharpParserRuleContext node)
        {
            if (node is XP.PrimaryExpressionContext pec)
            {
                return pec.Expr is XP.VoCastExpressionContext;
            }
            return false;
        }
        internal static bool IsVoConvert(this XSharpParserRuleContext node)
        {
            if (node is XP.PrimaryExpressionContext pec)
            {
                return pec.Expr is XP.VoConversionExpressionContext;
            }
            return false;
        }
        internal static bool IsCastClass(this XSharpParserRuleContext node)
        {
            return node != null && node.Start?.Type == XSharpLexer.CASTCLASS;
        }
    }
}
