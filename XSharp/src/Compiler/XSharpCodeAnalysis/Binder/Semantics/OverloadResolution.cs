//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

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

        private static BetterResult PreferMostDerived<TMember>(MemberResolutionResult<TMember> m1, MemberResolutionResult<TMember> m2, ref HashSet<DiagnosticInfo> useSiteDiagnostics) where TMember : Symbol
        {
            var t1 = m1.Member.ContainingType;
            var t2 = m2.Member.ContainingType;

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

        private BetterResult DetermineAssemblyPriority(AssemblySymbol asm1, AssemblySymbol asm2)
        {
            // prefer overload in dialect specific over other assemblies
            if (asm1.IsRTDLL(XSharpTargetDLL.VO) ||
                asm1.IsRTDLL(XSharpTargetDLL.VFP) ||
                asm1.IsRTDLL(XSharpTargetDLL.XPP))
            {
                if (asm2.IsRTDLL(XSharpTargetDLL.Core) ||
                    asm2.IsRTDLL(XSharpTargetDLL.RT))
                {
                    return BetterResult.Left;
                }
            }
            if (asm2.IsRTDLL(XSharpTargetDLL.VO) ||
                asm2.IsRTDLL(XSharpTargetDLL.VFP) ||
                asm2.IsRTDLL(XSharpTargetDLL.XPP))
            {
                if (asm1.IsRTDLL(XSharpTargetDLL.Core) ||
                    asm1.IsRTDLL(XSharpTargetDLL.RT))
                {
                    return BetterResult.Right;
                }
            }
            return BetterResult.Neither;
        }

        private bool checkMatchingParameters(ImmutableArray<ParameterSymbol> pars, ArrayBuilder<BoundExpression> arguments, ref int score, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            var equals = true;
            int len = pars.Length;
            if (arguments.Count < len)
            {
                len = arguments.Count;
            }
            for (int i = 0; i < len; i++)
            {
                var parType = pars[i].Type;
                var arg = arguments[i];
                var argType = arguments[i].Type;
                if (argType is not { })
                    continue;
                var isConst = arg.ConstantValue != null;

                if (TypeEquals(parType, argType, ref useSiteDiagnostics))
                {
                    score += isConst ? 90 : 100;
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
                if (TypeEquals(parType, argType, ref useSiteDiagnostics))
                {
                    score += isConst ? 90 : 100;
                    continue;
                }


                if (parType.IsObjectType() || parType.IsUsualType())
                {
                    if (argType.IsValidVOUsualType(Compilation))
                    {
                        score += isConst ? 45 : 50;
                    }
                }
                else if (argType.IsUsualType())
                {
                    if (parType.IsXNumericType() &&
                        (parType.SpecialType == SpecialType.System_Double || parType.IsFloatType()))
                    {
                        score += isConst ? 55 : 60;
                    }
                    else if (parType.IsValidVOUsualType(Compilation))
                    {
                        score += isConst ? 45 : 50;
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

        private bool MatchUsualParameters(Symbol m1, Symbol m2, ArrayBuilder<BoundExpression> arguments, out BetterResult result)
        {
            result = BetterResult.Neither;
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
                        result = BetterResult.Left;
                    else
                        result = BetterResult.Right;
                    return true;
                }
            }
            return false;
        }
        private bool MatchRefParameters(Symbol m1, Symbol m2, ArrayBuilder<BoundExpression> arguments, out HashSet<DiagnosticInfo> useSiteDiagnostics, out BetterResult result)
        {
            useSiteDiagnostics = null;
            result = BetterResult.Neither;
            var parsLeft = m1.GetParameters();
            var parsRight = m2.GetParameters();
            var leftHasRef = parsLeft.Any(p => p.RefKind.IsByRef());
            var rightHasRef = parsRight.Any(p => p.RefKind.IsByRef());
            if (!leftHasRef && !rightHasRef)
                return false;
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
                        result = BetterResult.Right;
                        return true;
                    }
                    if (rightType.IsUsualType() && argType.IsNotUsualType() && !leftType.IsArray())
                    {
                        result = BetterResult.Left;
                        return true;
                    }
                    // Prefer the method with Object type over the one with Object[] type
                    if (leftType.IsObjectType() && rightType.IsArray() && ((ArrayTypeSymbol)rightType).ElementType.IsObjectType())
                    {
                        result = BetterResult.Left;
                        return true;
                    }
                    if (rightType.IsObjectType() && leftType.IsArray() && ((ArrayTypeSymbol)leftType).ElementType.IsObjectType())
                    {
                        result = BetterResult.Right;
                        return true;
                    }
                    // Now check for REF parameters and possible REF arguments
                    if (argCanBeByRef)
                    {
                        var op = arg as BoundAddressOfOperator;
                        var opType = op?.Operand?.Type;
                        if (refLeft == RefKind.Ref && Equals(opType, leftType))
                        {
                            result = BetterResult.Left;
                            return true;
                        }
                        if (refRight == RefKind.Ref && Equals(opType, rightType))
                        {
                            result = BetterResult.Right;
                            return true;
                        }
                        if (refLeft != refRight)
                        {
                            if (refLeft == RefKind.Ref)
                            {
                                result = BetterResult.Left;
                                return true;
                            }
                            if (refRight == RefKind.Ref)
                            {
                                result = BetterResult.Right;
                                return true;
                            }
                        }
                    }
                    if (refLeft != refRight)
                    {
                        if (TypeEquals(leftType, argType, ref useSiteDiagnostics) && refLeft != RefKind.None && argCanBeByRef)
                        {
                            result = BetterResult.Left;
                            return true;
                        }
                        if (TypeEquals(rightType, argType, ref useSiteDiagnostics) && refRight != RefKind.None && argCanBeByRef)
                        {
                            result = BetterResult.Right;
                            return true;
                        }
                        if (TypeEquals(leftType, argType, ref useSiteDiagnostics) && refLeft == RefKind.None && !argCanBeByRef)
                        {
                            result = BetterResult.Left;
                            return true;
                        }
                        if (TypeEquals(rightType, argType, ref useSiteDiagnostics) && refRight == RefKind.None && !argCanBeByRef)
                        {
                            result = BetterResult.Right;
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        /// <summary>
        /// This function tries to decide which of 2 overloads needs to be picked.
        /// The logic is VERY complicated and fragile
        /// In the code below m1 is called Left and m2 is called Right (to match the return BetterLeft and BetterRight)
        /// </summary>
        /// <typeparam name="TMember"></typeparam>
        /// <param name="m1"></param>
        /// <param name="m2"></param>
        /// <param name="arguments"></param>
        /// <param name="result"></param>
        /// <param name="useSiteDiagnostics"></param>
        /// <returns></returns>
        private bool VOBetterFunctionMember<TMember>(
            MemberResolutionResult<TMember> m1,
            MemberResolutionResult<TMember> m2,
            ArrayBuilder<BoundExpression> arguments,
            out BetterResult result,
            out HashSet<DiagnosticInfo> useSiteDiagnostics
            )
            where TMember : Symbol
        {
            result = BetterResult.Neither;
            bool Ambiguous = false;
            // Prefer the member not declared in VulcanRT, if applicable
            useSiteDiagnostics = null;
            int leftScore = 0;
            int rightScore = 0;
            if (Compilation.Options.HasRuntime)
            {
                m1.Member.GetParameters();
                var asm1 = m1.Member.ContainingAssembly;
                var asm2 = m2.Member.ContainingAssembly;
                if (asm1 != asm2 && GetSignature(m1.Member) == GetSignature(m2.Member))
                {
                    // prefer non runtime over runtime to allow customers to override built-in functions
                    if (asm1.IsRT() != asm2.IsRT() )
                    {
                        if (asm1.IsRT())
                        {
                            result = BetterResult.Right;
                            return true;
                        }
                        else if (asm2.IsRT())
                        {
                            result = BetterResult.Left;
                            return true;
                        }
                    }
                    if (asm1.IsRT() && asm2.IsRT())
                    {
                        result = DetermineAssemblyPriority(asm1, asm2);
                        if (result != BetterResult.Neither)
                            return true;
                    }
                    // prefer functions/method in the current assembly over external methods
                    if (asm1.IsFromCompilation(Compilation))
                    {
                        result = BetterResult.Left;
                        return true;
                    }
                    if (asm2.IsFromCompilation(Compilation))
                    {
                        result = BetterResult.Right;
                        return true;
                    }
                }
                if (m1.Member.HasClipperCallingConvention() != m2.Member.HasClipperCallingConvention())
                {
                    if (m1.Member.HasClipperCallingConvention())
                        result = BetterResult.Right;
                    else
                        result = BetterResult.Left;
                    return true;
                }

                if (m1.Member.GetParameterCount() == m2.Member.GetParameterCount())
                {
                    // In case of 2 methods with the same # of parameters 
                    // we have different / extended rules compared to C#
                    var parsLeft = m1.Member.GetParameters();
                    var parsRight = m2.Member.GetParameters();
                    var len = parsLeft.Length;
                    if (arguments.Count < len)
                        len = arguments.Count;

                    // Now check for REF parameters and possible REF arguments
                    // now fall back to original type (and not addressof type)
                    if (MatchRefParameters(m1.Member, m2.Member, arguments, out useSiteDiagnostics, out result))
                    {
                        return true;
                    }
                    if (MatchUsualParameters(m1.Member, m2.Member, arguments, out result))
                    {
                        return true;
                    }


                    // check if all left and types are equal. The score is the # of matching types
                    bool equalLeft = checkMatchingParameters(parsLeft, arguments, ref leftScore, ref useSiteDiagnostics);
                    bool equalRight = checkMatchingParameters(parsRight, arguments, ref rightScore, ref useSiteDiagnostics);
                    // Only exit here when one of the two is better than the other
                    if (equalLeft != equalRight)
                    {
                        if (equalLeft)
                            result = BetterResult.Left;
                        else
                            result = BetterResult.Right;
                        return true;
                    }

                    if (leftScore != rightScore)
                    {
                        if (leftScore > rightScore)
                        {
                            result = BetterResult.Left;
                        }
                        else
                        {
                            result = BetterResult.Right;
                        }
                        return true;
                    }
                    /*
                    for (int i = 0; i < len; i++)
                    {
                        var parLeft = parsLeft[i];
                        var parRight = parsRight[i];
                        var arg = arguments[i];
                        var argType = arg.Type;
                        var leftType = parLeft.Type;
                        var rightType = parRight.Type;

                        if (!Equals(leftType, rightType))
                        {
                            // Prefer the method with a more specific parameter which is not an array type over USUAL
                            if (leftType.IsUsualType() && argType.IsNotUsualType() && !rightType.IsArray())
                            {
                                result = BetterResult.Right;
                                return true;
                            }
                            if (rightType.IsUsualType() && argType.IsNotUsualType() && !leftType.IsArray())
                            {
                                result = BetterResult.Left;
                                return true;
                            }
                            // Prefer the method with Object type over the one with Object[] type
                            if (leftType.IsObjectType() && rightType.IsArray() && ((ArrayTypeSymbol)rightType).ElementType.IsObjectType())
                            {
                                result = BetterResult.Left;
                                return true;
                            }
                            if (rightType.IsObjectType() && leftType.IsArray() && ((ArrayTypeSymbol)leftType).ElementType.IsObjectType())
                            {
                                result = BetterResult.Right;
                                return true;
                            }
                            argType = arg.Type;
                            // Handle passing Enum values to methods that have a non enum parameter
                            if (argType?.TypeKind == TypeKind.Enum)
                            {
                                // First check if they have the enum type itself
                                if (TypeEquals(argType, leftType, ref useSiteDiagnostics))
                                {
                                    result = BetterResult.Left;
                                    return true;
                                }
                                if (TypeEquals(argType, rightType, ref useSiteDiagnostics))
                                {
                                    result = BetterResult.Right;
                                    return true;
                                }
                                // Then check the underlying type
                                argType = argType.GetEnumUnderlyingType();
                                if (TypeEquals(argType, leftType, ref useSiteDiagnostics))
                                {
                                    result = BetterResult.Left;
                                    return true;
                                }
                                if (TypeEquals(argType, rightType, ref useSiteDiagnostics))
                                {
                                    result = BetterResult.Right;
                                    return true;
                                }
                            }
                            // VoFloat prefers overload with double over all other conversions
                            if (argType.IsFloatType() || argType.IsUsualType())
                            {
                                var doubleType = Compilation.GetSpecialType(SpecialType.System_Double);
                                if (TypeEquals(leftType, doubleType, ref useSiteDiagnostics))
                                {
                                    result = BetterResult.Left;
                                    return true;
                                }
                                if (TypeEquals(rightType, doubleType, ref useSiteDiagnostics))
                                {
                                    result = BetterResult.Right;
                                    return true;
                                }
                            }
                            // if argument is numeric and one of the two types is also and the other not
                            // then prefer the numeric type
                            if (argType?.SpecialType != null && (argType.SpecialType.IsNumericType() || Equals(argType, Compilation.FloatType())))
                            {
                                if (leftType.SpecialType.IsNumericType() && !rightType.SpecialType.IsNumericType())
                                {
                                    result = BetterResult.Left;
                                    return true;
                                }
                                if (rightType.SpecialType.IsNumericType() && !leftType.SpecialType.IsNumericType())
                                {
                                    result = BetterResult.Right;
                                    return true;
                                }
                                if (!Equals(leftType, rightType))
                                {
                                    if (leftType.IsFloatType())
                                    {
                                        result = BetterResult.Left;
                                        return true;
                                    }
                                    if (rightType.IsFloatType())
                                    {
                                        result = BetterResult.Right;
                                        return true;
                                    }
                                }
                                var leftIntegral = leftType.IsIntegralType();
                                var rightIntegral = rightType.IsIntegralType();
                                if (leftIntegral != rightIntegral)
                                {
                                    if (argType.IsIntegralType())
                                    {
                                        result = leftIntegral ? BetterResult.Left : BetterResult.Right;
                                    }
                                    else
                                    {
                                        result = rightIntegral ? BetterResult.Left : BetterResult.Right;
                                    }
                                    return true;
                                }
                            }

                            // handle case where argument is usual and the method is not usual
                            // prefer method with "native VO" parameter type
                            if (argType.IsUsualType())
                            {
                                // no need to check if parleft or parright are equal or usual that was checked above
                                // is there an VO style conversion possible ?
                                var leftConvert = leftType.IsValidVOUsualType(Compilation);
                                var rightConvert = rightType.IsValidVOUsualType(Compilation);
                                if (leftConvert != rightConvert)
                                {
                                    // One is a valid conversion, the other is not.
                                    if (leftConvert)
                                        result = BetterResult.Left;
                                    else
                                        result = BetterResult.Right;
                                    return true;
                                }
                                // prefer fractional parameters so no information gets lost
                                if (leftType.IsFractionalType() != rightType.IsFractionalType())
                                {
                                    if (leftType.IsFractionalType())
                                        result = BetterResult.Left;
                                    else
                                        result = BetterResult.Right;
                                    return true;

                                }
                            }
                        }
                    }
                    */
                }
                else
                {
                    // parameter counts are different for m1 and m2
                    // choose the one where the parameter count matches the #help of arguments
                    if (m1.Member.GetParameterCount() == arguments.Count)
                    {
                        result = BetterResult.Left;
                        return true;
                    }
                    if (m2.Member.GetParameterCount() == arguments.Count)
                    {
                        result = BetterResult.Right;
                        return true;
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

                            var info = GenerateAmbiguousWarning(r1, r2);

                            useSiteDiagnostics = new HashSet<DiagnosticInfo>();
                            useSiteDiagnostics.Add(info);
                            return true;
                        }
                    }
                }
            }

            // generate warning that function takes precedence over static method
            var func1 = m1.Member.IsStatic && m1.Member.ContainingType.IsFunctionsClass();
            var func2 = m2.Member.IsStatic && m2.Member.ContainingType.IsFunctionsClass();
            if (func1 && !func2)
            {
                result = BetterResult.Left;
                var info = GenerateFuncMethodWarning(m1.Member, m2.Member);

                useSiteDiagnostics = new HashSet<DiagnosticInfo>();
                useSiteDiagnostics.Add(info);
                return true;
            }
            else if (func2 && !func1)
            {
                result = BetterResult.Right;
                var info = GenerateFuncMethodWarning(m2.Member, m1.Member);
                useSiteDiagnostics = new HashSet<DiagnosticInfo>();
                useSiteDiagnostics.Add(info);
                return true;
            }
            return false;
            // Local Functions
            CSDiagnosticInfo GenerateAmbiguousWarning(Symbol r1, Symbol r2)
            {

                var info = new CSDiagnosticInfo(ErrorCode.WRN_XSharpAmbiguous,
                        new object[] {
                        r1.Name,
                        r1.Kind.ToString(),
                        new FormattedSymbol(r1, SymbolDisplayFormat.CSharpErrorMessageFormat),
                        r1.ContainingAssembly.Name,
                        r2.Kind.ToString(),
                        new FormattedSymbol(r2, SymbolDisplayFormat.CSharpErrorMessageFormat),
                        r2.ContainingAssembly.Name,
                        });
                return info;
            }
            CSDiagnosticInfo GenerateFuncMethodWarning(Symbol s1, Symbol s2)
            {
                return new CSDiagnosticInfo(ErrorCode.WRN_FunctionsTakePrecedenceOverMethods,
                new object[] {
                    s1.Name,
                    new FormattedSymbol(s1, SymbolDisplayFormat.CSharpErrorMessageFormat),
                    new FormattedSymbol(s2, SymbolDisplayFormat.CSharpErrorMessageFormat)
                    });
            }

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
            return node.Start?.Type == XSharpLexer.CASTCLASS;
        }
    }
}
