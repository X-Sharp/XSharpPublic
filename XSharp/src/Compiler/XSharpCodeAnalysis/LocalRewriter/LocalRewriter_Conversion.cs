//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using Microsoft.CodeAnalysis.CSharp.Symbols;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class LocalRewriter
    {

        // Cache the Usual2Ptr conversion since it may be used quite often and there are 43 overloads for op_Implicit.
        private MethodSymbol getImplicitOperatorByReturnType(TypeSymbol srcType, TypeSymbol destType)
        {
            var members = srcType.GetMembers(WellKnownMemberNames.ImplicitConversionName);
            foreach (MethodSymbol m in members)
            {
                if (TypeSymbol.Equals(m.ReturnType, destType))
                {
                    return m;
                }

            }
            return null;
        }

        internal static MethodSymbol getImplicitOperatorByParameterType(TypeSymbol srcType, TypeSymbol destType)
        {
            var members = srcType.GetMembers(WellKnownMemberNames.ImplicitConversionName);
            foreach (MethodSymbol m in members)
            {
                if (m.ParameterCount > 0)
                {
                    var pt = m.GetParameterTypes()[0].Type as TypeSymbol;
                    if (TypeSymbol.Equals(pt, destType))
                    {
                        return m;
                    }
                }

            }
            return null;
        }

        private MethodSymbol getExplicitOperator(TypeSymbol srcType, TypeSymbol destType)
        {
            var members = srcType.GetMembers(WellKnownMemberNames.ExplicitConversionName);
            foreach (MethodSymbol m in members)
            {
                if (TypeSymbol.Equals(m.ReturnType, destType))
                {
                    return m;
                }

            }
            return null;
        }
        private ConversionKind UnBoxXSharpType(ref BoundExpression rewrittenOperand, ConversionKind conversionKind, TypeSymbol rewrittenType)
        {
            // If the XSpecial flag is set then this not really unboxing but some special operation

            var special = rewrittenOperand.Syntax.XSpecial;
            if ((rewrittenType.IsPointerType() || rewrittenType.IsPszType()) && rewrittenOperand.Type.IsObjectType() && special)
            {
                rewrittenOperand = _factory.Convert(_compilation.GetSpecialType(SpecialType.System_IntPtr), rewrittenOperand, Conversion.Unboxing);
                conversionKind = ConversionKind.Identity;
                return conversionKind;
            }

            if (_compilation.Options.HasRuntime)
            {
                // test C323 
                if (special && (rewrittenOperand.Type.IsPointerType() || rewrittenOperand.Type.IsPszType()))
                {
                    rewrittenOperand = new BoundConversion(rewrittenOperand.Syntax, rewrittenOperand,
                        Conversion.Identity, false, false,
                        conversionGroupOpt: null,
                        constantValueOpt: null,
                        type: _compilation.GetSpecialType(SpecialType.System_IntPtr));
                    return conversionKind;
                }
                var nts = rewrittenOperand?.Type as NamedTypeSymbol;
                if (nts is { })
                {
                    var usualType = _compilation.UsualType();
                    nts = nts.ConstructedFrom;
                    // Ticket C575: Assign Interface to USUAL
                    // Marked as Boxing in Conversions.cs
                    // Implementation here
                    if (nts.IsInterface && rewrittenType.IsUsualType())
                    {
                        var m = getImplicitOperatorByParameterType(usualType, _compilation.GetSpecialType(SpecialType.System_Object));
                        if (m != null)
                        {
                            rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                            rewrittenOperand.WasCompilerGenerated = true;
                            return ConversionKind.Identity;
                        }
                    }
                    if (nts.IsUsualType())
                    {
                        return XsRewriteUsualType(ref rewrittenOperand, rewrittenType, conversionKind);
                    }
                    if (special)
                    {
                        if (nts.IsObjectType())
                        {
                            if (rewrittenType.IsReferenceType)
                            {
                                rewrittenOperand = MakeConversionNode(rewrittenOperand, rewrittenType, @checked: true, acceptFailingConversion: false);
                                rewrittenOperand.WasCompilerGenerated = true;
                                return ConversionKind.ImplicitReference;
                            }
                            else
                            {
                                return ConversionKind.Unboxing;
                            }
                        }
                        if (nts.IsFractionalType())
                        {
                            if (nts.IsFloatType())
                            {
                                return XsRewriteNumericType(nts, ref rewrittenOperand, rewrittenType, SpecialType.System_Double);
                            }
                            else if (nts.IsCurrencyType())
                            {
                                return XsRewriteNumericType(nts, ref rewrittenOperand, rewrittenType, SpecialType.System_Decimal);
                            }
                            rewrittenOperand = MakeConversionNode(rewrittenOperand, _compilation.FloatType(), false, false);
                            return XsRewriteNumericType(_compilation.FloatType(), ref rewrittenOperand, rewrittenType, SpecialType.System_Double);
                        }
                    }
                }
            }
            return conversionKind;

        }

        ConversionKind XsRewriteUsualType(ref BoundExpression rewrittenOperand, TypeSymbol rewrittenType, ConversionKind conversionKind)
        {
            // USUAL -> WINBOOL, use LOGIC as intermediate type
            var usualType = _compilation.UsualType();
            if (rewrittenType.IsWinBoolType())
            {
                MethodSymbol m = getImplicitOperatorByReturnType(usualType, _compilation.GetSpecialType(SpecialType.System_Boolean));
                rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                rewrittenOperand.WasCompilerGenerated = true;
                return ConversionKind.Identity;
            }
            if (rewrittenType.IsWinDateType())
            {
                MethodSymbol m = getImplicitOperatorByParameterType(_compilation.WinDateType(), _compilation.GetWellKnownType(WellKnownType.XSharp___Usual));
                rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                rewrittenOperand.WasCompilerGenerated = true;
                return ConversionKind.Identity;
            }
            if (rewrittenType.SpecialType == SpecialType.System_Decimal && _compilation.Options.XSharpRuntime)
            {
                MethodSymbol m = getImplicitOperatorByReturnType(usualType, _compilation.GetSpecialType(SpecialType.System_Decimal));
                rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                rewrittenOperand.WasCompilerGenerated = true;
                return ConversionKind.Identity;
            }

            if (rewrittenType.IsPointerType())
            {
                // Pointer types are not really boxed
                // we call the appropriate implicit operator here
                MethodSymbol m = null;
                m = getImplicitOperatorByReturnType(usualType, rewrittenType);
                if (m == null)
                {
                    m = getImplicitOperatorByReturnType(usualType, _compilation.GetSpecialType(SpecialType.System_IntPtr));
                }
                if (m != null)
                {
                    rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                    rewrittenOperand.WasCompilerGenerated = true;
                    return ConversionKind.ExplicitPointerToPointer;
                }
            }
            else if (rewrittenType.SpecialType == SpecialType.System_DateTime)
            {
                rewrittenOperand = _factory.StaticCall(usualType, ReservedNames.ToObject, rewrittenOperand);
                return ConversionKind.Unboxing;
            }
            else // System.Decimals, Objects and reference types, but not String
            {

                // check to see if we are casting to an interface that the usual type supports
                if (rewrittenType.IsInterfaceType())
                {
                    foreach (var interf in usualType.AllInterfacesNoUseSiteDiagnostics)
                    {
                        if (TypeSymbol.Equals(interf, rewrittenType))
                        {
                            return ConversionKind.ImplicitReference;
                        }
                    }
                }
                // special case for __CastClass
                var xnode = rewrittenOperand.Syntax.Parent?.XNode as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParserRuleContext;
                if (xnode != null && xnode.IsCastClass())
                {
                    if (rewrittenType.IsUsualType())
                        conversionKind = ConversionKind.Unboxing;
                    else
                        conversionKind = ConversionKind.Boxing;

                }
                else
                {
                    rewrittenOperand = _factory.StaticCall(usualType, ReservedNames.ToObject, rewrittenOperand);
                    if (rewrittenType.IsObjectType())
                    {
                        conversionKind = ConversionKind.Identity;
                    }
                    else if (rewrittenType.IsReferenceType)
                    {
                        rewrittenOperand = MakeConversionNode(rewrittenOperand, rewrittenType, @checked: true, acceptFailingConversion: false);
                        conversionKind = ConversionKind.ImplicitReference;
                    }
                    else if (!rewrittenType.IsWinDateType())
                    {
                        conversionKind = ConversionKind.Unboxing;
                    }
                }
            }
            return conversionKind;
        }
        ConversionKind XsRewriteNumericType(NamedTypeSymbol ourtype, ref BoundExpression rewrittenOperand, TypeSymbol rewrittenType, SpecialType fallbacktype)
        {
            MethodSymbol m = getExplicitOperator(ourtype, rewrittenType);
            if (m != null)
            {
                rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                rewrittenOperand.WasCompilerGenerated = true;
                return ConversionKind.Identity;
            }
            m = getImplicitOperatorByReturnType(ourtype, rewrittenType);
            if (m != null)
            {
                rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                rewrittenOperand.WasCompilerGenerated = true;
                return ConversionKind.Identity;

            }
            if (rewrittenType.IsObjectType() || rewrittenType.IsUsualType())
            {
                return ConversionKind.Boxing;

            }
            // what else, any other numeric type Convert to Double first and then to destination type
            m = getImplicitOperatorByReturnType(ourtype, _compilation.GetSpecialType(fallbacktype));
            if (m != null)  // this should never happen. This is an implicit converter
            {
                rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                rewrittenOperand.WasCompilerGenerated = true;
                rewrittenOperand = MakeConversionNode(rewrittenOperand.Syntax,
                    rewrittenOperand: rewrittenOperand,
                    rewrittenType: rewrittenType,
                    conversion: Conversion.ImplicitNumeric,
                    @checked: true,
                    explicitCastInCode: false
                    );
                rewrittenOperand.WasCompilerGenerated = true;
                return ConversionKind.Identity;

            }
            return ConversionKind.Boxing;
        }
    }
}
