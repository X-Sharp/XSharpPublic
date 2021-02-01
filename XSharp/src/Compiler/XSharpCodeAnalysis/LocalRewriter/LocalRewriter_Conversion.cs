//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Roslyn.Utilities;

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

        private MethodSymbol getImplicitOperatorByParameterType(TypeSymbol srcType, TypeSymbol destType)
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

            if ((rewrittenType.IsPointerType()  || rewrittenType.IsPszType() )
                && rewrittenOperand.Type.IsObjectType() && _compilation.Options.Dialect.AllowPointerMagic())
            {
                rewrittenOperand = _factory.Convert(_compilation.GetSpecialType(SpecialType.System_IntPtr), rewrittenOperand, Conversion.Unboxing);
                conversionKind = ConversionKind.Identity;
                return conversionKind;
            }
            // else it's boxing
            var nts = rewrittenOperand?.Type as NamedTypeSymbol;

            if (_compilation.Options.HasRuntime)
            {
                var usualType = _compilation.UsualType();
                if (nts is { })
                {
                    nts = nts.ConstructedFrom;
                }
                // Ticket C575: Assign Interface to USUAL
                // Marked as Boxing in Conversions.cs
                // Implementation here
                if (nts is { } && nts.IsInterface && rewrittenType.IsUsualType() )
                {

                    var m = getImplicitOperatorByParameterType(usualType, _compilation.GetSpecialType(SpecialType.System_Object));
                    if (m != null)
                    {
                        rewrittenOperand = _factory.StaticCall(rewrittenType, (MethodSymbol)m, rewrittenOperand);
                        rewrittenOperand.WasCompilerGenerated = true;
                        return ConversionKind.Identity;
                    }
                }

                if (nts.IsUsualType())
                {
                    // USUAL -> WINBOOL, use LOGIC as intermediate type
                    if (rewrittenType.IsWinBoolType())
                    {
                        MethodSymbol m = getImplicitOperatorByReturnType(usualType, _compilation.GetSpecialType(SpecialType.System_Boolean));
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
                        rewrittenOperand = _factory.StaticCall(usualType, ReservedNames.ToObject , rewrittenOperand);
                        return ConversionKind.Unboxing;
                    }
                    else // System.Decimals, Objects and reference types, but not String
                    {

                        // check to see if we are casting to an interface that the usualtype supports
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
                            else
                            {
                                conversionKind = ConversionKind.Unboxing;
                            }
                        }
                    }
                }
                if (nts.IsFloatType() && rewrittenType is NamedTypeSymbol)
                {
                    var floatType = _compilation.FloatType();
                    MethodSymbol m = getExplicitOperator(floatType, rewrittenType as NamedTypeSymbol);
                    if (m != null)
                    {
                        rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                        rewrittenOperand.WasCompilerGenerated = true;
                        return ConversionKind.Identity;
                    }
                    m = getImplicitOperatorByReturnType(floatType, rewrittenType as NamedTypeSymbol);
                    if (m != null)
                    {
                        rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                        rewrittenOperand.WasCompilerGenerated = true;
                        return ConversionKind.Identity;

                    }
                    if (rewrittenType.IsObjectType() ||rewrittenType.IsUsualType())
                    {
                        return ConversionKind.Boxing;

                    }
                    // what else, any other numeric type Convert to Double first and then to destination type
                    m = getImplicitOperatorByReturnType(floatType, _compilation.GetSpecialType(SpecialType.System_Double));
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

                }
                if ((rewrittenOperand.Type.IsPointerType() || rewrittenOperand.Type.IsPszType())
                    && _compilation.Options.Dialect.AllowPointerMagic())
                {
                    rewrittenOperand = new BoundConversion(rewrittenOperand.Syntax, rewrittenOperand,
                        Conversion.Identity, false, false,
                        conversionGroupOpt: default,
                        constantValueOpt: default, 
                        type: _compilation.GetSpecialType(SpecialType.System_IntPtr));
                }
            }
            return conversionKind;
        }
    }
}
