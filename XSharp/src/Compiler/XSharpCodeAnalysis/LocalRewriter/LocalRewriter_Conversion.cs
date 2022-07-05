//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using Microsoft.CodeAnalysis.CSharp.Symbols;
using System.Diagnostics;

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

        private ConversionKind UnBoxSpecialType(ref BoundExpression rewrittenOperand, Conversion conversion, TypeSymbol rewrittenType, bool explicitCastInCode)
        {
            Debug.Assert(conversion.IsSpecial);
            ConversionKind conversionKind = conversion.Kind;
            if ((rewrittenType.IsPointerType() || rewrittenType.IsPszType()) && rewrittenOperand.Type.IsObjectType())
            {
                rewrittenOperand = _factory.Convert(_compilation.GetSpecialType(SpecialType.System_IntPtr), rewrittenOperand, Conversion.Unboxing);
                conversionKind = ConversionKind.Identity;
                return conversionKind;
            }

            if (_compilation.Options.HasRuntime)
            {
                // test C323 
                if ((rewrittenOperand.Type.IsPointerType() || rewrittenOperand.Type.IsPszType()))
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
                    if (nts.IsXNumericType() && rewrittenType.IsXNumericType())
                    {
                        if (nts.IsFloatType() || nts.IsCurrencyType())
                        {
                            // Get the right Explicit Operator
                            return XsRewriteOurNumericType(nts, ref rewrittenOperand, rewrittenType, explicitCastInCode);
                        }
                        else
                        {
                            // Get a conversion or Convert.To<type> depending on the setting of /vo11
                            return XsRewriteSystemNumericType(ref rewrittenOperand, rewrittenType, explicitCastInCode, false);
                        }
                    }
                }
            }
            return conversionKind;
        }

        internal static ErrorCode DetermineConversionError(TypeSymbol source, TypeSymbol destination)
        {
            var sourceType = source.XsSpecialtype();
            var destType = destination.XsSpecialtype();
            if (sourceType == destType)
                return ErrorCode.Void;
            switch (sourceType)
            {
                case SpecialType.System_Double:
                    switch (destType)
                    {
                        case SpecialType.System_Decimal:
                            return ErrorCode.Void;

                        default:
                            return ErrorCode.WRN_ConversionMayLeadToLossOfData;
                    }
                case SpecialType.System_Single:
                    switch (destType)
                    {
                        case SpecialType.System_Double:
                        case SpecialType.System_Decimal:
                            return ErrorCode.Void;

                        default:
                            return ErrorCode.WRN_ConversionMayLeadToLossOfData;
                    }
                case SpecialType.System_Decimal:
                    switch (destType)
                    {
                        case SpecialType.System_Double:
                        case SpecialType.System_Single:
                            return ErrorCode.Void;

                        default:
                            return ErrorCode.WRN_ConversionMayLeadToLossOfData;
                    }
                case SpecialType.System_Int64:
                case SpecialType.System_UInt64:
                    switch (destType)
                    {
                        case SpecialType.System_Single:
                        case SpecialType.System_Double:
                        case SpecialType.System_Decimal:
                            return ErrorCode.Void;
                        case SpecialType.System_Int64:
                        case SpecialType.System_UInt64:
                            return ErrorCode.WRN_Conversion;
                        default:
                            return ErrorCode.WRN_ConversionMayLeadToLossOfData;

                    }
                case SpecialType.System_Int32:
                case SpecialType.System_UInt32:
                    switch (destType)
                    {
                        case SpecialType.System_Int64:
                        case SpecialType.System_UInt64:
                        case SpecialType.System_Decimal:
                        case SpecialType.System_Single:
                        case SpecialType.System_Double:
                            return ErrorCode.Void;
                        case SpecialType.System_UInt32:
                        case SpecialType.System_Int32:
                            return ErrorCode.WRN_Conversion;
                        default:
                            return ErrorCode.WRN_ConversionMayLeadToLossOfData;

                    }
                case SpecialType.System_Int16:
                case SpecialType.System_UInt16:
                    switch (destType)
                    {
                        case SpecialType.System_UInt32:
                        case SpecialType.System_Int32:
                        case SpecialType.System_UInt64:
                        case SpecialType.System_Int64:
                        case SpecialType.System_Decimal:
                        case SpecialType.System_Single:
                        case SpecialType.System_Double:
                            return ErrorCode.Void;
                        case SpecialType.System_UInt16:
                        case SpecialType.System_Int16:
                            return ErrorCode.WRN_Conversion;
                        default:
                            return ErrorCode.WRN_ConversionMayLeadToLossOfData;

                    }
                case SpecialType.System_Byte:
                case SpecialType.System_SByte:
                    switch (destType)
                    {
                        case SpecialType.System_Int16:
                        case SpecialType.System_UInt16:
                        case SpecialType.System_Int32:
                        case SpecialType.System_UInt32:
                        case SpecialType.System_Int64:
                        case SpecialType.System_UInt64:
                        case SpecialType.System_Decimal:
                        case SpecialType.System_Single:
                        case SpecialType.System_Double:
                            return ErrorCode.Void;
                        case SpecialType.System_Byte:
                        case SpecialType.System_SByte:
                            return ErrorCode.WRN_Conversion;
                        default:
                            return ErrorCode.WRN_ConversionMayLeadToLossOfData;

                    }
            }
            return ErrorCode.Void;
        }

        ConversionKind XsRewriteSystemNumericType(ref BoundExpression rewrittenOperand, TypeSymbol rewrittenType, bool explicitcastincode, bool noError)
        {
            if (!noError) //  && !explicitcastincode)
            {
                var error = DetermineConversionError(rewrittenOperand.Type, rewrittenType);
                if (error != ErrorCode.Void)
                {
                    _factory.Diagnostics.Add(error, rewrittenOperand.Syntax.Location, rewrittenOperand.Type, rewrittenType);
                }
            }
            var vo11 = _compilation.Options.HasOption(CompilerOption.Vo11, rewrittenOperand.Syntax);
            // should we work differently with explicit cast 
            if (vo11) //  && ! explicitcastincode)
            {
                var type = rewrittenOperand.Type;
                var convert = _compilation.GetWellKnownType(WellKnownType.System_Convert);
                MethodSymbol m = null;
                foreach (var symbol in convert.GetMembers())
                {
                    if (symbol is MethodSymbol method)
                    {
                        if (Equals(method.ReturnType, rewrittenType))
                        {
                            if (method.ParameterCount == 1)
                            {
                                if (Equals(method.Parameters[0].Type, type))
                                {
                                    m = method;
                                    break;
                                }
                            }
                        }
                    }
                }
                if (m != null)
                {
                    rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                    rewrittenOperand.WasCompilerGenerated = true;
                    return ConversionKind.Identity;
                }
            }
            rewrittenOperand = MakeConversionNode(rewrittenOperand, rewrittenType, @checked: true, acceptFailingConversion: false);
            rewrittenOperand.WasCompilerGenerated = true;
            return ConversionKind.Identity;
        }
        private ConversionKind UnBoxUsualType(ref BoundExpression rewrittenOperand, Conversion conversion, TypeSymbol rewrittenType, bool explicitCastInCode)
        {
            Debug.Assert(rewrittenOperand.Type.IsUsualType());
            // USUAL -> WINBOOL, use LOGIC as intermediate type
            ConversionKind conversionKind = conversion.Kind;
            var usualType = rewrittenOperand.Type;
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
        ConversionKind XsRewriteOurNumericType(NamedTypeSymbol ourtype, ref BoundExpression rewrittenOperand, TypeSymbol rewrittenType, bool explicitCastInCode)
        {
            Debug.Assert(ourtype.IsFloatType() || ourtype.IsCurrencyType());
            // should we work differently with explicit cast 
            //if (!explicitCastInCode)
            {
                var error = DetermineConversionError(rewrittenOperand.Type, rewrittenType);
                if (error != ErrorCode.Void)
                {
                    _factory.Diagnostics.Add(error, rewrittenOperand.Syntax.Location, rewrittenOperand.Type, rewrittenType);
                }
            }
            if (ourtype.IsFloatType())
            {
                rewrittenOperand = MakeConversionNode(rewrittenOperand, _compilation.GetSpecialType(SpecialType.System_Double), false, false);
            }
            else
            {
                rewrittenOperand = MakeConversionNode(rewrittenOperand, _compilation.GetSpecialType(SpecialType.System_Decimal), false, false); ;

            }
            if ( Equals(rewrittenOperand.Type, rewrittenType))
            {
                return ConversionKind.Identity;
            }
            return XsRewriteSystemNumericType(ref rewrittenOperand, rewrittenType, explicitCastInCode, true);
            
            //MethodSymbol m = getExplicitOperator(ourtype, rewrittenType);
            //if (m != null)
            //{
            //    rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
            //    rewrittenOperand.WasCompilerGenerated = true;
            //    return ConversionKind.Identity;
            //}
            //m = getImplicitOperatorByReturnType(ourtype, rewrittenType);
            //if (m != null)
            //{
            //    rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
            //    rewrittenOperand.WasCompilerGenerated = true;
            //    return ConversionKind.Identity;

            //}
            //if (rewrittenType.IsObjectType() || rewrittenType.IsUsualType())
            //{
            //    return ConversionKind.Boxing;

            //}
            //rewrittenOperand = MakeConversionNode(rewrittenOperand, rewrittenType, @checked: true, acceptFailingConversion: false);
            //rewrittenOperand.WasCompilerGenerated = true;
            //return ConversionKind.Identity;
        }
        //private MethodSymbol getExplicitOperator(TypeSymbol srcType, TypeSymbol destType)
        //{
        //    var members = srcType.GetMembers(WellKnownMemberNames.ExplicitConversionName);
        //    foreach (MethodSymbol m in members)
        //    {
        //        if (TypeSymbol.Equals(m.ReturnType, destType))
        //        {
        //            return m;
        //        }

        //    }
        //    return null;
        //}

    }
}
