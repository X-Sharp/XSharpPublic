// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using System.Collections.Generic;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal sealed partial class Conversions
    {

        override public bool HasBoxingConversion(TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            bool result = base.HasBoxingConversion(source, destination, ref useSiteDiagnostics);

            if (!result && _binder.Compilation.Options.IsDialectVO && destination != null && source is NamedTypeSymbol)
            {
                if (((NamedTypeSymbol)source).ConstructedFrom == _binder.Compilation.GetWellKnownType(WellKnownType.Vulcan___Usual))
                {
                    if (destination.IsReferenceType)
                    {
                        result = !destination.IsStringType()
                            && (destination as NamedTypeSymbol)?.ConstructedFrom != _binder.Compilation.GetWellKnownType(WellKnownType.Vulcan___Array)
                            && (destination as NamedTypeSymbol)?.ConstructedFrom != _binder.Compilation.GetWellKnownType(WellKnownType.Vulcan_Codeblock)
                            && (destination as NamedTypeSymbol)?.ConstructedFrom.IsDerivedFrom(_binder.Compilation.GetWellKnownType(WellKnownType.Vulcan_Codeblock), true, ref useSiteDiagnostics) != true
                            && !IsClipperArgsType(destination);
                    }
                    else
                    {
                        result = destination.SpecialType == SpecialType.None
                            && (destination as NamedTypeSymbol)?.ConstructedFrom != _binder.Compilation.GetWellKnownType(WellKnownType.Vulcan___Symbol)
                            && (destination as NamedTypeSymbol)?.ConstructedFrom != _binder.Compilation.GetWellKnownType(WellKnownType.Vulcan___Psz)
                            && (destination as NamedTypeSymbol)?.ConstructedFrom != _binder.Compilation.GetWellKnownType(WellKnownType.Vulcan___VOFloat)
                            && (destination as NamedTypeSymbol)?.ConstructedFrom != _binder.Compilation.GetWellKnownType(WellKnownType.Vulcan___VODate);
                    }
                }
            }

            return result;
        }

       protected override ConversionKind ClassifyVoNullLiteralConversion(BoundExpression source, TypeSymbol destination, out Conversion conv)
        {
            if (_binder.Compilation.Options.IsDialectVO &&
                ((NamedTypeSymbol)destination).ConstructedFrom == _binder.Compilation.GetWellKnownType(WellKnownType.Vulcan___Usual))
            {
                var usualType = _binder.Compilation.GetWellKnownType(WellKnownType.Vulcan___Usual);
                var op = usualType.GetOperators("op_Implicit")
                    .WhereAsArray(o => o.ParameterCount == 1 && o.ParameterTypes[0].IsObjectType() && o.ReturnType == usualType)
                    .AsSingleton() as MethodSymbol;
                if (op != null)
                {
                    var sourceType = _binder.Compilation.GetSpecialType(SpecialType.System_Object);
                    UserDefinedConversionAnalysis uca = UserDefinedConversionAnalysis.Normal(op, Conversion.ImplicitReference, Conversion.Identity, sourceType, destination);
                    UserDefinedConversionResult cr = UserDefinedConversionResult.Valid(new[] { uca }.AsImmutable(), 0);
                    conv = new Conversion(cr, isImplicit: true);
                    return ConversionKind.ImplicitUserDefined;
                }
            }

            conv = Conversion.NoConversion;
            return ConversionKind.NoConversion;
        }

        public override LambdaConversionResult IsAnonymousFunctionCompatibleWithType(UnboundLambda anonymousFunction, TypeSymbol type)
        {
            var res = base.IsAnonymousFunctionCompatibleWithType(anonymousFunction, type);

            if (res == LambdaConversionResult.BadTargetType && _binder.Compilation.Options.IsDialectVO)
            {
                if (type.IsCodeblock() || type.IsUsual() || type.IsObjectType())
                {
                    return LambdaConversionResult.Success;
                }

                HashSet<DiagnosticInfo> useSiteDiagnostics = null;
                var conv = ClassifyConversion(Compilation.GetWellKnownType(WellKnownType.Vulcan_Codeblock), type, ref useSiteDiagnostics);
                if (conv.Exists)
                {
                    return LambdaConversionResult.Success;
                }
            }

            return res;
        }
    }
    internal abstract partial class ConversionsBase
    {
        protected bool IsClipperArgsType(TypeSymbol args)
        {
            bool result = false;
            if (args is ArrayTypeSymbol)
            {
                var ats = args as ArrayTypeSymbol;
                Conversions conv = this as Conversions;
                if (conv != null  && conv.Compilation.Options.IsDialectVO)
                {
                    result = (ats.ElementType == conv.Compilation.GetWellKnownType(WellKnownType.Vulcan___Usual));
                }

            }
            return result;
        }
        private Conversion ClassifyVOImplicitBuiltInConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            if (this is Conversions)
            {
                Conversions conv = this as Conversions;
                if (source != null && destination != null )
                {
                    if (conv.Compilation.Options.VOSignedUnsignedConversion)
                    {
                        var srctype = source.SpecialType;
                        var dsttype = destination.SpecialType;
                        // when both numeric and both integral or both not integral
                        if (srctype.IsNumericType() && dsttype.IsNumericType()  && 
                            srctype.IsIntegralType() == dsttype.IsIntegralType())
                        {
                            // when both same # of bits and integral, use Identity conversion
                            if (srctype.SizeInBytes() == dsttype.SizeInBytes() &&
                                srctype.IsIntegralType() && dsttype.IsIntegralType())
                                return Conversion.Identity;
                            else
                                return Conversion.ImplicitNumeric;
                        }
                    }
                    if (conv.Compilation.Options.IsDialectVO)
                    {
                        // Allow to case BOOL <-> INTEGRAL
                        if (source.SpecialType.IsIntegralType() && destination.SpecialType == SpecialType.System_Boolean)
                            return Conversion.Identity;
                        if (destination.SpecialType.IsIntegralType() && source.SpecialType == SpecialType.System_Boolean)
                            return Conversion.Identity;
                    }
                    if (conv.Compilation.Options.LateBinding ||
                        conv.Compilation.Options.VOImplicitCastsAndConversions)
                    {
                        if (source.SpecialType == SpecialType.System_Object && destination.IsReferenceType && !IsClipperArgsType(destination))
                        {
                            // Convert Object -> Reference allowed with /lb and with /vo7
                            // except when converting to array of usuals
                            return Conversion.ImplicitReference;
                        }
                    }
                    // When /vo7 is enabled
                    if (conv.Compilation.Options.VOImplicitCastsAndConversions)
                    {
                        if (source.IsPointerType() && destination.IsPointerType())
                        {
                            // Convert Any Ptr -> Any Ptr allowed with /vo7
                            return Conversion.Identity;
                        }
                        if (source.IsIntegralType() && destination.IsPointerType())
                        {
                            // Convert Integral type -> Ptr Type allowed with /vo7
                            return Conversion.Identity;
                        }
                    }
                }

            }
            if (source != null)
            {
                if ((source.SpecialType == SpecialType.System_IntPtr || source.SpecialType == SpecialType.System_UIntPtr) && 
                    (destination.IsPointerType() || destination.IsVoStructOrUnion()))
                {
                    if (destination.IsPointerType())
                        return Conversion.IntPtr;
                    else
                        return Conversion.Identity;
                }
                if ((destination.SpecialType == SpecialType.System_IntPtr || destination.SpecialType == SpecialType.System_UIntPtr) 
                    && (source.IsPointerType() || source.IsVoStructOrUnion()))
                {
                    if (source.IsPointerType())
                        return Conversion.IntPtr;
                    else
                        return Conversion.Identity;
                }
            }
            if (sourceExpression.Kind == BoundKind.Literal && sourceExpression.IsLiteralNull())
            {
                Conversion conv;
                if (ClassifyVoNullLiteralConversion(sourceExpression, destination, out conv) != ConversionKind.NoConversion)
                {
                    return conv;
                }
            }
            return Conversion.NoConversion;
        }
        internal static bool HasImplicitVOConstantExpressionConversion(BoundExpression source, TypeSymbol destination)
        {
            var specialSource = source.Type.GetSpecialTypeSafe();
            if (specialSource == SpecialType.System_Double && destination.GetSpecialTypeSafe() == SpecialType.System_Single)
            {
                // TODO (nvk): Check numeric range before accepting conversion!
                return true;
            }
            else if (specialSource == SpecialType.System_UInt32 && destination.GetSpecialTypeSafe() == SpecialType.System_IntPtr)
            {
                return true;
            }
            return false;
        }
    }
}
