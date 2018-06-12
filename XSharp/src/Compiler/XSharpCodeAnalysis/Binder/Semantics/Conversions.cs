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

using Microsoft.CodeAnalysis.CSharp.Symbols;
using System.Collections.Generic;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal static class VOTypeConversions
    {
        internal static bool CanVOCast(this TypeSymbol source)
        {
            if (source.IsIntegralType())
                return true;
            if (source.IsVoidPointer())
                return true;
            if (source.IsPointerType())
                return true;
            if (source.SpecialType == SpecialType.System_IntPtr)
                return true;
            if (source.SpecialType == SpecialType.System_UIntPtr)
                return true;
            if (source.IsValueType)     // PSZ for example
                return true;
            if (source.IsReferenceType)
                return false;
            return false;
        }
    }
    internal sealed partial class Conversions
    {

        override public bool HasBoxingConversion(TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            bool result = base.HasBoxingConversion(source, destination, ref useSiteDiagnostics);

            if (!result && _binder.Compilation.Options.IsDialectVO && destination != null && source is NamedTypeSymbol)
            {
                var nts = source as NamedTypeSymbol;
                if (nts.ConstructedFrom == _binder.Compilation.UsualType())
                {
                    var constructedFrom = (destination as NamedTypeSymbol)?.ConstructedFrom;
                    if (destination.IsReferenceType)
                    {
                        // do not box string, array, codeblock  and clipperargs
                        result = destination.SpecialType != SpecialType.System_String 
                            && constructedFrom != null
                            && constructedFrom != _binder.Compilation.ArrayType()
                            && constructedFrom != _binder.Compilation.CodeBlockType()
                            && constructedFrom.IsDerivedFrom(_binder.Compilation.CodeBlockType(), TypeCompareKind.IgnoreDynamicAndTupleNames, ref useSiteDiagnostics) != true
                            && !IsClipperArgsType(destination);
                    }
                    else if (destination.IsPointerType())
                    {
                       return true;
                    }
                    else if (destination.SpecialType == SpecialType.System_DateTime)
                    {
                        return true;
                    }
                    else
                    {
                        // do not box symbol, psz, vofloat, vodate
                        result = destination.SpecialType == SpecialType.None
                            && constructedFrom != null
                            && constructedFrom != _binder.Compilation.SymbolType()
                            && constructedFrom != _binder.Compilation.PszType()
                            && constructedFrom != _binder.Compilation.FloatType()
                            && constructedFrom != _binder.Compilation.DateType();
                    }
                }
                else if (nts.ConstructedFrom == _binder.Compilation.FloatType())
                {
                    if (destination != null && destination.SpecialType.IsNumericType())
                    {
                        result = true;
                    }

                }
            }
            // Ticket C575: Assign Interface to USUAL
            // Implementation in LocalRewriter_Conversion.cs
            if (destination == Compilation.UsualType())
            {
                if (source.IsInterfaceType())
                    return true;
            }
            return result;
        }

        ConversionKind ClassifyVoNullLiteralConversion(BoundExpression source, TypeSymbol destination, out Conversion conv)
        {
            if (_binder.Compilation.Options.IsDialectVO && destination is NamedTypeSymbol)
            {
                var usualType = _binder.Compilation.UsualType();
                var nts = destination as NamedTypeSymbol;
                if ( nts.ConstructedFrom == usualType)
                {
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
            }
            conv = Conversion.NoConversion;
            return ConversionKind.NoConversion;
        }

        public override LambdaConversionResult IsAnonymousFunctionCompatibleWithType(UnboundLambda anonymousFunction, TypeSymbol type)
        {
            var res = base.IsAnonymousFunctionCompatibleWithType(anonymousFunction, type);

            if (res == LambdaConversionResult.BadTargetType && _binder.Compilation.Options.IsDialectVO)
            {
                if (type == Compilation.CodeBlockType() || type == Compilation.UsualType() || type.IsObjectType())
                {
                    return LambdaConversionResult.Success;
                }

                var conv = Compilation.ClassifyConversion(Compilation.CodeBlockType(), type);
                if (conv.Exists)
                {
                    return LambdaConversionResult.Success;
                }
            }

            return res;
        }

        protected override Conversion ClassifyCoreImplicitConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            // Parameters checks have been done in the calling code
            // The following conversion Rules are for all dialects
            var srcType = source.SpecialType;
            var dstType = destination.SpecialType;
            // From and to CHAR
            if (srcType == SpecialType.System_Char)
            {
                if (dstType == SpecialType.System_UInt16)
                    return Conversion.Identity;
                if (Compilation.Options.VOSignedUnsignedConversion) // vo4
                {
                    if (dstType == SpecialType.System_Byte)
                        return Conversion.ImplicitNumeric;
                }

            }

            if (dstType == SpecialType.System_Char)
            {
                if (srcType == SpecialType.System_UInt16)
                {
                    return Conversion.Identity;
                }
                if (srcType == SpecialType.System_Byte)
                {
                    return Conversion.ImplicitNumeric;
                }
            }
            // From IntPtr -> Anything
            if (srcType == SpecialType.System_IntPtr || srcType == SpecialType.System_UIntPtr)
            {
                if (destination.IsPointerType())
                {
                    return Conversion.IntPtr;
                }
                if (destination.IsVoStructOrUnion() || destination.IsIntegralType())
                {
                    return Conversion.Identity;
                }
            }
            // From Anything -> IntPtr
            if (dstType == SpecialType.System_IntPtr || dstType == SpecialType.System_UIntPtr)
            {
                if (source.IsVoStructOrUnion() || source.IsIntegralType())
                {
                    return Conversion.Identity;
                }
                else if (source.IsPointerType())
                {
                    return Conversion.Identity;
                }
            }
            if (Compilation.Options.VOSignedUnsignedConversion) // vo4
            {
                var result = Conversion.NoConversion;

                // when both numeric and both integral or both not integral
                if (srcType.IsNumericType() && dstType.IsNumericType())
                {
                    if (srcType.IsIntegralType() == dstType.IsIntegralType())
                    {
                        // when both same # of bits and integral, use Identity conversion
                        if (srcType.SizeInBytes() == dstType.SizeInBytes() &&
                            srcType.IsIntegralType() && dstType.IsIntegralType())
                            result = Conversion.Identity;
                        else
                            result = Conversion.ImplicitNumeric;
                    }
                    // Vulcan also allows to convert floating point types <-> integral types
                    else
                    {
                        result = Conversion.ImplicitNumeric;
                    }
                }

                if (result != Conversion.NoConversion)
                    return result;
            }

            return Conversion.NoConversion;

        }
        protected override Conversion ClassifyVOImplicitBuiltInConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            // Parameters checks have been done in the calling code
            var srcType = source.SpecialType;
            var dstType = destination.SpecialType;
            bool voCast = false;
            bool voConvert = false;
            if (sourceExpression.Syntax != null)
            {
                var xNode = sourceExpression.Syntax.XNode;
                while (!(xNode is XP.VoCastExpressionContext) &&
                    ! (xNode is XP.VoConversionExpressionContext) )
                {
                    if (xNode == null)
                        break;
                    xNode = xNode.Parent as IXParseTree;
                }
                if (xNode != null)
                {
                    voCast = xNode is XP.VoCastExpressionContext;
                    voConvert = xNode is XP.VoConversionExpressionContext;
                }
            }
            // TYPE(_CAST, expr) allows almost everything
            // source must be PTR, Integral, IntPtr, UIntPtr
            // this is handled in CanVOCast
            if (voCast && source.CanVOCast() && destination.CanVOCast())
            {
                // No _CAST on USUAL
                if (source == Compilation.UsualType())
                {
                    return Conversion.NoConversion;
                }
                // Allow cast -> BOOLEAN
                // from all allowed types
                if (dstType == SpecialType.System_Boolean)
                {
                    return Conversion.Identity;
                }
                // Allow cast -> INTEGRAL
                // except from NullableTypes and Reference Types
                if (dstType.IsIntegralType() && ! source.IsNullableType() &&! source.IsReferenceType)
                {
                    if (srcType.IsNumericType())
                    {
                        // always implicit numeric conversion
                        return Conversion.ImplicitNumeric;
                    }
                    if (source.SpecialType == SpecialType.System_Boolean)
                    {
                        return Conversion.Identity;
                    }

                    // Allow PTR -> Integral when size matches
                    if (source.IsVoidPointer())
                    {
                        if (dstType.SizeInBytes() == 4 && Compilation.Options.Platform == Platform.X86)
                        {
                            return Conversion.Identity;
                        }
                        if (dstType.SizeInBytes() == 8 && Compilation.Options.Platform == Platform.X64)
                        {
                            return Conversion.Identity;
                        }
                    }
                }
                // Allow cast -> PTR when 
                // source is integral and source size matches the Integral size
                // source is Ptr, IntPtr, UintPtr
                // source is PSZ
                if (destination is PointerTypeSymbol)
                {
                    if (source.IsIntegralType())
                    {
                        if (Compilation.Options.Platform == Platform.X86 && srcType.SizeInBytes() == 4)
                        {
                            return Conversion.Identity;
                        }
                        if (Compilation.Options.Platform == Platform.X64 && srcType.SizeInBytes() == 8)
                        {
                            return Conversion.Identity;
                        }
                    }
                    if (source.IsPointerType() || source.IsVoidPointer() 
                        || source.SpecialType == SpecialType.System_IntPtr || source.SpecialType == SpecialType.System_UIntPtr)
                    {
                        return Conversion.Identity;
                    }
                    if (source == Compilation.PszType())
                    {
                        return Conversion.Identity;
                    }
                }
                // Allow cast -> PSZ
                if (destination == Compilation.PszType())
                {
                    return Conversion.Identity;
                }
            }
            if (voConvert)
            {
                // we need to convert BYTE(<p>) to dereferencing the <p>
                // This is done else where in Binder.BindVulcanPointerDereference()
                // Integer conversions
                if (srcType.IsNumericType() && dstType.IsNumericType() &&
                    srcType.IsIntegralType() == dstType.IsIntegralType())
                {
                    // always implicit numeric conversion
                    return Conversion.ImplicitNumeric;
                }
            }
            if (source == Compilation.UsualType())
            {
                // Usual -> Decimal. Get the object out of the Usual and let the rest be done by Roslyn
                if (destination == Compilation.UsualType())
                    return Conversion.NoConversion;
                if (dstType == SpecialType.System_Decimal)
                    return Conversion.Boxing;
                // Usual -> OBJECT. Get the object out of the Usual 
                // Our special call will call in LocalWriter.UnBoxVOType will
                // convert the Unbox operation to a call to __Usual.ToObject()
                // This method will return the Contents of the usual as an object 
                // and not the usual itself as an object
                else if (dstType == SpecialType.System_Object)
                {
                    // All Objects are boxed in a usual
                    return Conversion.Boxing;
                }
                else if (destination.IsReferenceType && !IsClipperArgsType(destination) && ! destination.IsStringType())
                {
                    // all user reference types are boxed. But not the Usual[] args and not string
                    return Conversion.Boxing;
                }
                else if (destination.IsPointerType())
                {
                    // Not really boxed, but we handle this in LocalRewriter.UnBoxXSharpType
                    return Conversion.Boxing;
                }
            }

            if (Compilation.Options.LateBinding ||                 // lb
                Compilation.Options.VOImplicitCastsAndConversions) // vo7
            {
                if (srcType == SpecialType.System_Object)
                {
                    if (destination.IsReferenceType && !IsClipperArgsType(destination))
                    {
                        // Convert Object -> Reference allowed with /lb and with /vo7
                        // except when converting to array of usuals
                        return Conversion.ImplicitReference;
                    }
                    if (destination.IsPointerType() || destination.SpecialType == SpecialType.System_IntPtr)
                        return Conversion.Identity;
                }
                if (dstType == SpecialType.System_Object)
                {
                    if (source.IsReferenceType)
                        return Conversion.ImplicitReference;
                    if (source.IsPointerType() || source.SpecialType == SpecialType.System_IntPtr)
                        return Conversion.Identity;
                }
            }
            if (Compilation.Options.VOImplicitCastsAndConversions)
            {
                // Convert Any Ptr -> Any Ptr 
                if (source.IsPointerType() && destination.IsPointerType())
                {
                    return Conversion.Identity;
                }
            }
            // Convert Integral type -> Ptr Type 
            if (source.IsIntegralType() && destination.IsPointerType())
            {
                if (Compilation.Options.Platform == Platform.X86 && srcType.SizeInBytes() <= 4)
                {
                    return Conversion.Identity;
                }
                if (Compilation.Options.Platform == Platform.X64 && srcType.SizeInBytes() <= 8)
                {
                    return Conversion.Identity;
                }
            }
            // When unsafe we always allow to cast void * to typed *
            // Is this OK ?
            // See ticket C425
            if (source.IsVoidPointer() && destination.IsPointerType() && Compilation.Options.AllowUnsafe)
            {
                return Conversion.Identity;
            }
            if (srcType.IsIntegralType() && dstType.IsIntegralType())
            {
                if (srcType.SizeInBytes() < dstType.SizeInBytes())
                {
                    return Conversion.ImplicitNumeric;
                }
            }
            if (destination == Compilation.PszType() || 
                destination.IsVoidPointer())
            {
                if (source.SpecialType == SpecialType.System_String)
                {
                    return Conversion.ImplicitReference;
                }
            }
            // when nothing else, then use the Core rules
            return ClassifyCoreImplicitConversionFromExpression(sourceExpression, source, destination, ref useSiteDiagnostics);
        }

        protected override Conversion ClassifyNullConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            if (sourceExpression.Kind == BoundKind.Literal && sourceExpression.IsLiteralNull())
            {
                Conversion result;
                if (ClassifyVoNullLiteralConversion(sourceExpression, destination, out result) != ConversionKind.NoConversion)
                {
                    return result;
                }
            }
            return Conversion.NoConversion;
        }
        protected override Conversion ClassifyXSImplicitBuiltInConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            if (source == null || destination == null)
            {
                return ClassifyNullConversionFromExpression(sourceExpression, source, destination, ref useSiteDiagnostics);
            }
            if (Compilation.Options.IsDialectVO)
                return ClassifyVOImplicitBuiltInConversionFromExpression(sourceExpression, source, destination, ref useSiteDiagnostics);
            else
                return ClassifyCoreImplicitConversionFromExpression(sourceExpression, source, destination, ref useSiteDiagnostics);
        }

        protected override bool IsClipperArgsType(TypeSymbol args)
        {
            bool result = false;
            if (args is ArrayTypeSymbol)
            {
                var ats = args as ArrayTypeSymbol;
                if (Compilation.Options.IsDialectVO)
                {
                    result = (ats.ElementType == Compilation.UsualType());
                }

            }
            return result;
        }

    }

    internal abstract partial class ConversionsBase
    {
        protected virtual bool IsClipperArgsType(TypeSymbol args)
        {
            return false;
        }

        protected virtual Conversion ClassifyCoreImplicitConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            return Conversion.NoConversion;
        }
        protected virtual Conversion ClassifyVOImplicitBuiltInConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            return Conversion.NoConversion;
        }
        protected virtual Conversion ClassifyNullConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            return Conversion.NoConversion;
        }

        protected virtual Conversion ClassifyXSImplicitBuiltInConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
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
