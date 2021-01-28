//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

using Microsoft.CodeAnalysis.CSharp.Symbols;
using System.Collections.Generic;
using System.Linq;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal static class VOTypeConversions
    {

        internal static bool IsByRef(this RefKind kind)
        {
            switch (kind)
            {
                case RefKind.Ref:
                case RefKind.Out:
                case RefKind.In:
                    return true;
            }
            return false;
        }
        internal static bool IsIFormatProvider(this TypeSymbol source)
        {
            return source.IsInterfaceType() && source.Name == "IFormatProvider";
        }
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

            if (!result && _binder.Compilation.Options.HasRuntime && ! (destination  is null) && source is NamedTypeSymbol)
            {
                var nts = source as NamedTypeSymbol;
                if (nts.ConstructedFrom.IsUsualType())
                {
                    var destFrom = (destination as NamedTypeSymbol)?.ConstructedFrom;
                    if (destination.IsReferenceType)
                    {
                        // do not box string, array, codeblock  and clipperargs
                        result = !destination.IsStringType()
                            && ! (destFrom is null )
                            && !Equals(destFrom, _binder.Compilation.ArrayType())
                            && !Equals(destFrom, _binder.Compilation.CodeBlockType())
                            && !destination.IsIFormatProvider()
                            && destFrom.IsDerivedFrom(_binder.Compilation.CodeBlockType(), TypeCompareKind.IgnoreDynamicAndTupleNames, ref useSiteDiagnostics) != true
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
                            && !( destFrom is null )
                            && !Equals(destFrom, _binder.Compilation.SymbolType())
                            && !Equals(destFrom, _binder.Compilation.PszType())
                            && !Equals(destFrom, _binder.Compilation.FloatType())
                            && !Equals(destFrom, _binder.Compilation.DateType());
                    }
                }
                else if (Equals(nts.ConstructedFrom, _binder.Compilation.FloatType()))
                {
                    if (!(destination is null) && destination.SpecialType.IsNumericType())
                    {
                        result = true;
                    }
                }
            }
            // Ticket C575: Assign Interface to USUAL
            // Implementation in LocalRewriter_Conversion.cs
            if (destination.IsUsualType())
            {
                if (source.IsInterfaceType())
                {
                    result = true;
                }
            }
            return result;
        }

        ConversionKind ClassifyVoNullLiteralConversion(BoundExpression source, TypeSymbol destination, out Conversion conv)
        {
            if (_binder.Compilation.Options.HasRuntime && destination is NamedTypeSymbol)
            {
                var usualType = _binder.Compilation.UsualType();
                var nts = destination as NamedTypeSymbol;
                if ( nts.ConstructedFrom.IsUsualType())
                {
                    var op = usualType.GetOperators(WellKnownMemberNames.ImplicitConversionName)
                        .WhereAsArray(o => o.ParameterCount == 1 && o.Parameters[0].Type.IsObjectType()
                        && Equals(o.ReturnType, usualType))
                        .First() as MethodSymbol;
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

            if (res == LambdaConversionResult.BadTargetType && _binder.Compilation.Options.HasRuntime)
            {
                if (type.IsCodeblockType() || type.IsUsualType() || type.IsObjectType())
                {
                    return LambdaConversionResult.Success;
                }
                // Todo RvdH Check if can be converted
                /*
                TypeSymbol cb = Compilation.CodeBlockType();
                var conv = Compilation.ClassifyConversion(cb, type);
                if (conv.Exists)
                {
                    return LambdaConversionResult.Success;
                }
                */
                return LambdaConversionResult.Success;
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
                if (Compilation.Options.HasOption(CompilerOption.SignedUnsignedConversion, sourceExpression.Syntax)) // vo4
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
                if (destination.SpecialType == SpecialType.System_Object)
                {
                    return Conversion.Boxing;
                }
            }
            else if (source.IsPointerType() || source.IsPszType())
            {
                if (destination.SpecialType == SpecialType.System_Object)
                {
                    if (Compilation.Options.Dialect.AllowPointerMagic())
                    {
                        // not really boxing, but this is handled in UnBoxXSharpType where the pointer is converted to an IntPtr
                        return Conversion.Boxing;
                    }
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
            if (Compilation.Options.HasOption(CompilerOption.SignedUnsignedConversion, sourceExpression.Syntax)) // vo4
            {
                // This compiler option only applies to numeric types
                if (srcType.IsNumericType() && dstType.IsNumericType())
                {
                    if (srcType.IsIntegralType() &&  dstType.IsIntegralType())
                    {
                        // when both same # of bits and integral, use Identity conversion
                        if (srcType.SizeInBytes() == dstType.SizeInBytes())
                            return Conversion.Identity;
                        else
                            // otherwise implicit conversion
                            return Conversion.ImplicitNumeric;
                    }
                    // VO/Vulcan also allows to convert floating point types <-> integral types
                    else if (srcType.IsIntegralType() || dstType.IsIntegralType())
                    {
                        return Conversion.ImplicitNumeric;
                    }
                }

            }
            if (XsIsImplicitBinaryOperator(sourceExpression, destination,null))
            {
                return Conversion.ImplicitNumeric;
            }    

            return Conversion.NoConversion;

        }

        internal bool XsIsImplicitBinaryOperator(BoundExpression expression, TypeSymbol targetType, Binder binder)
        {
            if (expression is BoundBinaryOperator binop && targetType.SpecialType.IsIntegralType())
            {
                var sourceType = binop.LargestOperand(this.Compilation,false);
                if (TypeSymbol.Equals(sourceType, targetType))
                {
                    return true;
                }
                if (sourceType.IsIntegralType())
                {
                    var sourceSize = sourceType.SpecialType.SizeInBytes();
                    var targetSize = targetType.SpecialType.SizeInBytes();
                    if (sourceSize < targetSize)
                    {
                        return true;
                    }
                    else if (sourceSize > targetSize)
                    {
                        return false;
                    }
                    // same size, but different type. Only allowed when SignedUnsignedConversion is active
                    else if (Compilation.Options.HasOption(CompilerOption.SignedUnsignedConversion, expression.Syntax)) // vo4
                    {
                        return true;
                    }
                }
            }
            return false;
        }

        protected override Conversion ClassifyVOImplicitBuiltInConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            // Parameters checks have been done in the calling code
            var srcType = source.SpecialType;
            var dstType = destination.SpecialType;
            bool voCast = false;
            bool voConvert = false;
            bool typeCast = false;
            bool vo7 = Compilation.Options.HasOption(CompilerOption.ImplicitCastsAndConversions, sourceExpression.Syntax);
            if (sourceExpression.Syntax != null)
            {
                var xNode = sourceExpression.Syntax.XNode;
                if (xNode is XP.PrimaryExpressionContext pex)
                    xNode = pex.Expr;
                while (xNode != null)
                {
                    voCast = xNode is XP.VoCastExpressionContext;
                    if (voCast)
                        break;
                    voConvert = xNode is XP.VoConversionExpressionContext;
                    if (voConvert)
                        break;
                    typeCast = xNode is XP.TypeCastContext;
                    if (typeCast)
                        break;
                    xNode = xNode.Parent as IXParseTree;
                    if (xNode is XP.StatementContext)
                        break;
                }
            }
            if (vo7 && (typeCast || voCast || voConvert))
            {
                // Allow cast -> BOOLEAN
                if (dstType == SpecialType.System_Boolean && srcType.IsIntegralType())
                {
                    if (sourceExpression is BoundExpression be && be.Type.SpecialType == SpecialType.System_Boolean)
                    {
                        return Conversion.Identity;
                    }
                }
            }
            // TYPE(_CAST, expr) allows almost everything
            // source must be PTR, Integral, IntPtr, UIntPtr
            // this is handled in CanVOCast
            if (voCast && source.CanVOCast() && destination.CanVOCast())
            {
                // No _CAST on USUAL
                if (source.IsUsualType())
                {
                    return Conversion.NoConversion;
                }
                // Allow LOGIC(_CAST
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
                    if (source.IsVoidPointer() && Compilation.Options.Dialect.AllowPointerMagic())
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
                        return Conversion.IntegerToPointer;
                    }
                    if (source.IsPointerType() || source.IsVoidPointer() )
                    {
                        if (Compilation.Options.Dialect.AllowPointerMagic())
                        {
                            return Conversion.Identity;
                        }
                    }
                    if (source.SpecialType == SpecialType.System_IntPtr || source.SpecialType == SpecialType.System_UIntPtr)
                    {
                        return Conversion.Identity;
                    }
                    if (source.IsPszType())
                    {
                        return Conversion.Identity;
                    }
                }
                // Allow cast -> PSZ
                if (destination.IsPszType())
                {
                    return Conversion.Identity;
                }
            }
            if (voConvert)
            {
                // we need to convert BYTE(<p>) to dereferencing the <p>
                // This is done else where in Binder.BindVOPointerDereference()
                // Integer conversions
                if (srcType.IsNumericType() && dstType.IsNumericType() &&
                    srcType.IsIntegralType() == dstType.IsIntegralType())
                {
                    // always implicit numeric conversion
                    return Conversion.ImplicitNumeric;
                }
            }
            if (source.IsUsualType())
            {
                if (destination.IsUsualType())
                    return Conversion.NoConversion;
                if (dstType == SpecialType.System_Decimal)
                    // Usual -> Decimal. Get the object out of the Usual and let the rest be done by Roslyn
                    return Conversion.Boxing;
                // Usual -> OBJECT. Get the object out of the Usual 
                // Our special call will call in UnBoxXSharpType will
                // convert the Unbox operation to a call to __Usual.ToObject()
                // This method will return the Contents of the usual as an object 
                // and not the usual itself as an object
                else if (dstType == SpecialType.System_Object)
                {
                    // All Objects are boxed in a usual
                    return Conversion.Boxing;
                }
                else if (destination.IsReferenceType && !IsClipperArgsType(destination) && ! destination.IsStringType() && !destination.IsIFormatProvider())
                {
                    // all user reference types are boxed. But not the Usual[] args and not string
                    return Conversion.Boxing;
                }
                else if (destination.IsPointerType())
                {
                    // not really boxing, but this is handled in UnBoxXSharpType 
                    return Conversion.Boxing;
                }
            }
            if (voCast && srcType == SpecialType.System_Object)
            {
                var xnode = sourceExpression.Syntax.XNode as XSharpParserRuleContext;
                if (xnode.IsCastClass() && destination.IsUsualType())
                {
                    // __CASTCLASS(USUAL, OBJECT)
                    // not really boxing, but this is handled in UnBoxXSharpType 
                    return Conversion.Boxing; 
                }
            }

            if (Compilation.Options.LateBindingOrFox(sourceExpression.Syntax) || vo7)                // lb or vo7
            {
                if (srcType == SpecialType.System_Object)
                {
                    if (destination.IsReferenceType && !IsClipperArgsType(destination))
                    {
                        // Convert Object -> Reference allowed with /lb and with /vo7
                        // except when converting to array of usuals
                        return Conversion.ImplicitReference;
                    }
                    if (destination.IsPointerType() || destination.SpecialType == SpecialType.System_IntPtr || destination.IsPszType())
                    {
                        if (Compilation.Options.Dialect.AllowPointerMagic())
                        {
                            // not really boxing, but this is handled in UnBoxXSharpType 
                            return Conversion.Boxing;
                        }
                    }
                }
                if (dstType == SpecialType.System_Object)
                {
                    if (source.IsReferenceType)
                    {
                        return Conversion.ImplicitReference;
                    }
                }
            }
            if (vo7)
            {
                // Convert Any Ptr -> Any Ptr 
                if (source.IsPointerType() && destination.IsPointerType())
                {
                    return Conversion.Identity;
                }
            }
            // Convert Integral type -> Ptr Type 
            if (source.IsIntegralType() && destination.IsPointerType() && Compilation.Options.Dialect.AllowPointerMagic())
            {
                if (Compilation.Options.Platform == Platform.X86 && srcType.SizeInBytes() <= 4)
                {
                    return Conversion.Identity;
                }
                if (Compilation.Options.Platform == Platform.X64 && srcType.SizeInBytes() <= 8)
                {
                    return Conversion.Identity;
                }
                return Conversion.IntegerToPointer;
            }
            // When unsafe we always allow to cast void * to typed *
            // Is this OK ?
            // See ticket C425
            
            if (source.IsVoidPointer() && destination.IsPointerType() &&
                Compilation.Options.HasOption(CompilerOption.ImplicitCastsAndConversions, sourceExpression.Syntax) &&
                Compilation.Options.Dialect.AllowPointerMagic())
            {
                return Conversion.Identity;
            }
            if (srcType.IsIntegralType() && dstType.IsIntegralType())
            {
                if (srcType.SizeInBytes() < dstType.SizeInBytes()
                    || sourceExpression is BoundConditionalOperator)
					// IIF expressions with literals are always seen as Int, even when the values are asmall
                {
                    return Conversion.ImplicitNumeric;
                }
            }
            if (destination.IsPszType() || destination.IsVoidPointer())
            {
                if (source.IsStringType())
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
            if (source is null || destination is null )
            {
                return ClassifyNullConversionFromExpression(sourceExpression, source, destination, ref useSiteDiagnostics);
            }
            if (Compilation.Options.HasRuntime)
                return ClassifyVOImplicitBuiltInConversionFromExpression(sourceExpression, source, destination, ref useSiteDiagnostics);
            else
                return ClassifyCoreImplicitConversionFromExpression(sourceExpression, source, destination, ref useSiteDiagnostics);
        }

        protected override bool IsClipperArgsType(TypeSymbol args)
        {
            bool result = false;
            if (args is ArrayTypeSymbol ats)
            {
                if (Compilation.Options.HasRuntime)
                {
                    result = ats.ElementType.IsUsualType();
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
