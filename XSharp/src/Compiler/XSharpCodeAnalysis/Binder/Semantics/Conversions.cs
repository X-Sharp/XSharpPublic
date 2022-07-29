//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable

using System.Collections.Generic;
using System.Linq;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

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
        public override bool HasBoxingConversion(TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            bool result = base.HasBoxingConversion(source, destination, ref useSiteDiagnostics);

            if (!result && _binder.Compilation.Options.HasRuntime && destination is { } && source is NamedTypeSymbol)
            {
                if (source.IsUsualType())
                {
                    var destFrom = (destination as NamedTypeSymbol)?.ConstructedFrom;
                    if (destination.IsReferenceType)
                    {
                        // do not box string, array, codeblock and clipperargs
                        result = !destination.IsStringType()
                            && destFrom is { }
                            && !destFrom.IsArrayType()
                            && !destFrom.IsCodeblockType()
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
                            && destFrom is { }
                            && !destFrom.IsSymbolType()
                            && !destFrom.IsPszType()
                            && !destFrom.IsFloatType()
                            && !destFrom.IsCurrencyType()
                            && !destFrom.IsDateType();
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
                if (destination.IsUsualType())
                {
                    var usualType = _binder.Compilation.UsualType();
                    var op = usualType.GetOperators(WellKnownMemberNames.ImplicitConversionName)
                        .WhereAsArray(o => o.ParameterCount == 1
                        && o.Parameters[0].Type.IsObjectType()
                        && o.ReturnType.IsUsualType())
                        .First();
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
            }

            return res;
        }
        // Please note that Conversion.Special is a 'pseudo' boxing conversion
        // with a special flag set. This will trigger code in the local rewriter
        // to convert things 'our way', applying knowledge about our types
        // and about our compiler options, such as /vo11
        protected override Conversion ClassifyCoreImplicitConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            // Parameters checks have been done in the calling code
            // The following conversion Rules are for all dialects 
            var srcType = source.SpecialType;
            var dstType = destination.SpecialType;
            var syntax = sourceExpression.Syntax;
            // From and to CHAR
            var vo4 = Compilation.Options.HasOption(CompilerOption.Vo4, syntax);
            if (srcType == SpecialType.System_Char)
            {
                if (dstType == SpecialType.System_UInt16)
                    return Conversion.Identity;
                if (vo4)
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
                    return Conversion.Special;
                }
            }
            else if (source.IsPointerType() || source.IsPszType())
            {
                if (destination.SpecialType == SpecialType.System_Object)
                {
                    if (Compilation.Options.Dialect.AllowPointerMagic())
                    {
                        // not really boxing but we'll handle the actual conversion later
                        // see UnBoxXSharpType() in LocalRewriter_Conversion.cs
                        return Conversion.Special;
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
            if (!srcType.Equals(dstType) && Compilation.Options.HasRuntime)
            {
                // These compiler options only applies to numeric types
                Conversion result = Conversion.NoConversion;
                if (source.IsXNumericType() && destination.IsXNumericType())
                {
                    if (vo4 || syntax.XNoTypeWarning)
                    {
                        srcType = source.XsSpecialtype();
                        dstType = destination.XsSpecialtype();
                        if (srcType.IsIntegralType() && dstType.IsIntegralType())
                        {
                            // when both same # of bits and integral, use Identity conversion
                            if (srcType.SizeInBytes() == dstType.SizeInBytes() && sourceExpression.IsExpressionWithNumericConstant())
                                result = Conversion.Identity;
                            else
                                result = Conversion.ImplicitNumeric;
                        }
                        else
                        {
                            // VO/Vulcan also allows to convert floating point types <-> integral types
                            // not really boxing but we'll handle the actual conversion later
                            // see UnBoxXSharpType() in LocalRewriter_Conversion.cs
                            // The method of converting is chosen by inspecting the /vo11 compiler option
                            result = Conversion.Special;
                        }
                    }
                }
                if (result != Conversion.NoConversion)
                {
                    return result;
                }
            }
            if (XsIsImplicitBinaryOperator(sourceExpression, destination, null))
            {
                return Conversion.ImplicitNumeric;
            }
            return Conversion.NoConversion;
        }

        internal bool XsIsImplicitBinaryOperator(BoundExpression expression, TypeSymbol targetType, Binder binder)
        {
            if (expression is BoundBinaryOperator binop && targetType.SpecialType.IsIntegralType())
            {
                var sourceType = binop.LargestOperand(this.Compilation);
                if (Equals(sourceType, targetType))
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
                }
            }
            return false;
        }

        protected override Conversion ClassifyVOImplicitBuiltInConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            // Parameters checks have been done in the calling code
            var srcType = source.SpecialType;
            var dstType = destination.SpecialType;
            var syntax = sourceExpression.Syntax;
            bool vo7 = Compilation.Options.HasOption(CompilerOption.ImplicitCastsAndConversions, syntax);

            if (Equals(source, destination))
            {
                return Conversion.Identity;
            }

            if (source.IsUsualType())
            {
                if (dstType == SpecialType.System_Decimal ||
                    dstType == SpecialType.System_DateTime)
                // Usual -> Decimal. Get the object out of the Usual and let the rest be done by Roslyn
                {
                    return Conversion.Special;
                }
                // Usual -> OBJECT. Get the object out of the Usual 
                // Our special call will call in UnBoxXSharpType will
                // convert the Unbox operation to a call to __Usual.ToObject()
                // This method will return the Contents of the usual as an object 
                // and not the usual itself as an object
                else if (dstType == SpecialType.System_Object)
                {
                    // All Objects are boxed in a usual
                    return Conversion.Special;
                }
                else if (destination.IsReferenceType && !IsClipperArgsType(destination) && !destination.IsStringType() && !destination.IsIFormatProvider())
                {
                    // all user reference types are boxed. But not the Usual[] args and not string
                    return Conversion.Special;
                }
                else if (destination.IsPointerType())
                {
                    // not really boxing but we'll handle the actual conversion later
                    // see UnBoxXSharpType() in LocalRewriter_Conversion.cs
                    return Conversion.Special;
                }
            }

            if (Compilation.Options.LateBindingOrFox(sourceExpression.Syntax) || vo7)                // lb or vo7
            {
                if (srcType == SpecialType.System_Object && dstType != SpecialType.System_Object)
                {
                    if (destination.IsReferenceType && !IsClipperArgsType(destination))
                    {
                        // Convert Object -> Reference allowed with /lb and with /vo7
                        // not really boxing but we'll handle generating the castclass later
                        // see UnBoxXSharpType() in LocalRewriter_Conversion.cs
                        return Conversion.Special;
                    }
                    if (destination.IsPointerType() || destination.SpecialType == SpecialType.System_IntPtr || destination.IsPszType())
                    {
                        if (Compilation.Options.Dialect.AllowPointerMagic())
                        {
                            // not really boxing but we'll handle the actual conversion later
                            // see UnBoxXSharpType() in LocalRewriter_Conversion.cs
                            return Conversion.Special;
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
                vo7 && Compilation.Options.Dialect.AllowPointerMagic())
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
            if (destination.IsPszType() && source.IsStringType())
            {
                 return Conversion.ImplicitReference;
            }
            if (srcType == SpecialType.System_Object || dstType == SpecialType.System_Object)
            {
                var xnode = sourceExpression.Syntax.XNode as XSharpParserRuleContext;
                if (xnode.IsCastClass())
                {
                    // __CASTCLASS(USUAL, OBJECT)
                    // not really boxing but we'll handle the actual conversion later
                    // see UnBoxXSharpType() in LocalRewriter_Conversion.cs
                    return Conversion.Special;
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
            if (source is null || destination is null)
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

        protected override Conversion ClassifyXSExplicitBuiltInConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, bool forCast, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            var syntax = sourceExpression.Syntax;
            var vo4 = Compilation.Options.HasOption(CompilerOption.Vo4, syntax);
            if (!forCast && !vo4)
            {
                return Conversion.NoConversion;
            }
            var srcType = source.SpecialType;
            var dstType = destination.SpecialType;

            if (source.IsFractionalType() && destination.SpecialType.IsIntegralType() )
            {
                if (vo4 && destination is { } && destination.IsXNumericType())
                {
                    // not really boxing but we'll handle the actual conversion later
                    // see UnBoxXSharpType() in LocalRewriter_Conversion.cs
                    return Conversion.Special;
                }
            }
            bool vo7 = Compilation.Options.HasOption(CompilerOption.ImplicitCastsAndConversions, syntax);
            // Allow cast -> BOOLEAN
            if (vo7 && dstType == SpecialType.System_Boolean && source.IsIntegralType())
            {
                if (sourceExpression is BoundExpression be && be.Type.SpecialType == SpecialType.System_Boolean)
                {
                    return Conversion.Identity;
                }
            }
            bool voCast = false;
            bool voConvert = false;
            if (syntax != null)
            {
                var xNode = sourceExpression.Syntax.XNode;
                if (xNode is XP.PrimaryExpressionContext pex)
                    xNode = pex.Expr;
                while (xNode != null)
                {
                    if (xNode is XP.AssignmentExpressionContext)
                        break;
                    // voCast = type(_CAST, expr )
                    voCast = xNode is XP.VoCastExpressionContext;
                    if (voCast)
                        break;
                    // voConvert = type( expr )
                    voConvert = xNode is XP.VoConversionExpressionContext;
                    if (voConvert)
                        break;
                    // typeCast = (type) expr
                    xNode = xNode.Parent as IXParseTree;
                    if (xNode is XP.StatementContext)
                        break;
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
                if (dstType.IsIntegralType() && !source.IsNullableType() && !source.IsReferenceType)
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
                    if (source.IsPointerType() || source.IsVoidPointer())
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
                    // PSZ -> Pointer: No implicit conversion. There is a user defined conversion in the PSZ type
                    if (source.IsPszType())
                    {
                        return Conversion.NoConversion;
                    }
                }
                if (destination.IsPszType() && source is PointerTypeSymbol)
                {
                    // pointer -> PSZ: No implicit conversion. There is a user defined conversion in the PSZ type
                    return Conversion.NoConversion;
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
            return Conversion.NoConversion;
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

        protected virtual Conversion ClassifyXSExplicitBuiltInConversionFromExpression(BoundExpression sourceExpression, TypeSymbol source, TypeSymbol destination, bool forCast, ref HashSet<DiagnosticInfo> useSiteDiagnostics)
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
