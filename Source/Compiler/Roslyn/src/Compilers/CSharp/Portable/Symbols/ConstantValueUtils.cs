// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System.Collections.Immutable;
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.PooledObjects;
#if XSHARP
using static LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;
#endif
namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal sealed class EvaluatedConstant
    {
        public readonly ConstantValue Value;
        public readonly ImmutableArray<Diagnostic> Diagnostics;

        public EvaluatedConstant(ConstantValue value, ImmutableArray<Diagnostic> diagnostics)
        {
            this.Value = value;
            this.Diagnostics = diagnostics.NullToEmpty();
        }
    }

    internal static class ConstantValueUtils
    {
        public static ConstantValue EvaluateFieldConstant(
            SourceFieldSymbol symbol,
            EqualsValueClauseSyntax equalsValueNode,
            HashSet<SourceFieldSymbolWithSyntaxReference> dependencies,
            bool earlyDecodingWellKnownAttributes,
            DiagnosticBag diagnostics)
        {
            var compilation = symbol.DeclaringCompilation;
            var binderFactory = compilation.GetBinderFactory((SyntaxTree)symbol.Locations[0].SourceTree);
            var binder = binderFactory.GetBinder(equalsValueNode);
            if (earlyDecodingWellKnownAttributes)
            {
                binder = new EarlyWellKnownAttributeBinder(binder);
            }
            var inProgressBinder = new ConstantFieldsInProgressBinder(new ConstantFieldsInProgress(symbol, dependencies), binder);
            BoundFieldEqualsValue boundValue = BindFieldOrEnumInitializer(inProgressBinder, symbol, equalsValueNode, diagnostics);
            var initValueNodeLocation = equalsValueNode.Value.Location;

            var value = GetAndValidateConstantValue(boundValue.Value, symbol, symbol.Type, initValueNodeLocation, diagnostics);
            Debug.Assert(value != null);

            return value;
        }

        private static BoundFieldEqualsValue BindFieldOrEnumInitializer(
            Binder binder,
            FieldSymbol fieldSymbol,
            EqualsValueClauseSyntax initializer,
            DiagnosticBag diagnostics)
        {
            var enumConstant = fieldSymbol as SourceEnumConstantSymbol;
            Binder collisionDetector = new LocalScopeBinder(binder);
            collisionDetector = new ExecutableCodeBinder(initializer, fieldSymbol, collisionDetector);
            BoundFieldEqualsValue result;

            if ((object)enumConstant != null)
            {
                result = collisionDetector.BindEnumConstantInitializer(enumConstant, initializer, diagnostics);
            }
            else
            {
                result = collisionDetector.BindFieldInitializer(fieldSymbol, initializer, diagnostics);
            }

            return result;
        }

        internal static string UnescapeInterpolatedStringLiteral(string s)
        {
            var builder = PooledStringBuilder.GetInstance();
            var stringBuilder = builder.Builder;
            int formatLength = s.Length;
            for (int i = 0; i < formatLength; i++)
            {
                char c = s[i];
                stringBuilder.Append(c);
                if ((c == '{' || c == '}') && (i + 1) < formatLength && s[i + 1] == c)
                {
                    i++;
                }
            }
            return builder.ToStringAndFree();
        }

        internal static ConstantValue GetAndValidateConstantValue(
            BoundExpression boundValue,
            Symbol thisSymbol,
            TypeSymbol typeSymbol,
            Location initValueNodeLocation,
            DiagnosticBag diagnostics)
        {
            var value = ConstantValue.Bad;
            CheckLangVersionForConstantValue(boundValue, diagnostics);
            if (!boundValue.HasAnyErrors)
            {
                if (typeSymbol.TypeKind == TypeKind.TypeParameter)
                {
                    diagnostics.Add(ErrorCode.ERR_InvalidConstantDeclarationType, initValueNodeLocation, thisSymbol, typeSymbol);
                }
                else
                {
                    bool hasDynamicConversion = false;
                    var unconvertedBoundValue = boundValue;
                    while (unconvertedBoundValue.Kind == BoundKind.Conversion)
                    {
                        var conversion = (BoundConversion)unconvertedBoundValue;
                        hasDynamicConversion = hasDynamicConversion || conversion.ConversionKind.IsDynamic();
                        unconvertedBoundValue = conversion.Operand;
                    }

                    // If we have already computed the unconverted constant value, then this call is cheap
                    // because BoundConversions store their constant values (i.e. not recomputing anything).
                    var constantValue = boundValue.ConstantValue;

#if XSHARP
                    // Make sure the boundValue is Completely processed
                    // this may happen for DIM arrays inside VOSTRUCT where the dimension is a VO Define
                    // such as 
                    /*
                        DEFINE CNLEN       := 1
                        DEFINE DNLEN       := CNLEN
                        VOSTRUCT _winRASDIALPARAMS
                           MEMBER dwSize AS DWORD
                           MEMBER DIM szEntryName[ RAS_MaxEntryName + 1 ] AS BYTE
                           MEMBER DIM szPhoneNumber[ RAS_MaxPhoneNumber + 1 ] AS BYTE
                           MEMBER DIM szUserName[ UNLEN + 1 ] AS BYTE
                           MEMBER DIM szPassword[ PWLEN + 1 ] AS BYTE
                           MEMBER DIM szDomain[ DNLEN + 1 ] AS BYTE

                     * 
                     * The Parser could not determine that DNLEN is a const because it was the result of an expression
                     * so it has created a readonly field. The binder may be able to detect that it is a const so it will
                     * change the type from readonly field to a const
                     */
                    if (boundValue is not BoundLiteral &&
                        boundValue.Syntax is ExpressionSyntax &&
                        thisSymbol is SourceFieldSymbol fs && fs.GetNonNullSyntaxNode().XNode is not ClassvarContext &&
                        (constantValue == null || constantValue == ConstantValue.Bad))
                    {
                        //System.Diagnostics.Debug.WriteLine($" Bind field {thisSymbol.Name}");
                        var es = boundValue.Syntax as ExpressionSyntax;
                        var compilation = thisSymbol.DeclaringCompilation;
                        var binderFactory = compilation.GetBinderFactory((SyntaxTree)thisSymbol.Locations[0].SourceTree);
                        var binder = binderFactory.GetBinder(boundValue.Syntax);
                        boundValue = binder.BindValue(es, diagnostics, Binder.BindValueKind.RValue);
                        constantValue = boundValue.ConstantValue;
                    }

                    if ((boundValue as BoundFieldAccess)?.FieldSymbol.Type.SpecialType == SpecialType.System_IntPtr && 
                        ((boundValue as BoundFieldAccess)?.FieldSymbol.Name == "Zero" || (boundValue as BoundFieldAccess)?.FieldSymbol.IsConst == true))
                    {
                        constantValue = ConstantValue.Create(0);
                    }
#endif

                    var unconvertedConstantValue = unconvertedBoundValue.ConstantValue;
                    if (unconvertedConstantValue != null &&
                        !unconvertedConstantValue.IsNull &&
                        typeSymbol.IsReferenceType &&
                        typeSymbol.SpecialType != SpecialType.System_String)
                    {
                        // Suppose we are in this case:
                        //
                        // const object x = "some_string"
                        //
                        // A constant of type object can only be initialized to
                        // null; it may not contain an implicit reference conversion
                        // from string.
                        //
                        // Give a special error for that case.
                        diagnostics.Add(ErrorCode.ERR_NotNullConstRefField, initValueNodeLocation, thisSymbol, typeSymbol);

                        // If we get here, then the constantValue will likely be null.
                        // However, it seems reasonable to assume that the programmer will correct the error not
                        // by changing the value to "null", but by updating the type of the constant.  Consequently,
                        // we retain the unconverted constant value so that it can propagate through the rest of
                        // constant folding.
                        constantValue = constantValue ?? unconvertedConstantValue;
                    }

                    if (constantValue != null && !hasDynamicConversion)
                    {
                        value = constantValue;
                    }
                    else
                    {
                        diagnostics.Add(ErrorCode.ERR_NotConstantExpression, initValueNodeLocation, thisSymbol);
                    }
                }
            }

            return value;
        }

        private sealed class CheckConstantInterpolatedStringValidity : BoundTreeWalkerWithStackGuardWithoutRecursionOnTheLeftOfBinaryOperator
        {
            internal readonly DiagnosticBag diagnostics;

            public CheckConstantInterpolatedStringValidity(DiagnosticBag diagnostics)
            {
                this.diagnostics = diagnostics;
            }

            public override BoundNode VisitInterpolatedString(BoundInterpolatedString node)
            {
                Binder.CheckFeatureAvailability(node.Syntax, MessageID.IDS_FeatureConstantInterpolatedStrings, diagnostics);
                return null;
            }
        }

        internal static void CheckLangVersionForConstantValue(BoundExpression expression, DiagnosticBag diagnostics)
        {
            if (!(expression.Type is null) && expression.Type.IsStringType())
            {
                var visitor = new CheckConstantInterpolatedStringValidity(diagnostics);
                visitor.Visit(expression);
            }
        }
    }
}
