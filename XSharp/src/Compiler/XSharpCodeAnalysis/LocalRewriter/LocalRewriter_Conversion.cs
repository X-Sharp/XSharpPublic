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
        private MethodSymbol getImplicitOperator(TypeSymbol srcType, TypeSymbol destType)
        {
            var members = srcType.GetMembers("op_Implicit");
            foreach (MethodSymbol m in members)
            {
                if (m.ReturnType == destType)
                {
                    return m;
                }

            }
            return null;
        }
        private MethodSymbol getExplicitOperator(TypeSymbol srcType, TypeSymbol destType)
        {
            var members = srcType.GetMembers("op_Explicit");
            foreach (MethodSymbol m in members)
            {
                if (m.ReturnType == destType)
                {
                    return m;
                }

            }
            return null;
        }
        private ConversionKind UnBoxXSharpType(ref BoundExpression rewrittenOperand, ConversionKind conversionKind, TypeSymbol rewrittenType)
        {
            var nts = rewrittenOperand?.Type as NamedTypeSymbol;

            if (_compilation.Options.IsDialectVO)
            {
                var usualType = _compilation.UsualType();
                if (nts != null)
                {
                    nts = nts.ConstructedFrom;
                }
                // Ticket C575: Assign Interface to USUAL
                // Marked as Boxing in Conversions.cs
                // Implementation here
                if (nts != null && nts.IsInterface && rewrittenType == usualType)
                {
                    var members = usualType.GetMembers("op_Implicit");
                    foreach (var m in members)
                    {
                        var pt = m.GetParameterTypes()[0] as TypeSymbol;
                        if (pt == _compilation.GetSpecialType(SpecialType.System_Object))
                        { 
                            rewrittenOperand = _factory.StaticCall(rewrittenType, (MethodSymbol) m, rewrittenOperand);
                            rewrittenOperand.WasCompilerGenerated = true;
                            return ConversionKind.Identity;
                        }
                    }
                }

                if (nts == usualType)
                {
                    // USUAL -> WINBOOL, use LOGIC as intermediate type
                    if (rewrittenType== _compilation.WinBoolType())
                    {
                        MethodSymbol m = null;
                        m = getImplicitOperator(usualType, _compilation.GetSpecialType(SpecialType.System_Boolean));
                        rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                        rewrittenOperand.WasCompilerGenerated = true;
                        return ConversionKind.Identity;
                    }

                    if (rewrittenType.IsPointerType())
                    {
                        // Pointer types are not really boxed
                        // we call the appropriate implicit operator here
                        MethodSymbol m = null;
                        m = getImplicitOperator(usualType, rewrittenType);
                        if (m == null)
                        {
                            m = getImplicitOperator(usualType, _compilation.GetSpecialType(SpecialType.System_IntPtr));
                        }
                        if (m != null)
                        {
                            rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                            rewrittenOperand.WasCompilerGenerated = true;
                            return ConversionKind.PointerToPointer;
                        }
                    }
                    else if (rewrittenType.SpecialType == SpecialType.System_DateTime)
                    {
                        rewrittenOperand = _factory.StaticCall(usualType, XSharpFunctionNames.ToObject , rewrittenOperand);
                        return ConversionKind.Unboxing;
                    }
                    else // System.Decimals, Objects and reference types, but not String
                    {
                        // special case for __CastClass
                        var xnode = rewrittenOperand.Syntax.Parent?.XNode as LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParserRuleContext;
                        if (xnode != null && xnode.IsCastClass())
                        {
                            conversionKind = ConversionKind.Boxing;
                        }
                        else
                        {
                            rewrittenOperand = _factory.StaticCall(usualType, XSharpFunctionNames.ToObject, rewrittenOperand);
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
                var floatType = _compilation.FloatType();
                if (nts == floatType && rewrittenType is NamedTypeSymbol)
                {
                    MethodSymbol m = getExplicitOperator(floatType, rewrittenType as NamedTypeSymbol);
                    if (m != null)
                    {
                        rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                        rewrittenOperand.WasCompilerGenerated = true;
                        return ConversionKind.Identity;
                    }
                    m = getImplicitOperator(floatType, rewrittenType as NamedTypeSymbol);
                    if (m != null)
                    {
                        rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                        rewrittenOperand.WasCompilerGenerated = true;
                        return ConversionKind.Identity;

                    }
                    if (rewrittenType == _compilation.GetSpecialType(SpecialType.System_Object) ||
                        rewrittenType == _compilation.UsualType())
                    {
                        return ConversionKind.Boxing;

                    }
                    // what else, any other numeric type Convert to Double first and then to destination type
                    m = getImplicitOperator(floatType, _compilation.GetSpecialType(SpecialType.System_Double));
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
            }
            return conversionKind;
        }
    }
}
