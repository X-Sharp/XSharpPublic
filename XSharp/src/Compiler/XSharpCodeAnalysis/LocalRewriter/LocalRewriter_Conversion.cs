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
        private MethodSymbol getImplicitOperator(NamedTypeSymbol srcType, NamedTypeSymbol destType)
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
        private MethodSymbol getExplicitOperator(NamedTypeSymbol srcType, NamedTypeSymbol destType)
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
        private ConversionKind UnBoxVOType(ref BoundExpression rewrittenOperand, ConversionKind conversionKind, TypeSymbol rewrittenType)
        {
            if (_compilation.Options.IsDialectVO)
            {
                var usualType = _compilation.GetWellKnownType(WellKnownType.Vulcan___Usual);
                var floatType = _compilation.GetWellKnownType(WellKnownType.Vulcan___VOFloat);
                var nts = rewrittenOperand.Type as NamedTypeSymbol;
                if (nts != null)
                {
                    nts = nts.ConstructedFrom;
                }
                if (nts == usualType)
                {
                    if (rewrittenType.IsPointerType())
                    {
                        // Pointer types are not really boxed
                        // we call the appropriate implicit operator here
                        MethodSymbol m = getImplicitOperator(usualType, _compilation.GetSpecialType(SpecialType.System_IntPtr));
                        if (m != null)
                        { 
                            rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                            rewrittenOperand.WasCompilerGenerated = true;
                            return ConversionKind.PointerToPointer; 
                        }
                    }
                    else if (rewrittenType.SpecialType == SpecialType.System_DateTime)
                    {
                        rewrittenOperand = _factory.StaticCall(usualType, VulcanToObject, rewrittenOperand);
                        return ConversionKind.Unboxing;
                    }
                    else // System.Decimals, Objects and reference types, but not String
                    {
                        rewrittenOperand = _factory.StaticCall(usualType, VulcanToObject, rewrittenOperand);
                        conversionKind = rewrittenType.IsObjectType() ? ConversionKind.Identity : rewrittenType.IsReferenceType ? ConversionKind.ImplicitReference : ConversionKind.Unboxing;
                    }
                }
                else if (nts == floatType && rewrittenType is NamedTypeSymbol)
                {
                    MethodSymbol m = getExplicitOperator(floatType, rewrittenType as NamedTypeSymbol);
                    if (m != null)
                    {
                        rewrittenOperand = _factory.StaticCall(rewrittenType, m, rewrittenOperand);
                        rewrittenOperand.WasCompilerGenerated = true;
                        return ConversionKind.Identity;
                    }
                }
            }
            return conversionKind;
        }


    }
}