/*
   Copyright 2016 XSharp B.V.

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
        private ConversionKind UnBoxVOType(ref BoundExpression rewrittenOperand, ConversionKind conversionKind, TypeSymbol rewrittenType)
        {
            if (_compilation.Options.IsDialectVO)
            {
                var usualType = _compilation.GetWellKnownType(WellKnownType.Vulcan___Usual);
                var nts = rewrittenOperand.Type as NamedTypeSymbol;
                if (nts != null)
                {
                    nts = nts.ConstructedFrom;
                }
                if (nts != null && nts == usualType)
                {
                    rewrittenOperand = _factory.StaticCall(usualType, "ToObject", rewrittenOperand);
                    conversionKind = rewrittenType.IsObjectType() ? ConversionKind.Identity : rewrittenType.IsReferenceType ? ConversionKind.ImplicitReference : ConversionKind.Unboxing;
                }
            }
            return conversionKind;
        }


    }
}