// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

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
        private ConversionKind UnBoxVOType(BoundExpression rewrittenOperand, ConversionKind conversionKind, TypeSymbol rewrittenType)
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