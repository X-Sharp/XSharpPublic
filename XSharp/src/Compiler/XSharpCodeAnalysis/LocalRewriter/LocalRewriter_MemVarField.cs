//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class LocalRewriter
    {
        private BoundExpression MemVarFieldAccess(XsVariableSymbol property)
        {
            var getMethod = property.GetMethod;
            Debug.Assert((object)getMethod != null);
            if (property.HasAlias)
            {
                var arg1 = MakeConversionNode(_factory.Literal(property.Alias), getMethod.ParameterTypes[0], false);
                var arg2 = MakeConversionNode(_factory.Literal(property.Name), getMethod.ParameterTypes[1], false);
                return _factory.StaticCall(null, getMethod, arg1, arg2);

            }
            else
            {
                var arg1 = MakeConversionNode(_factory.Literal(property.Name), getMethod.ParameterTypes[0], false);
                return _factory.StaticCall(null, getMethod, arg1);
            }
        }
        private BoundExpression MemVarFieldAssign(XsVariableSymbol property, BoundExpression rewrittenRight)
        {
            var setMethod = property.SetMethod;
            if (property.HasAlias)
            {
                var arg1 = MakeConversionNode(_factory.Literal(property.Alias), setMethod.ParameterTypes[0], false);
                var arg2 = MakeConversionNode(_factory.Literal(property.Name), setMethod.ParameterTypes[1], false);
                var arg3 = MakeConversionNode(rewrittenRight, setMethod.ParameterTypes[2], false);
                return _factory.StaticCall(null, setMethod, arg1, arg2, arg3);

            }
            else
            {
                var arg1 = _factory.Literal(property.Name);
                var arg2 = MakeConversionNode(rewrittenRight, _compilation.UsualType(), false);
                return _factory.StaticCall(null, setMethod, arg1, arg2);
            }

        }
    }
}
