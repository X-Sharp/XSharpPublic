//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class LocalRewriter
    {
        // DO NOT use _Factory.StaticCall() here. We need the original syntaxnode here
        // so we can check later in IsFoxMemberAccess that we want to
        // handle the cursor.FieldName syntax
        //
        private BoundExpression MemVarFieldAccess(SyntaxNode syntax, XsVariableSymbol property)
        {
            var getMethod = property.GetMethod;
            Debug.Assert(getMethod != null);
            if (property.HasAlias)
            {
                var arg1 = MakeConversionNode(_factory.Literal(property.Alias), getMethod.ParameterTypes[0], false);
                var arg2 = MakeConversionNode(_factory.Literal(property.Name), getMethod.ParameterTypes[1], false);
                return BoundCall.Synthesized(syntax, null, getMethod, arg1, arg2);
            }
            else
            {
                var arg1 = MakeConversionNode(_factory.Literal(property.Name), getMethod.ParameterTypes[0], false);
                return BoundCall.Synthesized(syntax, null, getMethod, arg1);
            }
        }
        private BoundExpression MemVarFieldAssign(SyntaxNode syntax, XsVariableSymbol property, BoundExpression rewrittenRight)
        {
            var setMethod = property.SetMethod;
            Debug.Assert(setMethod != null);
            if (property.HasAlias)
            {
                var arg1 = MakeConversionNode(_factory.Literal(property.Alias), setMethod.ParameterTypes[0], false);
                var arg2 = MakeConversionNode(_factory.Literal(property.Name), setMethod.ParameterTypes[1], false);
                var arg3 = MakeConversionNode(rewrittenRight, setMethod.ParameterTypes[2], false);
                return BoundCall.Synthesized(syntax, null, setMethod, ImmutableArray.Create(arg1, arg2,arg3));

            }
            else
            {
                var arg1 = _factory.Literal(property.Name);
                var arg2 = MakeConversionNode(rewrittenRight, _compilation.UsualType(), false);
                return BoundCall.Synthesized(syntax, null, setMethod, arg1, arg2);

            }

        }
    }
}
