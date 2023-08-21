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
        // DO NOT use _Factory.StaticCall() here. We need the original syntaxnode here
        // so we can check later in IsFoxMemberAccess that we want to
        // handle the cursor.FieldName syntax
        //
        private BoundExpression MemVarFieldAccess(SyntaxNode syntax, XsVariableSymbol property, BoundPropertyAccess? bpa)
        {
            var getMethod = property.GetMethod;
            _factory.Syntax = syntax;

            Debug.Assert( getMethod is { });
            if (property.HasAlias)
            {
                var arg1 = MakeConversionNode(_factory.Literal(property.Alias), getMethod.Parameters[0].Type, false);
                var arg2 = MakeConversionNode(_factory.Literal(property.Name), getMethod.Parameters[1].Type, false);
                var call = BoundCall.Synthesized(syntax, null, getMethod, arg1, arg2);
                // Keep track of the original PropertyAccess in case they are passing the variable by reference
                call.PropertyAccess = bpa;
                return call;
            }
            else
            {
                var arg1 = MakeConversionNode(_factory.Literal(property.Name), getMethod.Parameters[0].Type, false);
                var call = BoundCall.Synthesized(syntax, null, getMethod, arg1);
                call.PropertyAccess = bpa;
                return call;
            }
        }
        private BoundExpression MemVarFieldAssign(SyntaxNode syntax, XsVariableSymbol property, BoundExpression rewrittenRight)
        {
            _factory.Syntax = syntax;

            var setMethod = property.SetMethod;
            Debug.Assert(setMethod is { });
            if (property.HasAlias)
            {
                var arg1 = MakeConversionNode(_factory.Literal(property.Alias), setMethod.Parameters[0].Type, false);
                var arg2 = MakeConversionNode(_factory.Literal(property.Name), setMethod.Parameters[1].Type, false);
                var arg3 = MakeConversionNode(rewrittenRight, setMethod.Parameters[2].Type, false);
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
