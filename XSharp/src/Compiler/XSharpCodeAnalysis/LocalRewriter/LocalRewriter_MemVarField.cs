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

        //private MethodSymbol FindUsualObjectCtor()
        //{
        //    var objectType = _compilation.GetSpecialType(SpecialType.System_Object);

        //    var ctor = _compilation.UsualType().GetMembers(".ctor").Where(c => c.GetParameterCount() == 1 && c.GetParameterTypes()[0] == objectType).FirstOrDefault();
        //    return (MethodSymbol) ctor;
        //}
        private BoundExpression MemVarFieldAccess(SyntaxNode syntax, XsVariableSymbol property)
        {
            var getMethod = property.GetMethod;
            Debug.Assert((object)getMethod != null);
            if (property.HasAlias)
            {
                var stringType = _compilation.GetSpecialType(SpecialType.System_String);
                var lit = new BoundLiteral(syntax, ConstantValue.Create(property.Alias), stringType);
                
                var arg1 = MakeConversionNode(lit, getMethod.ParameterTypes[0], false);
                var arg2 = new BoundLiteral(syntax, ConstantValue.Create(property.Name), stringType);
                return BoundCall.Synthesized(syntax, null, getMethod, arg1, arg2);

            }
            else
            {
                var arg1 = new BoundLiteral(syntax, ConstantValue.Create(property.Name), _compilation.GetSpecialType(SpecialType.System_String));
                return BoundCall.Synthesized(syntax, null, getMethod, arg1);
            }
        }
        private BoundExpression MemVarFieldAssign(SyntaxNode syntax,XsVariableSymbol property, BoundExpression rewrittenRight)
        {
            var setMethod = property.SetMethod;
            if (property.HasAlias)
            {
                var stringType = _compilation.GetSpecialType(SpecialType.System_String);

                var lit = new BoundLiteral(syntax, ConstantValue.Create(property.Alias), stringType);

                var arg1 = MakeConversionNode(lit, setMethod.ParameterTypes[0], false);
                var arg2 = new BoundLiteral(syntax, ConstantValue.Create(property.Name), stringType);
                var arg3 = MakeConversionNode(rewrittenRight, setMethod.ParameterTypes[2], false);
                return BoundCall.Synthesized(syntax, null, setMethod, ImmutableArray.Create(arg1, arg2, arg3));

            }
            else
            {
                var arg1 = new BoundLiteral(syntax, ConstantValue.Create(property.Name), _compilation.GetSpecialType(SpecialType.System_String));
                return BoundCall.Synthesized(syntax, null, setMethod, arg1, MakeConversionNode(rewrittenRight, _compilation.UsualType(), false));
            }

        }
    }
}
