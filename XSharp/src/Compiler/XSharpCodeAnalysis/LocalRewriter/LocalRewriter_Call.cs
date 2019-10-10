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
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class LocalRewriter
    {

        private static ConstantValue XsDefaultValue(ParameterSymbol parameter, CSharpCompilation compilation)
        {
            TypeSymbol parameterType = parameter.Type;
            var defaultConstantValue = parameter.GetVODefaultParameter();
            if (defaultConstantValue == null)
                return null;
            if (parameterType is NamedTypeSymbol &&
                ((NamedTypeSymbol)parameterType).ConstructedFrom == compilation.PszType())
            {

                if (defaultConstantValue.StringValue != null)
                {
                    // ToDo
                    // when the parameter is of type PSZ and there was a literal default string
                    // then Vulcan generates a special literal array in the calling code.
                    // For example Foo(s := "abcë" AS PSZ) becomes a
                    // Foo([DefaultParameterValue("abc\x00eb", 0)] __Psz s)
                    //
                    // and in the calling code the default parameter is stored as a field of the <Module> class
                    //
                    // .field assembly static valuetype $ArrayType$5 䌤㘲␵$PSZ$_15_1 = ((61 62 63 EB 00))
                    //
                    // and a type is declared for the array size of 5 bytes. This type is declared in the global namespace:
                    //
                    // [StructLayout(LayoutKind.Explicit, Size=5, Pack=1)]
                    //    public struct $ArrayType$5
                    //    {
                    //    }
                    //
                    // The call to the function becomes
                    // Foo((__Psz) &䌤㘲␵$PSZ$_15_1);
                    // Nikos can you implement something like this ?
                    //
                    defaultConstantValue = ConstantValue.Null;
                }
            }
            return defaultConstantValue;
        }
        private void XsInsertMissingOptionalArguments(SyntaxNode syntax,
            ImmutableArray<ParameterSymbol> parameters,
            BoundExpression[] arguments,
            ArrayBuilder<RefKind> refKinds,
            ArrayBuilder<LocalSymbol> temps,
            ThreeState enableCallerInfo = ThreeState.Unknown
            )
        {
            Debug.Assert(refKinds.Count == arguments.Length);
            for (int p = 0; p < arguments.Length; ++p)
            {
                if (arguments[p] == null || arguments[p].Syntax.XIsMissingArgument)
                {
                    ParameterSymbol parameter = parameters[p];
                    BoundExpression expr = GetDefaultParameterValue(syntax, parameter, enableCallerInfo) ;
                    if (expr is BoundDefaultExpression && parameter.Type == _compilation.UsualType())
                    {
                        var flds = _compilation.UsualType().GetMembers("_NIL");
                        var type = new BoundTypeExpression(syntax, null, _compilation.UsualType());
                        expr = _factory.Field(type, (FieldSymbol) flds[0]);
                    }
                      
                    BoundAssignmentOperator boundAssignmentToTemp;
                    BoundLocal boundTemp = _factory.StoreToTemp(expr, out boundAssignmentToTemp);
                    expr = new BoundSequence(
                                        syntax,
                                        locals: ImmutableArray<LocalSymbol>.Empty,
                                        sideEffects: ImmutableArray.Create<BoundExpression>(boundAssignmentToTemp),
                                        value: boundTemp,
                                        type: boundTemp.Type);
                    refKinds[p] = parameters[p].RefKind;
                    temps.Add(boundTemp.LocalSymbol);
                  

                    arguments[p] = expr;
                    Debug.Assert(arguments[p].Type == parameter.Type);

                    if (parameters[p].RefKind == RefKind.In)
                    {
                        Debug.Assert(refKinds[p] == RefKind.None);
                        refKinds[p] = RefKind.In;
                    }
                }

            }
        }
    }
}
