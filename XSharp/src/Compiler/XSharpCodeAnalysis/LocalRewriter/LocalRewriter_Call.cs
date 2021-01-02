﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

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


        private BoundExpression XsAdjustBoundCall(BoundExpression expression)
        {
            // check for BoundInvocationExpression where MethodSymbol has the NeedAccessToLocals attribute
            // and for /fox2
            if (_compilation.Options.Dialect == XSharpDialect.FoxPro &&
                _compilation.Options.HasOption(CompilerOption.FoxExposeLocals, expression.Syntax) &&
                 expression is BoundCall bc)
            {
                var sym = bc.Method;
                if (sym.NeedAccessToLocals())
                {
                    var localsymbols = new List<LocalSymbol>();
                    var binder = bc.BinderOpt;
                    while (binder != null)
                    {
                        localsymbols.AddRange(binder.Locals);
                        if (binder is InMethodBinder)
                            break;
                        binder = binder.Next;
                    }


                    // write prelude and after code
                    // prelude code should register the locals
                    /*
                     assume the following code:

                     LOCAL a := "Robert" AS STRING
                     LOCAL b := 42 AS LONG
                     ? Type(b")
                     RETURN
                     // This code gets generated by the compiler to allow the Type() function to access the locals

                     __LocalPut("a", a)
                     __LocalPut("b", b)
                     var temp := Type(b")
                     IF __LocalsUpdated()
                        a := __LocalGet("a")
                        b := __LocalGet("b")
                     ENDIF
                     __LocalsClear()
                     */
                    var rtType = _compilation.RuntimeFunctionsType();
                    {
                        var exprs = ImmutableArray.CreateBuilder<BoundExpression>();
                        var block = ImmutableArray.CreateBuilder<BoundExpression>();
                        var usual = _compilation.UsualType();
                        foreach (var localsym in localsymbols)
                        {
                            var name = localsym.Name;
                            if (name.IndexOf("$") >= 0)
                                continue;
                            var localvar = _factory.Local(localsym);
                            var localname = _factory.Literal(name);

                            // __LocalPut("name", (USUAL) localvar)
                            var value = MakeConversionNode(localvar, usual, false);
                            var mcall = _factory.StaticCall(rtType, ReservedNames.LocalPut, localname, value);
                            exprs.Add(mcall);

                            // create assignment expression for inside the block that is executed when locals are updated
                            // LocalVar := (CorrectType) __LocalGet("name")
                            mcall = _factory.StaticCall(rtType, ReservedNames.LocalGet, localname);
                            value = MakeConversionNode(mcall, localsym.Type, false);
                            var ass = _factory.AssignmentExpression(localvar, value);
                            block.Add(ass);
                        }
                        // we need an array of the local symbols for the sequence
                        var locals = ImmutableArray.CreateBuilder<LocalSymbol>();
                        var tempSym = _factory.SynthesizedLocal(expression.Type);
                        locals.Add(tempSym);
                        var tempLocal = _factory.Local(tempSym);

                        // var temp := <original expression>
                        var callorig = _factory.AssignmentExpression(tempLocal, expression);
                        exprs.Add(callorig);

                        // create condition  __LocalsUpdated()
                        var cond = _factory.StaticCall(rtType, ReservedNames.LocalsUpdated);
                        var t = _factory.Literal(true);
                        var f = _factory.Literal(false);

                        // create a sequence with the assignment expressions, return true (because the conditional expression needs a value)
                        var assignmentsequence = _factory.Sequence(block.ToArray(), t);

                        // iif ( __localupdated(), <assignmentsequence>, false)
                        var condexpr = _factory.Conditional(cond, assignmentsequence, f, t.Type);
                        exprs.Add(condexpr);

                        // __LocalsClear()
                        var clear = _factory.StaticCall(rtType, ReservedNames.LocalsClear);
                        exprs.Add(VisitExpression(clear));
                        // create a sequence that returns the temp var.
                        expression = _factory.Sequence(locals.ToImmutable(), exprs.ToImmutable(), tempLocal);
                    }
                }
            }
            return expression;
        }

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
                    if (expr is BoundDefaultExpression && parameter.Type.IsUsualType())
                    {
                        var flds = _compilation.UsualType().GetMembers("_NIL");
                        var type = _factory.Type(_compilation.UsualType());
                        expr = _factory.Field(type, (FieldSymbol) flds[0]);
                    }
                      
                    BoundAssignmentOperator boundAssignmentToTemp;
                    BoundLocal boundTemp = _factory.StoreToTemp(expr, out boundAssignmentToTemp);
                    expr = _factory.Sequence(new BoundExpression[]{ boundAssignmentToTemp}, boundTemp);
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