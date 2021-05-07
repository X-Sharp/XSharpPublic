//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System;
using System.Linq;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.Symbols;
using Microsoft.CodeAnalysis;
using System.Collections.Concurrent;

namespace Microsoft.CodeAnalysis.CSharp.Symbols
{
    internal static partial class ParameterSymbolExtensions
    {
        public static bool HasVODefaultParameter(this ParameterSymbol param)
        {
            if (param is { }) // prevent calling equals operator on ParameterSymbol
            {
                var attrs = param.GetAttributes();
                foreach (var attr in attrs)
                {
                    var atype = attr.AttributeClass;
                    if (atype.IsOurAttribute(OurTypeNames.DefaultParameterAttribute))
                        return true;
                }
            }
            return false;
        }

        public static BoundExpression GetVODefaultParameter(this ParameterSymbol param, SyntaxNode syntax)
        {
            ConstantValue constant;
            if (param is { }) // prevent calling equals operator on ParameterSymbol
            {
                var attrs = param.GetAttributes();
                foreach (var attr in attrs)
                {
                    var atype = attr.AttributeClass;
                    if (atype.IsOurAttribute(OurTypeNames.DefaultParameterAttribute))
                    {

                        var arg = attr.CommonConstructorArguments[0];
                        if (arg.Value == null)
                            return new BoundDefaultExpression(syntax, param.Type);
                        int desc = attr.CommonConstructorArguments[1].DecodeValue<int>(SpecialType.System_Int32);
                        if (desc == 0 && TypeSymbol.Equals(param.Type, param.DeclaringCompilation.PszType()))
                        {
                            desc = 4;
                        }
                        TypeSymbol strType;
                        BoundLiteral lit;
                        MethodSymbol ctor;
                        switch (desc)
                        {
                            case 0:
                                // normal .Net Object
                                // return value  or null
                                if (arg.Type != null && arg.Value != null)
                                {
                                    if (arg.Type.SpecialType == SpecialType.None)
                                    {
                                        // Enum type? can be casted to Int32
                                        constant = ConstantValue.Create(arg.Value, SpecialType.System_Int32);
                                    }
                                    else
                                    {
                                        constant = ConstantValue.Create(arg.Value, arg.Type.SpecialType);
                                    }
                                    return new BoundLiteral(syntax, constant, param.Type);
                                }
                                break;
                            case 1:
                                // NIL. We should not get here.
                                return new BoundDefaultExpression(syntax, param.Type);
                            case 2:
                                // Date, value should be long of ticks. Return DateTime
                                var longValue = arg.DecodeValue<long>(SpecialType.System_Int64);
                                if (longValue == 0L)
                                {
                                    // Null_Date
                                    return new BoundDefaultExpression(syntax, param.Type);
                                }

                                constant = ConstantValue.Create(new DateTime(longValue));
                                var dtType = param.DeclaringCompilation.GetSpecialType(SpecialType.System_DateTime);
                                lit = new BoundLiteral(syntax, constant, dtType);
                                ctor = param.DeclaringCompilation.DateType().GetConstructor(dtType);
                                if (ctor != null)
                                {
                                    return new BoundObjectCreationExpression(syntax, ctor, lit);
                                }
                                // if it fails then return an error value below
                                break;
                            case 3:
                                // Symbol, value should be a string literal or null
                                constant = ConstantValue.Create(arg.DecodeValue<string>(SpecialType.System_String));
                                strType = param.DeclaringCompilation.GetSpecialType(SpecialType.System_String);
                                lit = new BoundLiteral(syntax, constant, strType);
                                ctor = param.DeclaringCompilation.SymbolType().GetConstructor(strType);
                                if (ctor != null)
                                {
                                    return new BoundObjectCreationExpression(syntax, ctor, lit);
                                }
                                // if it fails then return an error value below
                                break;

                            case 4:
                                // Psz, value should be a string or null
                                constant = ConstantValue.Create(arg.DecodeValue<string>(SpecialType.System_String));
                                strType = param.DeclaringCompilation.GetSpecialType(SpecialType.System_String);
                                lit = new BoundLiteral(syntax, constant, strType);
                                ctor = param.DeclaringCompilation.PszType().GetConstructor(strType);
                                if (ctor != null)
                                {
                                    return new BoundObjectCreationExpression(syntax, ctor, lit);
                                }
                                // if it fails then return an error value below
                                break;
                            case 5:
                                // IntPtr, return value as IntPtr
                                IntPtr p = new IntPtr(arg.DecodeValue<int>(SpecialType.System_Int32));
                                constant = ConstantValue.Create(p);
                                return new BoundLiteral(syntax, constant, param.DeclaringCompilation.GetSpecialType(SpecialType.System_IntPtr));
                            default:
                                return new BoundDefaultExpression(syntax, param.Type);
                        }
                    }
                }
            }
            return new BoundDefaultExpression(syntax, param.Type);
        }

        private static MethodSymbol GetConstructor(this TypeSymbol type, TypeSymbol paramType)
        {
            return (MethodSymbol)type.GetMembers(".ctor").Where(m => m.GetParameterCount() == 1 &&
                                                TypeSymbol.Equals(m.GetParameterTypes()[0].Type, paramType)).FirstOrDefault();
        }

        public static TypeWithAnnotations GetActualType(this ParameterSymbol parameter)
        {
            var type = parameter.Type;
            if (type is { } && type.SpecialType == SpecialType.System_IntPtr)
            {
                var attrs = parameter.GetAttributes();
                foreach (var attr in attrs)
                {
                    var atype = attr.AttributeClass;
                    if (atype.IsOurAttribute(OurTypeNames.ActualTypeAttribute))
                    {
                        object t = attr.CommonConstructorArguments[0].DecodeValue<object>(SpecialType.None);
                        if (t is TypeSymbol)
                        {
                            type = (TypeSymbol)t;
                            break;
                        }
                    }
                }
            }
            return TypeWithAnnotations.Create(type);
        }
    }
}
