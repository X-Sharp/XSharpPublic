//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Generic;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis.CSharp.Symbols;
namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class Binder
    {
        BoundExpression XsCreateConversionNonIntegralNumeric(TypeSymbol targetType, BoundExpression expression, DiagnosticBag diagnostics, Conversion conversion)
        {
            if (Compilation.Options.VOArithmeticConversions)
            {
                // call Convert.To..() to round the result
                var mem = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Convert__ToInt32Double);
                switch (targetType.SpecialType)
                {
                    case SpecialType.System_UInt32:
                        mem = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Convert__ToUInt32Double);
                        break;
                    case SpecialType.System_Int16:
                        mem = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Convert__ToInt16Double);
                        break;
                    case SpecialType.System_UInt16:
                        mem = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Convert__ToUInt16Double);
                        break;
                    case SpecialType.System_Int64:
                        mem = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Convert__ToInt64Double);
                        break;
                    case SpecialType.System_UInt64:
                        mem = Compilation.GetWellKnownTypeMember(WellKnownMember.System_Convert__ToUInt64Double);
                        break;
                    case SpecialType.System_Int32:
                    default:
                        break;
                }

                var args = new List<BoundExpression>();
                args.Add(expression);
                return new BoundCall(expression.Syntax, expression, (MethodSymbol)mem,
                    args.ToImmutableArray(), default(ImmutableArray<string>), default(ImmutableArray<RefKind>), false, false,
                    false, default(ImmutableArray<int>), LookupResultKind.Viable, null, targetType);
            }
            else
            {
                // call (type) Expression to truncate the result
                return CreateConversion(expression.Syntax, expression, conversion, false, targetType, diagnostics);
            }

        }
    }
}

