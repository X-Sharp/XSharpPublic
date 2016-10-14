// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;
namespace Microsoft.CodeAnalysis.CSharp
{
    internal sealed partial class LocalRewriter
    {
        public BoundExpression MakeVODynamicGetMember(BoundExpression loweredReceiver, string name)
        {
            if (((NamedTypeSymbol)loweredReceiver.Type).ConstructedFrom == _compilation.GetWellKnownType(WellKnownType.Vulcan___Usual))
                loweredReceiver = _factory.StaticCall(_compilation.GetWellKnownType(WellKnownType.Vulcan___Usual), "ToObject", loweredReceiver);
            return _factory.StaticCall(_compilation.GetWellKnownType(WellKnownType.VulcanRTFuncs_Functions), "IVarGet",
                MakeConversion(loweredReceiver, _compilation.GetSpecialType(SpecialType.System_Object), false),
                new BoundLiteral(loweredReceiver.Syntax, ConstantValue.Create(name), _compilation.GetSpecialType(SpecialType.System_String)));
        }

        public BoundExpression MakeVODynamicSetMember(BoundExpression loweredReceiver, string name, BoundExpression loweredValue)
        {
            if (((NamedTypeSymbol)loweredReceiver.Type).ConstructedFrom == _compilation.GetWellKnownType(WellKnownType.Vulcan___Usual))
                loweredReceiver = _factory.StaticCall(_compilation.GetWellKnownType(WellKnownType.Vulcan___Usual), "ToObject", loweredReceiver);
            return _factory.StaticCall(_compilation.GetWellKnownType(WellKnownType.VulcanRTFuncs_Functions), "IVarPut",
                MakeConversion(loweredReceiver, _compilation.GetSpecialType(SpecialType.System_Object), false),
                new BoundLiteral(loweredReceiver.Syntax, ConstantValue.Create(name), _compilation.GetSpecialType(SpecialType.System_String)),
                loweredValue.Type == null ? new BoundDefaultOperator(loweredValue.Syntax, _compilation.GetWellKnownType(WellKnownType.Vulcan___Usual))
                : MakeConversion(loweredValue, _compilation.GetWellKnownType(WellKnownType.Vulcan___Usual), false));
        }

        public BoundExpression MakeVODynamicInvokeMember(BoundExpression loweredReceiver, string name, ImmutableArray<BoundExpression> args)
        {
            var convArgs = new ArrayBuilder<BoundExpression>();
            foreach (var a in args)
            {
                if (a.Type == null)
                    convArgs.Add(new BoundDefaultOperator(a.Syntax, _compilation.GetWellKnownType(WellKnownType.Vulcan___Usual)));
                else
                    convArgs.Add(MakeConversion(a, _compilation.GetWellKnownType(WellKnownType.Vulcan___Usual), false));
            }
            var aArgs = _factory.Array(_compilation.GetWellKnownType(WellKnownType.Vulcan___Usual), convArgs.ToArrayAndFree());
            return _factory.StaticCall(_compilation.GetWellKnownType(WellKnownType.VulcanRTFuncs_Functions), "__InternalSend",
                    MakeConversion(loweredReceiver, _compilation.GetWellKnownType(WellKnownType.Vulcan___Usual), false),
                    new BoundLiteral(loweredReceiver.Syntax, ConstantValue.Create(name), _compilation.GetSpecialType(SpecialType.System_String)),
                    aArgs);
        }
    }
}