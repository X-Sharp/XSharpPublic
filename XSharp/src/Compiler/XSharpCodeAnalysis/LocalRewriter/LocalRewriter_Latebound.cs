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
            var usualType = _compilation.UsualType();
            if (((NamedTypeSymbol)loweredReceiver.Type).ConstructedFrom == usualType)
                loweredReceiver = _factory.StaticCall(usualType, XSharpFunctionNames.ToObject, loweredReceiver);
            return _factory.StaticCall(_compilation.RuntimeFunctionsType(), XSharpFunctionNames.IVarGet,
                MakeConversionNode(loweredReceiver, _compilation.GetSpecialType(SpecialType.System_Object), false),
                new BoundLiteral(loweredReceiver.Syntax, ConstantValue.Create(name), _compilation.GetSpecialType(SpecialType.System_String)));
        }

        public BoundExpression MakeVODynamicSetMember(BoundExpression loweredReceiver, string name, BoundExpression loweredValue)
        {
            var usualType = _compilation.UsualType();
            if (((NamedTypeSymbol)loweredReceiver.Type).ConstructedFrom == usualType)
                loweredReceiver = _factory.StaticCall(usualType, XSharpFunctionNames.ToObject, loweredReceiver);
            return _factory.StaticCall(_compilation.RuntimeFunctionsType(), XSharpFunctionNames.IVarPut,
                MakeConversionNode(loweredReceiver, _compilation.GetSpecialType(SpecialType.System_Object), false),
                new BoundLiteral(loweredReceiver.Syntax, ConstantValue.Create(name), _compilation.GetSpecialType(SpecialType.System_String)),
                loweredValue.Type == null ? new BoundDefaultOperator(loweredValue.Syntax, usualType)
                : MakeConversionNode(loweredValue, usualType, false));
        }

        public BoundExpression MakeVODynamicInvokeMember(BoundExpression loweredReceiver, string name, ImmutableArray<BoundExpression> args)
        {
            if (loweredReceiver.Type == _compilation.ArrayType())
            {
                return MakeASend(loweredReceiver, name, args);
            }
            var convArgs = new ArrayBuilder<BoundExpression>();
            var usualType = _compilation.UsualType();
            foreach (var a in args)
            {
                
                if (a.Type == null && ! a.Syntax.XIsCodeBlock)
                    convArgs.Add(new BoundDefaultOperator(a.Syntax, usualType));
                else
                   convArgs.Add(MakeConversionNode(a, usualType, false));
            }
            var aArgs = _factory.Array(usualType, convArgs.ToImmutableAndFree());
            return _factory.StaticCall(_compilation.RuntimeFunctionsType(), XSharpFunctionNames.Send,
                    MakeConversionNode(loweredReceiver, usualType, false),
                    new BoundLiteral(loweredReceiver.Syntax, ConstantValue.Create(name), _compilation.GetSpecialType(SpecialType.System_String)),
                    aArgs);
        }

        public BoundExpression MakeASend(BoundExpression loweredReceiver, string name, ImmutableArray<BoundExpression> args)
        {
            var convArgs = new ArrayBuilder<BoundExpression>();
            var usualType = _compilation.UsualType();
            var arrayType = _compilation.ArrayType();
            foreach (var a in args)
            {
                if (a.Type == null)
                    convArgs.Add(new BoundDefaultOperator(a.Syntax, usualType));
                else
                    convArgs.Add(MakeConversionNode(a, usualType, false));
            }
            var aArgs = _factory.Array(usualType, convArgs.ToImmutableAndFree());
            var expr = _factory.StaticCall(_compilation.RuntimeFunctionsType(), XSharpFunctionNames.ASend,
                    MakeConversionNode(loweredReceiver, arrayType, false),
                    new BoundLiteral(loweredReceiver.Syntax, ConstantValue.Create(name), _compilation.GetSpecialType(SpecialType.System_String)),
                    aArgs);
            return expr;
        }

    }
}