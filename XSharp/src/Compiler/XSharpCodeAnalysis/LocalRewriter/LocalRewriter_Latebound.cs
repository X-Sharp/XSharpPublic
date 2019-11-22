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
using Microsoft.CodeAnalysis.PooledObjects;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal sealed partial class LocalRewriter
    {
        public BoundExpression MakeVODynamicGetMember(BoundExpression loweredReceiver, string name)
        {
            var usualType = _compilation.UsualType();
            if (((NamedTypeSymbol)loweredReceiver.Type).ConstructedFrom == usualType)
                loweredReceiver = _factory.StaticCall(usualType, ReservedNames.ToObject, loweredReceiver);
            return _factory.StaticCall(_compilation.RuntimeFunctionsType(), ReservedNames.IVarGet,
                MakeConversionNode(loweredReceiver, _compilation.GetSpecialType(SpecialType.System_Object), false),
                new BoundLiteral(loweredReceiver.Syntax, ConstantValue.Create(name), _compilation.GetSpecialType(SpecialType.System_String)));
        }

        public BoundExpression MakeVODynamicSetMember(BoundExpression loweredReceiver, string name, BoundExpression loweredValue)
        {
            var usualType = _compilation.UsualType();
            if (((NamedTypeSymbol)loweredReceiver.Type).ConstructedFrom == usualType)
                loweredReceiver = _factory.StaticCall(usualType, ReservedNames.ToObject, loweredReceiver);
            return _factory.StaticCall(_compilation.RuntimeFunctionsType(), ReservedNames.IVarPut,
                MakeConversionNode(loweredReceiver, _compilation.GetSpecialType(SpecialType.System_Object), false),
                new BoundLiteral(loweredReceiver.Syntax, ConstantValue.Create(name), _compilation.GetSpecialType(SpecialType.System_String)),
                loweredValue.Type == null ? new BoundDefaultExpression(loweredValue.Syntax, usualType)
                : MakeConversionNode(loweredValue, usualType, false));
        }

        public BoundExpression MakeVODynamicInvokeMember(BoundExpression loweredReceiver, string name,BoundDynamicInvocation node, ImmutableArray<BoundExpression> args)           
        {
            if (loweredReceiver.Type == _compilation.ArrayType())
            {
                if (_compilation.Options.Dialect.AllowASend())
                { 
                    
                    return MakeASend(loweredReceiver, name, args);
                }
                // This should not happen because we are not converting the method call to a dynamic call, but better safe than sorry.
                return null;
            }
            // for a method call the hierarchy is:
            // loweredReceiver = object
            // loweredReceiver.Parent = MemberAccess
            // loweredReceiver.Parent.Parent = InvocationExpression
            // loweredReceiver.Parent.Parent.Syntax.XNode = MethodCallContext
            //
            var parent = loweredReceiver.Syntax?.Parent?.Parent;
            var xnode = parent.XNode as XSharpParser.MethodCallContext;
            if (xnode != null && xnode.HasRefArguments)
            {
                return RewriteLateBoundCallWithRefParams(loweredReceiver, name, node, args);
            }

            var convArgs = new ArrayBuilder<BoundExpression>();
            var usualType = _compilation.UsualType();
            foreach (var a in args)
            {
                
                if (a.Type == null && ! a.Syntax.XIsCodeBlock)
                    convArgs.Add(new BoundDefaultExpression(a.Syntax, usualType));
                else
                   convArgs.Add(MakeConversionNode(a, usualType, false));
            }
            var aArgs = _factory.Array(usualType, convArgs.ToImmutableAndFree());
            // Note: Make sure the first parameter in __InternalSend() in the runtime is a USUAL!
            return _factory.StaticCall(_compilation.RuntimeFunctionsType(), ReservedNames.InternalSend,
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
                    convArgs.Add(new BoundDefaultExpression(a.Syntax, usualType));
                else
                    convArgs.Add(MakeConversionNode(a, usualType, false));
            }
            var aArgs = _factory.Array(usualType, convArgs.ToImmutableAndFree());
            var expr = _factory.StaticCall(_compilation.RuntimeFunctionsType(), ReservedNames.ASend,
                    MakeConversionNode(loweredReceiver, arrayType, false),
                    new BoundLiteral(loweredReceiver.Syntax, ConstantValue.Create(name), _compilation.GetSpecialType(SpecialType.System_String)),
                    aArgs);
            _diagnostics.Add(new CSDiagnostic(new CSDiagnosticInfo(ErrorCode.WRN_ASend, name), loweredReceiver.Syntax.Location));
            return expr;
        }

    }
}
