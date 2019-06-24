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
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class LocalRewriter
    {

        internal BoundNode VisitCtorCallClipperConvention(BoundObjectCreationExpression node)
        {
            if (node.Arguments.Length > 0 && node.Syntax is ObjectCreationExpressionSyntax oces)
            {
                var xnode = oces.XNode as XSharpParser.CtorCallContext;
                if (xnode == null || !xnode.HasRefArguments)
                    return null;
                if (!node.Constructor.HasClipperCallingConvention())
                    return null;
                // not sure if we have to return a object creation expression, or if we can return
                // a method call expression that returns a new object of the right type.
                // need to try.
                // the idea is the same as for method calls.
            }
            
            return null;
        }
        internal BoundNode VisitCallClipperConvention(BoundCall node)
        {
            // some generated nodes do not have an invocation expression syntax node
            if (node.Arguments.Length > 0 && node.Syntax is InvocationExpressionSyntax ies)
            {
                var xnode = ies.XNode as XSharpParser.MethodCallContext;
                if (xnode == null || !xnode.HasRefArguments)
                    return null;
                if (!node.Method.HasClipperCallingConvention())
                    return null;
            }
            // create new method with parameter types that match the types of the arguments
            // return type of the method should be the same as the return type from the original method call.
            // and a 'ref' modifier or 'out' modifier should be set for parameters when an argument was passed by reference.
            // We can later call this method with the original arguments list.
            // inside the method create a parameter array from the passed parameters. Deref the arguments that were by ref.
            // if the returntype from the method is not void then also create a result variable for the return value
            // call the original method with the parameter array created before and assign the return value (if any)
            // foreach ref variable assign the value from the params array back into the (ref) usual
            // and finally return the returnvalue (or nothing)
            // then create a new BoundCall node based on the original node but replace the Method with the newly created method.
            return null;
        }
    }
}
