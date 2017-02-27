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
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeGen;
using Microsoft.CodeAnalysis.CSharp.Emit;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Emit;
using Roslyn.Utilities;
namespace Microsoft.CodeAnalysis.CSharp
{
    internal sealed partial class MethodCompiler 
    {
        internal static BoundStatement RewriteXSharpMethod(MethodSymbol method, 
            BoundStatement body, 
            int methodOrdinal,
            TypeCompilationState compilationState,
            DiagnosticBag diagnostics)
        {
            switch (method.Name)
            {
                case XSharpSpecialNames.AppInit:
                    body = LocalRewriter.RewriteAppInit(method, body, diagnostics);
                    break;
                case XSharpSpecialNames.AppExit:
                    body = LocalRewriter.RewriteAppExit(method, body, diagnostics);
                    break;
                case XSharpSpecialNames.ExitProc:
                    body = LocalRewriter.RewriteExit(method, body, diagnostics);
                    break;
                case VulcanFunctionNames.RunInitProcs:
                    body = LocalRewriter.RewriteRunInitProc(method,body,diagnostics);
                    break;
            }
            return body;

        }
    }

}