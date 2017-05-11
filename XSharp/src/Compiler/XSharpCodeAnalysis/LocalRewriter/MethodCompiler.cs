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

        /*
    internal static MethodSymbol UpdateGeneratedMethod(MethodSymbol method, DiagnosticBag diagnostics)
    {
        var original = method as SourceConstructorSymbol;
        var container = original.ContainingSymbol as SourceMemberContainerTypeSymbol;
        var syntax = original.SyntaxNode as ConstructorDeclarationSyntax;
        var baseType = method.ContainingType.BaseType;
        var members = baseType.GetMembers(".ctor");
        if (members.Length >= 1)
        {
            var baseconstructor = members[0] as MethodSymbol;
            if (baseconstructor.ParameterCount != 0)
            {
                var count = baseconstructor.ParameterCount;
                ParameterListSyntax parameterlist = syntax.ParameterList;
                ConstructorInitializerSyntax initializer = syntax.Initializer;
                var parambuilder = SeparatedSyntaxListBuilder<ParameterSyntax>.Create();
                var argbuilder = SeparatedSyntaxListBuilder<ArgumentSyntax>.Create();
                for (int i = 0; i < count; i++)
                {
                    if (i > 0)
                    {
                        parambuilder.AddSeparator(SyntaxFactory.Token(SyntaxKind.CommaToken));
                        argbuilder.AddSeparator(SyntaxFactory.Token(SyntaxKind.CommaToken));
                    }
                    var param = baseconstructor.Parameters[i];
                    var typesym = param.Type;
                    var typesyntax = SyntaxFactory.ParseTypeName(typesym.Name);
                    var id = SyntaxFactory.Identifier(param.Name);
                    var par = SyntaxFactory.Parameter(id);
                    par = par.Update(par.AttributeLists, par.Modifiers, typesyntax, id, par.Default);
                    parambuilder.Add(par);
                    var arg = SyntaxFactory.Argument(SyntaxFactory.IdentifierName(param.Name));
                    argbuilder.Add(arg);
                }
                parameterlist = parameterlist.Update(
                    parameterlist.OpenParenToken,
                    parambuilder.ToList(),
                    parameterlist.CloseParenToken);
                var argumentlist = initializer.ArgumentList;
                argumentlist = argumentlist.Update(argumentlist.OpenParenToken,argbuilder.ToList(),argumentlist.CloseParenToken);
                initializer = initializer.Update(
                    initializer.ColonToken,
                    initializer.ThisOrBaseKeyword,
                    argumentlist);
                // we must adjust the parameters for the declaration
                // and also the chained call
                syntax = syntax.Update(
                    syntax.AttributeLists, 
                    syntax.Modifiers,
                    syntax.Identifier, 
                    parameterlist, 
                    initializer, 
                    syntax.Body, 
                    syntax.SemicolonToken
                    );
                //method = SourceConstructorSymbol.CreateConstructorSymbol(container, syntax, diagnostics);
            }

        }
        return method;
    }
        */

    }
    internal static class XsMethodExtensions
    {
        internal static bool IsGeneratedConstructor(this MethodSymbol method)
        {
            var scs = method as SourceConstructorSymbol;
            if (scs == null)
                return false;
            var node = scs.SyntaxNode as CSharpSyntaxNode;
            if (node == null)
                return false;
            //return node.XGenerated;
            return false;

        }
    }

}