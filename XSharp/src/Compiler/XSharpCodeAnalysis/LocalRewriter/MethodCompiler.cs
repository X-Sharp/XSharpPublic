//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Symbols;

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
                case ReservedNames.RunInitProcs:
                    body = LocalRewriter.RewriteRunInitProc(method,body,diagnostics);
                    break;
            }
            switch (method.MethodKind)
            {
                case MethodKind.PropertyGet:
                case MethodKind.PropertySet:
                    var node = method.GetNonNullSyntaxNode();
                    if (node.XGenerated)
                    {
                        if (body is BoundBlock oldbody )
                        {
                            var newbody = new BoundBlock(oldbody.Syntax, oldbody.Locals,oldbody.Statements,oldbody.HasErrors) { WasCompilerGenerated = true };
                            body = newbody;
                        }
                    }
                    break;
            }

            var xnode = method.GetNonNullSyntaxNode().XNode as XSharpParserRuleContext;
            if (xnode is XSharpParser.ClsmethodContext cmc)
            {
                xnode = cmc.Member;
            }
            else if (xnode is XSharpParser.FoxclsmethodContext fmc)
            {
                xnode = fmc.Member;
            }
            if (xnode is XSharpParser.IEntityContext iec)
            {
                 body = LocalRewriter.RemoveUnusedVars(iec.Data, body, diagnostics);
            }

            return body;
        }
    }
}
