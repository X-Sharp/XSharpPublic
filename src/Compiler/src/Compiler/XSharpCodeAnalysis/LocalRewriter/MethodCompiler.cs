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

        internal void CheckFoxProClass(NamedTypeSymbol containingType)
        {
            if (containingType.DeclaringCompilation.Options.Dialect == XSharpDialect.FoxPro)
            {
                if (containingType.GetNonNullSyntaxNode()?.XNode?.GetChild(0) is XSharpParser.FoxclassContext)
                {
                    if (!containingType.InheritsFromVfpCustom())
                    {
                        //ERR_FoxDefineClassMustInheritFromCustom
                        var info = new CSDiagnosticInfo(ErrorCode.ERR_FoxDefineClassMustInheritFromCustom);
                        _diagnostics.Add(new CSDiagnostic(info, containingType.Locations[0]));
                    }
                }
            }
        }

        internal static BoundStatement RewriteXSharpMethod(MethodSymbol method,
            BoundStatement body,
            int methodOrdinal,
            TypeCompilationState compilationState,
            BindingDiagnosticBag diagnostics)
        {
            var methodNode = method.GetNonNullSyntaxNode();
            if (!methodNode.XGenerated)
            {
                foreach (var par in method.Parameters)
                {
                    par.ValidateDefaultParameter(diagnostics);
                }
            }
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
                    body = LocalRewriter.RewriteRunInitProc(method, body, diagnostics);
                    break;
                default:
                    break;
            }
            switch (method.MethodKind)
            {
                case MethodKind.PropertyGet:
                case MethodKind.PropertySet:
                    if (methodNode.XGenerated)
                    {
                        if (body is BoundBlock oldbody )
                        {
                            var newbody = new BoundBlock(oldbody.Syntax, oldbody.Locals,oldbody.Statements,oldbody.HasErrors) { WasCompilerGenerated = true };
                            body = newbody;
                        }
                    }
                    break;
            }

            var xnode = methodNode.XNode as XSharpParserRuleContext;
            if (xnode is XSharpParser.ClsmethodContext cmc)
            {
                xnode = cmc.Member;
            }
            else if (xnode is XSharpParser.FoxclsmethodContext fmc)
            {
                xnode = fmc.Member;
            }
            if (xnode is XSharpParser.IMemberContext iec)
            {
                 body = LocalRewriter.RemoveUnusedVars(iec.Data, body, diagnostics);
            }

            return body;
        }
    }
}
