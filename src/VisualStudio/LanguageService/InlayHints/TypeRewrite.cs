//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;

namespace XSharp.LanguageService.InlayHints
{
    /// <summary>
    /// The double-click on a type hint: writes the inferred type into the code. X# puts the type after the
    /// initializer (<c>LOCAL x := e AS T</c>), <c>LOCAL x AS T := e</c> does not parse.
    /// </summary>
    internal static class TypeRewrite
    {
        /// <summary>The edit, or null when the declaration has no readable explicit form.</summary>
        internal static TypeEdit For(DeclarationCandidate decl, IList<IToken> tokens, string typeName, Func<string, string> keyword)
        {
            if (decl == null || string.IsNullOrEmpty(typeName) || decl.DeclaratorCount != 1)
                return null;
            var code = tokens.Where(t => t.Channel == XSharpLexer.DefaultTokenChannel).ToList();
            var nameIndex = code.IndexOf(decl.Name);
            if (nameIndex < 1)
                return null;
            // the VAR or IMPLIED right before the name
            var implied = code[nameIndex - 1];
            if (implied.Type != XSharpLexer.VAR && implied.Type != XSharpLexer.IMPLIED)
                return null;
            var asType = " " + keyword("AS") + " " + typeName;
            switch (decl.Kind)
            {
                case ImpliedDeclarationKind.Var:
                case ImpliedDeclarationKind.LocalImplied:
                    if (decl.Keyword.Type == XSharpLexer.STATIC || decl.Initializer == null || decl.Initializer.Count == 0)
                        return null;
                    var last = decl.Initializer[decl.Initializer.Count - 1];
                    return new TypeEdit
                    {
                        Changes = new List<(int, int, string)>
                        {
                            (decl.Keyword.StartIndex, implied.StopIndex + 1 - decl.Keyword.StartIndex, keyword("LOCAL")),
                            (last.StopIndex + 1, 0, asType)
                        }
                    };
                case ImpliedDeclarationKind.ForEach:
                    return new TypeEdit
                    {
                        Changes = new List<(int, int, string)>
                        {
                            (implied.StartIndex, decl.Name.StopIndex + 1 - implied.StartIndex, decl.Name.Text + asType)
                        }
                    };
                default:
                    // FOR LOCAL i := 1 AS INT TO n is valid but hardly readable; USING has no LOCAL ... AS form
                    return null;
            }
        }
    }
}
