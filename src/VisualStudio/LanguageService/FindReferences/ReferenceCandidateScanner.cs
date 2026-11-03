//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;

namespace XSharp.LanguageService.FindReferences
{
    /// <summary>
    /// Finds the tokens in a file that may refer to a symbol, by name only.
    /// Comments, strings and code in inactive #ifdef blocks never match.
    /// </summary>
    internal static class ReferenceCandidateScanner
    {
        internal static bool MayContain(string text, string name)
        {
            return text.IndexOf(name, StringComparison.OrdinalIgnoreCase) >= 0;
        }

        internal static IList<IToken> FindIdentifierTokens(IList<IToken> tokens, string name)
        {
            var result = new List<IToken>();
            foreach (var token in tokens)
            {
                if (token.Type == XSharpLexer.ID &&
                    token.Channel == XSharpLexer.DefaultTokenChannel &&
                    string.Equals(token.Text, name, StringComparison.OrdinalIgnoreCase))
                {
                    result.Add(token);
                }
            }
            return result;
        }

        /// <summary>
        /// The tokens that may refer to a constructor of the type: creations <c>Foo{...}</c>, the chains
        /// <c>SUPER(...)</c> and <c>SELF(...)</c>, and the <c>CONSTRUCTOR</c> keyword of a declaration.
        /// </summary>
        internal static IList<IToken> FindConstructorTokens(IList<IToken> tokens, string typeName)
        {
            var result = new List<IToken>();
            for (int i = 0; i < tokens.Count; i++)
            {
                var token = tokens[i];
                if (token.Channel != XSharpLexer.DefaultTokenChannel)
                    continue;
                switch (token.Type)
                {
                    case XSharpLexer.CONSTRUCTOR:
                        result.Add(token);
                        break;
                    case XSharpLexer.SUPER:
                    case XSharpLexer.SELF:
                        if (NextType(tokens, i) == XSharpLexer.LPAREN)
                            result.Add(token);
                        break;
                    case XSharpLexer.ID:
                        if (NextType(tokens, i) == XSharpLexer.LCURLY &&
                            string.Equals(token.Text, typeName, StringComparison.OrdinalIgnoreCase))
                            result.Add(token);
                        break;
                }
            }
            return result;
        }

        private static int NextType(IList<IToken> tokens, int index)
        {
            for (int i = index + 1; i < tokens.Count; i++)
            {
                if (tokens[i].Channel == XSharpLexer.DefaultTokenChannel)
                    return tokens[i].Type;
            }
            return XSharpLexer.Eof;
        }
    }
}
