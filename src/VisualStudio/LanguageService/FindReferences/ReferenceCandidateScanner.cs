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
    }
}
