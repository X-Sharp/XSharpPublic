//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Generic;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using XSharpModel;

namespace XSharp.LanguageService.FindReferences
{
    /// <summary>
    /// Resolves the symbol at a search location. Shared by Go To Definition and Find References.
    /// The caller is responsible for ModelWalker.Suspend() / Resume().
    /// </summary>
    internal static class SymbolResolver
    {
        internal static IList<IXSymbol> ResolveAt(ref XSharpSearchLocation location, out CompletionState state, out IList<IToken> tokenList)
        {
            var result = new List<IXSymbol>();
            tokenList = XSharpTokenTools.GetTokensUnderCursor(location, out state);
            result.AddRange(XSharpLookup.RetrieveElement(location, tokenList, state));
            if (result.Count == 0 && tokenList.Count > 1)
            {
                // try again with just the last element in the list
                var token = tokenList[tokenList.Count - 1];
                if (token.Type == XSharpLexer.ID)
                {
                    tokenList.Clear();
                    tokenList.Add(token);
                    location = location.With(location.CurrentNamespace);
                    result.AddRange(XSharpLookup.RetrieveElement(location, tokenList, state));
                }
            }
            return result;
        }
    }
}
