//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Generic;
using System.Linq;
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

        /// <summary>The declared type of the local, parameter, field or property that the identifier names, otherwise null.</summary>
        internal static string TypeOf(XSharpSearchLocation context, IToken identifier)
        {
            var location = context.With(identifier.Line - 1, identifier.StartIndex);
            var symbol = ResolveAt(ref location, out _, out _).FirstOrDefault();
            if (symbol is IXVariableSymbol || (symbol is IXMemberSymbol && (symbol.Kind.IsField() || symbol.Kind.IsProperty())))
                return symbol.TypeName;
            return null;
        }
    }
}
