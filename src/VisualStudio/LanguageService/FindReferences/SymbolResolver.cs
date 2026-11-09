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

        /// <summary>
        /// The lookup resolves the name in <c>Foo{...}</c> to the type. A creation calls a constructor: return the
        /// constructors of the type, or the type when it declares none.
        /// </summary>
        internal static IList<IXSymbol> ForCreation(IList<IXSymbol> symbols, IList<IToken> tokens, IToken name)
        {
            if (name == null || tokens == null || !(symbols.FirstOrDefault() is IXTypeSymbol type))
                return symbols;
            var index = name.TokenIndex >= 0 && name.TokenIndex < tokens.Count && tokens[name.TokenIndex] == name
                ? name.TokenIndex
                : tokens.IndexOf(name);
            // Foo{...} and Foo<T>{...}
            var open = OverloadFilter.ListAfterName(tokens, index);
            if (open < 0 || tokens[open].Type != XSharpLexer.LCURLY)
                return symbols;
            var ctors = type.GetConstructors();
            return ctors.Length == 0 ? symbols : ctors.Cast<IXSymbol>().ToList();
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
