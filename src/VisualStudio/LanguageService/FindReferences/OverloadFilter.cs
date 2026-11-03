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
    /// The lookup returns every overload of a name. Find References narrows them down by the number of
    /// arguments at the call site; argument types are not compared.
    /// </summary>
    internal static class OverloadFilter
    {
        /// <summary>The number of arguments when the name is followed by an argument list, otherwise null.</summary>
        internal static int? ArgumentCount(IList<IToken> tokens, IToken name)
        {
            var index = name.TokenIndex >= 0 && name.TokenIndex < tokens.Count && tokens[name.TokenIndex] == name
                ? name.TokenIndex
                : tokens.IndexOf(name);
            if (index < 0)
                return null;
            int depth = 0;
            int commas = 0;
            bool hasContent = false;
            for (int i = index + 1; i < tokens.Count; i++)
            {
                var token = tokens[i];
                if (token.Channel != XSharpLexer.DefaultTokenChannel)
                    continue;
                if (depth == 0)
                {
                    if (token.Type != XSharpLexer.LPAREN)
                        return null;
                    depth = 1;
                    continue;
                }
                switch (token.Type)
                {
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LBRKT:
                    case XSharpLexer.LCURLY:
                        depth++;
                        hasContent = true;
                        break;
                    case XSharpLexer.RPAREN:
                    case XSharpLexer.RBRKT:
                    case XSharpLexer.RCURLY:
                        depth--;
                        if (depth == 0)
                            return hasContent || commas > 0 ? commas + 1 : 0;
                        break;
                    case XSharpLexer.COMMA:
                        if (depth == 1)
                            commas++;
                        else
                            hasContent = true;
                        break;
                    case XSharpLexer.EOS:
                    case XSharpLexer.Eof:
                        return null;
                    default:
                        hasContent = true;
                        break;
                }
            }
            return null;
        }

        /// <summary>The argument count for the identifier at (or just before) the caret position.</summary>
        internal static int? ArgumentCountAt(IList<IToken> tokens, int position)
        {
            var name = tokens?.FirstOrDefault(t => t.Type == XSharpLexer.ID && t.StartIndex <= position && position <= t.StopIndex + 1);
            return name == null ? null : ArgumentCount(tokens, name);
        }

        /// <summary>Can the member be called with this number of arguments?</summary>
        internal static bool Accepts(IXMemberSymbol member, int argumentCount)
        {
            // CLIPPER calling convention: any number of arguments
            if (member.CallingConvention == CallingConvention.Clipper)
                return true;
            var parameters = member.Parameters ?? new IXParameterSymbol[0];
            int total = parameters.Count;
            int required = parameters.Count(p => p.ParamType != ParamType.Params && string.IsNullOrEmpty(p.Value));
            bool hasParamArray = parameters.Any(p => p.ParamType == ParamType.Params);
            if (Fits(argumentCount, required, total, hasParamArray))
                return true;
            // an extension method can also be called with the instance as first argument
            return member.IsExtension && Fits(argumentCount, required - 1, total - 1, hasParamArray);
        }

        private static bool Fits(int count, int required, int total, bool hasParamArray)
        {
            return count >= required && (hasParamArray || count <= total);
        }

        /// <summary>
        /// Keeps the members that accept the number of arguments. Without a count, or when no member accepts it
        /// (for example when the parameters are unknown), all symbols are kept.
        /// </summary>
        internal static IList<IXSymbol> Filter(IList<IXSymbol> symbols, int? argumentCount)
        {
            if (argumentCount == null || symbols.Count < 2)
                return symbols;
            var kept = symbols.Where(s => !(s is IXMemberSymbol member) || Accepts(member, argumentCount.Value)).ToList();
            return kept.Count == 0 ? symbols : kept;
        }
    }
}
