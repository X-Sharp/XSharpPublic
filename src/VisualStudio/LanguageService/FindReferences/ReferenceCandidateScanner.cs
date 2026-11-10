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
        /// <summary>
        /// The name as a whole word (case-insensitive) in comments, string literals or excluded #ifdef code. In an
        /// interpolated string only the text outside its expressions counts: those are code.
        /// </summary>
        internal static IList<TextOccurrence> FindWordInText(IList<IToken> tokens, string name, TextTokenKinds kinds)
        {
            var result = new List<TextOccurrence>();
            if (kinds == TextTokenKinds.None || string.IsNullOrEmpty(name))
                return result;
            foreach (var token in tokens)
            {
                var kind = TextKindOf(token);
                if ((kind & kinds) == 0)
                    continue;
                var text = token.Text;
                var holes = token.Type == XSharpLexer.INTERPOLATED_STRING_CONST
                    ? InterpolationHoles(text, false)
                    : Array.Empty<(int Start, int Length)>();
                for (int pos = text.IndexOf(name, StringComparison.OrdinalIgnoreCase); pos >= 0;
                    pos = text.IndexOf(name, pos + name.Length, StringComparison.OrdinalIgnoreCase))
                {
                    if (!IsWordAt(text, pos, name.Length))
                        continue;
                    bool inHole = false;
                    foreach (var (start, length) in holes)
                        inHole |= pos >= start && pos < start + length;
                    if (!inHole)
                        result.Add(new TextOccurrence(token, token.StartIndex + pos, name.Length, kind));
                }
            }
            return result;
        }

        private static TextTokenKinds TextKindOf(IToken token)
        {
            if (token.Channel == XSharpLexer.DEFOUTCHANNEL)
                return TextTokenKinds.InactiveCode;
            switch (token.Type)
            {
                case XSharpLexer.SL_COMMENT:
                case XSharpLexer.ML_COMMENT:
                case XSharpLexer.DOC_COMMENT:
                    return TextTokenKinds.Comments;
                case XSharpLexer.STRING_CONST:
                case XSharpLexer.ESCAPED_STRING_CONST:
                case XSharpLexer.BRACKETED_STRING_CONST:
                case XSharpLexer.TEXT_STRING_CONST:
                case XSharpLexer.CHAR_CONST:
                case XSharpLexer.INTERPOLATED_STRING_CONST:
                    return TextTokenKinds.Strings;
            }
            return TextTokenKinds.None;
        }

        private static bool IsWordAt(string text, int start, int length)
        {
            bool IsWordChar(char c) => char.IsLetterOrDigit(c) || c == '_';
            return (start == 0 || !IsWordChar(text[start - 1])) &&
                (start + length >= text.Length || !IsWordChar(text[start + length]));
        }

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
        /// The tokens that may refer to a constructor of the type: creations <c>Foo{...}</c> and <c>Foo&lt;T&gt;{...}</c>, the chains
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
                        // Foo{...} and Foo<T>{...}
                        if (string.Equals(token.Text, typeName, StringComparison.OrdinalIgnoreCase))
                        {
                            var open = OverloadFilter.ListAfterName(tokens, i);
                            if (open >= 0 && tokens[open].Type == XSharpLexer.LCURLY)
                                result.Add(token);
                        }
                        break;
                }
            }
            return result;
        }

        /// <summary>
        /// The expressions of an interpolated string token (<c>i"..."</c>, <c>ie"..."</c>, <c>ei"..."</c>), as
        /// offsets into its text, without their format (<c>{x::N2}</c>, with /allowdot also <c>{x:N2}</c>).
        /// Follows CreateInterPolatedStringExpression of the compiler; <c>{{</c> and <c>}}</c> are no expressions.
        /// </summary>
        internal static IList<(int Start, int Length)> InterpolationHoles(string text, bool allowDot)
        {
            var result = new List<(int Start, int Length)>();
            var open = text.IndexOf('"');
            if (open < 0 || text.Length < open + 2)
                return result;
            bool extended = open == 2;
            int end = text.Length - 1;
            int nestLevel = 0;
            int holeStart = 0;
            bool inString = false;
            bool afterBackSlash = false;
            for (int i = open + 1; i < end; i++)
            {
                var c = text[i];
                var next = i + 1 < end ? text[i + 1] : '\0';
                if (nestLevel > 0 && extended)
                {
                    // a string inside an expression may contain braces
                    if (c == '\\')
                    {
                        afterBackSlash = !afterBackSlash;
                        continue;
                    }
                    if (c == '"' && !afterBackSlash)
                        inString = !inString;
                    afterBackSlash = false;
                    if (inString)
                        continue;
                }
                if (c == '{')
                {
                    if (nestLevel == 0 && next == '{')
                        i++;
                    else if (nestLevel++ == 0)
                        holeStart = i + 1;
                }
                else if (c == '}')
                {
                    if (nestLevel == 0)
                    {
                        if (next == '}')
                            i++;
                    }
                    else if (--nestLevel == 0)
                    {
                        var expression = text.Substring(holeStart, i - holeStart);
                        var format = expression.IndexOf("::", StringComparison.Ordinal);
                        if (format < 0 && allowDot)
                            format = expression.IndexOf(':');
                        var length = format > 0 ? format : expression.Length;
                        if (length > 0)
                            result.Add((holeStart, length));
                    }
                }
            }
            return result;
        }

        /// <summary>
        /// The names inside the expressions of interpolated strings. Each expression is lexed with
        /// <paramref name="lex"/> and its tokens are moved to their place in the file.
        /// </summary>
        internal static IList<InterpolatedCandidate> FindInterpolatedTokens(IList<IToken> tokens, string name,
            Func<string, IList<IToken>> lex, bool allowDot)
        {
            var result = new List<InterpolatedCandidate>();
            foreach (var token in tokens)
            {
                if (token.Type != XSharpLexer.INTERPOLATED_STRING_CONST || token.Channel != XSharpLexer.DefaultTokenChannel)
                    continue;
                var text = token.Text;
                if (text.IndexOf(name, StringComparison.OrdinalIgnoreCase) < 0)
                    continue;
                foreach (var (start, length) in InterpolationHoles(text, allowDot))
                {
                    var expression = text.Substring(start, length);
                    if (expression.IndexOf(name, StringComparison.OrdinalIgnoreCase) < 0)
                        continue;
                    var holeTokens = new List<IToken>();
                    foreach (var t in lex(expression))
                    {
                        if (t.Type == XSharpLexer.Eof || t.Type == XSharpLexer.EOS || !(t is CommonToken moved))
                            continue;
                        // freeze the text: a CommonToken reads it from its input at StartIndex..StopIndex
                        moved.Text = t.Text;
                        moved.StartIndex = token.StartIndex + start + t.StartIndex;
                        moved.StopIndex = token.StartIndex + start + t.StopIndex;
                        moved.Line = token.Line;
                        moved.Column = token.Column + start + t.Column;
                        moved.TokenIndex = -1;
                        holeTokens.Add(moved);
                    }
                    foreach (var t in FindIdentifierTokens(holeTokens, name))
                        result.Add(new InterpolatedCandidate { Token = t, String = token, Tokens = holeTokens });
                }
            }
            return result;
        }

        /// <summary>The tokens of the file with the string replaced by the expression of the candidate.</summary>
        internal static IList<IToken> Splice(IList<IToken> tokens, InterpolatedCandidate candidate)
        {
            var result = new List<IToken>(tokens.Count + candidate.Tokens.Count);
            foreach (var token in tokens)
            {
                if (token == candidate.String)
                    result.AddRange(candidate.Tokens);
                else
                    result.Add(token);
            }
            return result;
        }

        /// <summary>Is the name sent to an object: <c>o:Name</c>?</summary>
        internal static bool IsSend(IList<IToken> tokens, IToken name)
        {
            var index = name.TokenIndex >= 0 && name.TokenIndex < tokens.Count && tokens[name.TokenIndex] == name
                ? name.TokenIndex
                : tokens.IndexOf(name);
            for (int i = index - 1; i >= 0; i--)
            {
                if (tokens[i].Channel == XSharpLexer.DefaultTokenChannel)
                    return tokens[i].Type == XSharpLexer.COLON;
            }
            return false;
        }

        /// <summary>The symbol literals <c>#Name</c> of the name, as used by Send() and IVarGet().</summary>
        internal static IList<IToken> FindSymbolTokens(IList<IToken> tokens, string name)
        {
            var result = new List<IToken>();
            foreach (var token in tokens)
            {
                if (token.Type == XSharpLexer.SYMBOL_CONST &&
                    token.Channel == XSharpLexer.DefaultTokenChannel &&
                    string.Equals(token.Text.TrimStart('#'), name, StringComparison.OrdinalIgnoreCase))
                {
                    result.Add(token);
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

    /// <summary>A name inside an expression of an interpolated string.</summary>
    internal sealed class InterpolatedCandidate
    {
        /// <summary>The name, at its place in the file.</summary>
        internal IToken Token { get; set; }
        /// <summary>The INTERPOLATED_STRING_CONST token that contains it.</summary>
        internal IToken String { get; set; }
        /// <summary>The tokens of the expression around the name, at their place in the file.</summary>
        internal IList<IToken> Tokens { get; set; }
    }

    /// <summary>Where <see cref="ReferenceCandidateScanner.FindWordInText"/> looks for a name.</summary>
    [Flags]
    internal enum TextTokenKinds
    {
        None = 0,
        Comments = 1,
        Strings = 2,
        /// <summary>Code in an excluded #ifdef block</summary>
        InactiveCode = 4
    }

    /// <summary>The name as a whole word inside a comment, a string or inactive code.</summary>
    internal readonly struct TextOccurrence
    {
        internal TextOccurrence(IToken token, int start, int length, TextTokenKinds kind)
        {
            Token = token;
            Start = start;
            Length = length;
            Kind = kind;
        }
        internal IToken Token { get; }
        /// <summary>Position in the file text</summary>
        internal int Start { get; }
        internal int Length { get; }
        internal TextTokenKinds Kind { get; }
    }
}
