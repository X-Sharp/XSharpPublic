//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Generic;
using System.Linq;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using XSharp.LanguageService.FindReferences;

namespace XSharp.LanguageService.InlayHints
{
    /// <summary>A call with arguments: <c>Name(...)</c> or a creation <c>Name{...}</c>.</summary>
    internal sealed class CallCandidate
    {
        internal IToken Name { get; set; }
        internal bool IsCreation { get; set; }
        /// <summary>Called on an instance (<c>o:Name(...)</c>, <c>o.Name(...)</c>): an extension method gets no argument for its first parameter.</summary>
        internal bool IsMemberAccess { get; set; }
        /// <summary>The tokens of each argument; an omitted argument is an empty list.</summary>
        internal IList<IList<IToken>> Arguments { get; set; }
    }

    internal enum ImpliedDeclarationKind
    {
        Var,
        LocalImplied,
        ForEach,
        For,
        Using
    }

    /// <summary>A variable whose type the compiler infers: <c>VAR x := e</c>, <c>FOREACH VAR x IN c</c>, ...</summary>
    internal sealed class DeclarationCandidate
    {
        internal ImpliedDeclarationKind Kind { get; set; }
        internal IToken Name { get; set; }
        /// <summary>The expression after <c>:=</c>; for FOREACH the container after IN.</summary>
        internal IList<IToken> Initializer { get; set; }
        /// <summary>The first token of the declaration (VAR, LOCAL, IMPLIED, FOREACH, FOR, USING, STATIC).</summary>
        internal IToken Keyword { get; set; }
        /// <summary>The number of variables declared by the statement (<c>VAR a := 1, b := 2</c> is 2).</summary>
        internal int DeclaratorCount { get; set; }
    }

    /// <summary>Finds the places where an inlay hint may go, on lexer tokens only.</summary>
    internal static class HintCandidates
    {
        internal static IReadOnlyList<CallCandidate> Calls(IList<IToken> tokens)
        {
            var result = new List<CallCandidate>();
            var code = DefaultChannel(tokens);
            for (int i = 0; i + 1 < code.Count; i++)
            {
                var name = code[i];
                if (!IsName(name.Type))
                    continue;
                var next = code[i + 1].Type;
                if (next != XSharpLexer.LPAREN && next != XSharpLexer.LCURLY)
                    continue;
                if (i > 0 && IsDeclarationKeyword(code[i - 1].Type))
                    continue;
                // by position: looking the name up in the list made a large file quadratic
                var arguments = OverloadFilter.Arguments(code, i);
                if (arguments == null || arguments.Count == 0)
                    continue;
                var before = i > 0 ? code[i - 1].Type : XSharpLexer.EOS;
                result.Add(new CallCandidate
                {
                    Name = name,
                    IsCreation = next == XSharpLexer.LCURLY,
                    IsMemberAccess = before == XSharpLexer.COLON || before == XSharpLexer.DOT,
                    Arguments = arguments
                });
            }
            return result;
        }

        internal static IReadOnlyList<DeclarationCandidate> Declarations(IList<IToken> tokens)
        {
            var result = new List<DeclarationCandidate>();
            var code = DefaultChannel(tokens);
            for (int i = 0; i < code.Count; i++)
            {
                var type = code[i].Type;
                if (type != XSharpLexer.VAR && type != XSharpLexer.IMPLIED)
                    continue;
                // LOCAL IMPLIED: the IMPLIED was handled with the LOCAL
                var start = i;
                if (type == XSharpLexer.IMPLIED && i > 0 && code[i - 1].Type == XSharpLexer.LOCAL)
                    start = i - 1;
                var prev = start > 0 ? code[start - 1].Type : XSharpLexer.EOS;
                ImpliedDeclarationKind kind;
                IToken keyword = code[start];
                if (prev == XSharpLexer.FOR)
                {
                    kind = ImpliedDeclarationKind.For;
                    keyword = code[start - 1];
                }
                else if (prev == XSharpLexer.FOREACH)
                {
                    kind = ImpliedDeclarationKind.ForEach;
                    keyword = code[start - 1];
                }
                else if (prev == XSharpLexer.USING || (prev == XSharpLexer.STATIC && start > 1 && code[start - 2].Type == XSharpLexer.USING))
                {
                    kind = ImpliedDeclarationKind.Using;
                    keyword = code[prev == XSharpLexer.USING ? start - 1 : start - 2];
                }
                else if (prev == XSharpLexer.EOS || prev == XSharpLexer.STATIC)
                {
                    kind = type == XSharpLexer.VAR ? ImpliedDeclarationKind.Var : ImpliedDeclarationKind.LocalImplied;
                    if (prev == XSharpLexer.STATIC)
                        keyword = code[start - 1];
                }
                else
                {
                    // OUT VAR x, IS Foo VAR x, ...
                    continue;
                }
                var found = kind == ImpliedDeclarationKind.ForEach ? ForEach(code, i + 1, keyword)
                    : kind == ImpliedDeclarationKind.For ? For(code, i + 1, keyword)
                    : Declarators(code, i + 1, kind, keyword);
                result.AddRange(found);
            }
            return result;
        }

        private static IEnumerable<DeclarationCandidate> ForEach(IList<IToken> code, int index, IToken keyword)
        {
            if (index + 1 >= code.Count || !IsName(code[index].Type) || code[index + 1].Type != XSharpLexer.IN)
                yield break;
            var container = TakeUntil(code, index + 2, t => false, out _);
            yield return new DeclarationCandidate
            {
                Kind = ImpliedDeclarationKind.ForEach, Name = code[index], Initializer = container, Keyword = keyword, DeclaratorCount = 1
            };
        }

        private static IEnumerable<DeclarationCandidate> For(IList<IToken> code, int index, IToken keyword)
        {
            if (index + 1 >= code.Count || !IsName(code[index].Type) || code[index + 1].Type != XSharpLexer.ASSIGN_OP)
                yield break;
            var start = TakeUntil(code, index + 2,
                t => t == XSharpLexer.TO || t == XSharpLexer.UPTO || t == XSharpLexer.DOWNTO, out _);
            yield return new DeclarationCandidate
            {
                Kind = ImpliedDeclarationKind.For, Name = code[index], Initializer = start, Keyword = keyword, DeclaratorCount = 1
            };
        }

        private static IReadOnlyList<DeclarationCandidate> Declarators(IList<IToken> code, int index, ImpliedDeclarationKind kind, IToken keyword)
        {
            var result = new List<DeclarationCandidate>();
            while (index < code.Count)
            {
                if (code[index].Type == XSharpLexer.CONST)
                    index++;
                if (index + 1 >= code.Count || !IsName(code[index].Type) || code[index + 1].Type != XSharpLexer.ASSIGN_OP)
                    break;
                var name = code[index];
                var initializer = TakeUntil(code, index + 2, t => t == XSharpLexer.COMMA, out var end);
                result.Add(new DeclarationCandidate { Kind = kind, Name = name, Initializer = initializer, Keyword = keyword });
                if (end >= code.Count || code[end].Type != XSharpLexer.COMMA)
                    break;
                index = end + 1;
            }
            foreach (var decl in result)
                decl.DeclaratorCount = result.Count;
            return result;
        }

        /// <summary>The tokens from <paramref name="index"/> up to a top-level stop token or the end of the statement.</summary>
        private static IList<IToken> TakeUntil(IList<IToken> code, int index, System.Func<int, bool> stop, out int end)
        {
            var result = new List<IToken>();
            int depth = 0;
            for (end = index; end < code.Count; end++)
            {
                var type = code[end].Type;
                if (type == XSharpLexer.EOS || type == XSharpLexer.Eof)
                    break;
                if (depth == 0 && stop(type))
                    break;
                switch (type)
                {
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LBRKT:
                    case XSharpLexer.LCURLY:
                        depth++;
                        break;
                    case XSharpLexer.RPAREN:
                    case XSharpLexer.RBRKT:
                    case XSharpLexer.RCURLY:
                        depth--;
                        break;
                }
                result.Add(code[end]);
            }
            return result;
        }

        private static IList<IToken> DefaultChannel(IList<IToken> tokens)
        {
            return tokens.Where(t => t.Channel == XSharpLexer.DefaultTokenChannel).ToList();
        }

        private static bool IsName(int type)
        {
            return type == XSharpLexer.ID || XSharpLexer.IsPositionalKeyword(type);
        }

        private static bool IsDeclarationKeyword(int type)
        {
            switch (type)
            {
                case XSharpLexer.FUNCTION:
                case XSharpLexer.PROCEDURE:
                case XSharpLexer.METHOD:
                case XSharpLexer.ACCESS:
                case XSharpLexer.ASSIGN:
                case XSharpLexer.PROPERTY:
                case XSharpLexer.EVENT:
                case XSharpLexer.DELEGATE:
                case XSharpLexer.OPERATOR:
                case XSharpLexer.DESTRUCTOR:
                case XSharpLexer.CLASS:
                case XSharpLexer.STRUCTURE:
                case XSharpLexer.INTERFACE:
                case XSharpLexer.DLL:
                case XSharpLexer.DIM:
                case XSharpLexer.DECLARE:
                case XSharpLexer.DEFINE:
                    return true;
                default:
                    return false;
            }
        }
    }
}
