//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Generic;
using System.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>How a call uses the result of the method it calls.</summary>
    internal enum CallResultKind
    {
        /// <summary>The call is a statement of its own: the method returns VOID.</summary>
        Unused,
        /// <summary>The call site fixes the type (<see cref="CallResult.Type"/>).</summary>
        Type,
        /// <summary>The result is assigned to <see cref="CallResult.Variable"/>: its declared type.</summary>
        Variable,
        /// <summary><c>RETURN Foo()</c>: the return type of the enclosing member.</summary>
        Return,
        /// <summary>Anything else.</summary>
        Unknown,
    }

    /// <summary>What the call site of a method that does not exist yet says about its return type.</summary>
    internal sealed class CallResult
    {
        internal CallResultKind Kind { get; private set; }
        internal string Type { get; private set; }
        internal IToken Variable { get; private set; }

        private static readonly CallResult Unknown = new CallResult { Kind = CallResultKind.Unknown };

        /// <summary>
        /// How the call <c>name(...)</c> (with its receiver chain, <c>SELF:</c>, <c>oVar:</c>, ...) uses its result:
        /// a statement of its own, a condition (LOGIC), <c>LOCAL x := Foo() AS T</c>, <c>x := Foo()</c>,
        /// <c>RETURN Foo()</c>. Anything else, a call inside an expression or an argument, is unknown.
        /// </summary>
        internal static CallResult Of(IList<IToken> tokens, IToken name)
        {
            var index = name.TokenIndex >= 0 && name.TokenIndex < tokens.Count && tokens[name.TokenIndex] == name
                ? name.TokenIndex
                : tokens.IndexOf(name);
            if (index < 0)
                return Unknown;
            var end = CloseOfList(tokens, Next(tokens, index));
            if (end < 0)
                return Unknown;
            var before = Previous(tokens, StartOfChain(tokens, index));
            var after = Next(tokens, end);
            var beforeType = before < 0 ? XSharpLexer.EOS : tokens[before].Type;
            var afterType = after < 0 ? XSharpLexer.EOS : tokens[after].Type;
            var endsStatement = IsEndOfStatement(afterType);
            switch (beforeType)
            {
                case XSharpLexer.EOS:
                case XSharpLexer.SEMI:
                    return endsStatement ? new CallResult { Kind = CallResultKind.Unused } : Unknown;
                case XSharpLexer.RETURN:
                    return endsStatement ? new CallResult { Kind = CallResultKind.Return } : Unknown;
                case XSharpLexer.ASSIGN_OP:
                    return Assignment(tokens, before, after, afterType);
                case XSharpLexer.IF:
                case XSharpLexer.ELSEIF:
                case XSharpLexer.WHILE:
                case XSharpLexer.CASE:
                case XSharpLexer.UNTIL:
                    break;
                default:
                    if (!IsLogicOperator(beforeType))
                        return Unknown;
                    break;
            }
            return endsStatement || IsLogicOperator(afterType) && afterType != XSharpLexer.NOT && afterType != XSharpLexer.LOGIC_NOT
                ? new CallResult { Kind = CallResultKind.Type, Type = "LOGIC" }
                : Unknown;
        }

        /// <summary><c>LOCAL x := Foo() AS T</c> gives T, <c>x := Foo()</c> and <c>SELF:x := Foo()</c> the variable x.</summary>
        private static CallResult Assignment(IList<IToken> tokens, int assign, int after, int afterType)
        {
            if (afterType == XSharpLexer.AS)
            {
                var type = TypeText(tokens, after + 1);
                return string.IsNullOrEmpty(type) ? Unknown : new CallResult { Kind = CallResultKind.Type, Type = type };
            }
            if (!IsEndOfStatement(afterType))
                return Unknown;
            var target = Previous(tokens, assign);
            if (target < 0 || tokens[target].Type != XSharpLexer.ID)
                return Unknown;
            var first = Previous(tokens, target);
            var firstType = first < 0 ? XSharpLexer.EOS : tokens[first].Type;
            // LOCAL x := Foo() without a type and VAR x := Foo() say nothing about the type
            if (firstType != XSharpLexer.EOS && firstType != XSharpLexer.SEMI && firstType != XSharpLexer.COLON && firstType != XSharpLexer.DOT)
                return Unknown;
            return new CallResult { Kind = CallResultKind.Variable, Variable = tokens[target] };
        }

        /// <summary>The type after AS up to the end of the statement; blanks only between two words (ARRAY OF STRING).</summary>
        private static string TypeText(IList<IToken> tokens, int start)
        {
            var text = new StringBuilder();
            IToken last = null;
            for (int i = start; i < tokens.Count; i++)
            {
                var token = tokens[i];
                if (token.Channel != XSharpLexer.DefaultTokenChannel)
                    continue;
                if (IsEndOfStatement(token.Type))
                    break;
                if (last != null && IsWord(last.Text) && IsWord(token.Text))
                    text.Append(' ');
                text.Append(token.Text);
                last = token;
            }
            return text.ToString();
        }

        private static bool IsWord(string text) => text.Length > 0 && (char.IsLetterOrDigit(text[0]) || text[0] == '_');

        /// <summary>The first token of the receiver chain before the name (<c>SELF</c> in <c>SELF:oOwner:Foo</c>).</summary>
        private static int StartOfChain(IList<IToken> tokens, int index)
        {
            for (int depth = 0; depth < 50; depth++)
            {
                var op = Previous(tokens, index);
                if (op < 0 || (tokens[op].Type != XSharpLexer.COLON && tokens[op].Type != XSharpLexer.DOT))
                    return index;
                var receiver = Previous(tokens, op);
                if (receiver < 0)
                    return index;
                var type = tokens[receiver].Type;
                if (type == XSharpLexer.RPAREN || type == XSharpLexer.RBRKT)
                {
                    receiver = OpenOfList(tokens, receiver);
                    if (receiver < 0)
                        return index;
                    var callee = Previous(tokens, receiver);
                    if (callee >= 0 && tokens[callee].Type == XSharpLexer.ID)
                        receiver = callee;
                }
                index = receiver;
            }
            return index;
        }

        /// <summary>The ')' that closes the list opened at <paramref name="open"/>, or -1 (no list, or the statement ends first).</summary>
        private static int CloseOfList(IList<IToken> tokens, int open)
        {
            if (open < 0 || tokens[open].Type != XSharpLexer.LPAREN)
                return -1;
            int depth = 0;
            for (int i = open; i < tokens.Count; i++)
            {
                var token = tokens[i];
                if (token.Channel != XSharpLexer.DefaultTokenChannel)
                    continue;
                switch (token.Type)
                {
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LBRKT:
                    case XSharpLexer.LCURLY:
                        depth++;
                        break;
                    case XSharpLexer.RPAREN:
                    case XSharpLexer.RBRKT:
                    case XSharpLexer.RCURLY:
                        if (--depth == 0)
                            return i;
                        break;
                    case XSharpLexer.EOS:
                    case XSharpLexer.Eof:
                        return -1;
                }
            }
            return -1;
        }

        /// <summary>The '(' or '[' that opens the list closed at <paramref name="close"/>, or -1.</summary>
        private static int OpenOfList(IList<IToken> tokens, int close)
        {
            int depth = 0;
            for (int i = close; i >= 0; i--)
            {
                var token = tokens[i];
                if (token.Channel != XSharpLexer.DefaultTokenChannel)
                    continue;
                switch (token.Type)
                {
                    case XSharpLexer.RPAREN:
                    case XSharpLexer.RBRKT:
                    case XSharpLexer.RCURLY:
                        depth++;
                        break;
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LBRKT:
                    case XSharpLexer.LCURLY:
                        if (--depth == 0)
                            return i;
                        break;
                    case XSharpLexer.EOS:
                        return -1;
                }
            }
            return -1;
        }

        private static bool IsEndOfStatement(int type) => type == XSharpLexer.EOS || type == XSharpLexer.Eof || type == XSharpLexer.SEMI;

        private static bool IsLogicOperator(int type)
        {
            switch (type)
            {
                case XSharpLexer.AND:
                case XSharpLexer.OR:
                case XSharpLexer.NOT:
                case XSharpLexer.LOGIC_AND:
                case XSharpLexer.LOGIC_OR:
                case XSharpLexer.LOGIC_NOT:
                    return true;
                default:
                    return false;
            }
        }

        private static int Previous(IList<IToken> tokens, int index)
        {
            for (int i = index - 1; i >= 0; i--)
            {
                if (tokens[i].Channel == XSharpLexer.DefaultTokenChannel)
                    return i;
            }
            return -1;
        }

        private static int Next(IList<IToken> tokens, int index)
        {
            for (int i = index + 1; i < tokens.Count; i++)
            {
                if (tokens[i].Channel == XSharpLexer.DefaultTokenChannel)
                    return i;
            }
            return -1;
        }
    }
}
