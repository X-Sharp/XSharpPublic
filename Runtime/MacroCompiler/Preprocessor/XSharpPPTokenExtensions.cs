//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Generic;
using XSharp.MacroCompiler.Syntax;

namespace XSharp.MacroCompiler.Preprocessor
{
    internal static class XSharpPPTokenExtensions
    {
        internal static void AddRange<T>(this IList<T> tokens, IList<T> toadd)
        {
            foreach (var token in toadd)
            {
                tokens.Add(token);
            }
        }
        internal static TokenType La(this Token[] tokens, int pos)
        {
            if (pos >= 0 && pos < tokens.Length)
                return tokens[pos].type;
            return 0;
        }
        internal static bool IsName(this Token[] tokens, int pos)
        {
            if (pos >= 0 && pos < tokens.Length)
            {
                return tokens[pos].IsName();
            }
            return false;
        }

        internal static bool HasStopTokens(this PPTokenType value)
        {
            switch (value)
            {
                case PPTokenType.MatchList:
                case PPTokenType.MatchLike:
                case PPTokenType.MatchWild:
                    return true; ;
            }
            return false;
        }
       

        internal static void TrimLeadingSpaces(this IList<Token> tokens)
        {
            while (tokens.Count > 0 &&
                tokens[0].channel == Channel.HIDDENCHANNEL)
            {
                tokens.RemoveAt(0);
            }
        }
        internal static string AsTokenString( this IList<Token> tokens)
        {
            var sb = new System.Text.StringBuilder();
            int i = 0;
            foreach (var token in tokens)
            {
                if (token.channel != Channel.HIDDENCHANNEL)
                {
                    sb.Append(i);
                    sb.Append(" ");
                    sb.Append(token.ToString());
                    sb.AppendLine();
                    i++;
                }
            }
            return sb.ToString();
        }

        internal static string TrailingWs(this Token token)
        {
            if (token == null || token.source == null)
                return "";
            var source = token.source.SourceText;
            if (source == null)
                return "";
            var index = token.end;
            var result = "";
            while (index < source.Length)
            {
                var follow = source[index];
                if (follow == ' ' || follow == '\t')
                {
                    result += follow;
                }
                else
                {
                    break;
                }
                index += 1;
            }
            return result;
        }
        public static bool isWhiteSpace(char ch)
        {
            // this is surprisingly faster than the equivalent if statement
            switch (ch) {
                case '\u0009': case '\u000A': case '\u000B': case '\u000C': case '\u000D':
                case '\u0020': case '\u0085': case '\u00A0': case '\u1680': case '\u2000':
                case '\u2001': case '\u2002': case '\u2003': case '\u2004': case '\u2005':
                case '\u2006': case '\u2007': case '\u2008': case '\u2009': case '\u200A':
                case '\u2028': case '\u2029': case '\u202F': case '\u205F': case '\u3000':
                    return true;
                default:
                    return false;
            }
        }

        public static string TrimAllWithInplaceCharArray(this string str)
        {
            var src = str.ToCharArray();
            var len = str.Length;
            int idest = 0;
            bool instring = false;
            char stringEnd = ' ';
            bool lastIsWhiteSpace = false;
            for (int i = 0; i < len; i++)
            {
                bool lAdd = true;
                var ch = src[i];
                switch (ch)
                {
                    case '"':
                    case '\'':
                        if (instring && ch == stringEnd)
                        {
                           instring = false;
                            stringEnd = ' ';
                        }
                        else
                        {
                            stringEnd = ch;
                            instring = true;
                        }
                        break;
                    default:
                        if (!instring && lastIsWhiteSpace && isWhiteSpace(ch))
                        {
                            lAdd = false;
                        }
                        else
                            lAdd = true;
                        break;
                }
                if (lAdd)
                {
                    src[idest++] = ch;
                }
                lastIsWhiteSpace = isWhiteSpace(ch);
            }
            return new string(src, 0, idest);
        }

        internal static string AsString(this IList<Token> tokens)
        {
            string result = "";
            if (tokens != null)
            {
                foreach (var t in tokens)
                {
                    result = result + t.Text ;
                    if (!t.Text.EndsWith(" "))
                        result += " ";
                }

            }
            return TrimAllWithInplaceCharArray(result);
        }

        internal static IList<Token> CloneArray(this IList<Token> tokens)
        {
            var clone = new Token[tokens.Count];
            for (int i = 0; i < tokens.Count; i++)
            {
                clone[i] = new Token(tokens[i]);
            }
            return clone ;
        }
        internal static bool IsName(this Token token)
        {
            return token != null && (token.type == TokenType.ID
                || (token.type > TokenType.FIRST_KEYWORD && token.type < TokenType.LAST_KEYWORD)
                || (token.type > TokenType.FIRST_NULL && token.type < TokenType.LAST_NULL));
        }
        internal static bool IsEOS(this Token token)
        {
            return token != null && (token.type == TokenType.NL || token.type == TokenType.EOS);
        }
        internal static string FileName(this Token token)
        {
            if (token == null)
                return "";
            return token?.source?.SourceName;
        }

        internal static bool IsOptional(this PPTokenType type)
        {
            return type == PPTokenType.MatchOptional ||
                type == PPTokenType.ResultOptional ||
                type == PPTokenType.MatchWholeUDC;
        }

        internal static bool IsLiteral(this Token token)
        {
            if (token == null)
                return false;
            return token.type > TokenType.FIRST_CONSTANT && token.type < TokenType.LAST_CONSTANT;
        }

        public static bool IsOperator(this Token token)
        {
            return (token.type > TokenType.FIRST_OPERATOR && token.type < TokenType.LAST_OPERATOR);
        }

        public static bool IsKeyword(this Token token)
        {
            return (token.type > TokenType.FIRST_KEYWORD && token.type < TokenType.LAST_KEYWORD)
                || (token.type > TokenType.FIRST_NULL && token.type < TokenType.LAST_NULL);
        }
        public static bool IsIdentifier(this Token token)
        {
            return token.type == TokenType.ID || token.subtype == TokenType.ID;
        }

        public static bool IsString(this Token token)
        {
            switch (token.type)
            {
                case TokenType.CHAR_CONST:
                case TokenType.STRING_CONST:
                case TokenType.ESCAPED_STRING_CONST:
                case TokenType.INTERPOLATED_STRING_CONST:
                case TokenType.INCOMPLETE_STRING_CONST:
// TODO nvk
// case TokenType.TEXT_STRING_CONST:
// case TokenType.BRACKETED_STRING_CONST:
                    return true;
            }
            return false;
        }

        internal static bool NeedsLeft(this Token token)
        {
            if (token == null)
                return false;
            switch (token.type)
            {
                case TokenType.ASSIGN_ADD:
                case TokenType.ASSIGN_BITAND:
                case TokenType.ASSIGN_BITOR:
                case TokenType.ASSIGN_DIV:
                case TokenType.ASSIGN_EXP:
                case TokenType.ASSIGN_LSHIFT:
                case TokenType.ASSIGN_MOD:
                case TokenType.ASSIGN_MUL:
                case TokenType.ASSIGN_OP:
                case TokenType.ASSIGN_RSHIFT:
                case TokenType.ASSIGN_SUB:
                case TokenType.ASSIGN_XOR:
                    return true;
                case TokenType.COLON:
                case TokenType.DOT:
                    return true;
            }
            if (token.IsPrefix())
                return false;
            return token.IsBinary();
        }
        internal static bool NeedsRight(this Token token)
        {
            return token.IsBinary() || token.IsPrefix();
        }
        internal static bool IsBinary(this Token token)
        {
            if (token == null)
                return false;
            // see xsharp.g4 binaryExpression
            // tokens in same order 
            switch (token.type)
            {
                case TokenType.EXP:
                case TokenType.MULT:
                case TokenType.DIV:
                case TokenType.MOD:
                case TokenType.PLUS:
                case TokenType.MINUS:
                case TokenType.LSHIFT:
                case TokenType.RSHIFT:
                case TokenType.LT:
                case TokenType.LTE:
                case TokenType.GT:
                case TokenType.GTE:
                case TokenType.EQ:
                case TokenType.EEQ:
                case TokenType.SUBSTR:
                case TokenType.NEQ:
                case TokenType.NEQ2:
                case TokenType.AMP:
                case TokenType.TILDE:
                case TokenType.PIPE:
                case TokenType.AND:
                case TokenType.OR:
                case TokenType.LOGIC_AND:
                case TokenType.LOGIC_XOR:
                case TokenType.LOGIC_OR:
                    return true;
                case TokenType.COLON:
                case TokenType.DOT:
                case TokenType.ALIAS:
                    return true;
            }
            return false;
        }
        internal static bool IsPrefix(this Token token)
        {
            if (token == null)
                return false;
            switch (token.type)
            {
                case TokenType.PLUS:              // see xsharp.g4 prefixExpression
                case TokenType.MINUS:
                case TokenType.TILDE:
                case TokenType.ADDROF:
                case TokenType.DEC:
                case TokenType.INC:
                case TokenType.LOGIC_NOT:
                case TokenType.NOT:
                    return true;
            }
            return false;
        }

        internal static bool IsPostFix(this Token token)
        {
            if (token == null)
                return false;
            switch (token.type)
            {
                case TokenType.DEC:           // see xsharp.g4 postfixExpression
                case TokenType.INC:
                    return true;
            }
            return false;
        }
        internal static bool IsWildCard(this Token token)
        {
            switch (token.type)
            {
                case TokenType.QMARK:
                case TokenType.MULT:
                    return true;
            }
            return false;
        }
        internal static bool IsOpen(this Token token, ref TokenType closeType)
        {
            if (token == null)
                return false;
            switch (token.type)
            {
                case TokenType.LBRKT:
                    closeType = TokenType.RBRKT;
                    return true;
                case TokenType.LCURLY:
                    closeType = TokenType.RCURLY;
                    return true;
                case TokenType.LPAREN:
                    closeType = TokenType.RPAREN;
                    return true;
            }
            closeType = 0;
            return false;
        }
        internal static bool IsClose(this Token token)
        {
            if (token == null)
                return false;
            switch (token.type)
            {
                case TokenType.RBRKT:
                case TokenType.RCURLY:
                case TokenType.RPAREN:
                    return true;
            }
            return false;
        }

        internal static bool IsPrimary(this Token token)
        {
            if (token.IsName())
                return true;
            if (token.IsLiteral())
                return true;
            return false;
        }
        internal static bool IsPrimaryOrPrefix(this Token token)
        {
            if (token.IsPrimary())
                return true;
            if (token.IsPrefix())
                return true;
            return false;

        }



        internal static bool CanJoin(this Token token, Token nextToken)
        {
            if (token == null )
            {
                return nextToken.IsName() || nextToken.IsLiteral() || nextToken.IsPrefix() ;
            }
            if (token.IsPrefix() || token.IsBinary())          // we allow .and. .not. and even .not. .not.
                return (nextToken.IsPrimaryOrPrefix());
            if (nextToken.IsBinary() || nextToken.NeedsLeft() || nextToken.IsPostFix())
                return (token.IsPrimary() ||token.IsClose());

            return false;
        }
        internal static bool IsNeutral(this Token token)
        {
            if (token == null)
                return false;
            switch (token.type)
            {
                case TokenType.DEC:
                case TokenType.INC:
                    return true;
            }
            return false;
        }

        internal static bool IsEndOfCommand(this Token token)
        {
            if (token == null)
                return false;
            return token.type == TokenType.SEMI;
        }
        internal static bool IsEndOfLine(this Token token)
        {
            if (token == null)
                return false;
            return token.type == TokenType.NL;
        }
    }

}
