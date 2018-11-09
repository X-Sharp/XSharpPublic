/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
using System.Collections.Generic;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Roslyn.Utilities;
namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal static class XSharpPPTokenExtensions
    {

        internal static void TrimLeadingSpaces(this IList<XSharpToken> tokens)
        {
            while (tokens.Count > 0 &&
                tokens[0].Channel == XSharpLexer.Hidden)
            {
                tokens.RemoveAt(0);
            }
        }
        internal static string AsTokenString( this IList<IToken> tokens)
        {
            var sb = new System.Text.StringBuilder();
            int i = 0;
            foreach (var token in tokens)
            {
                if (token.Channel != XSharpLexer.Hidden)
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

        internal static string AsTokenString(this IList<XSharpToken> tokens)
        {
            return ((IList<IToken>)tokens).AsTokenString();
        }
        internal static string TrailingWs(this IToken token)
        {
            if (token == null || token.TokenSource == null)
                return "";
            var source = token.TokenSource.InputStream as ICharStream;
            if (source == null)
                return "";
            var index = token.StopIndex+1;
            var result = "";
            while (index < source.Size)
            {
                var interval = new Interval(index, index);
                var follow = source.GetText(interval);
                if (follow[0] == ' ' || follow[0] == '\t')
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

        internal static string AsString(this IList<XSharpToken> tokens)
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

        internal static IList<IToken> CloneArray(this IList<IToken> tokens)
        {
            var clone = new XSharpToken[tokens.Count];
            for (int i = 0; i < tokens.Count; i++)
            {
                clone[i] = new XSharpToken(tokens[i]);
            }
            return clone ;
        }
        internal static bool IsName(this IToken token)
        {
            return token != null && (token.Type == XSharpLexer.ID || XSharpLexer.IsKeyword(token.Type));
        }
        internal static bool IsEOS(this IToken token)
        {
            return token != null && (token.Type == XSharpLexer.NL || token.Type == XSharpLexer.EOS);
        }
        internal static string FileName(this IToken token)
        {
            if (token == null)
                return "";
            var src = token?.TokenSource?.SourceName;
            return PathUtilities.GetFileName(src);
        }

        internal static bool IsOptional(this PPTokenType type)
        {
            return type == PPTokenType.MatchOptional ||
                type == PPTokenType.ResultOptional;
        }

        internal static PPTokenType GetTokenType(this PPTokenType type)
        {
            return (PPTokenType)( (int) type & 0x0F);
        }

        internal static bool CanStartExpression(this IToken token)
        {
            return true;
        }
        internal static bool IsLiteral(this IToken token)
        {
            if (token == null)
                return false;
            return XSharpLexer.IsConstant(token.Type);
        }
        internal static bool CanQuote(this IToken token)
        {
            if (token == null)
                return false;
            if (token.IsName())
                return true;
            if (token.IsLiteral())
                return true;
            if (token.IsClose())
                return true;
            return false;
        }

        internal static bool NeedsLeft(this IToken token)
        {
            if (token == null)
                return false;
            switch (token.Type)
            {
                case XSharpLexer.ASSIGN_ADD:
                case XSharpLexer.ASSIGN_BITAND:
                case XSharpLexer.ASSIGN_BITOR:
                case XSharpLexer.ASSIGN_DIV:
                case XSharpLexer.ASSIGN_EXP:
                case XSharpLexer.ASSIGN_LSHIFT:
                case XSharpLexer.ASSIGN_MOD:
                case XSharpLexer.ASSIGN_MUL:
                case XSharpLexer.ASSIGN_OP:
                case XSharpLexer.ASSIGN_RSHIFT:
                case XSharpLexer.ASSIGN_SUB:
                case XSharpLexer.ASSIGN_XOR:
                    return true;
                case XSharpLexer.COLON:
                case XSharpLexer.DOT:
                    return true;
            }
            if (token.IsPrefix())
                return false;
            return token.IsBinary();
        }
        internal static bool NeedsRight(this IToken token)
        {
            return token.IsBinary() || token.IsPrefix();
        }
        internal static bool IsBinary(this IToken token)
        {
            if (token == null)
                return false;
            // see xsharp.g4 binaryExpression
            // tokens in same order 
            switch (token.Type)
            {
                case XSharpLexer.EXP:
                case XSharpLexer.MULT:
                case XSharpLexer.DIV:
                case XSharpLexer.MOD:
                case XSharpLexer.PLUS:
                case XSharpLexer.MINUS:
                case XSharpLexer.LSHIFT:
                case XSharpLexer.RSHIFT:
                case XSharpLexer.LT:
                case XSharpLexer.LTE:
                case XSharpLexer.GT:
                case XSharpLexer.GTE:
                case XSharpLexer.EQ:
                case XSharpLexer.EEQ:
                case XSharpLexer.SUBSTR:
                case XSharpLexer.NEQ:
                case XSharpLexer.NEQ2:
                case XSharpLexer.AMP:
                case XSharpLexer.TILDE:
                case XSharpLexer.PIPE:
                case XSharpLexer.LOGIC_AND:
                case XSharpLexer.AND:
                case XSharpLexer.LOGIC_XOR:
                case XSharpLexer.LOGIC_OR:
                case XSharpLexer.OR:
                    return true;
                case XSharpLexer.COLON:
                case XSharpLexer.DOT:
                case XSharpLexer.ALIAS:
                    return true;
            }
            return false;
        }
        internal static bool IsPrefix(this IToken token)
        {
            if (token == null)
                return false;
            switch (token.Type)
            {
                case XSharpLexer.PLUS:              // see xsharp.g4 prefixExpression
                case XSharpLexer.MINUS:
                case XSharpLexer.TILDE:
                case XSharpLexer.ADDROF:
                case XSharpLexer.DEC:
                case XSharpLexer.INC:
                case XSharpLexer.LOGIC_NOT:
                case XSharpLexer.NOT:
                    return true;
            }
            return false;
        }

        internal static bool IsPostFix(this IToken token)
        {
            if (token == null)
                return false;
            switch (token.Type)
            {
                case XSharpLexer.DEC:           // see xsharp.g4 postfixExpression
                case XSharpLexer.INC:
                    return true;
            }
            return false;
        }

        internal static bool IsOpen(this IToken token, ref int closeType)
        {
            if (token == null)
                return false;
            switch (token.Type)
            {
                case XSharpLexer.LBRKT:
                    closeType = XSharpLexer.RBRKT;
                    return true;
                case XSharpLexer.LCURLY:
                    closeType = XSharpLexer.RCURLY;
                    return true;
                case XSharpLexer.LPAREN:
                    closeType = XSharpLexer.RPAREN;
                    return true;
            }
            closeType = 0;
            return false;
        }
        internal static bool IsClose(this IToken token)
        {
            if (token == null)
                return false;
            switch (token.Type)
            {
                case XSharpLexer.RBRKT:
                case XSharpLexer.RCURLY:
                case XSharpLexer.RPAREN:
                    return true;
            }
            return false;
        }

        internal static bool IsPrimary(this IToken token)
        {
            if (token.IsName())
                return true;
            if (token.IsLiteral())
                return true;
            return false;
        }
        internal static bool CanJoin(this IToken token, IToken nextToken)
        {
            if (token == null )
            {
                return nextToken.IsName() || nextToken.IsLiteral() || nextToken.IsPrefix() ;
            }
            if (token.IsPrefix() || token.IsBinary())
                return (nextToken.IsPrimary());
            if (nextToken.IsBinary() || nextToken.NeedsLeft() || nextToken.IsPostFix())
                return (token.IsPrimary() ||token.IsClose());

            return false;
        }
        internal static bool IsNeutral(this IToken token)
        {
            if (token == null)
                return false;
            switch (token.Type)
            {
                case XSharpLexer.DEC:
                case XSharpLexer.INC:
                    return true;
            }
            return false;
        }

        internal static bool IsEndOfCommand(this IToken token)
        {
            if (token == null)
                return false;
            return token.Type == XSharpLexer.SEMI;
        }
        internal static bool IsEndOfLine(this IToken token)
        {
            if (token == null)
                return false;
            return token.Type == XSharpLexer.NL;
        }
    }

}
