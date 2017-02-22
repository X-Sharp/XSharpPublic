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
using System;
using System.Collections.Generic;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Diagnostics;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal static class PPTokenExtensions
    {
        internal static string TrailingWs(this IToken token)
        {
            var source = token.TokenSource.InputStream as ICharStream;
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

        internal static string AsString(this IList<PPToken> tokens)
        {
            string result = "";
            if (tokens != null)
            {
                foreach (var t in tokens)
                {
                    result = result + t.Text + t.TrailingWs();
                }

            }
            return result;
        }
        internal static IList<IToken> ToIListIToken(this IList<PPToken> tokens)
        {
            var clone = new IToken[tokens.Count];
            int i = 0;
            foreach (var t in tokens)
            {
                clone[i] = new PPToken(t);
                i++;
            }
            return clone;
        }
        internal static PPToken[] ToArrayPPToken(this IList<IToken> tokens)
        {
            var clone = new PPToken[tokens.Count];
            tokens.CopyTo(clone, 0);
            return clone;
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
            return type.HasFlag(PPTokenType.Optional);
        }
        internal static bool IsNested(this PPTokenType type)
        {
            return type.HasFlag(PPTokenType.Nested);
        }

        internal static bool IsMatched(this PPTokenType type)
        {
            return type.HasFlag(PPTokenType.Matched);
        }
        internal static bool IsResultMarker(this PPTokenType type)
        {
            return type.HasFlag(PPTokenType.ResultMask);
        }
        internal static bool IsMatchMarker(this PPTokenType type)
        {
            type = type.GetTokenType();
            return type != PPTokenType.Token && !type.HasFlag(PPTokenType.ResultMask); 
        }

        internal static PPTokenType GetTokenType(this PPTokenType type)
        {
            return (PPTokenType)( type & PPTokenType.TypeMask);
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
                return (token.IsPrimary());

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
