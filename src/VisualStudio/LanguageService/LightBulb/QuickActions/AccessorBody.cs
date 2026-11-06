//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>Rewrites the body of an ASSIGN for the SET of a property, on lexer tokens of the body text.</summary>
    internal static class AccessorBody
    {
        /// <summary>
        /// Replaces the identifier <paramref name="parameter"/> (case-insensitive) with <paramref name="replacement"/>;
        /// strings, comments and longer names stay as they are. <paramref name="tokens"/> are lexed from <paramref name="body"/>.
        /// </summary>
        internal static string RenameParameter(string body, IList<IToken> tokens, string parameter, string replacement)
        {
            var code = tokens.Where(t => t.Channel == XSharpLexer.DefaultTokenChannel).ToList();
            var hits = new List<IToken>();
            for (int i = 0; i < code.Count; i++)
            {
                if (code[i].Type != XSharpLexer.ID || !string.Equals(code[i].Text, parameter, StringComparison.OrdinalIgnoreCase))
                    continue;
                // SELF:oOwner, oOther.oOwner, _FIELD->oOwner name a member or field, not the parameter
                if (i > 0 && IsMemberAccess(code[i - 1].Type))
                    continue;
                hits.Add(code[i]);
            }
            foreach (var token in hits.OrderByDescending(t => t.StartIndex))
                body = body.Substring(0, token.StartIndex) + replacement + body.Substring(token.StopIndex + 1);
            return body;
        }

        private static bool IsMemberAccess(int type)
        {
            return type == XSharpLexer.COLON || type == XSharpLexer.DOT || type == XSharpLexer.ALIAS || type == XSharpLexer.COLONCOLON;
        }

        private static bool IsAssignment(int type)
        {
            switch (type)
            {
                case XSharpLexer.ASSIGN_OP:
                case XSharpLexer.ASSIGN_ADD:
                case XSharpLexer.ASSIGN_SUB:
                case XSharpLexer.ASSIGN_MUL:
                case XSharpLexer.ASSIGN_DIV:
                case XSharpLexer.ASSIGN_MOD:
                case XSharpLexer.ASSIGN_EXP:
                case XSharpLexer.ASSIGN_BITAND:
                case XSharpLexer.ASSIGN_BITOR:
                case XSharpLexer.ASSIGN_XOR:
                case XSharpLexer.ASSIGN_LSHIFT:
                case XSharpLexer.ASSIGN_RSHIFT:
                case XSharpLexer.ASSIGN_QQMARK:
                case XSharpLexer.INC:
                case XSharpLexer.DEC:
                    return true;
                default:
                    return false;
            }
        }

        /// <summary>
        /// A setter returns nothing, but the compiler still evaluates the expression of <c>RETURN x</c>. An expression
        /// without calls and assignments (<c>c</c>, <c>-1</c>, <c>SELF:_c</c>) is dropped; an assignment or a call
        /// (<c>RETURN SELF:_c := c</c>, <c>RETURN SELF:Changed()</c>) becomes a statement of its own, followed by
        /// <c>RETURN</c> on the next line. Null when a RETURN cannot be rewritten safely: an expression that mixes a
        /// call or assignment with other operators, or a RETURN that does not start its line.
        /// </summary>
        internal static string RewriteReturns(string body, IList<IToken> tokens, CodeStyle style)
        {
            var code = tokens.Where(t => t.Channel == XSharpLexer.DefaultTokenChannel && t.Type != XSharpLexer.Eof).ToList();
            var edits = new List<(int Start, int Stop, string Text)>();
            for (int i = 0; i < code.Count; i++)
            {
                if (code[i].Type != XSharpLexer.RETURN || (i > 0 && code[i - 1].Type != XSharpLexer.EOS))
                    continue;
                var expression = new List<IToken>();
                for (int j = i + 1; j < code.Count && code[j].Type != XSharpLexer.EOS; j++)
                    expression.Add(code[j]);
                if (expression.Count == 0)
                    continue;
                var returnToken = code[i];
                var last = expression[expression.Count - 1];
                var lineStart = body.LastIndexOf('\n', Math.Max(0, returnToken.StartIndex - 1)) + 1;
                var indent = body.Substring(lineStart, returnToken.StartIndex - lineStart);
                // IF x ; RETURN y ; ENDIF: no own line to put the statement on
                if (indent.Trim().Length > 0)
                    return null;
                bool hasCall = expression.Any(t => t.Type == XSharpLexer.LPAREN);
                bool hasAssignment = expression.Any(t => IsAssignment(t.Type));
                if (!hasCall && !hasAssignment)
                {
                    edits.Add((returnToken.StopIndex + 1, last.StopIndex, ""));
                    continue;
                }
                if (!IsStatement(expression))
                    return null;
                var expressionText = body.Substring(expression[0].StartIndex, last.StopIndex - expression[0].StartIndex + 1);
                edits.Add((returnToken.StartIndex, last.StopIndex, expressionText + style.NewLine + indent + returnToken.Text));
            }
            foreach (var (start, stop, text) in edits.OrderByDescending(e => e.Start))
                body = body.Substring(0, start) + text + body.Substring(stop + 1);
            return body;
        }

        /// <summary>
        /// The expression can stand as a statement: an assignment (a member chain followed by an assignment operator)
        /// or a call (a member chain ending in its argument list), with no other operator at the outer level.
        /// </summary>
        private static bool IsStatement(IList<IToken> expression)
        {
            int depth = 0;
            int firstOperator = -1;
            for (int i = 0; i < expression.Count; i++)
            {
                var type = expression[i].Type;
                if (type == XSharpLexer.LPAREN || type == XSharpLexer.LBRKT || type == XSharpLexer.LCURLY)
                    depth++;
                else if (type == XSharpLexer.RPAREN || type == XSharpLexer.RBRKT || type == XSharpLexer.RCURLY)
                    depth--;
                else if (depth == 0 && type != XSharpLexer.ID && type != XSharpLexer.SELF && type != XSharpLexer.SUPER && !IsMemberAccess(type))
                {
                    firstOperator = i;
                    break;
                }
            }
            if (firstOperator < 0)
                return expression[expression.Count - 1].Type == XSharpLexer.RPAREN;
            return firstOperator > 0 && IsAssignment(expression[firstOperator].Type);
        }
    }
}
