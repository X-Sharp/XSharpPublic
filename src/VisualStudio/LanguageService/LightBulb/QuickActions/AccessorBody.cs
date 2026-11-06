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
            var hits = tokens.Where(t => t.Type == XSharpLexer.ID && t.Channel == XSharpLexer.DefaultTokenChannel &&
                                         string.Equals(t.Text, parameter, StringComparison.OrdinalIgnoreCase))
                             .OrderByDescending(t => t.StartIndex);
            foreach (var token in hits)
                body = body.Substring(0, token.StartIndex) + replacement + body.Substring(token.StopIndex + 1);
            return body;
        }

        /// <summary>
        /// A setter returns nothing: <c>RETURN x</c> with a single token <c>x</c> becomes <c>RETURN</c>; with a longer
        /// expression (<c>RETURN SELF:_c := c</c> – the compiler still evaluates it) the expression becomes a statement
        /// of its own, followed by <c>RETURN</c> on the next line with the same indentation.
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
                if (expression.Count == 1)
                {
                    edits.Add((returnToken.StopIndex + 1, last.StopIndex, ""));
                    continue;
                }
                var expressionText = body.Substring(expression[0].StartIndex, last.StopIndex - expression[0].StartIndex + 1);
                var lineStart = body.LastIndexOf('\n', Math.Max(0, returnToken.StartIndex - 1)) + 1;
                var indent = body.Substring(lineStart, returnToken.StartIndex - lineStart);
                edits.Add((returnToken.StartIndex, last.StopIndex, expressionText + style.NewLine + indent + returnToken.Text));
            }
            foreach (var (start, stop, text) in edits.OrderByDescending(e => e.Start))
                body = body.Substring(0, start) + text + body.Substring(stop + 1);
            return body;
        }
    }
}
