//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;

namespace XSharp.LanguageService.Editors.LightBulb.QuickActions
{
    /// <summary>An ACCESS or ASSIGN to convert: its body (the lines after the header) and its signature.</summary>
    internal sealed class AccessorSource
    {
        internal AccessorSource(string body, string parameterName, IList<string> indexParameters, string visibility, IList<string> indexNames = null)
        {
            IndexNames = indexNames ?? new string[0];
            Body = body ?? "";
            ParameterName = parameterName;
            IndexParameters = indexParameters ?? new string[0];
            Visibility = (visibility ?? "").Trim().ToUpperInvariant();
        }

        internal string Body { get; }

        /// <summary>The value parameter of an ASSIGN; null for an ACCESS.</summary>
        internal string ParameterName { get; }

        /// <summary>The index parameters as written (<c>n AS INT</c>).</summary>
        internal IList<string> IndexParameters { get; }

        /// <summary>The visibility keyword(s) written on the member, empty for the default.</summary>
        internal string Visibility { get; }

        /// <summary>The names of the index parameters (the ASSIGN's are renamed to the ACCESS's).</summary>
        internal IList<string> IndexNames { get; }
    }

    /// <summary>The PROPERTY that replaces an ACCESS/ASSIGN pair.</summary>
    internal static class PropertyConverter
    {
        /// <summary>
        /// The property text (ends with a line break). One line (<c>GET expr SET expr</c>) when the ACCESS is exactly
        /// <c>RETURN expr</c> and the ASSIGN one expression, both on one line; otherwise GET/SET blocks with the bodies.
        /// The wider visibility goes on the property, the narrower one on its accessor.
        /// </summary>
        internal static string Build(string name, string type, AccessorSource access, AccessorSource assign, string memberIndent,
            CodeStyle style, Func<string, IList<IToken>> lex)
        {
            memberIndent = memberIndent ?? "";
            var getBody = access?.Body;
            string setBody = null;
            if (assign != null)
            {
                setBody = assign.ParameterName == null ? assign.Body
                    : AccessorBody.RenameParameter(assign.Body, lex(assign.Body), assign.ParameterName, style.Kw("VALUE"));
                setBody = MapIndexNames(setBody, assign.IndexNames, access?.IndexNames, lex);
                setBody = AccessorBody.RewriteReturns(setBody, lex(setBody), style);
                if (setBody == null)
                    return null;
                setBody = DropFinalReturn(setBody, lex);
            }
            var (propertyVisibility, getVisibility, setVisibility) = Visibilities(access?.Visibility, assign?.Visibility);
            var index = (access ?? assign).IndexParameters;
            var header = new StringBuilder(memberIndent);
            if (propertyVisibility.Length > 0)
                header.Append(style.Kw(propertyVisibility)).Append(' ');
            header.Append(style.Kw("PROPERTY")).Append(' ').Append(name);
            if (index.Count > 0)
                header.Append('[').Append(string.Join(", ", index)).Append(']');
            if (!string.IsNullOrEmpty(type))
                header.Append(' ').Append(style.Kw("AS")).Append(' ').Append(type);

            var getExpression = getBody == null ? null : SingleReturnExpression(getBody, lex);
            var setExpression = setBody == null ? null : SingleExpression(setBody, lex);
            bool oneLine = (getBody == null || getExpression != null) && (setBody == null || setExpression != null);
            if (oneLine)
            {
                if (getBody != null)
                    header.Append(' ').Append(Prefix(getVisibility, style)).Append(style.Kw("GET")).Append(' ').Append(getExpression);
                if (setBody != null)
                    header.Append(' ').Append(Prefix(setVisibility, style)).Append(style.Kw("SET")).Append(' ').Append(setExpression);
                return header.Append(style.NewLine).ToString();
            }
            var accessorIndent = memberIndent + style.IndentUnit;
            var bodyIndent = accessorIndent + style.IndentUnit;
            var text = new StringBuilder(header.ToString()).Append(style.NewLine);
            if (getBody != null)
                Block(text, Prefix(getVisibility, style) + style.Kw("GET"), style.Kw("END GET"), getBody, accessorIndent, bodyIndent, style);
            if (setBody != null)
                Block(text, Prefix(setVisibility, style) + style.Kw("SET"), style.Kw("END SET"), setBody, accessorIndent, bodyIndent, style);
            return text.Append(memberIndent).Append(style.Kw("END PROPERTY")).Append(style.NewLine).ToString();
        }

        private static string Prefix(string visibility, CodeStyle style)
        {
            return visibility.Length == 0 ? "" : style.Kw(visibility) + " ";
        }

        private static void Block(StringBuilder text, string open, string close, string body, string accessorIndent, string bodyIndent, CodeStyle style)
        {
            text.Append(accessorIndent).Append(open).Append(style.NewLine);
            foreach (var line in Reindent(body, bodyIndent))
                text.Append(line).Append(style.NewLine);
            text.Append(accessorIndent).Append(close).Append(style.NewLine);
        }

        /// <summary>The body lines with their common indentation replaced by <paramref name="indent"/>; blank lines at the ends dropped.</summary>
        private static IEnumerable<string> Reindent(string body, string indent)
        {
            var lines = body.Replace("\r\n", "\n").Split('\n').ToList();
            while (lines.Count > 0 && lines[0].Trim().Length == 0)
                lines.RemoveAt(0);
            while (lines.Count > 0 && lines[lines.Count - 1].Trim().Length == 0)
                lines.RemoveAt(lines.Count - 1);
            var common = lines.Where(l => l.Trim().Length > 0)
                .Select(l => l.Length - l.TrimStart().Length)
                .DefaultIfEmpty(0).Min();
            return lines.Select(l => l.Trim().Length == 0 ? "" : indent + l.Substring(Math.Min(common, l.Length)));
        }

        /// <summary>
        /// The property's index parameters are the ACCESS's: the ASSIGN's index names are renamed to them (through
        /// placeholders, so that swapped names do not collide).
        /// </summary>
        private static string MapIndexNames(string body, IList<string> from, IList<string> to, Func<string, IList<IToken>> lex)
        {
            if (from == null || to == null || from.Count == 0 || from.Count != to.Count)
                return body;
            var placeholders = from.Select((n, i) => "Xs$Index" + i).ToList();
            for (int i = 0; i < from.Count; i++)
            {
                if (!string.Equals(from[i], to[i], StringComparison.OrdinalIgnoreCase))
                    body = AccessorBody.RenameParameter(body, lex(body), from[i], placeholders[i]);
            }
            for (int i = 0; i < from.Count; i++)
                body = body.Replace(placeholders[i], to[i]);
            return body;
        }

        /// <summary>The expression of a body that is exactly <c>RETURN expr</c> on one line, else null.</summary>
        private static string SingleReturnExpression(string body, Func<string, IList<IToken>> lex)
        {
            var tokens = lex(body);
            if (HasComment(tokens))
                return null;
            var code = Code(tokens);
            if (code.Count < 2 || code[0].Type != XSharpLexer.RETURN || !OneStatementOnOneLine(code))
                return null;
            return body.Substring(code[1].StartIndex, code[code.Count - 1].StopIndex - code[1].StartIndex + 1);
        }

        /// <summary>The text of a body that is exactly one expression statement on one line, else null.</summary>
        private static string SingleExpression(string body, Func<string, IList<IToken>> lex)
        {
            var tokens = lex(body);
            if (HasComment(tokens))
                return null;
            var code = Code(tokens);
            if (code.Count == 0 || !OneStatementOnOneLine(code))
                return null;
            var first = code[0].Type;
            if (first != XSharpLexer.ID && first != XSharpLexer.SELF && first != XSharpLexer.SUPER)
                return null;
            return body.Substring(code[0].StartIndex, code[code.Count - 1].StopIndex - code[0].StartIndex + 1);
        }

        /// <summary>Comments would get lost in the one-line form.</summary>
        private static bool HasComment(IList<IToken> tokens)
        {
            return tokens.Any(t => t.Type == XSharpLexer.SL_COMMENT || t.Type == XSharpLexer.ML_COMMENT || t.Type == XSharpLexer.DOC_COMMENT);
        }

        /// <summary>Default-channel tokens without EOS/EOF at the end.</summary>
        private static List<IToken> Code(IList<IToken> tokens)
        {
            var code = tokens.Where(t => t.Channel == XSharpLexer.DefaultTokenChannel && t.Type != XSharpLexer.Eof).ToList();
            while (code.Count > 0 && code[code.Count - 1].Type == XSharpLexer.EOS)
                code.RemoveAt(code.Count - 1);
            return code;
        }

        private static bool OneStatementOnOneLine(List<IToken> code)
        {
            return code.All(t => t.Type != XSharpLexer.EOS) && code.All(t => t.Line == code[0].Line);
        }

        /// <summary>A setter that ends with a bare RETURN does not need it.</summary>
        private static string DropFinalReturn(string body, Func<string, IList<IToken>> lex)
        {
            var code = Code(lex(body));
            if (code.Count == 0 || code[code.Count - 1].Type != XSharpLexer.RETURN)
                return body;
            var last = code[code.Count - 1];
            if (code.Count == 1)
                return body.Substring(last.StopIndex + 1).Trim().Length == 0 ? "" : body;
            var separator = code[code.Count - 2];
            if (separator.Type != XSharpLexer.EOS)
                return body;
            // cut from the end of the statement before: "x := c ; RETURN" keeps "x := c"
            var cut = code.Count > 2 ? code[code.Count - 3].StopIndex + 1 : separator.StartIndex;
            return body.Substring(0, cut) + body.Substring(last.StopIndex + 1);
        }

        /// <summary>The visibility of the property and of each accessor: the wider one on the property, the narrower on its accessor.</summary>
        private static (string Property, string Get, string Set) Visibilities(string get, string set)
        {
            if (get == null)
                return (set ?? "", "", "");
            if (set == null)
                return (get, "", "");
            if (Rank(get) == Rank(set))
                return (get, "", "");
            return Rank(get) > Rank(set) ? (get, "", set) : (set, get, "");
        }

        private static int Rank(string visibility)
        {
            switch (visibility)
            {
                case "":
                case "PUBLIC":
                case "EXPORT":
                    return 5;
                case "PROTECTED INTERNAL":
                case "INTERNAL PROTECTED":
                    return 4;
                case "INTERNAL":
                    return 3;
                case "PROTECTED":
                    return 2;
                default:
                    return 1;
            }
        }
    }
}
