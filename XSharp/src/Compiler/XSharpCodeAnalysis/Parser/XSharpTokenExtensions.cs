//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System;
using System.Linq;
using System.Collections.Generic;
using System.Text;
#if !VSPARSER
using Microsoft.CodeAnalysis.Syntax.InternalSyntax;
using Roslyn.Utilities;
#endif
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal static class TokenExtensions
    {
        /// <summary>
        /// Return the literal as a normal .Net value for the preprocessor #if expressions
        /// </summary>
        /// <param name="context"></param>
        /// <returns></returns>
        internal static object GetValue(XSharpParser.LiteralValueContext context)
        {
            string text;
            text = context.Token.Text;
            switch (context.Token.Type)
            {
                case XSharpParser.TRUE_CONST:
                    return true;
                case XSharpParser.FALSE_CONST:
                    return false;
                case XSharpParser.CHAR_CONST:
                    if (text.StartsWith("c", StringComparison.OrdinalIgnoreCase))
                    {
                        text = text.Substring(1);
                        return CharValue(text);
                    }
                    else
                    {
                        return CharValue(text);
                    }
                case XSharpParser.STRING_CONST:
                case XSharpParser.INTERPOLATED_STRING_CONST:
                case XSharpParser.BRACKETED_STRING_CONST:
                case XSharpParser.INCOMPLETE_STRING_CONST:
                    return StringValue(text);
                case XSharpParser.ESCAPED_STRING_CONST:
                    return EscapedStringValue(text);
                case XSharpParser.SYMBOL_CONST:
                    return text.Substring(1).ToUpperInvariant();
                case XSharpParser.HEX_CONST:
                    switch (text.Last())
                    {
                        case 'U':
                        case 'u':
                            if (text.Length > 8 + 3)
                                return unchecked((ulong)HexValue(text.Substring(2)));
                            else
                                return unchecked((uint)HexValue(text.Substring(2)));
                        case 'L':
                        case 'l':
                            if (text.Length > 8 + 3)
                                return HexValue(text.Substring(2));
                            else
                                return unchecked((int)HexValue(text.Substring(2)));
                        default:
                            {
                                long l = HexValue(text.Substring(2));
                                if (l < 0)
                                    return unchecked((ulong)l);
                                else if (l > uint.MaxValue)
                                    return l;
                                else if (l > int.MaxValue)
                                    return unchecked((uint)l);
                                else
                                    return unchecked((int)l);
                            }
                    }
                case XSharpParser.BIN_CONST:
                    switch (text.Last())
                    {
                        case 'U':
                        case 'u':
                            if (text.Length > 32 + 3)
                                return unchecked((ulong)BinValue(text.Substring(2)));
                            else
                                return unchecked((uint)BinValue(text.Substring(2)));

                        case 'L':
                        case 'l':
                            if (text.Length > 32 + 3)
                                return BinValue(text.Substring(2));
                            else
                                return unchecked((int)BinValue(text.Substring(2)));
                        default:
                            {
                                long l = BinValue(text.Substring(2));
                                if (l < 0)
                                    return unchecked((ulong)l);
                                else if (l > uint.MaxValue)
                                    return l;
                                else if (l > int.MaxValue)
                                    return unchecked((uint)l);
                                else
                                    return unchecked((int)l);
                            }
                    }
                case XSharpParser.REAL_CONST:
                    if (text[0] == '$')
                    {
                        decimal value = decimal.Parse(text.Substring(1, text.Length - 1), System.Globalization.CultureInfo.InvariantCulture);
                        value = Math.Round(value, 4);
                        return value;
                    }
                    else
                    {
                        switch (text.Last())
                        {
                            case 'M':
                            case 'm':
                                return decimal.Parse(text.Substring(0, text.Length - 1), System.Globalization.CultureInfo.InvariantCulture);
                            case 'S':
                            case 's':
                                return float.Parse(text.Substring(0, text.Length - 1), System.Globalization.CultureInfo.InvariantCulture);
                            case 'D':
                            case 'd':
                                return double.Parse(text.Substring(0, text.Length - 1), System.Globalization.CultureInfo.InvariantCulture);
                            default:
                                return double.Parse(text, System.Globalization.CultureInfo.InvariantCulture);
                        }
                    }
                case XSharpParser.INT_CONST:
                    switch (text.Last())
                    {
                        case 'U':
                        case 'u':
                            try
                            {
                                ulong ul = ulong.Parse(text.Substring(0, text.Length - 1), System.Globalization.CultureInfo.InvariantCulture);
                                if (ul > uint.MaxValue)
                                    return ul;
                                else
                                    return unchecked((uint)ul);
                            }
                            catch (OverflowException)
                            {
                                return 0;
                            }
                        case 'L':
                        case 'l':
                            try
                            {
                                long l = long.Parse(text.Substring(0, text.Length - 1), System.Globalization.CultureInfo.InvariantCulture);
                                if (l > int.MaxValue)
                                    return l;
                                else
                                    return unchecked((int)l);
                            }
                            catch (OverflowException)
                            {
                                return 0;
                            }
                        default:
                            try
                            {
                                ulong un = 0;
                                long n = 0;
                                if (text.First() != '-')
                                {
                                    un = ulong.Parse(text, System.Globalization.CultureInfo.InvariantCulture);
                                    if (un <= long.MaxValue)
                                        n = unchecked((long)un);
                                }
                                else
                                {
                                    n = long.Parse(text, System.Globalization.CultureInfo.InvariantCulture);
                                }
                                if (un > long.MaxValue)
                                    return un;
                                else if (n > uint.MaxValue)
                                    return n;
                                else if (n > int.MaxValue)
                                    return unchecked((uint)n);
                                else
                                    return unchecked((int)n);
                            }
                            catch (OverflowException)
                            {
                                return 0;
                            }
                    }
                case XSharpParser.NULL:
                case XSharpParser.NULL_OBJECT:
                    return null;
                case XSharpParser.DATE_CONST:
                    return text;
                case XSharpParser.NULL_STRING:
                    return "";
                case XSharpParser.NULL_SYMBOL:
                    return "";
                case XSharpParser.NULL_FOX:
                    return DBNull.Value;
                case XSharpParser.NIL:
                case XSharpParser.NULL_ARRAY:
                case XSharpParser.NULL_CODEBLOCK:
                case XSharpParser.NULL_DATE:
                case XSharpParser.NULL_PSZ:
                case XSharpParser.NULL_PTR:
                    return null;
                default:
                    return text;
            }
        }
        private static bool IsHexDigit(char c) => (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');

        private static char EscapedChar(string s, ref int pos)
        {
            if (s[pos] != '\\' || pos == s.Length - 1)
                return s[pos++];
            else
            {
                switch (s[++pos])
                {
                    case '\\':
                    case '\'':
                    case '"':
                        return s[pos++];
                    case '0':
                        pos++;
                        return '\0';
                    case 'A':
                    case 'a':
                        pos++;
                        return '\a';
                    case 'B':
                    case 'b':
                        pos++;
                        return '\b';
                    case 'F':
                    case 'f':
                        pos++;
                        return '\f';
                    case 'N':
                    case 'n':
                        pos++;
                        return '\n';
                    case 'R':
                    case 'r':
                        pos++;
                        return '\r';
                    case 'T':
                    case 't':
                        pos++;
                        return '\t';
                    case 'V':
                    case 'v':
                        pos++;
                        return '\v';
                    case 'X':
                    case 'x':
                        {
                            int l = 0;
                            pos++;
                            while (l < 4 && pos + l < s.Length && IsHexDigit(s[pos + l]))
                                l++;
                            if (l > 0)
                            {
                                pos += l;
                                return (char)HexValue(s.Substring(pos - l, l));
                            }
                            else
                                return s[pos - 1];
                        }
                    case 'U':
                    case 'u':
                        {
                            int l = 0;
                            pos++;
                            while (l < 8 && pos + l < s.Length && IsHexDigit(s[pos + l]))
                                l++;
                            if (l == 4 || l == 8)
                            {
                                pos += l;
                                return (char)HexValue(s.Substring(pos - l, l));
                            }
                            else
                                return s[pos - 1];
                        }
                    default:
                        return s[pos++];
                }
            }
        }

        private static char CharValue(string text)
        {
            int p = 1;
            return EscapedChar(text, ref p);
        }

        private static string StringValue(string text)
        {
            return text.Substring(1, text.Length > 2 ? text.Length - 2 : 0);
        }

        internal static string EscapedStringValue(string text)
        {
            if (text.Length <= 3)
                return "";
            StringBuilder sb = new StringBuilder();
            int p = 2;
            while (p < text.Length - 1)
                sb.Append(EscapedChar(text, ref p));
            return sb.ToString();
        }

        internal static long HexValue(string text)
        {
            long r = 0;
            foreach (char c in text)
            {
                char cu = char.ToUpper(c);
                if (cu != 'U' && cu != 'L')
                {
                    r <<= 4;
                    if (cu >= '0' && cu <= '9')
                        r |= (long)(cu - '0');
                    else
                        r |= (long)((cu - 'A') + 10);
                }
            }
            return r;
        }

        private static long BinValue(string text)
        {
            long r = 0;
            foreach (char c in text)
            {
                char cu = char.ToUpper(c);
                if (cu != 'U')
                {
                    r <<= 1;
                    if (cu == '1')
                        r |= 1;
                }
            }
            return r;
        }
#if !VSPARSER
        public static SyntaxToken SyntaxIdentifier(this IToken token)
        {
            string text = token.Text;
            if (text.StartsWith("@@"))
            {
                text = text.Substring(2);
            }
            SyntaxToken r;
            switch (token.Type)
            {
                case XSharpParser.NAMEOF:
                    r = SyntaxFactory.Identifier(SyntaxKind.NameOfKeyword, null, text, text, null);
                    break;
                case XSharpParser.NINT:
                case XSharpParser.NUINT:
                case XSharpParser.DYNAMIC:
                    r = SyntaxFactory.MakeIdentifier(text.ToLower()); ;
                    break;
                default:
                    r = SyntaxFactory.MakeIdentifier(text);
                    break;
            }
            (token as CommonToken).Type = XSharpParser.ID;
            r.XNode = new XTerminalNodeImpl(token);
            return r;
        }

        //public static SyntaxToken SyntaxKeywordIdentifier(this IToken token)
        //{
        //    var r = SyntaxFactory.MakeIdentifier(token.Text.ToLower());
        //    r.XNode = new XTerminalNodeImpl(token);
        //    return r;
        //}

        public static SyntaxToken SyntaxNativeType(this IToken token)
        {
            SyntaxToken r;
            switch (token.Type)
            {
                case XSharpParser.BYTE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ByteKeyword);
                    break;
                case XSharpParser.DWORD:
                    r = SyntaxFactory.MakeToken(SyntaxKind.UIntKeyword);
                    break;
                case XSharpParser.SHORTINT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ShortKeyword);
                    break;
                case XSharpParser.INT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.IntKeyword);
                    break;
                case XSharpParser.INT64:
                    r = SyntaxFactory.MakeToken(SyntaxKind.LongKeyword);
                    break;
                case XSharpParser.LOGIC:
                    r = SyntaxFactory.MakeToken(SyntaxKind.BoolKeyword);
                    break;
                case XSharpParser.LONGINT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.IntKeyword);
                    break;
                case XSharpParser.OBJECT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ObjectKeyword);
                    break;
                case XSharpParser.REAL4:
                    r = SyntaxFactory.MakeToken(SyntaxKind.FloatKeyword);
                    break;
                case XSharpParser.REAL8:
                    r = SyntaxFactory.MakeToken(SyntaxKind.DoubleKeyword);
                    break;
                case XSharpParser.STRING:
                    r = SyntaxFactory.MakeToken(SyntaxKind.StringKeyword);
                    break;
                case XSharpParser.UINT64:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ULongKeyword);
                    break;
                case XSharpParser.WORD:
                    r = SyntaxFactory.MakeToken(SyntaxKind.UShortKeyword);
                    break;
                case XSharpParser.VOID:
                    r = SyntaxFactory.MakeToken(SyntaxKind.VoidKeyword);
                    break;
                case XSharpParser.CHAR:
                    r = SyntaxFactory.MakeToken(SyntaxKind.CharKeyword);
                    break;
                case XSharpParser.DECIMAL:
                    r = SyntaxFactory.MakeToken(SyntaxKind.DecimalKeyword);
                    break;
                default:
                    r = SyntaxFactory.MakeToken(SyntaxKind.BadToken).WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(0, token.Text.Length, ErrorCode.ERR_SyntaxError, token));
                    break;
            }
            r.XNode = new XTerminalNodeImpl(token);
            return r;
        }

        public static SyntaxToken SyntaxLiteralValue(this IToken token, CSharpParseOptions options, XSharpParserRuleContext context = null, List<PragmaOption> pragmaoptions = null)
        {
            SyntaxToken r;
            string text;
            text = token.Text;
            switch (token.Type)
            {
                case XSharpParser.ELLIPSIS:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ArgListKeyword);
                    break;
                case XSharpParser.TRUE_CONST:
                    r = SyntaxFactory.MakeToken(SyntaxKind.TrueKeyword);
                    break;
                case XSharpParser.FALSE_CONST:
                    r = SyntaxFactory.MakeToken(SyntaxKind.FalseKeyword);
                    break;
                case XSharpParser.CHAR_CONST:
                    if (text.StartsWith("c", StringComparison.OrdinalIgnoreCase))
                    {
                        text = text.Substring(1);
                        r = SyntaxFactory.Literal(SyntaxFactory.WS, text, CharValue(text), SyntaxFactory.WS);
                        if (text[1] != '\\' && text.Length > 3)     // c'\n' is allowed but not c'nn'
                        {
                            r = r.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_TooManyCharsInConst));
                        }
                    }
                    else
                    {
                        // Dialects with a single quote are excluded with a predicate in the lexer
                        r = SyntaxFactory.Literal(SyntaxFactory.WS, text, CharValue(text), SyntaxFactory.WS);
                    }
                    break;
                case XSharpParser.STRING_CONST:
                case XSharpParser.INTERPOLATED_STRING_CONST:
                case XSharpParser.BRACKETED_STRING_CONST:
                case XSharpParser.INCOMPLETE_STRING_CONST:
                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, StringValue(text), SyntaxFactory.WS);
                    if (text.StartsWith("'") && !options.Dialect.AllowStringsWithSingleQuotes())
                    {
                        r = r.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_FeatureNotAvailableInDialect, "Single Quoted Strings", options.Dialect.ToString()));
                    }
                    break;
                case XSharpParser.ESCAPED_STRING_CONST:
                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, EscapedStringValue(text), SyntaxFactory.WS);
                    break;
                case XSharpParser.SYMBOL_CONST:
                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, text.Substring(1).ToUpperInvariant(), SyntaxFactory.WS);
                    switch (options.Dialect)
                    {
                        case XSharpDialect.Core:
                            r.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_FeatureNotAvailableInDialect, token.Text, options.Dialect.ToString()));
                            break;
                        default:
                            // Ok
                            break;
                    }
                    break;
                case XSharpParser.HEX_CONST:
                    switch (token.Text.Last())
                    {
                        case 'U':
                        case 'u':
                            if (text.Length > 8 + 3)
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((ulong)HexValue(text.Substring(2))), SyntaxFactory.WS);
                            else
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((uint)HexValue(text.Substring(2))), SyntaxFactory.WS);
                            break;
                        case 'L':
                        case 'l':
                            if (text.Length > 8 + 3)
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, HexValue(text.Substring(2)), SyntaxFactory.WS);
                            else
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((int)HexValue(text.Substring(2))), SyntaxFactory.WS);
                            break;
                        default:
                            {
                                long l = HexValue(text.Substring(2));
                                if (l < 0)
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((ulong)l), SyntaxFactory.WS);
                                else if (l > uint.MaxValue)
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, l, SyntaxFactory.WS);
                                else if (l > int.MaxValue)
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((uint)l), SyntaxFactory.WS);
                                else
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((int)l), SyntaxFactory.WS);
                            }
                            break;
                    }
                    break;
                case XSharpParser.BIN_CONST:
                    switch (text.Last())
                    {
                        case 'U':
                        case 'u':
                            if (text.Length > 32 + 3)
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((ulong)BinValue(text.Substring(2))), SyntaxFactory.WS);
                            else
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((uint)BinValue(text.Substring(2))), SyntaxFactory.WS);
                            break;
                        case 'L':
                        case 'l':
                            if (text.Length > 32 + 3)
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, BinValue(text.Substring(2)), SyntaxFactory.WS);
                            else
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((int)BinValue(text.Substring(2))), SyntaxFactory.WS);
                            break;
                        default:
                            {
                                long l = BinValue(text.Substring(2));
                                if (l < 0)
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((ulong)l), SyntaxFactory.WS);
                                else if (l > uint.MaxValue)
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, l, SyntaxFactory.WS);
                                else if (l > int.MaxValue)
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((uint)l), SyntaxFactory.WS);
                                else
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((int)l), SyntaxFactory.WS);
                            }
                            break;
                    }
                    break;
                case XSharpParser.REAL_CONST:
                    if (text[0] == '$')
                    {
                        decimal value = decimal.Parse(text.Substring(1, text.Length - 1), System.Globalization.CultureInfo.InvariantCulture);
                        value = Math.Round(value, 4);
                        r = SyntaxFactory.Literal(SyntaxFactory.WS, text, value, SyntaxFactory.WS);
                    }
                    else
                    {
                        switch (text.Last())
                        {
                            case 'M':
                            case 'm':
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, decimal.Parse(text.Substring(0, text.Length - 1), System.Globalization.CultureInfo.InvariantCulture), SyntaxFactory.WS);
                                break;
                            case 'S':
                            case 's':
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, float.Parse(text.Substring(0, text.Length - 1), System.Globalization.CultureInfo.InvariantCulture), SyntaxFactory.WS);
                                break;
                            case 'D':
                            case 'd':
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, double.Parse(text.Substring(0, text.Length - 1), System.Globalization.CultureInfo.InvariantCulture), SyntaxFactory.WS);
                                break;
                            default:
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, double.Parse(text, System.Globalization.CultureInfo.InvariantCulture), SyntaxFactory.WS);
                                break;
                        }
                    }
                    break;
                case XSharpParser.INT_CONST:
                    switch (text.Last())
                    {
                        case 'U':
                        case 'u':
                            try
                            {
                                ulong ul = ulong.Parse(text.Substring(0, text.Length - 1), System.Globalization.CultureInfo.InvariantCulture);
                                if (ul > uint.MaxValue)
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, ul, SyntaxFactory.WS);
                                else
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((uint)ul), SyntaxFactory.WS);
                            }
                            catch (OverflowException)
                            {
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, 0, SyntaxFactory.WS)
                                    .WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_IntOverflow));
                            }
                            break;
                        case 'L':
                        case 'l':
                            try
                            {
                                long l = long.Parse(text.Substring(0, text.Length - 1), System.Globalization.CultureInfo.InvariantCulture);
                                if (l > int.MaxValue)
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, l, SyntaxFactory.WS);
                                else
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((int)l), SyntaxFactory.WS);
                            }
                            catch (OverflowException)
                            {
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, 0, SyntaxFactory.WS)
                                    .WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_IntOverflow));
                            }
                            break;
                        default:
                            try
                            {
                                ulong un = 0;
                                long n = 0;
                                if (text.First() != '-')
                                {
                                    un = ulong.Parse(text, System.Globalization.CultureInfo.InvariantCulture);
                                    if (un <= long.MaxValue)
                                        n = unchecked((long)un);
                                }
                                else
                                {
                                    n = long.Parse(text, System.Globalization.CultureInfo.InvariantCulture);
                                }
                                if (un > long.MaxValue)
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, un, SyntaxFactory.WS);
                                else if (n > uint.MaxValue)
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, n, SyntaxFactory.WS);
                                else if (n > int.MaxValue)
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((uint)n), SyntaxFactory.WS);
                                else
                                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, unchecked((int)n), SyntaxFactory.WS);
                            }
                            catch (OverflowException)
                            {
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, 0, SyntaxFactory.WS)
                                    .WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_IntOverflow));
                            }
                            break;
                    }
                    break;
                case XSharpParser.NULL:
                case XSharpParser.NULL_OBJECT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.NullKeyword);
                    break;
                case XSharpParser.DATE_CONST:
                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, text, SyntaxFactory.WS)
                        .WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_FeatureNotAvailableInDialect, "DATE constant (" + text + ")", options.Dialect.ToString()));
                    break;
                case XSharpParser.NULL_STRING:
                    switch (options.Dialect)
                    {
                        case XSharpDialect.Core:
                            r = SyntaxFactory.MakeToken(SyntaxKind.NullKeyword)
                            .WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_FeatureNotAvailableInDialect, text, options.Dialect.ToString()));
                            break;
                        default:
                            // Ok
                            if (options.HasOption(CompilerOption.NullStrings, context, pragmaoptions))
                                r = SyntaxFactory.Literal(SyntaxFactory.WS, text, "", SyntaxFactory.WS);
                            else
                                r = SyntaxFactory.MakeToken(SyntaxKind.NullKeyword);
                            break;
                    }
                    break;
                case XSharpParser.NULL_SYMBOL:
                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, "", SyntaxFactory.WS);
                    switch (options.Dialect)
                    {
                        case XSharpDialect.Core:
                            r.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_FeatureNotAvailableInDialect, text, options.Dialect.ToString()));
                            break;
                        default:
                            // Ok
                            break;
                    }
                    break;
                case XSharpParser.NULL_FOX:
                    r = SyntaxFactory.MakeIdentifier(SystemQualifiedNames.DBNullValue);
                    break;
                case XSharpParser.NIL:
                case XSharpParser.NULL_ARRAY:
                case XSharpParser.NULL_CODEBLOCK:
                case XSharpParser.NULL_DATE:
                case XSharpParser.NULL_PSZ:
                case XSharpParser.NULL_PTR:
                    // Actual expression is generated in the transformer
                    r = SyntaxFactory.MakeToken(SyntaxKind.NullKeyword);
                    switch (options.Dialect)
                    {
                        case XSharpDialect.Core:
                            r.WithAdditionalDiagnostics(new SyntaxDiagnosticInfo(ErrorCode.ERR_FeatureNotAvailableInDialect, text, options.Dialect.ToString()));
                            break;
                        default:
                            // Ok
                            break;
                    }
                    break;
                default: // nvk: This catches cases where a keyword/identifier is treated as a literal string
                    (token as CommonToken).Type = XSharpParser.STRING_CONST;
                    r = SyntaxFactory.Literal(SyntaxFactory.WS, text, text.StartsWith("@@") ? text.Substring(2) : text, SyntaxFactory.WS);
                    break;
            }
            r.XNode = new XTerminalNodeImpl(token);
            return r;
        }
        public static bool IsZeroLiteral(this IToken token)
        {
            return (token.Type == XSharpParser.INT_CONST || token.Type == XSharpParser.HEX_CONST || token.Type == XSharpParser.BIN_CONST)
                && token.SyntaxLiteralValue(CSharpParseOptions.Default) is SyntaxToken lv
                && lv.Kind == SyntaxKind.NumericLiteralToken
                && lv.ValueText == "0";
        }

        public static SyntaxToken SyntaxOp(this IToken token)
        {
            SyntaxToken r;
            switch (token.Type)
            {
                // C# does not have an EXP operator
                //case XSharpParser.EXP:
                //    r = SyntaxFactory.MakeToken(SyntaxKind.None);
                //    break;
                case XSharpParser.PLUS:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PlusToken);
                    break;
                case XSharpParser.MINUS:
                    r = SyntaxFactory.MakeToken(SyntaxKind.MinusToken);
                    break;
                case XSharpParser.MULT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.AsteriskToken);
                    break;
                case XSharpParser.DIV:
                    r = SyntaxFactory.MakeToken(SyntaxKind.SlashToken);
                    break;
                case XSharpParser.MOD:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PercentToken);
                    break;
                case XSharpParser.LSHIFT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.LessThanLessThanToken);
                    break;
                case XSharpParser.RSHIFT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.GreaterThanGreaterThanToken);
                    break;
                case XSharpParser.LT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.LessThanToken);
                    break;
                case XSharpParser.LTE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.LessThanEqualsToken);
                    break;
                case XSharpParser.GT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.GreaterThanToken);
                    break;
                case XSharpParser.GTE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.GreaterThanEqualsToken);
                    break;
                case XSharpParser.EQ:
                case XSharpParser.EEQ:
                    r = SyntaxFactory.MakeToken(SyntaxKind.EqualsEqualsToken);
                    break;
                case XSharpParser.NEQ:
                case XSharpParser.NEQ2:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ExclamationEqualsToken);
                    break;
                //case XSharpParser.SUBSTR:
                //    r = SyntaxKind.None;
                //    break;
                case XSharpParser.AMP:
                    r = SyntaxFactory.AmpersandToken;
                    break;
                case XSharpParser.TILDE:
                    // Note
                    // in VO ~is XOR for binary expressions and bitwise negation (Ones complement) for unary expressions
                    // VO uses ^ for Exponent
                    // in C# ^is XOR and ~is Bitwise negation (Ones complement)
                    // This method returns the Binary operator Caret
                    r = SyntaxFactory.MakeToken(SyntaxKind.CaretToken);
                    break;
                case XSharpParser.PIPE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.BarToken);
                    break;
                case XSharpParser.AND:
                case XSharpParser.LOGIC_AND:
                case XSharpParser.FOX_AND:
                    r = SyntaxFactory.MakeToken(SyntaxKind.AmpersandAmpersandToken);
                    break;
                case XSharpParser.BIT_AND:
                    r = SyntaxFactory.AmpersandToken;
                    break;
                case XSharpParser.OR:
                case XSharpParser.LOGIC_OR:
                case XSharpParser.FOX_OR:
                    r = SyntaxFactory.MakeToken(SyntaxKind.BarBarToken);
                    break;
                case XSharpParser.BIT_OR:
                    r = SyntaxFactory.MakeToken(SyntaxKind.BarToken);
                    break;
                case XSharpParser.BIT_NOT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.TildeToken);
                    break;
                case XSharpParser.BIT_XOR:
                    r = SyntaxFactory.MakeToken(SyntaxKind.CaretToken);
                    break;

                case XSharpParser.ASSIGN_OP:
                    r = SyntaxFactory.EqualsToken;
                    break;
                case XSharpParser.ASSIGN_ADD:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PlusEqualsToken);
                    break;
                case XSharpParser.ASSIGN_SUB:
                    r = SyntaxFactory.MakeToken(SyntaxKind.MinusEqualsToken);
                    break;
                case XSharpParser.ASSIGN_MUL:
                    r = SyntaxFactory.MakeToken(SyntaxKind.AsteriskEqualsToken);
                    break;
                case XSharpParser.ASSIGN_DIV:
                    r = SyntaxFactory.MakeToken(SyntaxKind.SlashEqualsToken);
                    break;
                case XSharpParser.ASSIGN_MOD:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PercentEqualsToken);
                    break;
                case XSharpParser.ASSIGN_BITAND:
                    r = SyntaxFactory.MakeToken(SyntaxKind.AmpersandEqualsToken);
                    break;
                case XSharpParser.ASSIGN_BITOR:
                    r = SyntaxFactory.MakeToken(SyntaxKind.BarEqualsToken);
                    break;
                case XSharpParser.ASSIGN_LSHIFT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.LessThanLessThanEqualsToken);
                    break;
                case XSharpParser.ASSIGN_RSHIFT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.GreaterThanGreaterThanEqualsToken);
                    break;
                case XSharpParser.ASSIGN_XOR:
                    r = SyntaxFactory.MakeToken(SyntaxKind.CaretEqualsToken);
                    break;
                case XSharpParser.ASSIGN_QQMARK:
                    r = SyntaxFactory.MakeToken(SyntaxKind.QuestionQuestionEqualsToken);
                    break;
                case XSharpParser.DEFAULT:
                case XSharpParser.QQMARK:
                    r = SyntaxFactory.MakeToken(SyntaxKind.QuestionQuestionToken);
                    break;
                case XSharpParser.ADDROF:
                    r = SyntaxFactory.AmpersandToken;
                    break;
                case XSharpParser.INC:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PlusPlusToken);
                    break;
                case XSharpParser.DEC:
                    r = SyntaxFactory.MakeToken(SyntaxKind.MinusMinusToken);
                    break;
                case XSharpParser.NOT:
                case XSharpParser.LOGIC_NOT:
                case XSharpParser.FOX_NOT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ExclamationToken);
                    break;
                case XSharpParser.LOGIC_XOR:
                case XSharpParser.FOX_XOR:
                    r = SyntaxFactory.MakeToken(SyntaxKind.CaretToken);
                    break;
                case XSharpParser.TRUE_CONST:
                    r = SyntaxFactory.MakeToken(SyntaxKind.TrueKeyword);
                    break;
                case XSharpParser.FALSE_CONST:
                    r = SyntaxFactory.MakeToken(SyntaxKind.FalseKeyword);
                    break;
                // C# does not have an EXP operator
                case XSharpParser.ASSIGN_EXP:
                default:
                    // return a valid operator with an error message prevents a crash in the compiler
                    r = SyntaxFactory.MakeToken(SyntaxKind.PlusToken).WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(0, "operator".Length, ErrorCode.ERR_SyntaxError, "operator"));
                    break;
            }
            r.XNode = new XTerminalNodeImpl(token);
            return r;
        }

        public static SyntaxToken SyntaxPrefixOp(this IToken token)
        {
            SyntaxToken r;
            switch (token.Type)
            {
                case XSharpParser.PLUS:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PlusToken);
                    break;
                case XSharpParser.MINUS:
                    r = SyntaxFactory.MakeToken(SyntaxKind.MinusToken);
                    break;
                case XSharpParser.TILDE:
                    // Note
                    // in VO ~is XOR for binary expressions and bitwise negation (Ones complement) for unary expressions
                    // VO uses ^ for Exponent
                    // in C# ^is XOR and ~is Bitwise negation (Ones complement)
                    // This method returns the Unary operator Tilde
                    r = SyntaxFactory.MakeToken(SyntaxKind.TildeToken);
                    break;
                case XSharpParser.ADDROF:
                    r = SyntaxFactory.AmpersandToken;
                    break;
                case XSharpParser.INC:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PlusPlusToken);
                    break;
                case XSharpParser.DEC:
                    r = SyntaxFactory.MakeToken(SyntaxKind.MinusMinusToken);
                    break;
                case XSharpParser.NOT:
                case XSharpParser.LOGIC_NOT:
                case XSharpParser.FOX_NOT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ExclamationToken);
                    break;
                case XSharpParser.LOGIC_XOR:
                case XSharpParser.FOX_XOR:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ExclamationToken);
                    break;
                case XSharpParser.TRUE_CONST:
                    r = SyntaxFactory.MakeToken(SyntaxKind.TrueKeyword);
                    break;
                case XSharpParser.FALSE_CONST:
                    r = SyntaxFactory.MakeToken(SyntaxKind.FalseKeyword);
                    break;
                default:
                    // return a valid operator with an error message prevents a crash in the compiler
                    r = SyntaxFactory.MakeToken(SyntaxKind.PlusToken).WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(0, "unary operator".Length, ErrorCode.ERR_SyntaxError, "unary operator"));
                    break;
            }
            r.XNode = new XTerminalNodeImpl(token);
            return r;
        }

        public static SyntaxToken SyntaxKeyword(this IToken token)
        {
            SyntaxToken r;
            var text = token.Text.ToLower();
            switch (token.Type)
            {
                case XSharpParser.ABSTRACT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.AbstractKeyword, text);
                    break;
                case XSharpParser.STATIC:
                    r = SyntaxFactory.MakeToken(SyntaxKind.StaticKeyword, text);
                    break;
                case XSharpParser.INTERNAL:
                    r = SyntaxFactory.MakeToken(SyntaxKind.InternalKeyword, text);
                    break;
                case XSharpParser.PUBLIC:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PublicKeyword, text);
                    break;
                case XSharpParser.EXPORT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PublicKeyword, text);
                    break;
                case XSharpParser.PRIVATE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PrivateKeyword, text);
                    break;
                case XSharpParser.HIDDEN:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PrivateKeyword, text);
                    break;
                case XSharpParser.NEW:
                    r = SyntaxFactory.MakeToken(SyntaxKind.NewKeyword, text);
                    break;
                case XSharpParser.PROTECTED:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ProtectedKeyword, text);
                    break;
                case XSharpParser.PARTIAL:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PartialKeyword, text);
                    break;
                case XSharpParser.EXTERN:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ExternKeyword, text);
                    break;
                case XSharpParser.UNSAFE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.UnsafeKeyword, text);
                    break;
                case XSharpParser.CHECKED:
                    r = SyntaxFactory.MakeToken(SyntaxKind.CheckedKeyword, text);
                    break;
                case XSharpParser.FIXED:
                    r = SyntaxFactory.MakeToken(SyntaxKind.FixedKeyword, text);
                    break;
                case XSharpParser.UNCHECKED:
                    r = SyntaxFactory.MakeToken(SyntaxKind.UncheckedKeyword, text);
                    break;
                case XSharpParser.ASYNC:
                    r = SyntaxFactory.MakeToken(SyntaxKind.AsyncKeyword, text);
                    break;
                case XSharpParser.AWAIT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.AwaitKeyword, text);
                    break;
                case XSharpParser.CASE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.CaseKeyword, text);
                    break;
                case XSharpParser.DEFAULT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.DefaultKeyword, text);
                    break;
                case XSharpParser.DELEGATE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.DelegateKeyword, text);
                    break;
                case XSharpParser.OTHERWISE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.DefaultKeyword, text);
                    break;
                case XSharpParser.REF:
                    r = SyntaxFactory.MakeToken(SyntaxKind.RefKeyword, text);
                    break;
                case XSharpParser.OUT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.OutKeyword, text);
                    break;
                case XSharpParser.PARAMS:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ParamsKeyword, text);
                    break;
                case XSharpParser.IN:
                    r = SyntaxFactory.MakeToken(SyntaxKind.InKeyword, text);
                    break;
                case XSharpParser.CONST:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ConstKeyword, text);
                    break;
                case XSharpParser.IS:
                    r = SyntaxFactory.MakeToken(SyntaxKind.IsKeyword, text);
                    break;
                case XSharpParser.CLASS:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ClassKeyword, text);
                    break;
                case XSharpParser.STRUCTURE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.StructKeyword, text);
                    break;
                case XSharpParser.SEALED:
                    r = SyntaxFactory.MakeToken(SyntaxKind.SealedKeyword, text);
                    break;
                case XSharpParser.OVERRIDE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.OverrideKeyword, text);
                    break;
                case XSharpParser.VIRTUAL:
                    r = SyntaxFactory.MakeToken(SyntaxKind.VirtualKeyword, text);
                    break;
                case XSharpParser.SELF:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ThisKeyword, text);
                    break;
                case XSharpParser.USING:
                    r = SyntaxFactory.MakeToken(SyntaxKind.UsingKeyword, text);
                    break;
                case XSharpParser.SUPER:
                    r = SyntaxFactory.MakeToken(SyntaxKind.BaseKeyword, text);
                    break;
                case XSharpParser.ARGLIST:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ArgListKeyword, text);
                    break;
                case XSharpParser.VAR:
                    r = SyntaxFactory.Identifier(XSharpSpecialNames.ImpliedTypeName);
                    break;
                case XSharpParser.THROW:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ThrowKeyword, text);
                    break;
                case XSharpParser.TRY:
                    r = SyntaxFactory.MakeToken(SyntaxKind.TryKeyword, text);
                    break;
                case XSharpParser.CATCH:
                    r = SyntaxFactory.MakeToken(SyntaxKind.CatchKeyword, text);
                    break;
                case XSharpParser.FINALLY:
                    r = SyntaxFactory.MakeToken(SyntaxKind.FinallyKeyword, text);
                    break;
                case XSharpParser.YIELD:
                    r = SyntaxFactory.MakeToken(SyntaxKind.YieldKeyword, text);
                    break;
                case XSharpParser.VOLATILE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.VolatileKeyword, text);
                    break;
                case XSharpParser.INITONLY:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ReadOnlyKeyword, text);
                    break;
                case XSharpParser.IMPLICIT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ImplicitKeyword, text);
                    break;
                case XSharpParser.EXPLICIT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ExplicitKeyword, text);
                    break;
                case XSharpParser.GLOBAL:
                    r = SyntaxFactory.MakeToken(SyntaxKind.GlobalKeyword, text);
                    break;
                case XSharpParser.INSTANCE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.None);
                    break;
                case XSharpParser.ASCENDING:
                    r = SyntaxFactory.MakeToken(SyntaxKind.AscendingKeyword, text);
                    break;
                case XSharpParser.DESCENDING:
                    r = SyntaxFactory.MakeToken(SyntaxKind.DescendingKeyword, text);
                    break;
                case XSharpParser.ADD:
                    r = SyntaxFactory.MakeToken(SyntaxKind.AddKeyword, text);
                    break;
                case XSharpParser.REMOVE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.RemoveKeyword, text);
                    break;
                case XSharpParser.VALUE:
                    r = SyntaxFactory.Identifier("value");
                    break;
                case XSharpParser.WHEN:
                    r = SyntaxFactory.MakeToken(SyntaxKind.WhenKeyword, text);
                    break;
                case XSharpParser.IF:
                    r = SyntaxFactory.MakeToken(SyntaxKind.IfKeyword, text);
                    break;
                case XSharpParser.WHILE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.WhileKeyword, text);
                    break;
                case XSharpParser.REPEAT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.DoKeyword, text);
                    break;
                case XSharpParser.FOR:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ForKeyword, text);
                    break;
                case XSharpParser.FOREACH:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ForEachKeyword, text);
                    break;
                case XSharpParser.GET:
                case XSharpParser.UDCSEP:
                    r = SyntaxFactory.MakeToken(SyntaxKind.GetKeyword, text);
                    break;
                case XSharpParser.SET:
                    r = SyntaxFactory.MakeToken(SyntaxKind.SetKeyword, text);
                    break;
                case XSharpParser.INIT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.InitKeyword, text);
                    break;
                case XSharpParser.EVENT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.EventKeyword, text);
                    break;
                case XSharpParser.RETURN:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ReturnKeyword, text);
                    break;
                case XSharpParser.EXIT:
                case XSharpParser.BREAK:
                    r = SyntaxFactory.MakeToken(SyntaxKind.BreakKeyword, text);
                    break;

                case XSharpParser.LOOP:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ContinueKeyword, text);
                    break;
                case XSharpParser.SWITCH:
                    r = SyntaxFactory.MakeToken(SyntaxKind.SwitchKeyword, text);
                    break;
                case XSharpParser.LOCK:
                    r = SyntaxFactory.MakeToken(SyntaxKind.LockKeyword, text);
                    break;
                case XSharpParser.WHERE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.WhereKeyword, text);
                    break;
                case XSharpParser.FROM:
                    r = SyntaxFactory.MakeToken(SyntaxKind.FromKeyword, text);
                    break;
                case XSharpParser.LET:
                    r = SyntaxFactory.MakeToken(SyntaxKind.LetKeyword, text);
                    break;
                case XSharpParser.JOIN:
                    r = SyntaxFactory.MakeToken(SyntaxKind.JoinKeyword, text);
                    break;
                case XSharpParser.ORDERBY:
                    r = SyntaxFactory.MakeToken(SyntaxKind.OrderByKeyword, text);
                    break;
                case XSharpParser.INTO:
                    r = SyntaxFactory.MakeToken(SyntaxKind.IntoKeyword, text);
                    break;
                case XSharpParser.ON:
                    r = SyntaxFactory.MakeToken(SyntaxKind.OnKeyword, text);
                    break;
                case XSharpParser.EQUALS:
                    r = SyntaxFactory.MakeToken(SyntaxKind.EqualsKeyword, text);
                    break;
                case XSharpParser.INTERFACE:
                    r = SyntaxFactory.MakeToken(SyntaxKind.InterfaceKeyword, text);
                    break;
                case XSharpParser.ENUM:
                    r = SyntaxFactory.MakeToken(SyntaxKind.EnumKeyword, text);
                    break;

                case XSharpParser.SELECT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.SelectKeyword, text);
                    break;
                case XSharpParser.GROUP:
                    r = SyntaxFactory.MakeToken(SyntaxKind.GroupKeyword, text);
                    break;
                case XSharpParser.BY:
                    r = SyntaxFactory.MakeToken(SyntaxKind.ByKeyword, text);
                    break;

                case XSharpParser.ACCESS:
                case XSharpParser.ALIGN:
                case XSharpParser.AS:
                case XSharpParser.ASSIGN:
                case XSharpParser.BEGIN:
                //case XSharpParser.CASE:
                case XSharpParser.CAST:
                case XSharpParser.CLIPPER:
                case XSharpParser.DEFINE:
                case XSharpParser.DIM:
                case XSharpParser.DLL:
                case XSharpParser.DO:
                case XSharpParser.DOWNTO:
                case XSharpParser.ELSE:
                case XSharpParser.ELSEIF:
                case XSharpParser.END:
                case XSharpParser.ENDCASE:
                case XSharpParser.ENDDO:
                case XSharpParser.ENDIF:
                case XSharpParser.FASTCALL:
                case XSharpParser.FIELD:
                case XSharpParser.FUNCTION:
                case XSharpParser.IIF:
                case XSharpParser.INHERIT:
                case XSharpParser.LOCAL:
                case XSharpParser.MEMBER:
                case XSharpParser.METHOD:
                case XSharpParser.NEXT:
                case XSharpParser.PASCAL:
                case XSharpParser.PROCEDURE:
                case XSharpParser.RECOVER:
                case XSharpParser.SEQUENCE:
                case XSharpParser.SIZEOF:
                case XSharpParser.STEP:
                case XSharpParser.STRICT:
                case XSharpParser.THISCALL:
                case XSharpParser.TO:
                case XSharpParser.TYPEOF:
                case XSharpParser.UNION:
                case XSharpParser.UPTO:
                case XSharpParser.AUTO:
                case XSharpParser.CONSTRUCTOR:
                case XSharpParser.DESTRUCTOR:
                case XSharpParser.IMPLEMENTS:
                case XSharpParser.IMPLIED:
                case XSharpParser.NAMESPACE:
                case XSharpParser.NOP:
                case XSharpParser.OPERATOR:
                case XSharpParser.PROPERTY:
                case XSharpParser.SCOPE:
                case XSharpParser.UNTIL:
                case XSharpParser.VOSTRUCT:
                    r = SyntaxFactory.Identifier(text);
                    break;
                default:
                    r = SyntaxFactory.MakeToken(SyntaxKind.BadToken).WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(0, text.Length, ErrorCode.ERR_SyntaxError, token));
                    break;
            }
            r.XNode = new XTerminalNodeImpl(token);
            return r;
        }

        public static SyntaxKind ConstraintKind(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.CLASS:
                    r = SyntaxKind.ClassConstraint;
                    break;
                case XSharpParser.STRUCTURE:
                    r = SyntaxKind.StructConstraint;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static SyntaxKind CtorInitializerKind(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.SELF:
                    r = SyntaxKind.ThisConstructorInitializer;
                    break;
                case XSharpParser.SUPER:
                    r = SyntaxKind.BaseConstructorInitializer;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static SyntaxKind AccessorKind(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.GET:
                case XSharpParser.UDCSEP:
                    r = SyntaxKind.GetAccessorDeclaration;
                    break;
                case XSharpParser.SET:
                    r = SyntaxKind.SetAccessorDeclaration;
                    break;
                case XSharpParser.INIT:
                    r = SyntaxKind.InitAccessorDeclaration;
                    break;
                case XSharpParser.ADD:
                    r = SyntaxKind.AddAccessorDeclaration;
                    break;
                case XSharpParser.REMOVE:
                    r = SyntaxKind.RemoveAccessorDeclaration;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static SyntaxKind ExpressionKindLiteral(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.TRUE_CONST:
                    r = SyntaxKind.TrueLiteralExpression;
                    break;
                case XSharpParser.FALSE_CONST:
                    r = SyntaxKind.FalseLiteralExpression;
                    break;
                case XSharpParser.CHAR_CONST:
                    r = SyntaxKind.CharacterLiteralExpression;
                    break;
                case XSharpParser.STRING_CONST:
                case XSharpParser.ESCAPED_STRING_CONST:
                case XSharpParser.INCOMPLETE_STRING_CONST:
                case XSharpParser.BRACKETED_STRING_CONST:
                    r = SyntaxKind.StringLiteralExpression;
                    break;
                case XSharpParser.INTERPOLATED_STRING_CONST:
                    r = SyntaxKind.InterpolatedStringExpression;
                    break;
                case XSharpParser.SYMBOL_CONST:
                    r = SyntaxKind.StringLiteralExpression;
                    break;
                case XSharpParser.HEX_CONST:
                    r = SyntaxKind.NumericLiteralExpression;
                    break;
                case XSharpParser.BIN_CONST:
                    r = SyntaxKind.NumericLiteralExpression;
                    break;
                case XSharpParser.REAL_CONST:
                    r = SyntaxKind.NumericLiteralExpression;
                    break;
                case XSharpParser.INT_CONST:
                    r = SyntaxKind.NumericLiteralExpression;
                    break;
                case XSharpParser.INVALID_NUMBER:
                    r = SyntaxKind.NumericLiteralExpression;
                    break;
                case XSharpParser.DATE_CONST:
                    r = SyntaxKind.NumericLiteralExpression;
                    break;
                case XSharpParser.DATETIME_CONST:
                    r = SyntaxKind.StringLiteralExpression;
                    break;
                case XSharpParser.NIL:
                case XSharpParser.NULL:
                case XSharpParser.NULL_ARRAY:
                case XSharpParser.NULL_CODEBLOCK:
                case XSharpParser.NULL_DATE:
                case XSharpParser.NULL_OBJECT:
                case XSharpParser.NULL_PSZ:
                case XSharpParser.NULL_PTR:
                case XSharpParser.NULL_STRING:
                case XSharpParser.NULL_SYMBOL:
                    r = SyntaxKind.NullLiteralExpression;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static SyntaxKind ExpressionKindBinaryOp(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                // C# does not have an EXP operator
                //case XSharpParser.EXP:
                //    r = SyntaxKind.None;
                //    break;
                case XSharpParser.PLUS:
                    r = SyntaxKind.AddExpression;
                    break;
                case XSharpParser.MINUS:
                    r = SyntaxKind.SubtractExpression;
                    break;
                case XSharpParser.MULT:
                    r = SyntaxKind.MultiplyExpression;
                    break;
                case XSharpParser.DIV:
                    r = SyntaxKind.DivideExpression;
                    break;
                case XSharpParser.MOD:
                    r = SyntaxKind.ModuloExpression;
                    break;
                case XSharpParser.LSHIFT:
                    r = SyntaxKind.LeftShiftExpression;
                    break;
                case XSharpParser.RSHIFT:
                    r = SyntaxKind.RightShiftExpression;
                    break;
                case XSharpParser.LT:
                    r = SyntaxKind.LessThanExpression;
                    break;
                case XSharpParser.LTE:
                    r = SyntaxKind.LessThanOrEqualExpression;
                    break;
                case XSharpParser.GT:
                    r = SyntaxKind.GreaterThanExpression;
                    break;
                case XSharpParser.GTE:
                    r = SyntaxKind.GreaterThanOrEqualExpression;
                    break;
                case XSharpParser.EQ:
                case XSharpParser.EEQ:
                    r = SyntaxKind.EqualsExpression;
                    break;
                case XSharpParser.NEQ:
                case XSharpParser.NEQ2:
                    r = SyntaxKind.NotEqualsExpression;
                    break;
                //case XSharpParser.SUBSTR:
                //    r = SyntaxKind.None;
                //    break;
                case XSharpParser.AMP:
                    r = SyntaxKind.BitwiseAndExpression;
                    break;
                case XSharpParser.TILDE:
                    r = SyntaxKind.ExclusiveOrExpression;
                    break;
                case XSharpParser.PIPE:
                    r = SyntaxKind.BitwiseOrExpression;
                    break;
                case XSharpParser.AND:
                case XSharpParser.LOGIC_AND:
                case XSharpParser.FOX_AND:
                    r = SyntaxKind.LogicalAndExpression;
                    break;
                case XSharpParser.BIT_AND:
                    r = SyntaxKind.BitwiseAndExpression;
                    break;
                case XSharpParser.OR:
                case XSharpParser.LOGIC_OR:
                case XSharpParser.FOX_OR:
                    r = SyntaxKind.LogicalOrExpression;
                    break;
                case XSharpParser.BIT_OR:
                    r = SyntaxKind.BitwiseOrExpression;
                    break;
                case XSharpParser.LOGIC_XOR:
                case XSharpParser.FOX_XOR:
                    r = SyntaxKind.ExclusiveOrExpression;
                    break;
                case XSharpParser.BIT_NOT:
                    r = SyntaxKind.BitwiseNotExpression;
                    break;
                case XSharpParser.BIT_XOR:
                    r = SyntaxKind.ExclusiveOrExpression;
                    break;

                case XSharpParser.ASSIGN_OP:
                    r = SyntaxKind.SimpleAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_ADD:
                    r = SyntaxKind.AddAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_SUB:
                    r = SyntaxKind.SubtractAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_MUL:
                    r = SyntaxKind.MultiplyAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_DIV:
                    r = SyntaxKind.DivideAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_MOD:
                    r = SyntaxKind.ModuloAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_BITAND:
                    r = SyntaxKind.AndAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_BITOR:
                    r = SyntaxKind.OrAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_LSHIFT:
                    r = SyntaxKind.LeftShiftAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_RSHIFT:
                    r = SyntaxKind.RightShiftAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_XOR:
                    r = SyntaxKind.ExclusiveOrAssignmentExpression;
                    break;
                case XSharpParser.ASSIGN_QQMARK:
                    r = SyntaxKind.CoalesceAssignmentExpression;
                    break;
                case XSharpParser.DEFAULT:
                case XSharpParser.QQMARK:
                    r = SyntaxKind.CoalesceExpression;
                    break;
                // C# does not have an EXP operator
                case XSharpParser.ASSIGN_EXP:
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static SyntaxToken ComplexToSimpleToken(this IToken token)
        {
            SyntaxToken r;
            switch (token.Type)
            {
                case XSharpParser.ASSIGN_ADD:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PlusToken);
                    break;
                case XSharpParser.ASSIGN_SUB:
                    r = SyntaxFactory.MakeToken(SyntaxKind.MinusToken);
                    break;
                case XSharpParser.ASSIGN_MUL:
                    r = SyntaxFactory.MakeToken(SyntaxKind.AsteriskToken);
                    break;
                case XSharpParser.ASSIGN_DIV:
                    r = SyntaxFactory.MakeToken(SyntaxKind.SlashToken);
                    break;
                case XSharpParser.ASSIGN_MOD:
                    r = SyntaxFactory.MakeToken(SyntaxKind.PercentToken);
                    break;
                case XSharpParser.ASSIGN_BITAND:
                    r = SyntaxFactory.AmpersandToken;
                    break;
                case XSharpParser.ASSIGN_BITOR:
                    r = SyntaxFactory.MakeToken(SyntaxKind.BarToken);
                    break;
                case XSharpParser.ASSIGN_LSHIFT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.LessThanLessThanToken);
                    break;
                case XSharpParser.ASSIGN_RSHIFT:
                    r = SyntaxFactory.MakeToken(SyntaxKind.GreaterThanGreaterThanToken);
                    break;
                case XSharpParser.ASSIGN_XOR:
                    r = SyntaxFactory.MakeToken(SyntaxKind.CaretToken);
                    break;
                case XSharpParser.ASSIGN_QQMARK:
                    r = SyntaxFactory.MakeToken(SyntaxKind.QuestionQuestionToken);
                    break;
                // C# does not have an EXP operator
                case XSharpParser.ASSIGN_EXP:
                default:
                    r = SyntaxFactory.MakeToken(SyntaxKind.DotToken);
                    break;
            }
            return r;

        }
        public static SyntaxKind ComplexToSimpleBinaryOp(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.ASSIGN_ADD:
                    r = SyntaxKind.AddExpression;
                    break;
                case XSharpParser.ASSIGN_SUB:
                    r = SyntaxKind.SubtractExpression;
                    break;
                case XSharpParser.ASSIGN_MUL:
                    r = SyntaxKind.MultiplyExpression;
                    break;
                case XSharpParser.ASSIGN_DIV:
                    r = SyntaxKind.DivideExpression;
                    break;
                case XSharpParser.ASSIGN_MOD:
                    r = SyntaxKind.ModuloExpression;
                    break;
                case XSharpParser.ASSIGN_BITAND:
                    r = SyntaxKind.BitwiseAndExpression;
                    break;
                case XSharpParser.ASSIGN_BITOR:
                    r = SyntaxKind.BitwiseOrExpression;
                    break;
                case XSharpParser.ASSIGN_LSHIFT:
                    r = SyntaxKind.LeftShiftExpression;
                    break;
                case XSharpParser.ASSIGN_RSHIFT:
                    r = SyntaxKind.RightShiftExpression;
                    break;
                case XSharpParser.ASSIGN_XOR:
                    r = SyntaxKind.ExclusiveOrExpression;
                    break;
                case XSharpParser.ASSIGN_QQMARK:
                    r = SyntaxKind.CoalesceExpression;
                    break;
                default:
                    r = SyntaxKind.EmptyStatement;
                    break;
            }
            return r;
        }

        public static SyntaxKind ExpressionKindPrefixOp(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.PLUS:
                    r = SyntaxKind.UnaryPlusExpression;
                    break;
                case XSharpParser.MINUS:
                    r = SyntaxKind.UnaryMinusExpression;
                    break;
                case XSharpParser.TILDE:
                    r = SyntaxKind.BitwiseNotExpression;
                    break;
                case XSharpParser.ADDROF:
                    r = SyntaxKind.AddressOfExpression;
                    break;
                case XSharpParser.INC:
                    r = SyntaxKind.PreIncrementExpression;
                    break;
                case XSharpParser.DEC:
                    r = SyntaxKind.PreDecrementExpression;
                    break;
                case XSharpParser.NOT:
                case XSharpParser.LOGIC_NOT:
                case XSharpParser.FOX_NOT:
                    r = SyntaxKind.LogicalNotExpression;
                    break;
                case XSharpParser.LOGIC_XOR:
                case XSharpParser.FOX_XOR:
                    r = SyntaxKind.LogicalNotExpression;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static SyntaxKind ExpressionKindPostfixOp(this IToken token)
        {
            SyntaxKind r;
            switch (token.Type)
            {
                case XSharpParser.INC:
                    r = SyntaxKind.PostIncrementExpression;
                    break;
                case XSharpParser.DEC:
                    r = SyntaxKind.PostDecrementExpression;
                    break;
                default:
                    throw new InvalidOperationException();
            }
            return r;
        }

        public static void AddCheckUnique(this SyntaxListBuilder list, SyntaxToken t)
        {
            if (t.Kind != SyntaxKind.None)
            {
                if (list.Any((int)t.Kind))
                {
                    t = t.WithAdditionalDiagnostics(
                        new SyntaxDiagnosticInfo(t.GetLeadingTriviaWidth(), t.Width, ErrorCode.ERR_DuplicateModifier, t.Text));
                }
                list.Add(t);
            }
        }
        internal static XNodeFlags SetFlag(this XNodeFlags oldFlag, XNodeFlags newFlag, bool set)
        {
            if (set)
                oldFlag |= newFlag;
            else
                oldFlag &= ~newFlag;
            return oldFlag;
        }

        public static void FixDefaultVisibility(this SyntaxListBuilder list, bool isInstance = false)
        {
            for (int i = 0; i < list.Count; i++)
            {
                var item = list[i];
                if (SyntaxFacts.IsAccessibilityModifier((SyntaxKind)item.RawKind))
                    return;
            }
            if (isInstance)
                list.Add(SyntaxFactory.MakeGeneratedToken(SyntaxKind.ProtectedKeyword));
            else
                list.Add(SyntaxFactory.MakeGeneratedToken(SyntaxKind.PublicKeyword));
        }

        public static bool CanBeVirtual(this SyntaxListBuilder list)
        {
            if (list.Any((int)SyntaxKind.StaticKeyword) ||
                list.Any((int)SyntaxKind.ExternKeyword) ||
                list.Any((int)SyntaxKind.AbstractKeyword) ||
                list.Any((int)SyntaxKind.PrivateKeyword))
            {
                return false;
            }
            return true;

        }
        public static void FixVirtual(this SyntaxListBuilder list, bool enforceOverride)
        {
            if (!list.CanBeVirtual())
                return;
            if (!list.Any((int)SyntaxKind.VirtualKeyword))
            {
                list.Add(SyntaxFactory.MakeGeneratedToken(SyntaxKind.VirtualKeyword));
            }
            if (list.Any((int)SyntaxKind.NewKeyword))
                return;
            if (!enforceOverride && !list.Any((int)SyntaxKind.OverrideKeyword))
            {
                list.Add(SyntaxFactory.MakeGeneratedToken(SyntaxKind.OverrideKeyword));
            }
        }

        public static void FixOverride(this SyntaxListBuilder list, bool enforceOverride)
        {
            if (enforceOverride ||
                !list.CanBeVirtual() ||
                list.Any((int)SyntaxKind.OverrideKeyword) ||
                list.Any((int)SyntaxKind.NewKeyword))
                return;
            list.Add(SyntaxFactory.MakeGeneratedToken(SyntaxKind.OverrideKeyword));
        }

        public static int GetVisibilityLevel(this SyntaxListBuilder list)
        {
            if (list.Any((int)SyntaxKind.PublicKeyword))
                return 0;
            if (list.Any((int)SyntaxKind.ProtectedKeyword))
            {
                if (list.Any((int)SyntaxKind.InternalKeyword))
                    return 1;
                else
                    return 2;
            }
            if (list.Any((int)SyntaxKind.InternalKeyword))
                return 2;
            if (list.Any((int)SyntaxKind.PrivateKeyword))
                return 3;
            return 0;
        }

        public static IToken GetLiteralToken(this IParseTree expr)
        {
            var pe = expr as XSharpParser.PrimaryExpressionContext;
            if (pe != null)
            {
                if (pe.Expr is XSharpParser.LiteralExpressionContext)
                {
                    var lit = pe.Expr as XSharpParser.LiteralExpressionContext;
                    var lv = lit.Literal;
                    return lv.Token;
                }
                if (pe.Expr is XSharpParser.ParenExpressionContext)
                {
                    var paren = pe.Expr as XSharpParser.ParenExpressionContext;
                    return paren.Expr.GetLiteralToken();
                }
            }
            return null;
        }

        public static bool IsLiteralString(this IParseTree expr)
        {
            var token = expr.GetLiteralToken();
            if (token != null)
            {
                return XSharpLexer.IsString(token.Type);
            }
            return false;
        }

        public static bool IsIdentifier(this ParserRuleContext context)
        {
            return context.Start == context.Stop && context.Start.Type == XSharpParser.ID;
        }
        public static bool IsAliasExpression(this IXParseTree context)
        {
            if (context is XSharpParser.PrimaryExpressionContext)
            {
                return context.GetChild(0) is XSharpParser.AliasedExpressionContext;
            }
            else if (context is XSharpParser.ArrayElementContext aec)
            {
                return aec.Expr.IsAliasExpression();
            }
            return false;
        }
#endif

    }
}
