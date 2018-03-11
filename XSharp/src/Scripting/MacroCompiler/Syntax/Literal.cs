using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler.Syntax
{
    static class Literals
    {
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

        internal static char CharValue(string text)
        {
            int p = 1;
            return EscapedChar(text, ref p);
        }

        internal static string StringValue(string text)
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

        internal static long BinValue(string text)
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
    }
}