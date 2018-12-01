using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.MacroCompiler
{
    using Syntax;
    using static Syntax.TokenAttr;

    partial class Lexer
    {
        // Input source
        string _Source;

        // Configuration
        MacroOptions _options;

        // Lexer state
        TokenType _lastToken = TokenType.NL;
        int _index = 0;
        bool _inDottedIdentifier = false;
        bool _hasEos = true;
        bool _inPp = false;

        // Lexer stats
        internal bool HasPreprocessorTokens = false;
        internal bool HasPPDefines = false;
        internal bool HasPPIncludes = false;
        internal bool HasPPMessages = false;
        internal bool HasPPRegions = false;
        internal bool HasPPIfdefs = false;
        internal bool HasPPUDCs = false;

        internal Lexer(string source, MacroOptions options)
        {
            _Source = source;
            _options = options;
        }

        internal bool AllowFourLetterAbbreviations
        {
            get { return _options.AllowFourLetterAbbreviations; }
        }
        internal bool AllowSingleQuotedStrings
        {
            get { return _options.AllowSingleQuotedStrings; }

        }
        internal bool AllowOldStyleComments
        {
            get { return _options.AllowOldStyleComments; }
        }

        IDictionary<string, TokenType> KwIds
        {
            get
            {
                return AllowFourLetterAbbreviations ? voKwIds : xsKwIds;
            }
        }

        IDictionary<string, TokenType> SymIds
        {
            get
            {
                return symIds;
            }
        }

        char La()
        {
            return _index < _Source.Length ? _Source[_index] : (char)0;
        }

        char La(int n)
        {
            return (_index+n-1) < _Source.Length ? _Source[_index+n-1] : (char)0;
        }

        bool Eoi()
        {
            return _index >= _Source.Length;
        }

        void Consume()
        {
            _index++;
        }

        void Consume(int n)
        {
            _index += n;
        }

        void Rewind(int pos)
        {
            _index = pos;
        }

        bool Expect(char c)
        {
            if (La() == c)
            {
                Consume();
                return true;
            }
            return false;
        }

        bool Expect(char c1, char c2)
        {
            if (La() == c1 && La(2) == c2)
            {
                Consume(2);
                return true;
            }
            return false;
        }

        bool ExpectEol()
        {
            bool r = false;
            if (La() == '\r')
            {
                Consume();
                r = true;
            }
            if (La() == '\n')
            {
                Consume();
                r = true;
            }
            return r;
        }

        bool ExpectAny(char c1, char c2)
        {
            var c = La();
            if (c == c1 || c == c2)
            {
                Consume();
                return true;
            }
            return false;
        }

        bool ExpectAny(char c1, char c2, char c3, char c4)
        {
            var c = La();
            if (c == c1 || c == c2 || c == c3 || c == c4)
            {
                Consume();
                return true;
            }
            return false;
        }

        bool ExpectRange(char c1, char c2)
        {
            if (La() >= c1 && La() <= c2)
            {
                Consume();
                return true;
            }
            return false;
        }

        bool ExpectIdStart()
        {
            var c = La();
            if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
                || (c >= '\u00C0' && c <= '\u00D6') || (c >= '\u00D8' && c <= '\u00F6')
                || (c >= '\u00F8' && c <= '\u02FF') || (c >= '\u0370' && c <= '\u037D')
                || (c >= '\u037F' && c <= '\u1FFF') || (c >= '\u200C' && c <= '\u200D'))
            {
                Consume();
                return true;
            }
            return false;
        }

        bool ExpectIdChar()
        {
            var c = La();
            if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
                    || (c >= '\u00C0' && c <= '\u00D6') || (c >= '\u00D8' && c <= '\u00F6')
                    || (c >= '\u00F8' && c <= '\u02FF') || (c >= '\u0370' && c <= '\u037D')
                    || (c >= '\u037F' && c <= '\u1FFF') || (c >= '\u200C' && c <= '\u200D')
                    || c == '\u00B7' || (c >= '\u0300' && c <= '\u036F') || (c >= '\u203F' && c <= '\u2040'))
            {
                Consume();
                return true;
            }
            return false;
        }

        bool ExpectLower(string s)
        {
            if (char.ToLower(La()) == s[0])
            {
                for(int i=1;i<=s.Length;i++)
                {
                    if (char.ToLower(La(i)) != s[i-1])
                        return false;
                }
                Consume(s.Length);
                return true;
            }
            return false;
        }

        bool Reach(char c)
        {
            if (!Eoi() && La() != c)
            {
                Consume();
                return false;
            }
            return true;
        }

        bool ReachEsc(char c)
        {
            if (!Eoi() && La() != c)
            {
                var esc = La() == '\\';
                Consume();
                if (!Eoi() && esc)
                    Consume();
                return false;
            }
            return true;
        }

        bool ReachEol()
        {
            if (!Eoi())
            {
                var c = La();
                if (c != '\r' && c == '\n')
                {
                    Consume();
                    return false;
                }
            }
            return true;
        }

        bool Reach(char c1, char c2)
        {
            if (!Eoi())
            {
                if (La() != c1 && La(2) == c2)
                {
                    Consume();
                    return false;
                }
            }
            return true;
        }

        internal void Reset()
        {
            _lastToken = TokenType.NL;
            _index = 0;
        }

        internal IList<Token> AllTokens()
        {
            var l = new List<Token>();
            Token t;
            while ((t = NextToken()) != null)
                l.Add(t);
            return l;
        }

        internal string GetText(Token t)
        {
            return _Source.Substring(t.start, t.length);
        }

        internal Token NextToken()
        {
            if (!Eoi())
            {
                do
                {
                    int start = _index;
                    TokenType t = TokenType.UNRECOGNIZED;
                    TokenType st = TokenType.UNRECOGNIZED;
                    Channel ch = Channel.DEFOUTCHANNEL;
                    string value = null;

                    var c = La();
                    if (c < 128)
                    {
                        t = specialTable[c];
                        Consume();
                    }
                    else
                    {
                        if (ExpectIdStart())
                            t = TokenType.ID;
                        else
                            Consume();
                    }

                    switch (t)
                    {
                        case TokenType.COLON:
                            if (Expect(':')) t = TokenType.COLONCOLON;
                            else if (Expect('=')) t = TokenType.ASSIGN_OP;
                            break;
                        case TokenType.PIPE:
                            if (Expect('|')) t = TokenType.OR;
                            else if (Expect('=')) t = TokenType.ASSIGN_BITOR;
                            break;
                        case TokenType.AMP:
                            if (Expect('&'))
                            {
                                if (AllowOldStyleComments)
                                {
                                    t = TokenType.SL_COMMENT;
                                    ch = Channel.HIDDENCHANNEL;
                                    while (!ReachEol()) ;
                                    break;
                                }
                                t = TokenType.AND;
                            }
                            else if (Expect('=')) t = TokenType.ASSIGN_BITAND;
                            break;
                        case TokenType.ADDROF:
                            if (Expect('@'))
                            {
                                t = TokenType.ID;
                                goto case TokenType.ID;
                            }
                            break;
                        case TokenType.MINUS:
                            if (Expect('>')) t = TokenType.ALIAS;
                            else if (Expect('-')) t = TokenType.DEC;
                            else if (Expect('=')) t = TokenType.ASSIGN_SUB;
                            break;
                        case TokenType.PLUS:
                            if (Expect('+')) t = TokenType.INC;
                            else if (Expect('=')) t = TokenType.ASSIGN_ADD;
                            break;
                        case TokenType.DIV:
                            if (Expect('=')) t = TokenType.ASSIGN_DIV;
                            else if (Expect('/'))
                            {
                                t = TokenType.SL_COMMENT;
                                ch = Channel.HIDDENCHANNEL;
                                if (Expect('/'))
                                {
                                    t = TokenType.DOC_COMMENT;
                                    ch = Channel.XMLDOCCHANNEL;
                                }
                                while (!ReachEol()) ;
                            }
                            else if (Expect('*'))
                            {
                                t = TokenType.ML_COMMENT;
                                ch = Channel.HIDDENCHANNEL;
                                while (!Reach('*','/')) ;
                                Expect('*', '/');
                            }
                            break;
                        case TokenType.MOD:
                            if (Expect('=')) t = TokenType.ASSIGN_MOD;
                            break;
                        case TokenType.EXP:
                            if (Expect('=')) t = TokenType.ASSIGN_EXP;
                            break;
                        case TokenType.LT:
                            if (Expect('<')) { t = TokenType.LSHIFT; if (Expect('=')) t = TokenType.ASSIGN_LSHIFT; }
                            else if (Expect('=')) t = TokenType.LTE;
                            else if (Expect('>')) t = TokenType.NEQ;
                            break;
                        case TokenType.GT:
                            if (Expect('=')) t = TokenType.GTE;
                            else if (Expect('>', '=')) t = TokenType.ASSIGN_RSHIFT;
                            break;
                        case TokenType.TILDE:
                            if (Expect('=')) t = TokenType.ASSIGN_XOR;
                            if (Expect('"'))
                            {
                                t = TokenType.WS;
                                ch = Channel.HIDDENCHANNEL;
                                while (!Reach('"')) ;
                                Expect('"');
                            }
                            break;
                        case TokenType.MULT:
                            if (_lastToken == TokenType.NL)
                            {
                                t = TokenType.SL_COMMENT;
                                ch = Channel.HIDDENCHANNEL;
                                while (!ReachEol()) ;
                            }
                            if (Expect('=')) t = TokenType.ASSIGN_MUL;
                            else if (Expect('*')) { t = TokenType.EXP; if (Expect('=')) t = TokenType.ASSIGN_EXP; }
                            break;
                        case TokenType.QMARK:
                            if (Expect('?')) t = TokenType.QQMARK;
                            break;
                        case TokenType.EQ:
                            if (Expect('=')) t = TokenType.EEQ;
                            else if (Expect('>')) t = TokenType.UDCSEP;
                            break;
                        case TokenType.NOT:
                            if (Expect('=')) t = TokenType.NEQ;
                            break;
                        case TokenType.SEMI:
                            while (ExpectAny(' ', '\t')) ;
                            if (Expect('/','/'))
                            {
                                t = TokenType.LINE_CONT;
                                ch = Channel.HIDDENCHANNEL;
                                while (!ReachEol()) ;
                            }
                            else if (AllowOldStyleComments && Expect('&', '&'))
                            {
                                t = TokenType.LINE_CONT_OLD;
                                ch = Channel.HIDDENCHANNEL;
                                while (!ReachEol()) ;
                            }
                            if (ExpectEol())
                            {
                                if (t == TokenType.SEMI) t = TokenType.LINE_CONT;
                                ch = Channel.HIDDENCHANNEL;
                            }
                            if (t == TokenType.SEMI && _index > start+1)
                            {
                                Rewind(start + 1);
                            }
                            break;
                        case TokenType.DOT:
                            if (La() >= '0' && La() <= '9') goto case TokenType.REAL_CONST;
                            if (!_inDottedIdentifier)
                            {
                                if (La(2) == '.')
                                {
                                    if (ExpectAny('F', 'f', 'N', 'n')) { Consume(); t = TokenType.FALSE_CONST; }
                                    else if (ExpectAny('T', 't', 'Y', 'y')) { Consume(); t = TokenType.TRUE_CONST; }
                                    else if (Expect('.')) { Consume(); t = TokenType.ELLIPSIS; }
                                }
                                else if (La(3) == '.')
                                {
                                    if (ExpectLower("or")) { Consume(); t = TokenType.LOGIC_OR; }
                                }
                                else if (La(4) == '.')
                                {
                                    if (ExpectLower("and")) { Consume(); t = TokenType.LOGIC_AND; }
                                    else if (ExpectLower("not")) { Consume(); t = TokenType.LOGIC_NOT; }
                                    else if (ExpectLower("xor")) { Consume(); t = TokenType.LOGIC_XOR; }
                                }
                            }
                            break;
                        case TokenType.NL:
                            if (c == '\r') Expect('\n');
                            break;
                        case TokenType.WS:
                            ch = Channel.HIDDENCHANNEL;
                            while (ExpectAny(' ', '\t')) ;
                            break;
                        case TokenType.NEQ2:
                            if (ExpectIdStart())
                            {
                                t = TokenType.SYMBOL_CONST;
                                while (ExpectIdChar()) ;
                                value = _Source.Substring(start, _index - start);
                                {
                                    TokenType tt;
                                    if (symIds.TryGetValue(value, out tt))
                                    {
                                        if (tt >= TokenType.FIRST_NULL && t <= TokenType.LAST_NULL)
                                        {
                                            t = TokenType.NEQ2;
                                            value = null;
                                            Rewind(start + 1);
                                        }
                                        else
                                        {
                                            t = tt;
                                            if (tt >= TokenType.PP_FIRST && t <= TokenType.PP_LAST)
                                            {
                                                _inPp = true;
                                                HasPreprocessorTokens = true;
                                                switch (tt)
                                                {
                                                    case TokenType.PP_COMMAND:
                                                    case TokenType.PP_TRANSLATE:
                                                        HasPPUDCs = true;
                                                        break;
                                                    case TokenType.PP_IFDEF:
                                                    case TokenType.PP_IFNDEF:
                                                    case TokenType.PP_ELSE:
                                                    case TokenType.PP_ENDIF:
                                                        HasPPIfdefs = true;
                                                        break;
                                                    case TokenType.PP_REGION:
                                                    case TokenType.PP_ENDREGION:
                                                        HasPPRegions = true;
                                                        break;
                                                    case TokenType.PP_ERROR:
                                                    case TokenType.PP_WARNING:
                                                        HasPPMessages = true;
                                                        break;
                                                    case TokenType.PP_INCLUDE:
                                                        HasPPIncludes = true;
                                                        break;
                                                    case TokenType.PP_DEFINE:
                                                    case TokenType.PP_UNDEF:
                                                        HasPPDefines = true;
                                                        break;
                                                    case TokenType.PP_LINE:
                                                    default:
                                                        break;
                                                }
                                            }
                                            else if (tt == TokenType.PRAGMA)
                                            {
                                                ch = Channel.PRAGMACHANNEL;
                                                while (!ReachEol()) ;
                                            }
                                        }
                                    }
                                }
                            }
                            break;
                        case TokenType.ID:
                            if (c == 'c' || c == 'C')
                            {
                                if (La() == '"' || La() == '\'') { Consume(); goto case TokenType.CHAR_CONST; }
                            }
                            else if (c == 'e' || c == 'E')
                            {
                                if (La() == '"') { Consume(); goto case TokenType.ESCAPED_STRING_CONST; } // escaped string
                                if ((La() == 'i' || La() == 'I') && La(2) == '"') { Consume(2); goto case TokenType.INTERPOLATED_STRING_CONST; } // interpolated escaped string
                            }
                            else if (c == 'i' || c == 'I')
                            {
                                if (La() == '"') { Consume(); goto case TokenType.INTERPOLATED_STRING_CONST; } // interpolated string
                                if ((La() == 'e' || La() == 'E') && La(2) == '"') { Consume(2); goto case TokenType.INTERPOLATED_STRING_CONST; } // interpolated escaped string
                            }
                            {
                                while (ExpectIdChar()) ;
                                bool nokw = _Source[start] == '@';
                                int idStart = nokw ? start + 2 : start;
                                value = _Source.Substring(idStart, _index - idStart);
                                if (!nokw)
                                {
                                    TokenType tt;
                                    if (KwIds.TryGetValue(value, out tt))
                                    {
                                        t = tt;
                                        if (IsSoftKeyword(tt))
                                            st = TokenType.ID;
                                    }
                                }
                            }
                            break;
                        case TokenType.INT_CONST:
                            if (c == '0')
                            {
                                if (ExpectAny('X','x'))
                                {
                                    while (ExpectRange('0', '9') || ExpectRange('A', 'F') || ExpectRange('a', 'f')) ;
                                    ExpectAny('U', 'u', 'L', 'l');
                                }
                                if (ExpectAny('B', 'b'))
                                {
                                    while (ExpectRange('0', '1')) ;
                                    ExpectAny('U', 'u');
                                }
                            }
                            else
                            {
                                while (ExpectRange('0', '9')) ;
                                c = La();
                                if (c == '.' && (La(2) >= '0' && La(2) <= '9')) { Consume(); goto case TokenType.REAL_CONST; }
                                if (La() == 'E' || La() == 'e') goto case TokenType.REAL_CONST_EXP;
                                ExpectAny('U', 'u', 'L', 'l');
                            }
                            value = _Source.Substring(start, _index - start);
                            break;
                        case TokenType.REAL_CONST:
                            t = TokenType.REAL_CONST;
                            while (ExpectRange('0', '9')) ;
                            if (La() == '.' && (La(2) >= '0' && La(2) <= '9')) goto case TokenType.DATE_CONST;
                            if (La() == 'E' || La() == 'e') goto case TokenType.REAL_CONST_EXP;
                            if (!ExpectAny('S', 's', 'D', 'd'))
                                ExpectAny('M', 'm');
                            value = _Source.Substring(start, _index - start);
                            break;
                        case TokenType.REAL_CONST_EXP:
                            if (La() == 'E' || La() == 'e')
                            {
                                c = La(2);
                                if (c == '+' || c == '-' || (c >= '0' && c <= '9'))
                                {
                                    t = TokenType.REAL_CONST;
                                    Consume(2);
                                    while (ExpectRange('0', '9')) ;
                                    ExpectAny('S', 's', 'D', 'd');
                                }
                            }
                            value = _Source.Substring(start, _index - start);
                            break;
                        case TokenType.DATE_CONST:
                            {
                                string s = _Source.Substring(start, _index - start);
                                int z0 = s.IndexOf('.');
                                if (z0 > 0 && s.Length - z0 > 1 && s.Length - z0 <= 3 && s.Length <= 7)
                                {
                                    t = TokenType.DATE_CONST;
                                    Expect('.');
                                    ExpectRange('0', '9');
                                    ExpectRange('0', '9');
                                }
                            }
                            value = _Source.Substring(start, _index - start);
                            break;
                        case TokenType.CHAR_CONST:
                            t = TokenType.CHAR_CONST;
                            if (La() == '\\' && La(3) == '\'') Consume(3);
                            else { while (!Reach('\'')) ; if (!Expect('\'')) t = TokenType.INCOMPLETE_STRING_CONST; }
                            value = _Source.Substring(start, _index - start);
                            break;
                        case TokenType.STRING_CONST_SINGLE:
                            if (!AllowSingleQuotedStrings)
                                goto case TokenType.CHAR_CONST;
                            t = TokenType.STRING_CONST;
                            while (!Reach('\'')) ;
                            if (!Expect('\'')) t = TokenType.INCOMPLETE_STRING_CONST;
                            value = _Source.Substring(start, _index - start);
                            break;
                        case TokenType.STRING_CONST:
                            while (!Reach('"')) ;
                            if (!Expect('"')) t = TokenType.INCOMPLETE_STRING_CONST;
                            value = _Source.Substring(start, _index - start);
                            break;
                        case TokenType.ESCAPED_STRING_CONST:
                            t = TokenType.ESCAPED_STRING_CONST;
                            while (!ReachEsc('"')) ;
                            if (!Expect('"')) t = TokenType.INCOMPLETE_STRING_CONST;
                            value = _Source.Substring(start, _index - start);
                            break;
                        case TokenType.INTERPOLATED_STRING_CONST:
                            t = TokenType.INTERPOLATED_STRING_CONST;
                            while (!ReachEsc('"')) ;
                            if (!Expect('"')) t = TokenType.INCOMPLETE_STRING_CONST;
                            value = _Source.Substring(start, _index - start);
                            break;
                    }

                    if (!_inDottedIdentifier)
                    {
                        // Check if the current token is a valid Identifier (starts with A..Z or _) and is followed by a DOT
                        // In that case we change the type from Keyword to ID
                        if (st == TokenType.ID && La() == '.')
                        {
                            if (t != TokenType.SELF && t != TokenType.SUPER)
                            {
                                t = TokenType.ID;
                                st = TokenType.UNRECOGNIZED;
                            }
                            _inDottedIdentifier = true;
                        }
                        else if (t == TokenType.ID)
                        {
                            _inDottedIdentifier = true;
                        }
                    }
                    else
                    {
                        if (st == TokenType.ID)
                        {
                            t = TokenType.ID;
                            st = TokenType.UNRECOGNIZED;
                            // keep _inDottedIdentifier true
                        }
                        else if (t != TokenType.DOT && t != TokenType.ID)
                        {
                            _inDottedIdentifier = false;
                        }
                    }

                    if (t == TokenType.NL || t == TokenType.SEMI)
                    {
                        if (_hasEos)
                        {
                            if (t == TokenType.SEMI)
                            {
                                if (_lastToken != TokenType.SEMI)
                                    ch = Channel.HIDDENCHANNEL;
                            }
                            else
                            {
                                ch = Channel.HIDDENCHANNEL;
                            }
                        }
                        else
                        {
                            t = TokenType.EOS;
                            _hasEos = true;
                        }
                    }
                    else if (_hasEos && ch == Channel.DEFOUTCHANNEL)
                    {
                        _hasEos = false;
                    }

                    if (_inPp)
                    {
                        if (ch == Channel.DEFOUTCHANNEL)
                        {
                            ch = Channel.PREPROCESSORCHANNEL;
                            if (t == TokenType.NL || Eoi())
                                _inPp = false;
                        }
                    }

                    if (ch == Channel.DEFOUTCHANNEL)
                    {
                        _lastToken = t;
                        return new Token(t, st, start, _index - start, value, ch);
                    }
                } while (true);
            }
            if (!_hasEos)
            {
                _hasEos = true;
                return new Token(TokenType.EOS, TokenType.UNRECOGNIZED, _index, _index, null, Channel.DEFOUTCHANNEL);
            }
            return null;
        }
    }
}
