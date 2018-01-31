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
using System.Collections.Immutable;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp;
using System;
using Antlr4.Runtime;
namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser
{


    public partial class XSharpLexer 
    {
        // Several Help methods that can be used for colorizing in an editor
        public static bool IsKeyword(int iToken)
        {
            return iToken > XSharpLexer.FIRST_KEYWORD && iToken < XSharpLexer.LAST_KEYWORD;
        }
        public static bool IsOperator(int iToken)
        {
            return (iToken > XSharpLexer.FIRST_OPERATOR && iToken < XSharpLexer.LAST_OPERATOR)
                || (iToken > XSharpLexer.PP_FIRST && iToken < XSharpLexer.PP_LAST)
                || iToken == XSharpLexer.SEMI;
        }
        public static bool IsConstant(int iToken)
        {
            return (iToken > XSharpLexer.FIRST_CONSTANT && iToken < XSharpLexer.LAST_CONSTANT)
                || (iToken > XSharpLexer.FIRST_NULL && iToken < XSharpLexer.LAST_NULL)
                || iToken == XSharpLexer.MACRO;
        }
        public static bool IsIdentifier(int iToken)
        {
            return iToken == XSharpLexer.ID || iToken == XSharpLexer.KWID;
        }

        public static bool IsType(int iToken)
        {
            return (iToken > XSharpLexer.FIRST_TYPE && iToken < XSharpLexer.LAST_TYPE);
        }


        public static bool IsComment(int iToken)
        {
            return iToken == XSharpLexer.SL_COMMENT || iToken == XSharpLexer.ML_COMMENT || iToken == XSharpLexer.DOC_COMMENT;
        }

        private int fixPositionalKeywords(int keyword)
        {
            switch (keyword)
            {
                //case CONSTRUCTOR: to many prefixes allowed (EOS, modifiers, attributes etc)
                //case EVENT: to many prefixes allowed (EOS, modifiers, attributes etc)
                //case ENUM: to many prefixes allowed (EOS, modifiers, attributes etc)
                //case PROPERTY: to many prefixes allowed (EOS, END, modifiers, attributes etc)
                // case GET many options: after CRLF or SET or ID or Type ....
                // case SET many options: after CRLF or GET or ID or Type ....
                // case ADD many options: after CRLF or REMOVE or ID or Type ....
                // case REMOVE many options: after CRLF or ADD or ID or Type ....
                case EXPLICIT:
                case IMPLICIT:
                    if (_lastToken != OPERATOR)
                    {
                        return ID;
                    }
                    break;
                case DESTRUCTOR:
                    // can also appear after attribute
                    if (_lastToken != EOS && _lastToken != NL && _lastToken != EXTERN && _lastToken != RBRKT)
                    {
                        return ID;
                    }
                    break;
                case FINALLY:
                case CATCH:
                case REPEAT:
                case UNTIL:
                case YIELD:
                    if (_lastToken != EOS && _lastToken != NL)
                    {
                        return ID;
                    }
                    break;
                case SWITCH:  
                    if (_lastToken != EOS && _lastToken != NL && _lastToken != BEGIN && _lastToken != DO && _lastToken != END)
                    {
                        return ID;
                    }
                    break;
                case IMPLIED:
                case VAR:
                    if (_lastToken != EOS && _lastToken != NL && _lastToken != LOCAL && _lastToken != STATIC
                        && _lastToken != FOR && _lastToken != FOREACH && _lastToken != USING)
                    {
                        return ID;
                    }
                    break;
                case NAMESPACE:
                case SCOPE:
                case LOCK:
                    if (_lastToken != BEGIN && _lastToken != END )
                    {
                        return ID;
                    }
                    break;

            }
            return keyword;
        }

        public bool HasPreprocessorTokens => _hasPPTokens;
        bool _inId = false;
        bool _inPp = false;
        bool _hasEos = true;
        bool _hasPPTokens = false;
        private bool _isKw(IToken t)
        {
            switch (t.Channel)
            {
                case XSharpLexer.Hidden: // 1
                case XSharpLexer.XMLDOCCHANNEL:  // 2
                case XSharpLexer.DEFOUTCHANNEL: // 3
                case XSharpLexer.PRAGMACHANNEL: // 5
                    return false;
                case XSharpLexer.PREPROCESSORCHANNEL:  // 4
                case TokenConstants.DefaultChannel: // 0
                default:
                    char fc = t.Text?[0] ?? (Char)0;
                    return fc == '_' || (fc >= 'A' && fc <= 'Z') || (fc >= 'a' && fc <= 'z');
            }
        }
        IList<ParseErrorData> _lexErrors = new List<ParseErrorData>();
        internal IList<ParseErrorData> LexErrors { get { return _lexErrors; } }

        int _lastToken = NL;
        System.Text.StringBuilder _textSb = new System.Text.StringBuilder();

        private Tuple<ITokenSource, ICharStream> _sourcePair;
        public Tuple<ITokenSource, ICharStream> SourcePair
        {
            get
            {
                if (_sourcePair == null)
                {
                    _sourcePair = new Tuple<ITokenSource, ICharStream>(this, (ICharStream)this.InputStream);
                }
                return _sourcePair;
            }
        }
        public override IToken NextToken()
        {
            XSharpToken t;
            {
                var _startCharIndex = InputStream.Index;
                var _startColumn = Interpreter.Column;
                var _startLine = Interpreter.Line;
                int _type = -1;
                int _channel = TokenConstants.DefaultChannel;
                int c = InputStream.La(1);
                switch (c)
                {
                    case '(':
                        _type = LPAREN;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        break;
                    case ')':
                        _type = RPAREN;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        break;
                    case '{':
                        _type = LCURLY;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        break;
                    case '}':
                        _type = RCURLY;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        break;
                    case '[':
                        _type = LBRKT;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        break;
                    case ']':
                        _type = RBRKT;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        break;
                    case ':':
                        _type = COLON;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == ':')
                        {
                            _type = COLONCOLON;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        else if (c == '=')
                        {
                            _type = ASSIGN_OP;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        break;
                    case ',':
                        _type = COMMA;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        break;
                    case '\\':       // used inside #command to escape '<'
                        _type = BACKSLASH;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        break;
                    case '|':
                        _type = PIPE;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == '|')
                        {
                            _type = OR;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        else if (c == '=')
                        {
                            _type = ASSIGN_BITOR;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        break;
                    case '&':
                        if (_OldComment && InputStream.La(2) == '&')
                            break;
                        _type = AMP;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == '&')
                        {
                            _type = AND;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        else if (c == '=')
                        {
                            _type = ASSIGN_BITAND;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        break;
                    case '@':
                        if (InputStream.La(2) == '@')
                        {
                            break;
                        }
                        _type = ADDROF;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        break;
                    case '-':
                        _type = MINUS;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == '>')
                        {
                            _type = ALIAS;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        else if (c == '-')
                        {
                            _type = DEC;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        else if (c == '=')
                        {
                            _type = ASSIGN_SUB;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        break;
                    case '+':
                        _type = PLUS;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == '+')
                        {
                            _type = INC;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        else if (c == '=')
                        {
                            _type = ASSIGN_ADD;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        break;

                    case '/':
                        if (InputStream.La(2) == '/' || InputStream.La(2) == '*')
                        {
                            break;
                        }
                        _type = DIV;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == '=')
                        {
                            _type = ASSIGN_DIV;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        break;
                    case '%':
                        _type = MOD;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == '=')
                        {
                            _type = ASSIGN_MOD;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        break;
                    case '^':
                        _type = EXP;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == '=')
                        {
                            _type = ASSIGN_EXP;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        break;
                    case '<':
                        _type = LT;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == '<')
                        {
                            _type = LSHIFT;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                            c = InputStream.La(1);
                            if (c == '=')
                            {
                                _type = ASSIGN_LSHIFT;
                                _textSb.Append((char)c);
                                InputStream.Consume();
                            }
                        }
                        else if (c == '=')
                        {
                            _type = LTE;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        else if (c == '>')
                        {
                            _type = NEQ;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        break;
                    case '>':
                        _type = GT;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        // GreaterThanGreaterThanToken is synthesized in the parser since it is ambiguous (with closing nested type parameter lists)
                        if (c == '>' && InputStream.La(2) == '=')
                        {
                            _textSb.Append((char)c);    // >
                            InputStream.Consume();
                            c = InputStream.La(1);
                            _textSb.Append((char)c);    // =
                            InputStream.Consume();
                            _type = ASSIGN_RSHIFT;
                            InputStream.Consume();
                        }
                        else if (c == '=')
                        {
                            _type = GTE;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        break;
                    case '~':
                        _type = TILDE;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == '=')
                        {
                            _type = ASSIGN_XOR;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        if (c == '"')           // Old Style Pragma like ~"ONLYEARLY+", treat it as whitespace
                        {
                            _type = WS;
                            _channel = TokenConstants.HiddenChannel;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                            while (true)
                            {
                                c = InputStream.La(1);
                                InputStream.Consume();
                                _textSb.Append((char)c);
                                if (c == '"')
                                    break;
                            }
                        }
                        break;
                    case '*':
                        if (LastToken == NL)
                            break;
                        _type = MULT;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == '=')
                        {
                            _type = ASSIGN_MUL;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        else if (c == '*')
                        {
                            _type = EXP;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                            c = InputStream.La(1);
                            if (c == '=')
                            {
                                _type = ASSIGN_EXP;
                                _textSb.Append((char)c);
                                InputStream.Consume();
                            }
                        }
                        break;
                    case '?':
                        _type = QMARK;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == '?')
                        {
                            _type = QQMARK;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        break;
                    case '=':
                        _type = EQ;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == '=')
                        {
                            _type = EEQ;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        else if (c == '>')
                        {
                            _type = UDCSEP;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        break;
                    case '$':
                        _type = SUBSTR;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        break;
                    case '!':
                        _type = NOT;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        if (c == '=')
                        {
                            _type = NEQ;
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        break;
                    case '.':
                        if (InputStream.La(2) >= '0' && InputStream.La(2) <= '9')
                        {
                            break;
                        }
                        _type = DOT;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        if (!_inId)
                        {
                            if (InputStream.La(2) == '.')
                            {
                                c = InputStream.La(1);
                                if (c == 'F' || c == 'N' || c == 'f' || c == 'n')
                                {
                                    _type = FALSE_CONST;
                                    _textSb.Append((char)c);
                                    InputStream.Consume();
                                    _textSb.Append('.');
                                    InputStream.Consume();
                                }
                                else if (c == 'T' || c == 'Y' || c == 't' || c == 'y')
                                {
                                    _type = TRUE_CONST;
                                    _textSb.Append((char)c);
                                    InputStream.Consume();
                                    _textSb.Append('.');
                                    InputStream.Consume();
                                }
                                else if (c == '.')
                                {
                                    _type = ELLIPSIS;
                                    _textSb.Append('.');
                                    InputStream.Consume();
                                    _textSb.Append('.');
                                    InputStream.Consume();
                                }
                            }
                            else if (InputStream.La(3) == '.')
                            {
                                c = InputStream.La(1);
                                var c2 = InputStream.La(2);
                                if ((c == 'O' || c == 'o') && (c2 == 'R' || c2 == 'r'))
                                {
                                    _type = LOGIC_OR;
                                    _textSb.Append((char)c);
                                    InputStream.Consume();
                                    _textSb.Append((char)c2);
                                    InputStream.Consume();
                                    _textSb.Append('.');
                                    InputStream.Consume();
                                }
                            }
                            else if (InputStream.La(4) == '.')
                            {
                                c = InputStream.La(1);
                                var c2 = InputStream.La(2);
                                var c3 = InputStream.La(3);
                                if ((c == 'A' || c == 'a') && (c2 == 'N' || c2 == 'n') && (c3 == 'D' || c3 == 'd'))
                                {
                                    _type = LOGIC_AND;
                                    _textSb.Append((char)c);
                                    InputStream.Consume();
                                    _textSb.Append((char)c2);
                                    InputStream.Consume();
                                    _textSb.Append((char)c3);
                                    InputStream.Consume();
                                    _textSb.Append('.');
                                    InputStream.Consume();
                                }
                                else if ((c == 'N' || c == 'n') && (c2 == 'O' || c2 == 'o') && (c3 == 'T' || c3 == 't'))
                                {
                                    _type = LOGIC_NOT;
                                    _textSb.Append((char)c);
                                    InputStream.Consume();
                                    _textSb.Append((char)c2);
                                    InputStream.Consume();
                                    _textSb.Append((char)c3);
                                    InputStream.Consume();
                                    _textSb.Append('.');
                                    InputStream.Consume();
                                }
                                else if ((c == 'X' || c == 'x') && (c2 == 'O' || c2 == 'o') && (c3 == 'R' || c3 == 'r'))
                                {
                                    _type = LOGIC_XOR;
                                    _textSb.Append((char)c);
                                    InputStream.Consume();
                                    _textSb.Append((char)c2);
                                    InputStream.Consume();
                                    _textSb.Append((char)c3);
                                    InputStream.Consume();
                                    _textSb.Append('.');
                                    InputStream.Consume();
                                }
                            }
                        }
                        break;
                    case '\r':
                    case '\n':
                        _type = NL;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        if (c == '\r' && InputStream.La(1) == '\n')
                        {
                            c = InputStream.La(1);
                            _textSb.Append((char)c);
                            InputStream.Consume();
                        }
                        Interpreter.Line += 1;
                        c = InputStream.La(1);
                        Interpreter.Column = 0 - (InputStream.Index - _startCharIndex);
                        break;
                    case '\t':
                    case ' ':
                        _type = WS;
                        _channel = TokenConstants.HiddenChannel;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        while (c == ' ' || c == '\t')
                        {
                            _textSb.Append((char)c);
                            InputStream.Consume();
                            c = InputStream.La(1);
                        }
                        break;
                    case 'c':
                    case 'C':
                        if (InputStream.La(2) == '"' || InputStream.La(2) == '\'') // char const
                        {
                            break;
                        }
                        goto case 'a';
                    case 'e':
                    case 'E':
                        if (InputStream.La(2) == '"') // escaped string
                        {
                            break;
                        }
                        if ((InputStream.La(2) == 'i' || InputStream.La(2) == 'I') && InputStream.La(3) == '"') // interpolated escaped string
                        {
                            break;
                        }
                        goto case 'a';
                    case 'i':
                    case 'I':
                        if (InputStream.La(2) == '"') // interpolated string
                        {
                            break;
                        }
                        if ((InputStream.La(2) == 'e' || InputStream.La(2) == 'E') && InputStream.La(3) == '"') // interpolated escaped string
                        {
                            break;
                        }
                        goto case 'a';
                    case 'a':
                    case 'b':
                    case 'd':
                    case 'f':
                    case 'g':
                    case 'h':
                    case 'j':
                    case 'k':
                    case 'l':
                    case 'm':
                    case 'n':
                    case 'o':
                    case 'p':
                    case 'q':
                    case 'r':
                    case 's':
                    case 't':
                    case 'u':
                    case 'v':
                    case 'w':
                    case 'x':
                    case 'y':
                    case 'z':
                    case 'A':
                    case 'B':
                    case 'D':
                    case 'F':
                    case 'G':
                    case 'H':
                    case 'J':
                    case 'K':
                    case 'L':
                    case 'M':
                    case 'N':
                    case 'O':
                    case 'P':
                    case 'Q':
                    case 'R':
                    case 'S':
                    case 'T':
                    case 'U':
                    case 'V':
                    case 'W':
                    case 'X':
                    case 'Y':
                    case 'Z':
                    case '_':
                        _type = ID;
                        _textSb.Clear();
                        _textSb.Append((char)c);
                        InputStream.Consume();
                        c = InputStream.La(1);
                        while ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
                                || (c >= '\u00C0' && c <= '\u00D6') || (c >= '\u00D8' && c <= '\u00F6')
                                || (c >= '\u00F8' && c <= '\u02FF') || (c >= '\u0370' && c <= '\u037D')
                                || (c >= '\u037F' && c <= '\u1FFF') || (c >= '\u200C' && c <= '\u200D')
                                || c == '\u00B7' || (c >= '\u0300' && c <= '\u036F') || (c >= '\u203F' && c <= '\u2040')
                                )
                        {
                            _textSb.Append((char)c);
                            InputStream.Consume();
                            c = InputStream.La(1);
                        }
                        break;
                }
                if (_type >= 0)
                {
                    Interpreter.Column += (InputStream.Index - _startCharIndex);
                    t = TokenFactory.Create(this.SourcePair, _type, _textSb.ToString(), _channel, _startCharIndex, CharIndex - 1, _startLine, _startColumn) as XSharpToken;
                    Emit(t);
                }
                else
                {
                    t = base.NextToken() as XSharpToken;
                    if (t.Type == ML_COMMENT)
                    {
                        if (!t.Text.EndsWith("*/"))
                        {
                            _lexErrors.Add(new ParseErrorData(t, ErrorCode.ERR_OpenEndedComment));
                        }
                    }
                    if (t.Type == SYMBOL_CONST)
                    {
                        var text = t.Text.Substring(1);
                        if (KwIds.ContainsKey(text))
                        {
                            var kwid = KwIds[text];
                            if (kwid >= FIRST_NULL && kwid <= LAST_NULL && kwid != NULL)
                            {
                                // #NIL or #NULL_STRING etc., however #NULL must be allowed as Symbol
                                t.Text = "#";
                                t.Type = NEQ2;
                                t.StopIndex = t.StartIndex;
                                InputStream.Seek(t.StartIndex + 1);
                            }
                        }
                    }
                }
            }
            //System.Diagnostics.Debug.WriteLine("T[{0},{1}]:{2}",t.Line,t.Column,t.Text);
            var type = t.Type;
            if (type == ID)
            {
                int kwtype;
                if (KwIds.TryGetValue(t.Text, out kwtype))
                {
                    t.Type = kwtype;
                    t.Type = fixPositionalKeywords(t.Type);
                    type = t.Type;
                }
            }
            /*else if (type == KWID) {
                t.Type = ID;
            }*/
            else if (type == SYMBOL_CONST && (LastToken == NL || LastToken == UDCSEP))
            {
                int symtype;
                if (SymIds.TryGetValue(t.Text, out symtype))
                {
                    t.Type = symtype;
                    _inPp = true;
                    _hasPPTokens = true;
                }
                else if (_isScript)
                {
                    if (Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer.Equals(t.Text, "#R"))
                    {
                        t.Type = SCRIPT_REF;
                    }
                    else if (Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer.Equals(t.Text, "#LOAD"))
                    {
                        t.Type = SCRIPT_LOAD;
                    }
                }
            }
            if (!_inId)
            {
                if (_isKw(t) && InputStream.La(1) == (int)'.')
                {
                    if (t.Type != SELF && t.Type != SUPER)
                    {
                        t.Type = ID;
                    }
                    _inId = true;
                }
                else if (type == ID || type == KWID)
                    _inId = true;
            }
            else
            {
                if (_isKw(t))
                    t.Type = ID;
                else if (type != DOT && type != ID && type != KWID)
                    _inId = false;
            }
            if (type == NL || type == SEMI)
            {
                if (_hasEos)
                {
                    if (type == SEMI)
                    {
                        if (_lastToken != SEMI)
                            t.Channel = t.OriginalChannel = TokenConstants.HiddenChannel;
                    }
                    else
                        t.Channel = t.OriginalChannel = TokenConstants.HiddenChannel;
                }
                else
                {
                    t.Type = EOS;
                    _hasEos = true;
                }
            }
            else if (_hasEos && t.Channel == TokenConstants.DefaultChannel)
            {
                _hasEos = false;
            }
            else if (!_hasEos && type == Eof)
            {
                t.Type = EOS;
                _hasEos = true;
            }
            if (t.Channel == TokenConstants.DefaultChannel)
                _lastToken = type; // nvk: Note that this is the type before any modifications!!!

            if (_inPp)
            {
                // this is how a list of tokens for a #define will look like:
                // Token        Channel
                // #define      4
                // <space>      1
                // FOO          4
                // <space>      1
                // 1            4
                if (t.Channel == TokenConstants.DefaultChannel)
                {
                    t.Channel = t.OriginalChannel = PREPROCESSORCHANNEL;
                    if (type == NL || type == Eof)
                        _inPp = false;
                }
            }
            return t;
        }
        int LastToken
        {
            get { return _lastToken; }
        }

        bool _MacroLexer = false;
        public bool MacroLexer
        {
            get { return _MacroLexer; }
            set { _MacroLexer = value; }
        }

        bool _Four = false;
        public bool AllowFourLetterAbbreviations
        {
            get { return _Four; }
            set { _Four = value; }
        }
        bool _SingleQuotedStrings = false;
        public bool AllowSingleQuotedStrings
        {
            get { return _SingleQuotedStrings; }
            set { _SingleQuotedStrings = value; }

        }
        bool _OldComment = false;
        public bool AllowOldStyleComments
        {
            get { return _OldComment; }
            set { _OldComment = value; }
        }
        bool _isScript = false;
        public bool IsScript
        {
            get { return _isScript; }
            set { _isScript = value; }
        }
        static Object kwlock = new Object();
        static IDictionary<string, int> voKwIds = null;
        static IDictionary<string, int> xsKwIds = null;
        private IDictionary<string, int> _kwIds;

        private IDictionary<string, int> _getIds(bool lFour)
        {
            var ids = new Dictionary<string, int>(Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer);

            Dictionary<string, int> voKeywords = null;
            if (MacroLexer)
            {
                // short list of keywords used in Macro Expressions
                // keywords for statements and entities are not included
                voKeywords = new Dictionary<string, int>
                {
                    {"_AND", VO_AND},
                    {"BREAK", BREAK},
                    {"_CAST", CAST},
                    {"EXPORT", EXPORT},
                    {"FIELD", FIELD},
                    {"_FIELD", FIELD_},
                    {"IF", IF},
                    {"IIF", IIF},
                    {"IS", IS},
                    {"_NOT", VO_NOT},
                    {"_OR", VO_OR},
                    {"_XOR", VO_XOR},

			        // Predefined types
                    {"ARRAY", ARRAY},
                    {"BYTE", BYTE},
                    {"CODEBLOCK", CODEBLOCK},
                    {"DATE", DATE},
                    {"DWORD", DWORD},
                    {"FLOAT", FLOAT},
                    {"INT", INT},
                    {"LOGIC", LOGIC},
                    {"LONGINT", LONGINT},
                    {"OBJECT", OBJECT},
                    {"PSZ", PSZ},
                    {"PTR", PTR},
                    {"REAL4", REAL4},
                    {"REAL8", REAL8},
                    {"SHORTINT", SHORTINT},
                    {"STRING", STRING},
                    {"SYMBOL", SYMBOL},
                    {"USUAL", USUAL},
                    {"VOID", VOID},
                    {"WORD", WORD},

			        // Null types
			        {"NIL", NIL},
                    {"NULL", NULL},
                    {"NULL_ARRAY", NULL_ARRAY},
                    {"NULL_CODEBLOCK", NULL_CODEBLOCK},
                    {"NULL_DATE", NULL_DATE},
                    {"NULL_OBJECT", NULL_OBJECT},
                    {"NULL_PSZ", NULL_PSZ},
                    {"NULL_PTR", NULL_PTR},
                    {"NULL_STRING", NULL_STRING},
                    {"NULL_SYMBOL", NULL_SYMBOL},

			        // Consts
			        {"FALSE", FALSE_CONST},
                    {"TRUE", TRUE_CONST},

                };
            }
            else
            {
                voKeywords = new Dictionary<string, int>
            {
                {"ACCESS", ACCESS},
                {"ALIGN", ALIGN},
                {"_AND", VO_AND},
                {"AS", AS},
                {"ASPEN", ASPEN},
                {"ASSIGN", ASSIGN},
                {"BEGIN", BEGIN},
                {"BREAK", BREAK},
                {"CALLBACK", CALLBACK},
                {"CASE", CASE},
                {"_CAST", CAST},
                {"CLASS", CLASS},
                {"CLIPPER", CLIPPER},
                {"DECLARE", DECLARE},
                {"DEFINE", DEFINE},
                {"DIM", DIM},
                {"_DLL", DLL},
                {"DLLEXPORT", DLLEXPORT},
                {"DO", DO},
                {"DOWNTO", DOWNTO},
                {"ELSE", ELSE},
                {"ELSEIF", ELSEIF},
                {"END", END},
                {"ENDCASE", ENDCASE},
                {"ENDDO", ENDDO},
                {"ENDIF", ENDIF},
                {"EXIT", EXIT},
                {"EXPORT", EXPORT},
                {"FASTCALL", FASTCALL},
                {"FIELD", FIELD},
                {"_FIELD", FIELD_},
                {"FOR", FOR},
                {"FUNCTION", FUNCTION},
                {"GLOBAL", GLOBAL},
                {"HIDDEN", HIDDEN},
                {"IF", IF},
                {"IIF", IIF},
                {"IN", IN},
                {"INHERIT", INHERIT},
                //{"_INIT1", INIT1}, VO does not allow 4 letter abbreviations
                //{"_INIT2", INIT2}, VO does not allow 4 letter abbreviations
                //{"_INIT3", INIT3}, VO does not allow 4 letter abbreviations
                {"INSTANCE", INSTANCE},
                {"IS", IS},
                {"LOCAL", LOCAL},
                {"LOOP", LOOP},
                {"MEMBER", MEMBER},
                {"MEMVAR", MEMVAR},
                {"METHOD", METHOD},
                {"NEXT", NEXT},
                {"_NOT", VO_NOT},
                {"_OR", VO_OR},
                {"OTHERWISE", OTHERWISE},
                {"PARAMETERS", PARAMETERS},
                {"PASCAL", PASCAL},
                {"PRIVATE", PRIVATE},
                {"PROCEDURE", PROCEDURE},
                {"PROTECTED", PROTECTED},
                {"PUBLIC", PUBLIC},
                {"RECOVER", RECOVER},
                {"REF", REF},
                {"RETURN", RETURN},
                {"SELF", SELF},
                {"SEQUENCE", SEQUENCE},
                //{"_SIZEOF", SIZEOF},  VO does not allow 4 letter abbreviations
                {"STATIC", STATIC},
                {"STEP", STEP},
                {"STRICT", STRICT},
                {"SUPER", SUPER},
                {"THISCALL", THISCALL},
                {"TO", TO},
                //{"_TYPEOF", TYPEOF},  VO does not allow 4 letter abbreviations
                {"UNION", UNION},
                {"UPTO", UPTO},
                {"USING", USING},
                {"WINCALL", WINCALL},
                {"WHILE", WHILE},
                {"_XOR", VO_XOR},

			    // Predefined types
                {"ARRAY", ARRAY},
                {"BYTE", BYTE},
                {"CODEBLOCK", CODEBLOCK},
                {"DATE", DATE},
                {"DWORD", DWORD},
                {"FLOAT", FLOAT},
                {"INT", INT},
                {"LOGIC", LOGIC},
                {"LONGINT", LONGINT},
                {"OBJECT", OBJECT},
                {"PSZ", PSZ},
                {"PTR", PTR},
                {"REAL4", REAL4},
                {"REAL8", REAL8},
                {"SHORTINT", SHORTINT},
                {"STRING", STRING},
                {"SYMBOL", SYMBOL},
                {"USUAL", USUAL},
                {"VOID", VOID},
                {"WORD", WORD},

			    // Null types
			    {"NIL", NIL},
                {"NULL", NULL},
                {"NULL_ARRAY", NULL_ARRAY},
                {"NULL_CODEBLOCK", NULL_CODEBLOCK},
                {"NULL_DATE", NULL_DATE},
                {"NULL_OBJECT", NULL_OBJECT},
                {"NULL_PSZ", NULL_PSZ},
                {"NULL_PTR", NULL_PTR},
                {"NULL_STRING", NULL_STRING},
                {"NULL_SYMBOL", NULL_SYMBOL},

			    // Consts
			    {"FALSE", FALSE_CONST},
                {"TRUE", TRUE_CONST},


                };
            }
            if (!lFour )     
            {
                // These are predefined abbreviations of some keywords that are also valid in Vulcan
                if (!MacroLexer)
                {
                    voKeywords.Add("PROC", PROC);
                    voKeywords.Add("FUNC", FUNC);
                    voKeywords.Add("PROTECT", PROTECTED);
                }
                voKeywords.Add("SHORT", SHORTINT);
                voKeywords.Add("LONG", LONGINT);
                voKeywords.Add("_CODEBLOCK", CODEBLOCK);
            }
            foreach (var text in voKeywords.Keys)
            {
                var token = voKeywords[text];
                ids.Add(text, token);
                if (lFour )
                {
                    var s = text;
                    while (s.Length > 4)
                    {
                        s = s.Substring(0, s.Length - 1);
                        if (!ids.ContainsKey(s))
                            ids.Add(s, token);
                    }
                }
            }
            if (_Four)
            {
                ids.Add("ANY", USUAL);
            }
            Dictionary<string, int> keywords = null;
            if (MacroLexer)
            {
                keywords = new Dictionary<string, int>
                {
                    // short list of keywords used in Macro Expressions
                    // keywords for statements and entities are not included
                    // also the preprocessor macros are not included
                    // VO keywords that cannot be abbreviated

                    {"_SIZEOF", SIZEOF},
                    {"_TYPEOF", TYPEOF},  
                
                    // Vulcan keywords
                    {"CHAR", CHAR},
                    {"DEFAULT", DEFAULT},
                    {"SIZEOF", SIZEOF},
                    {"TYPEOF", TYPEOF},

			        // XSharp keywords
                    {"ASTYPE", ASTYPE},
                    {"ASYNC", ASYNC},
                    {"AWAIT", AWAIT},
                    {"CHECKED", CHECKED},
                    {"UNCHECKED", UNCHECKED},

			        // Vulcan types
			        {"INT64", INT64},
                    {"UINT64", UINT64},

			        // XSharp types
			        {"DYNAMIC", DYNAMIC},

                };
            }
            else
            {
                keywords = new Dictionary<string, int>
                {
                    // VO keywords that cannot be abbreviated

                    { "_INIT1", INIT1},
                    { "_INIT2", INIT2},
                    { "_INIT3", INIT3},
                    { "_SIZEOF", SIZEOF},
                    { "_TYPEOF", TYPEOF},  
                
                    // Vulcan keywords
			        { "ABSTRACT", ABSTRACT},
                    { "ANSI", ANSI},
                    { "AUTO", AUTO},
                    { "CATCH", CATCH},
                    { "CHAR", CHAR},
                    { "CONSTRUCTOR", CONSTRUCTOR},
                    { "CONST", CONST},
                    { "DEFAULT", DEFAULT},
                    { "DELEGATE", DELEGATE},
                    { "DESTRUCTOR", DESTRUCTOR},
                    { "ENUM", ENUM},
                    { "EVENT", EVENT},
                    { "EXPLICIT", EXPLICIT},
                    { "FINALLY", FINALLY},
                    { "FOREACH", FOREACH},
                    { "GET", GET},
                    { "IMPLEMENTS", IMPLEMENTS},
                    { "IMPLICIT", IMPLICIT},
                    { "IMPLIED", IMPLIED},
                    { "INITONLY", INITONLY},
                    { "INTERFACE", INTERFACE},
                    { "INTERNAL", INTERNAL},
                    { "LOCK", LOCK},
                    { "NAMESPACE", NAMESPACE},
                    { "NEW", NEW},
                    { "ON", ON},
                    { "OPERATOR", OPERATOR},
                    { "OUT", OUT},
                    { "PARAMS", PARAMS},
                    { "PARTIAL", PARTIAL},
                    { "PROPERTY", PROPERTY},
                    { "REPEAT", REPEAT},
                    { "SCOPE", SCOPE},
                    { "SEALED", SEALED},
                    { "SET", SET},
                    { "SIZEOF", SIZEOF},
                    { "STRUCTURE", STRUCTURE},
                    { "STRUCT", STRUCTURE},
                    { "THROW", THROW},
                    { "TRY", TRY},
                    { "TYPEOF", TYPEOF},
                    { "UNICODE", UNICODE},
                    { "UNTIL", UNTIL},
                    { "VALUE", VALUE},
                    { "VIRTUAL", VIRTUAL},
                    { "VOSTRUCT", VOSTRUCT},

			        // XSharp keywords
			        { "__ARGLIST", ARGLIST},
                    { "ADD", ADD},
                    { "ASCENDING", ASCENDING},
                    { "ASSEMBLY", ASSEMBLY},
                    { "ASTYPE", ASTYPE},
                    { "ASYNC", ASYNC},
                    { "AWAIT", AWAIT},
                    { "BY", BY},
                    { "CHECKED", CHECKED},
                    { "DESCENDING", DESCENDING},
                    { "EQUALS", EQUALS},
                    { "EXTERN", EXTERN},
                    { "FIXED", FIXED},
                    { "FROM", FROM},
                    { "GROUP", GROUP},
                    { "INTO", INTO},
                    { "JOIN", JOIN},
                    { "LET", LET},
                    { "NOP", NOP},
                    { "MODULE", MODULE},
                    { "NAMEOF", NAMEOF},
                    { "ORDERBY", ORDERBY},
                    { "OVERRIDE", OVERRIDE},
                    { "REMOVE", REMOVE},
                    { "SELECT", SELECT},
                    { "SWITCH", SWITCH},
                    { "UNCHECKED", UNCHECKED},
                    { "UNSAFE", UNSAFE},
                    { "VAR", VAR},
                    { "VOLATILE", VOLATILE},
                    { "WHERE", WHERE},
                    { "YIELD", YIELD},

			        // Vulcan types
			        { "INT64", INT64},
                    { "UINT64", UINT64},

			        // XSharp types
			        { "DYNAMIC", DYNAMIC},

			        // Macros
			        { "__ARRAYBASE__", MACRO},
                    { "__CLR2__", MACRO},
                    { "__CLR4__", MACRO},
                    { "__CLRVERSION__", MACRO},
                    { "__DATE__", MACRO},
                    { "__DATETIME__", MACRO},
                    { "__DEBUG__", MACRO},
                    { "__DIALECT__", MACRO},
                    { "__DIALECT_CORE__", MACRO},
                    { "__DIALECT_VO__", MACRO},
                    { "__DIALECT_VULCAN__", MACRO},
                    { "__DIALECT_HARBOUR__", MACRO},
                    { "__ENTITY__", MACRO},
                    { "__FILE__", MACRO},
                    { "__FUNCTIONS__", MACRO},
                    { "__LINE__", MACRO},
                    { "__MODULE__", MACRO},
                    { "__SIG__", MACRO},
                    { "__SRCLOC__", MACRO},
                    { "__SYSDIR__", MACRO},
                    { "__TIME__", MACRO},
                    { "__UTCTIME__", MACRO},
                    { "__VERSION__", MACRO},
                    { "__VO1__", MACRO},
                    { "__VO2__", MACRO},
                    { "__VO3__", MACRO},
                    { "__VO4__", MACRO},
                    { "__VO5__", MACRO},
                    { "__VO6__", MACRO},
                    { "__VO7__", MACRO},
                    { "__VO8__", MACRO},
                    { "__VO9__", MACRO},
                    { "__VO10__", MACRO},
                    { "__VO11__", MACRO},
                    { "__VO12__", MACRO},
                    { "__VO13__", MACRO},
                    { "__VO14__", MACRO},
                    { "__VO15__", MACRO},
                    { "__VO16__", MACRO},
                    { "__WINDIR__", MACRO},
                    { "__WINDRIVE__", MACRO},
                    { "__XSHARP__", MACRO},
                };

            }
            // These keywords are inserted without abbreviations
            foreach (var text in keywords.Keys)
            {
                var token = keywords[text];
                ids.Add(text, token);
            }
            return ids.ToImmutableDictionary(Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer);
        }

        public IDictionary<string, int> KwIds
        {
            get
            {
                if (_kwIds == null)
                {
                    lock (kwlock)
                    {
                        if (!_Four)
                            _kwIds = xsKwIds;
                        else
                            _kwIds = voKwIds;
                    }
                    if (_kwIds == null)
                    {
                        _kwIds = _getIds(_Four);
                        lock (kwlock)
                        {
                            if (!_Four)
                                xsKwIds = _kwIds;
                            else
                                voKwIds = _kwIds;
                        }
                    }
                }
                return _kwIds;
            }
        }

        static IDictionary<string, int> _symIds;

        private IDictionary<string, int> _getppIds()
        {
            // Macro lexer has no preprocessor keywords
            var symIds = new Dictionary<string, int>(Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer)
            {
                {"#COMMAND", PP_COMMAND},		// #command   <matchPattern> => <resultPattern>
			    {"#DEFINE", PP_DEFINE},			// #define <idConstant> [<resultText>] or #define <idFunction>([<arg list>]) [<exp>]
			    {"#ELSE", PP_ELSE},				// #ifdef <identifier>   <statements>...[#else]   <statements>...#endif
			    {"#ENDIF", PP_ENDIF},			// #ifdef <identifier>   <statements>...[#else]   <statements>...#endif
			    {"#ENDREGION", PP_ENDREGION},	// #region [description]sourceCode#endregion
			    {"#ERROR", PP_ERROR},			// #error [errorMessage]
			    {"#IFDEF", PP_IFDEF},			// #ifdef <identifier>   <statements>...[#else]   <statements>...#endif
			    {"#IFNDEF", PP_IFNDEF},			// #ifndef <identifier>   <statements>...[#else]   <statements>...#endif
			    {"#INCLUDE", PP_INCLUDE},		// #include "<headerfilename>"
			    {"#LINE", PP_LINE},				// #line <number> [FileName] or #line default
			    {"#REGION", PP_REGION},			// #region [description]sourceCode#endregion
			    {"#TRANSLATE", PP_TRANSLATE},	// #translate <matchPattern> => <resultPattern>
			    {"#UNDEF", PP_UNDEF},			// #undef <identifier>
			    {"#WARNING", PP_WARNING},		// #warning [warningMessage]
			    {"#XCOMMAND", PP_COMMAND},		// #xcommand   <matchPattern> => <resultPattern>  // alias for #command   , no 4 letter abbrev
			    {"#XTRANSLATE", PP_TRANSLATE},	// #xtranslate <matchPattern> => <resultPattern>  // alias for #translate , no 4 letter abbrev
		    };
            if (MacroLexer)
            {
                symIds.Clear();
            }
            return symIds.ToImmutableDictionary(Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer);
        }

        public IDictionary<string, int> SymIds
        {
            get
            {
                if (_symIds == null)
                {

                    lock (kwlock)
                    {
                        _symIds = _getppIds();
                    }
                }
                return _symIds;
            }
        }

        static public XSharpLexer Create(string text, string fileName, CSharpParseOptions options = null)
        {
            var stream = new AntlrInputStream(text);
            stream.name = fileName;
            var lexer = new XSharpLexer(stream);
            lexer.TokenFactory = XSharpTokenFactory.Default;
#if !TEST
            if (options == null)
                options = CSharpParseOptions.Default;
            lexer.AllowFourLetterAbbreviations = options.Dialect.AllowFourLetterAbbreviations();
            lexer.AllowOldStyleComments = options.Dialect.AllowOldStyleComments();
            lexer.AllowSingleQuotedStrings = options.Dialect.AllowStringsWithSingleQuotes();
            lexer.IsScript = options.Kind == Microsoft.CodeAnalysis.SourceCodeKind.Script;
            lexer.MacroLexer = options.MacroScript;
#endif
            return lexer;
        }

        public BufferedTokenStream GetTokenStream()
        {
            var tokenstream = new BufferedTokenStream(this);
            tokenstream.Fill();
            return tokenstream;
        }
    }
}
