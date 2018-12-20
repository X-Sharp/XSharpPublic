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
        #region Static Helper Methods
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
        public static bool IsPositionalKeyword(int iToken)
        {
            if (iToken > FIRST_POSITIONAL_KEYWORD && iToken < LAST_POSITIONAL_KEYWORD)
                return true;
            return false;
        }


        public static bool IsComment(int iToken)
        {
            return iToken == XSharpLexer.SL_COMMENT || iToken == XSharpLexer.ML_COMMENT || iToken == XSharpLexer.DOC_COMMENT;
        }


        #endregion

        #region Properties and Fields
        // Properties to set the behavior of the Lexer
        public CSharpParseOptions Options { get; set; }
        public XSharpDialect Dialect => Options.Dialect;
        private bool AllowOldStyleComments => Dialect.AllowOldStyleComments();
        private bool AllowFourLetterAbbreviations => Dialect.AllowFourLetterAbbreviations();
        private bool AllowSingleQuotedStrings => Dialect.AllowStringsWithSingleQuotes();
        private bool AllowXBaseVariables => Dialect.AllowXBaseVariables();
        public bool IsScript { get; set; }
        public bool IsMacroLexer { get; set; }
        // Properties that show what the contents of the Lexer buffer was
        public bool HasPragmas { get; private set; }
        public bool HasPreprocessorTokens { get; private set; }
        public bool HasPPDefines { get; private set; }
        public bool HasPPIfdefs { get; private set; }
        public bool HasPPIncludes { get; private set; }
        public bool HasPPRegions { get; private set; }
        public bool HasPPMessages { get; private set; }
        public bool HasPPUDCs { get; private set; }
        public bool HasPPMacros { get; private set; }
        public bool HasDocComments { get; private set; }
        public bool MustBeProcessed => HasPPMessages || HasPPUDCs || HasPPIncludes || HasPPMacros || HasPPIfdefs;

        internal IList<ParseErrorData> LexErrors => _lexErrors;
        int LastToken => _lastToken;

        bool _inDottedIdentifier = false;
        bool _currentLineIsPreprocessorDefinition = false;
        bool _currentLineHasEos = true;
        int _lastToken = NL;
        IList<ParseErrorData> _lexErrors = new List<ParseErrorData>();

        System.Text.StringBuilder _textSb = new System.Text.StringBuilder();
        int _lineStartCharIndex;
        int _startCharIndex;
        int _startColumn;
        int _startLine;
        int _tokenType;
        int _tokenChannel;
        int _nextChar = 0;

        static Object kwlock = new Object();
        static IDictionary<string, int> voKwIds = null;
        static IDictionary<string, int> xppKwIds = null;
        static IDictionary<string, int> xsKwIds = null;
        private IDictionary<string, int> _kwIds;
        static IDictionary<string, int> _symPPIds;


        #endregion

 
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

        void parseOne(int type)
        {
            _tokenType = type;
            _textSb.Append((char)nextChar);
            InputStream.Consume();
            _nextChar = 0;
        }

        void parseOne()
        {
            _textSb.Append((char)nextChar);
            InputStream.Consume();
            _nextChar = 0;
        }

        void parseToEol()
        {
            while (nextChar != TokenConstants.Eof && nextChar != '\r' && nextChar != '\n')
                parseOne();
        }

        bool parseNewLine()
        {
            if (nextChar == '\n' || nextChar == '\r')
            {
                if (nextChar == '\r' && InputStream.La(2) == '\n')
                    parseOne();
                parseOne();
                Interpreter.Line += 1;
                Interpreter.Column = 0;
                _lineStartCharIndex = InputStream.Index;
                return true;
            }
            return nextChar == TokenConstants.Eof;
        }

        void parseWhitespace()
        {
            parseOne(WS);
            _tokenChannel = TokenConstants.HiddenChannel;
            while (nextChar == ' ' || nextChar == '\t')
                parseOne();
        }

        void parseSlComment()
        {
            parseOne(SL_COMMENT);
            _tokenChannel = TokenConstants.HiddenChannel;
            parseToEol();
        }

        void parseDocComment()
        {
            parseOne(DOC_COMMENT);
            _tokenChannel = XMLDOCCHANNEL;
            HasDocComments = true;
            parseToEol();
        }

        void parseMlComment()
        {
            _tokenChannel = TokenConstants.HiddenChannel;
            while (nextChar != TokenConstants.Eof)
            {
                if (nextChar == '*' && InputStream.La(2) == '/')
                    break;
                if (!parseNewLine())
                    parseOne();
            }
            if (nextChar != TokenConstants.Eof)
            {
                parseOne();
                parseOne();
            }
        }

        void parseId()
        {
            _tokenType = ID;
            if (InputStream.La(1) == '@')
            {
                parseOne();
                parseOne();
            }
            parseOne();
            var c = nextChar;
            while ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
                    || (c >= '\u00C0' && c <= '\u00D6') || (c >= '\u00D8' && c <= '\u00F6')
                    || (c >= '\u00F8' && c <= '\u02FF') || (c >= '\u0370' && c <= '\u037D')
                    || (c >= '\u037F' && c <= '\u1FFF') || (c >= '\u200C' && c <= '\u200D')
                    || c == '\u00B7' || (c >= '\u0300' && c <= '\u036F') || (c >= '\u203F' && c <= '\u2040')
                    )
            {
                parseOne();
                c = nextChar;
            }
        }

        void tryParseSymbol()
        {
            var c = nextChar;
            if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
                || (c >= '\u00C0' && c <= '\u00D6') || (c >= '\u00D8' && c <= '\u00F6')
                || (c >= '\u00F8' && c <= '\u02FF') || (c >= '\u0370' && c <= '\u037D')
                || (c >= '\u037F' && c <= '\u1FFF') || (c >= '\u200C' && c <= '\u200D'))
            {
                parseOne(SYMBOL_CONST);
                c = nextChar;
                while ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
                        || (c >= '\u00C0' && c <= '\u00D6') || (c >= '\u00D8' && c <= '\u00F6')
                        || (c >= '\u00F8' && c <= '\u02FF') || (c >= '\u0370' && c <= '\u037D')
                        || (c >= '\u037F' && c <= '\u1FFF') || (c >= '\u200C' && c <= '\u200D')
                        || c == '\u00B7' || (c >= '\u0300' && c <= '\u036F') || (c >= '\u203F' && c <= '\u2040')
                        )
                {
                    parseOne();
                    c = nextChar;
                }
                var text = _textSb.ToString().Substring(1);
                if (KwIds.ContainsKey(text))
                {
                    var kwid = KwIds[text];
                    if (kwid >= FIRST_NULL && kwid <= LAST_NULL && kwid != NULL)
                    {
                        // #NIL or #NULL_STRING etc., however #NULL must be allowed as Symbol
                        _tokenType = NEQ2;
                        _textSb.Clear();
                        _textSb.Append('#');
                        InputStream.Seek(_startCharIndex + 1);
                    }
                }
                else if (text.Equals("USING", StringComparison.OrdinalIgnoreCase))
                {
                    _tokenType = USING;
                }
                else if (LastToken == NL && text.Equals("PRAGMA", StringComparison.OrdinalIgnoreCase))
                {
                    _tokenType = PRAGMA;
                    _tokenChannel = PRAGMACHANNEL;
                    HasPragmas = true;
                    while (nextChar != TokenConstants.Eof && nextChar != '\r' && nextChar != '\n')
                        parseOne();
                }
            }
        }

        void parseNumber()
        {
            _tokenType = nextChar == '.' ? REAL_CONST : INT_CONST;
            if (nextChar == '.')
                parseOne();
            else if (nextChar == '0')
            {
                parseOne();
                if (nextChar == 'x' || nextChar == 'X')
                {
                    parseOne(HEX_CONST);
                    while ((nextChar >= '0' && nextChar <= '9') || (nextChar >= 'A' && nextChar <= 'F') || (nextChar >= 'a' && nextChar <= 'f'))
                        parseOne();
                    if (nextChar == 'U' || nextChar == 'L' || nextChar == 'u' || nextChar == 'l')
                        parseOne();
                    return;
                }
                else if (nextChar == 'b' || nextChar == 'B')
                {
                    parseOne(BIN_CONST);
                    while (nextChar >= '0' && nextChar <= '1')
                        parseOne();
                    if (nextChar == 'U' || nextChar == 'u')
                        parseOne();
                    return;
                }
            }
            while (nextChar >= '0' && nextChar <= '9')
                parseOne();
            if (_tokenType == INT_CONST)
            {
                if (nextChar == 'U' || nextChar == 'L' || nextChar == 'u' || nextChar == 'l')
                {
                    parseOne();
                    return;
                }
                if (nextChar == '.' && (InputStream.La(2) >= '0' || InputStream.La(2) <= '9'))
                {
                    parseOne(REAL_CONST);
                    while (nextChar >= '0' && nextChar <= '9')
                        parseOne();
                }
            }
            if (_tokenType == REAL_CONST)
            {
                if (nextChar == '.')
                {
                    string s = _textSb.ToString();
                    int z0 = s.IndexOf('.');
                    if (z0 > 0 && s.Length - z0 > 1 && s.Length - z0 <= 3 && s.Length <= 7)
                    {
                        parseOne(DATE_CONST); // append dot
                                              // append day number
                        if (nextChar >= '0' && nextChar <= '9')
                            parseOne();
                        if (nextChar >= '0' && nextChar <= '9')
                            parseOne();
                        return;
                    }
                }
            }
            if (nextChar == 'E' || nextChar == 'e')
            {
                int c2 = InputStream.La(2);
                int c3 = InputStream.La(3);
                if (((c2 == '+' || c2 == '-') && (c3 >= '0' && c3 <= '9')) || (c2 >= '0' && c2 <= '9'))
                {
                    parseOne(REAL_CONST);
                    parseOne();
                    while (nextChar >= '0' && nextChar <= '9')
                        parseOne();
                }
            }
            if (nextChar == 'S' || nextChar == 'D' || nextChar == 's' || nextChar == 'd' || nextChar == 'M' || nextChar == 'm')
                parseOne(REAL_CONST);
        }

        void parseString()
        {
            _tokenType = STRING_CONST;
            if (!AllowSingleQuotedStrings && nextChar == '\'')
            {
                _tokenType = CHAR_CONST;
            }
            else if (nextChar == 'c' || nextChar == 'C')
            {
                parseOne(CHAR_CONST);
            }
            else
            {
                if (nextChar == 'E' || nextChar == 'e')
                {
                    parseOne(ESCAPED_STRING_CONST);
                }
                if (nextChar == 'I' || nextChar == 'i')
                {
                    parseOne(INTERPOLATED_STRING_CONST);
                    if (nextChar == 'E' || nextChar == 'e')
                        parseOne();
                }
            }
            {
                int q = nextChar;
                parseOne();
                bool allow_esc = _tokenType == CHAR_CONST ?
                    InputStream.La(1) == '\\' && InputStream.La(3) == q : _tokenType != STRING_CONST;
                bool esc = false;
                while (nextChar != TokenConstants.Eof && (nextChar != q || esc))
                {
                    esc = allow_esc && !esc && nextChar == '\\';
                    parseOne();
                }
                if (nextChar == q)
                    parseOne();
                else if (nextChar == TokenConstants.Eof)
                    _tokenType = INCOMPLETE_STRING_CONST;
            }
        }

        int nextChar { get { return _nextChar == 0 ? _nextChar = InputStream.La(1) : _nextChar; } }

        public override IToken NextToken()
        {
            XSharpToken t;
            {
                _lineStartCharIndex = InputStream.Index;
                _startCharIndex = InputStream.Index;
                _startColumn = Interpreter.Column;
                _startLine = Interpreter.Line;
                _tokenType = -1;
                _tokenChannel = TokenConstants.DefaultChannel;
                _textSb.Clear();
                switch (nextChar)
                {
                    case '(':
                        parseOne(LPAREN);
                        break;
                    case ')':
                        parseOne(RPAREN);
                        break;
                    case '{':
                        parseOne(LCURLY);
                        break;
                    case '}':
                        parseOne(RCURLY);
                        break;
                    case '[':
                        parseOne(LBRKT);
                        break;
                    case ']':
                        parseOne(RBRKT);
                        break;
                    case ':':
                        parseOne(COLON);
                        if (nextChar == ':')
                            parseOne(COLONCOLON);
                        else if (nextChar == '=')
                            parseOne(ASSIGN_OP);
                        break;
                    case ',':
                        parseOne(COMMA);
                        break;
                    case '\\':       // used inside #command to escape '<'
                        parseOne(BACKSLASH);
                        break;
                    case '|':
                        parseOne(PIPE);
                        if (nextChar == '|')
                            parseOne(OR);
                        else if (nextChar == '=')
                            parseOne(ASSIGN_BITOR);
                        break;
                    case '&':
                        parseOne(AMP);
                        if (Dialect.AllowOldStyleComments() && nextChar == '&')
                            parseSlComment();
                        else if (nextChar == '&')
                            parseOne(AND);
                        else if (nextChar == '=')
                            parseOne(ASSIGN_BITAND);
                        break;
                    case '@':
                        if (InputStream.La(2) == '@')
                            goto default;
                        parseOne(ADDROF);
                        break;
                    case '-':
                        parseOne(MINUS);
                        if (nextChar == '>')
                            parseOne(ALIAS);
                        else if (nextChar == '-')
                            parseOne(DEC);
                        else if (nextChar == '=')
                            parseOne(ASSIGN_SUB);
                        break;
                    case '+':
                        parseOne(PLUS);
                        if (nextChar == '+')
                            parseOne(INC);
                        else if (nextChar == '=')
                            parseOne(ASSIGN_ADD);
                        break;
                    case '/':
                        parseOne(DIV);
                        if (nextChar == '*')
                        {
                            parseOne(ML_COMMENT);
                            parseMlComment();
                            break;
                        }
                        else if (nextChar == '/')
                        {
                            parseOne(SL_COMMENT);
                            if (nextChar == '/')
                                parseDocComment();
                            else
                                parseSlComment();
                        }
                        else if (nextChar == '=')
                            parseOne(ASSIGN_DIV);
                        break;
                    case '%':
                        parseOne(MOD);
                        if (nextChar == '=')
                            parseOne(ASSIGN_MOD);
                        break;
                    case '^':
                        parseOne(EXP);
                        if (nextChar == '=')
                            parseOne(ASSIGN_EXP);
                        break;
                    case '<':
                        parseOne(LT);
                        if (nextChar == '<')
                        {
                            parseOne(LSHIFT);
                            if (nextChar == '=')
                                parseOne(ASSIGN_LSHIFT);
                        }
                        else if (nextChar == '=')
                            parseOne(LTE);
                        else if (nextChar == '>')
                            parseOne(NEQ);
                        break;
                    case '>':
                        parseOne(GT);
                        // GreaterThanGreaterThanToken is synthesized in the parser since it is ambiguous (with closing nested type parameter lists)
                        if (nextChar == '>' && InputStream.La(2) == '=')
                        {
                            parseOne(); // >
                            parseOne(ASSIGN_RSHIFT); // =
                        }
                        else if (nextChar == '=')
                            parseOne(GTE);
                        break;
                    case '~':
                        parseOne(TILDE);
                        if (nextChar == '=')
                            parseOne(ASSIGN_XOR);
                        else if (nextChar == '"')           // Old Style Pragma like ~"ONLYEARLY+", treat it as whitespace
                        {
                            parseOne(WS);
                            _tokenChannel = TokenConstants.HiddenChannel;
                            while (nextChar != TokenConstants.Eof && nextChar != '"')
                                parseOne();
                            if (nextChar != TokenConstants.Eof)
                                parseOne();
                        }
                        break;
                    case '*':
                        parseOne(MULT);
                        if (LastToken == NL)
                            parseSlComment();
                        else if (nextChar == '=')
                            parseOne(ASSIGN_MUL);
                        else if (nextChar == '*')
                        {
                            parseOne(EXP);
                            if (nextChar == '=')
                                parseOne(ASSIGN_EXP);
                        }
                        break;
                    case '?':
                        parseOne(QMARK);
                        if (nextChar == '?')
                            parseOne(QQMARK);
                        break;
                    case '=':
                        parseOne(EQ);
                        if (nextChar == '=')
                            parseOne(EEQ);
                        else if (nextChar == '>')
                            parseOne(UDCSEP);
                        break;
                    case '$':
                        parseOne(SUBSTR);
                        break;
                    case '!':
                        parseOne(NOT);
                        if (nextChar == '=')
                            parseOne(NEQ);
                        break;
                    case ';':
                        parseOne(SEMI);
                        do
                        {
                            while (nextChar == ' ' || nextChar == '\t')
                                parseOne();
                            if (nextChar == '/' && InputStream.La(2) == '*')
                            {
                                parseOne();
                                parseOne();
                                parseMlComment();
                            }
                        } while (nextChar == ' ' || nextChar == '\t');
                        if (nextChar == '/' && InputStream.La(2) == '/')
                            parseToEol();
                        else if (AllowOldStyleComments && nextChar == '&' && InputStream.La(2) == '&')
                            parseToEol();
                        if (parseNewLine())
                        {
                            _tokenType = LINE_CONT;
                            _tokenChannel = TokenConstants.HiddenChannel;
                        }
                        if (_tokenType == SEMI && _textSb.Length > 1)
                        {
                            _textSb.Remove(1, _textSb.Length - 1);
                            InputStream.Seek(_startCharIndex + 1);
                        }
                        break;
                    case '.':
                        if (InputStream.La(2) >= '0' && InputStream.La(2) <= '9')
                        {
                            goto case '0';
                        }
                        parseOne(DOT);
                        if (!_inDottedIdentifier)       // Do not translate .OR., .AND. etc Keywords that are part of a dotted identifier
                        {
                            if (InputStream.La(2) == '.')
                            {
                                var c1 = nextChar;
                                if (c1 == 'F' || c1 == 'N' || c1 == 'f' || c1 == 'n')
                                {
                                    parseOne(FALSE_CONST);
                                    parseOne();
                                }
                                else if (c1 == 'T' || c1 == 'Y' || c1 == 't' || c1 == 'y')
                                {
                                    parseOne(TRUE_CONST);
                                    parseOne();
                                }
                                else if (c1 == '.')
                                {
                                    parseOne(ELLIPSIS);
                                    parseOne();
                                }
                            }
                            else if (InputStream.La(3) == '.')
                            {
                                var c1 = nextChar;
                                var c2 = InputStream.La(2);
                                if ((c1 == 'O' || c1 == 'o') && (c2 == 'R' || c2 == 'r'))
                                {
                                    parseOne(LOGIC_OR);
                                    parseOne();
                                    parseOne();
                                }
                            }
                            else if (InputStream.La(4) == '.')
                            {
                                var c1 = nextChar;
                                var c2 = InputStream.La(2);
                                var c3 = InputStream.La(3);
                                if ((c1 == 'A' || c1 == 'a') && (c2 == 'N' || c2 == 'n') && (c3 == 'D' || c3 == 'd'))
                                {
                                    parseOne(LOGIC_AND);
                                    parseOne();
                                    parseOne();
                                    parseOne();
                                }
                                else if ((c1 == 'N' || c1 == 'n') && (c2 == 'O' || c2 == 'o') && (c3 == 'T' || c3 == 't'))
                                {
                                    parseOne(LOGIC_NOT);
                                    parseOne();
                                    parseOne();
                                    parseOne();
                                }
                                else if ((c1 == 'X' || c1 == 'x') && (c2 == 'O' || c2 == 'o') && (c3 == 'R' || c3 == 'r'))
                                {
                                    parseOne(LOGIC_XOR);
                                    parseOne();
                                    parseOne();
                                    parseOne();
                                }
                            }
                        }
                        break;
                    case '\r':
                    case '\n':
                        if (parseNewLine()) _tokenType = NL;
                        break;
                    case '\t':
                    case ' ':
                        parseWhitespace();
                        break;
                    case 'c':
                    case 'C':
                        if (InputStream.La(2) == '"' || InputStream.La(2) == '\'') // char const
                        {
                            goto case '\'';
                        }
                        goto case 'a';
                    case 'e':
                    case 'E':
                        if (InputStream.La(2) == '"') // escaped string
                        {
                            goto case '\'';
                        }
                        if ((InputStream.La(2) == 'i' || InputStream.La(2) == 'I') && InputStream.La(3) == '"') // interpolated escaped string
                        {
                            goto case '\'';
                        }
                        goto case 'a';
                    case 'i':
                    case 'I':
                        if (InputStream.La(2) == '"') // interpolated string
                        {
                            goto case '\'';
                        }
                        if ((InputStream.La(2) == 'e' || InputStream.La(2) == 'E') && InputStream.La(3) == '"') // interpolated escaped string
                        {
                            goto case '\'';
                        }
                        goto case 'a';
                    case 'a':
                    case 'b':
                       // c is handled above
                    case 'd':
                       // e is handled above
                    case 'f':
                    case 'g':
                    case 'h':
                       // i is handled above:
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
                       // 'C' is handled above
                    case 'D':
                       // 'E' is handled above
                    case 'F':
                    case 'G':
                    case 'H':
                       // 'I' is handled above
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
                        parseId();
                        break;
                    case '#':
                        parseOne(NEQ2);
                        tryParseSymbol();
                        break;
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                        parseNumber();
                        break;
                    case '\'':
                    case '"':
                        parseString();
                        break;
                    default:
                        {
                            var c = nextChar;
                            if (c == '@')
                                c = InputStream.La(3);
                            if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
                                    || (c >= '\u00C0' && c <= '\u00D6') || (c >= '\u00D8' && c <= '\u00F6')
                                    || (c >= '\u00F8' && c <= '\u02FF') || (c >= '\u0370' && c <= '\u037D')
                                    || (c >= '\u037F' && c <= '\u1FFF') || (c >= '\u200C' && c <= '\u200D'))
                                goto case 'a';
                        }
                        if (nextChar == TokenConstants.Eof)
                        {
                            _tokenType = Eof;
                            break;
                        }
                        parseOne(UNRECOGNIZED);
                        break;
                }
                Interpreter.Column += (InputStream.Index - _startCharIndex);
                t = TokenFactory.Create(this.SourcePair, _tokenType, _textSb.ToString(), _tokenChannel, _startCharIndex, CharIndex - 1, _startLine, _startColumn) as XSharpToken;
                Emit(t);
                if (t.Type == ML_COMMENT)
                {
                    if (!t.Text.EndsWith("*/"))
                    {
                        _lexErrors.Add(new ParseErrorData(t, ErrorCode.ERR_OpenEndedComment));
                    }
                }
            }
            var type = t.Type;
            if (findKeyWord(t, _lastToken))
            {
                type = t.Type;
            }
            else if (type == SYMBOL_CONST && (LastToken == NL || LastToken == UDCSEP))
            {
                int symtype;
                if (SymPPIds.TryGetValue(t.Text, out symtype))
                {
                    t.Type = symtype;
                    _currentLineIsPreprocessorDefinition = true;
                    HasPreprocessorTokens = true;
                    switch (symtype)
                    {
                        case PP_COMMAND:
                        case PP_TRANSLATE:
                            HasPPUDCs = true;
                            break;
                        case PP_IFDEF:
                        case PP_IFNDEF:
                        case PP_ELSE:
                        case PP_ENDIF:
                            HasPPIfdefs = true;
                            break;
                        case PP_REGION:
                        case PP_ENDREGION:
                            HasPPRegions = true;
                            break;
                        case PP_ERROR:
                        case PP_WARNING:
                            HasPPMessages = true;
                            break;
                        case PP_INCLUDE:
                            HasPPIncludes = true;
                            break;
                        case PP_DEFINE:
                        case PP_UNDEF:
                            HasPPDefines = true;
                            break;
                        case PP_LINE:
                        default:
                            break;

                    }
                }
                else if (IsScript)
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
            if (type != Eof)
            {
                if (!_inDottedIdentifier)
                {
                    // Check if the current token is a valid Identifier (starts with A..Z or _) and is followed by a DOT
                    // In that case we change the type from Keyword to ID
                    if (_isValidIdentifier(t) && InputStream.La(1) == (int)'.')
                    {
                        if (t.Type != SELF && t.Type != SUPER)
                        {
                            t.Type = ID;
                        }
                        _inDottedIdentifier = true;
                    }
                    else if (type == ID || type == KWID)
                    {
                        _inDottedIdentifier = true;
                    }
                }
                else
                {
                    // Check if the current token is a valid Identifier (starts with A..Z or _) 
                    if (_isValidIdentifier(t))
                    {
                        t.Type = ID;
                        // keep _inDottedIdentifier true
                    }
                    else if (type != DOT && type != ID && type != KWID)
                    {
                        _inDottedIdentifier = false;
                    }
                }
            }
            if (type == NL || type == SEMI)
            {
                if (_currentLineHasEos)
                {
                    if (type == SEMI)
                    {
                        if (_lastToken != SEMI)
                        {
                            t.Channel = t.OriginalChannel = TokenConstants.HiddenChannel;
                        }
                    }
                    else
                    {
                        t.Channel = t.OriginalChannel = TokenConstants.HiddenChannel;
                    }
                }
                else
                {
                    t.Type = EOS;
                    _currentLineHasEos = true;
                }
            }
            else if (_currentLineHasEos && t.Channel == TokenConstants.DefaultChannel)
            {
                _currentLineHasEos = false;
            }
            else if (!_currentLineHasEos && type == Eof)
            {
                t.Type = EOS;
                _currentLineHasEos = true;
            }
            if (t.Channel == TokenConstants.DefaultChannel)
            {
                _lastToken = type; // nvk: Note that this is the type before any modifications!!!
            }

            if (_currentLineIsPreprocessorDefinition)
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
                    {
                        // end of line ends the preprocessor defintion
                        _currentLineIsPreprocessorDefinition = false;
                    }
                }
            }
            return t;
        }

        #region Keywords and Preprocessor Lookup
        private int fixPositionalKeyword(int keyword, int lastToken)
        {
            // after the alias operator we treat everything as ID
            if (lastToken == XSharpLexer.ALIAS)
                return ID;
            switch (keyword)
            {
                // Some keywords are impossible to use as ID
                case SELF:
                case SUPER:
                case STATIC:
                case DIM:
                case CONST:
                case AS:
                    return keyword;
                case EXPLICIT:
                case IMPLICIT:
                    if (lastToken != OPERATOR)
                    {
                        return ID;
                    }
                    break;
                case DESTRUCTOR:
                    // can also appear after attribute
                    if (lastToken != EOS && lastToken != NL && lastToken != EXTERN && lastToken != RBRKT)
                    {
                        return ID;
                    }
                    break;
                case ASSEMBLY:
                case MODULE:
                    if (lastToken != LBRKT)
                    {
                        return ID;
                    }
                    break;
                case FINALLY:
                case CATCH:
                case REPEAT:
                case UNTIL:
                case YIELD:
                    if (lastToken != EOS && lastToken != NL)
                    {
                        return ID;
                    }
                    break;
                case SWITCH:
                    if (lastToken != EOS && lastToken != NL && lastToken != BEGIN && lastToken != DO && lastToken != END)
                    {
                        return ID;
                    }
                    break;
                case IMPLIED:
                case VAR:
                    if (lastToken != EOS && lastToken != NL && lastToken != LOCAL && lastToken != STATIC
                        && lastToken != FOR && lastToken != FOREACH && lastToken != USING)
                    {
                        // in XPP VAR is used in the class definition as well.
                        if (Dialect != XSharpDialect.XPP)
                            return ID;
                    }
                    break;
                case NAMESPACE:
                case SCOPE:
                case LOCK:
                    if (lastToken != BEGIN && lastToken != END)
                    {
                        return ID;
                    }
                    break;
                case MEMVAR:
                case PARAMETERS:
                    if (!AllowXBaseVariables)
                        return ID;
                    if (lastToken != EOS && lastToken != NL)
                    {
                        return ID;
                    }
                    break;
                default:
                    switch (lastToken)
                    {
                        case CLASS:
                            if (Dialect == XSharpDialect.XPP)   // XPP uses CLASS instead of STATIC
                                return keyword;
                            else
                                return ID;
                        case ACCESS:
                        case ASSIGN:
                            if (Dialect == XSharpDialect.XPP)   // XPP allows ACCESS ASSIGN or ASSIGN ACCESS in class definition
                                return keyword;
                            else
                                return ID;

                        // After these keywords we expect an ID
                        // Some of these also have a possible SELF, DIM, CONST or STATIC clause but these have been excluded above

                        case METHOD:
                        case PROCEDURE:
                        case PROC:
                        case FUNCTION:
                        case FUNC:
                        case INTERFACE:
                        case STRUCTURE:
                        case VOSTRUCT:
                        case UNION:
                        case DELEGATE:
                        case PROPERTY:
                        case EVENT:
                        case DEFINE:
                        case ENUM:
                        case MEMBER:
                        case DIM:
                        case LOCAL:
                        case VAR:
                        case IMPLIED:
                        // Linq
                        case FROM:
                        case LET:
                        case JOIN:
                        case INTO:
                            return ID;
                        case USING:
                            // BEGIN USING can/will be followed by a Variable Declaration
                            if (keyword != LOCAL && keyword != VAR)
                                return ID;
                            break;
                        case MEMVAR:            // VO & XPP: followed by list of names
                        case PARAMETERS:
                            return ID;


                    }
                    break;
            }
            return keyword;
        }
        private bool findKeyWord(XSharpToken token, int lastToken)
        {
            if (token.Type == ID && token.Channel == Lexer.DefaultTokenChannel)
            {
                int kwtype;
                if (KwIds.TryGetValue(token.Text, out kwtype))
                {
                    if (IsPositionalKeyword(kwtype) && (lastToken == COLON || lastToken == DOT))
                    {
                        ; // do nothing, no new keywords after colon or dot
                    }
                    else
                    {
                        kwtype = fixPositionalKeyword(kwtype, lastToken);
                        token.Type = kwtype;
                        if (kwtype == MACRO)
                        {
                            HasPPMacros = true;
                            HasPreprocessorTokens = true;
                        }
                        return true;
                    }
                }
            }
            return false;
        }
        internal IList<XSharpToken> ReclassifyTokens(IList<XSharpToken> tokens)
        {
            int lastType = EOS;
            XSharpToken last = null;
            foreach (XSharpToken token in tokens)
            {
                // Some keywords may have been seen as identifier because they were
                // originally in the Preprocessor Channel and for example not on a start 
                // of a line or command
                if (token.Channel == Lexer.DefaultTokenChannel)
                {
                    findKeyWord(token, lastType);
                }
                // Identifier tokens before a DOT are never Keyword but always a type or field/property
                if (token.Type == DOT)
                {
                    if (last != null && _isValidIdentifier(last))
                    {
                        last.Type = ID;
                    }
                }
                last = token;
                if (token.Type != WS)
                {
                    lastType = token.Type;
                }
            }
            return tokens;
        }
        private bool _isValidIdentifier(IToken t)
        {
            if (t == null || t.Text?.Length == 0 || t.Type == Eof)
                return false;

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

        private IDictionary<string, int> _getIds(bool lFour)
        {
            var ids = new Dictionary<string, int>(Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer);

            Dictionary<string, int> voKeywords = null;
            if (IsMacroLexer)
            {
                // short list of keywords used in Macro Expressions
                // keywords for statements and entities are not included
                voKeywords = new Dictionary<string, int>
                {
                    {"_AND", VO_AND},
                    {"BREAK", BREAK},
                    {"_CAST", CAST},
                    {"EXPORT", EXPORT},
                    {"_FIELD", FIELD},
                    {"IF", IF},
                    {"IIF", IIF},
                    {"IS", IS},
                    {"MEMVAR", MEMVAR},
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
                {"_FIELD", FIELD},
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
                {"_WINCALL", WINCALL},
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
                if (!IsMacroLexer)
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

            if (Dialect == XSharpDialect.XPP)
            {
                // XBase++ Keywords
                var xppKeywords = new Dictionary<string, int>
                {
                    // normal keywords
                    {"ENDSEQUENCE", END },
                    {"ENDFOR",   NEXT },
                    // class keywords
                    {"ENDCLASS",ENDCLASS},
                    {"READONLY",READONLY },
                    {"DEFERRED",DEFERRED },
                    {"FREEZE",  FREEZE},
                    {"FINAL",   FINAL},
                    {"SHARING", SHARING},
                    {"SHARED",  SHARED},
                    {"INLINE",  INLINE},
                    {"SYNC",    SYNC},
                    {"ASSIGNMENT",ASSIGNMENT},
                    {"EXPORTED",EXPORTED},
                    {"NOSAVE",  NOSAVE},
                    {"INTRODUCE",INTRODUCE }
                };
                var xppKeyWordAbbrev = new Dictionary<string, int>
                {
                    {"RETURN ",RETURN },
                    {"PRIVATE ",PRIVATE  },
                    {"PUBLIC",  PUBLIC },
                    {"FUNCTION",    FUNCTION },
                    {"MEMVAR",      MEMVAR },
                    {"PARAMETERS",  PARAMETERS},
                    {"PROCEDURE",   PROCEDURE},
                    {"OTHERWISE",   OTHERWISE },
                    {"RECOVER",     RECOVER },
                    {"SEQUENCE",    SEQUENCE }
                };
                foreach (var kw in xppKeywords)
                {
                    if (!ids.ContainsKey(kw.Key))
                        ids.Add(kw.Key, kw.Value);
                }
                foreach (var kw in xppKeyWordAbbrev)
                {
                    var name = kw.Key;
                    while (true)
                    {
                        if (! ids.ContainsKey(name))
                        {
                            ids.Add(name, kw.Value);
                        }
                        if (name.Length == 4)
                            break;
                        name = name.Substring(0, name.Length - 1);
                    }
                }
           }

            if (AllowFourLetterAbbreviations)
            {
                ids.Add("ANY", USUAL);
            }
            Dictionary<string, int> keywords = null;
            if (IsMacroLexer)
            {
                keywords = new Dictionary<string, int>
                {
                    // short list of keywords used in Macro Expressions
                    // keywords for statements and entities are not included
                    // also the preprocessor macros are not included
                    // VO keywords that cannot be abbreviated

                    {"_SIZEOF", SIZEOF},
                    {"_TYPEOF", TYPEOF},
                    {"CLASS", CLASS},  // For Anonymous types
                    {"DELEGATE", DELEGATE},  // For Delegate expressions

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

                    // No support for LINQ

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
                    { "__CASTCLASS", CASTCLASS},
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
                    { "OF", OF},
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
                    { "__FUNCTION__", MACRO},
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
                // Better safe than sorry
                if (! ids.ContainsKey(text))
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
                        if (!AllowFourLetterAbbreviations)
                            _kwIds = xsKwIds;
                        else if (Dialect == XSharpDialect.XPP)
                            _kwIds = xppKwIds;
                        else
                            _kwIds = voKwIds;
                    }
                    if (_kwIds == null)
                    {
                        _kwIds = _getIds(AllowFourLetterAbbreviations);
                        lock (kwlock)
                        {
                            if (!AllowFourLetterAbbreviations)
                                xsKwIds = _kwIds;
                            else if (Dialect == XSharpDialect.XPP)
                                xppKwIds = _kwIds;
                            else
                                voKwIds = _kwIds;
                        }
                    }
                }
                return _kwIds;
            }
        }

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
            if (Dialect == XSharpDialect.XPP)
            {
                symIds.Add("#XTRANS", PP_TRANSLATE);
                symIds.Add("#TRANS", PP_TRANSLATE);
            }
            if (IsMacroLexer)
            {
                symIds.Clear();
            }
            else
            {
                if (AllowFourLetterAbbreviations)
                {
                    var symFour = new Dictionary<string, int>(Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer);
                    foreach (var entry in symIds)
                    {
                        var name = entry.Key;       // '#' name
                        while (name.Length > 5)
                        {
                            name = name.Substring(0, name.Length - 1);
                            if (!symIds.ContainsKey(name) && ! symFour.ContainsKey(name))
                            {
                                symFour.Add(name, entry.Value);
                            }
                        }
                    }
                    foreach (var entry in symFour)
                    {
                        symIds.Add(entry.Key, entry.Value);
                    }
                }
            }
            return symIds.ToImmutableDictionary(Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer);
        }

        public IDictionary<string, int> SymPPIds
        {
            get
            {
                if (_symPPIds == null)
                {

                    lock (kwlock)
                    {
                        _symPPIds = _getppIds();
                    }
                }
                return _symPPIds;
            }
        }

        #endregion
        static public XSharpLexer Create(string text, string fileName, CSharpParseOptions options = null)
        {
            var stream = new AntlrInputStream(text);
            stream.name = fileName;
            var lexer = new XSharpLexer(stream);
            lexer.TokenFactory = XSharpTokenFactory.Default;
#if !TEST
            if (options == null)
                options = CSharpParseOptions.Default;
            lexer.Options = options;
            lexer.IsScript = options.Kind == Microsoft.CodeAnalysis.SourceCodeKind.Script;
            lexer.IsMacroLexer = options.MacroScript;
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


