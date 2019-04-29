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

        static object kwlock = new object();
        static IDictionary<string, int>[] _kwids;       // for each dialect its own _kwids
        static IDictionary<string, int> _symPPIds;
        static XSharpLexer()
        {
            _kwids = new IDictionary<string, int>[(int)XSharpDialect.Last];
            for (int i= 0; i < (int) XSharpDialect.Last; i++)
            {
                _kwids[i] = null;
            }
        }


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

        int La_1 { get { return InputStream.La(1); } }
        int La_2 { get { return InputStream.La(2); } }
        int La_3 { get { return InputStream.La(3); } }
        int La_4 { get { return InputStream.La(4); } }

        void parseInit()
        {
            _lineStartCharIndex = InputStream.Index;
            _startCharIndex = InputStream.Index;
            _startColumn = Interpreter.Column;
            _startLine = Interpreter.Line;
            _tokenType = -2;
            _tokenChannel = TokenConstants.DefaultChannel;
            _textSb.Clear();
        }

        int parseType() => _tokenType;

        bool parsingFailed() => _tokenType == -2;

        void parseType(int type)
        {
            _tokenType = type;
        }

        void parseSkip()
        {
            InputStream.Consume();
        }

        void parseOne(int type)
        {
            parseType(type);
            _textSb.Append((char)La_1);
            InputStream.Consume();
        }

        void parseOne()
        {
            _textSb.Append((char)La_1);
            InputStream.Consume();
        }

        void parseToEol()
        {
            while (La_1 != TokenConstants.Eof && La_1 != '\r' && La_1 != '\n')
                parseOne();
        }

        bool tryParseNewLine()
        {
            if (La_1 == '\n' || La_1 == '\r')
            {
                if (La_1 == '\r' && La_2 == '\n')
                    parseOne();
                parseOne();
                Interpreter.Line += 1;
                Interpreter.Column = 0;
                _lineStartCharIndex = InputStream.Index;
                return true;
            }
            return La_1 == TokenConstants.Eof;
        }

        void parseWhitespace()
        {
            parseOne(WS);
            _tokenChannel = TokenConstants.HiddenChannel;
            while (La_1 == ' ' || La_1 == '\t')
                parseOne();
        }

        void parseSlComment()
        {
            parseType(SL_COMMENT);
            _tokenChannel = TokenConstants.HiddenChannel;
            parseToEol();
        }

        void parseDocComment()
        {
            parseType(DOC_COMMENT);
            _tokenChannel = XMLDOCCHANNEL;
            HasDocComments = true;
            parseToEol();
        }

        void parseMlComment()
        {
            _tokenChannel = TokenConstants.HiddenChannel;
            while (La_1 != TokenConstants.Eof)
            {
                if (La_1 == '*' && La_2 == '/')
                    break;
                if (!tryParseNewLine())
                    parseOne();
            }
            if (La_1 != TokenConstants.Eof)
            {
                parseOne();
                parseOne();
            }
        }

        void parseId()
        {
            parseType(ID);
            if (La_1 == '@' && La_2 == '@')
            {
                parseOne();
                parseOne();
            }
            parseOne();
            var c = La_1;
            while ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
                    || (c >= '\u00C0' && c <= '\u00D6') || (c >= '\u00D8' && c <= '\u00F6')
                    || (c >= '\u00F8' && c <= '\u02FF') || (c >= '\u0370' && c <= '\u037D')
                    || (c >= '\u037F' && c <= '\u1FFF') || (c >= '\u200C' && c <= '\u200D')
                    || c == '\u00B7' || (c >= '\u0300' && c <= '\u036F') || (c >= '\u203F' && c <= '\u2040')
                    )
            {
                parseOne();
                c = La_1;
            }
        }

        void parseSymbol()
        {
            var c = La_1;
            if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
                || (c >= '\u00C0' && c <= '\u00D6') || (c >= '\u00D8' && c <= '\u00F6')
                || (c >= '\u00F8' && c <= '\u02FF') || (c >= '\u0370' && c <= '\u037D')
                || (c >= '\u037F' && c <= '\u1FFF') || (c >= '\u200C' && c <= '\u200D'))
            {
                parseOne(SYMBOL_CONST);
                c = La_1;
                while ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
                        || (c >= '\u00C0' && c <= '\u00D6') || (c >= '\u00D8' && c <= '\u00F6')
                        || (c >= '\u00F8' && c <= '\u02FF') || (c >= '\u0370' && c <= '\u037D')
                        || (c >= '\u037F' && c <= '\u1FFF') || (c >= '\u200C' && c <= '\u200D')
                        || c == '\u00B7' || (c >= '\u0300' && c <= '\u036F') || (c >= '\u203F' && c <= '\u2040')
                        )
                {
                    parseOne();
                    c = La_1;
                }
                var text = _textSb.ToString().Substring(1);
                if (KwIds.ContainsKey(text))
                {
                    var kwid = KwIds[text];
                    if (kwid >= FIRST_NULL && kwid <= LAST_NULL && kwid != NULL)
                    {
                        // #NIL or #NULL_STRING etc., however #NULL must be allowed as Symbol
                        parseType(NEQ2);
                        _textSb.Clear();
                        _textSb.Append('#');
                        InputStream.Seek(_startCharIndex + 1);
                    }
                    else if (text.Equals("USING", StringComparison.OrdinalIgnoreCase))
                    {
                        parseType(USING);
                    }
                }
                else if (LastToken == NL && text.Equals("PRAGMA", StringComparison.OrdinalIgnoreCase))
                {
                    parseType(PRAGMA);
                    parseToEol();
                    _tokenChannel = PRAGMACHANNEL;
                    HasPragmas = true;
                }
            }
        }

        void parseNumber()
        {
            parseType(La_1 == '.' ? REAL_CONST : INT_CONST);
            bool invalid = false;
            if (La_1 == '.')
                parseOne();
            else if (La_1 == '0')
            {
                parseOne();
                if (La_1 == 'x' || La_1 == 'X')
                {
                    parseOne(HEX_CONST);
                    while ((La_1 >= '0' && La_1 <= '9') || (La_1 >= 'A' && La_1 <= 'F') || (La_1 >= 'a' && La_1 <= 'f') || La_1 == '_')
                        parseOne();
                    if (_textSb[_textSb.Length-1]== '_') invalid = true;
                    if (La_1 == 'U' || La_1 == 'L' || La_1 == 'u' || La_1 == 'l')
                        parseOne();
                    if (invalid)
                        parseType(INVALID_NUMBER);
                    return;
                }
                else if (La_1 == 'b' || La_1 == 'B')
                {
                    parseOne(BIN_CONST);
                    while (La_1 >= '0' && La_1 <= '1' || La_1 == '_')
                        parseOne();
                    if (La_1 == 'U' || La_1 == 'u')
                        parseOne();
                    if (invalid)
                        parseType(INVALID_NUMBER);
                    return;
                }
            }
            while ((La_1 >= '0' && La_1 <= '9') || La_1 == '_')
                parseOne();
            if (_textSb[_textSb.Length - 1] == '_') invalid = true;
            if (parseType() == INT_CONST)
            {
                if (La_1 == 'U' || La_1 == 'L' || La_1 == 'u' || La_1 == 'l')
                {
                    parseOne();
                    if (invalid)
                        parseType(INVALID_NUMBER);
                    return;
                }
                if (La_1 == '.')
                {
                    parseOne(REAL_CONST);
                    if (La_1 >= '0' && La_1 <= '9')
                    {
                        while ((La_1 >= '0' && La_1 <= '9') || La_1 == '_')
                            parseOne();
                        if (_textSb[_textSb.Length - 1] == '_') invalid = true;
                    }
                }
            }
            if (parseType() == REAL_CONST)
            {
                if (La_1 == '.' && La_2 >= '0' && La_2 <= '9' &&
                    (!(La_3 >= '0' && La_3 <= '9') || !(La_4 >= '0' && La_4 <= '9')))
                {
                    string s = _textSb.ToString();
                    int z0 = s.IndexOf('.');
                    if (z0 > 0 && z0 <= 4 && s.Length > z0 + 1 && s.Length <= z0+3 && !s.Contains("_"))
                    {
                        parseOne(DATE_CONST); // append dot
                                              // append day number
                        if (La_1 >= '0' && La_1 <= '9')
                            parseOne();
                        if (La_1 >= '0' && La_1 <= '9')
                            parseOne();
                        return;
                    }
                }
            }
            if (La_1 == 'M' || La_1 == 'm')
                parseOne(REAL_CONST);
            if (La_1 == 'E' || La_1 == 'e')
            {
                int c2 = La_2;
                int c3 = La_3;
                if (((c2 == '+' || c2 == '-') && (c3 >= '0' && c3 <= '9')) || (c2 >= '0' && c2 <= '9'))
                {
                    parseOne(REAL_CONST);   // e
                    parseOne();             // +/-
                    while ((La_1 >= '0' && La_1 <= '9') || La_1 == '_')
                        parseOne();
                    if (_textSb[_textSb.Length - 1] == '_') invalid = true;
                }
            }
            if (La_1 == 'S' || La_1 == 'D' || La_1 == 's' || La_1 == 'd')
                parseOne(REAL_CONST);
            if (invalid)
                parseType(INVALID_NUMBER);
        }

        void parseString()
        {
            parseType(STRING_CONST);
            if (!AllowSingleQuotedStrings && La_1 == '\'')
            {
                parseType(CHAR_CONST);
            }
            else if (La_1 == 'c' || La_1 == 'C')
            {
                parseOne(CHAR_CONST);
            }
            else
            {
                if (La_1 == 'E' || La_1 == 'e')
                {
                    parseOne(ESCAPED_STRING_CONST);
                }
                if (La_1 == 'I' || La_1 == 'i')
                {
                    parseOne(INTERPOLATED_STRING_CONST);
                    if (La_1 == 'E' || La_1 == 'e')
                        parseOne();
                }
            }
            {
                int q = La_1;
                parseOne();
                bool allow_esc = parseType() == CHAR_CONST ?
                    La_1 == '\\' && La_3 == q : parseType() != STRING_CONST;
                bool esc = false;
                while (La_1 != TokenConstants.Eof && (La_1 != q || esc))
                {
                    esc = allow_esc && !esc && La_1 == '\\';
                    parseOne();
                }
                if (La_1 == q)
                    parseOne();
                else if (La_1 == TokenConstants.Eof)
                    parseType(INCOMPLETE_STRING_CONST);
            }
        }

        public override IToken NextToken()
        {
            XSharpToken t;
            {
                parseInit();
                if (La_1 == '\uFEFF') parseSkip();
                switch (La_1)
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
                        if (La_1 == ':')
                            parseOne(COLONCOLON);
                        else if (La_1 == '=')
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
                        if (La_1 == '|')
                            parseOne(OR);
                        else if (La_1 == '=')
                            parseOne(ASSIGN_BITOR);
                        break;
                    case '&':
                        parseOne(AMP);
                        if (Dialect.AllowOldStyleComments() && La_1 == '&')
                            parseSlComment();
                        else if (La_1 == '&')
                            parseOne(AND);
                        else if (La_1 == '=')
                            parseOne(ASSIGN_BITAND);
                        break;
                    case '@':
                        if (La_2 == '@')
                            goto default;
                        parseOne(ADDROF);
                        break;
                    case '-':
                        parseOne(MINUS);
                        if (La_1 == '>')
                            parseOne(ALIAS);
                        else if (La_1 == '-')
                            parseOne(DEC);
                        else if (La_1 == '=')
                            parseOne(ASSIGN_SUB);
                        break;
                    case '+':
                        parseOne(PLUS);
                        if (La_1 == '+')
                            parseOne(INC);
                        else if (La_1 == '=')
                            parseOne(ASSIGN_ADD);
                        break;
                    case '/':
                        parseOne(DIV);
                        if (La_1 == '*')
                        {
                            parseOne(ML_COMMENT);
                            parseMlComment();
                            break;
                        }
                        else if (La_1 == '/')
                        {
                            parseOne(SL_COMMENT);
                            if (La_1 == '/')
                                parseDocComment();
                            else
                                parseSlComment();
                        }
                        else if (La_1 == '=')
                            parseOne(ASSIGN_DIV);
                        break;
                    case '%':
                        parseOne(MOD);
                        if (La_1 == '=')
                            parseOne(ASSIGN_MOD);
                        break;
                    case '^':
                        parseOne(EXP);
                        if (La_1 == '=')
                            parseOne(ASSIGN_EXP);
                        break;
                    case '<':
                        parseOne(LT);
                        if (La_1 == '<')
                        {
                            parseOne(LSHIFT);
                            if (La_1 == '=')
                                parseOne(ASSIGN_LSHIFT);
                        }
                        else if (La_1 == '=')
                            parseOne(LTE);
                        else if (La_1 == '>')
                            parseOne(NEQ);
                        break;
                    case '>':
                        parseOne(GT);
                        // GreaterThanGreaterThanToken is synthesized in the parser since it is ambiguous (with closing nested type parameter lists)
                        if (La_1 == '>' && La_2 == '=')
                        {
                            parseOne(); // >
                            parseOne(ASSIGN_RSHIFT); // =
                        }
                        else if (La_1 == '=')
                            parseOne(GTE);
                        break;
                    case '~':
                        parseOne(TILDE);
                        if (La_1 == '=')
                            parseOne(ASSIGN_XOR);
                        else if (La_1 == '"')           // Old Style Pragma like ~"ONLYEARLY+", treat it as whitespace
                        {
                            parseOne(WS);
                            _tokenChannel = TokenConstants.HiddenChannel;
                            while (La_1 != TokenConstants.Eof && La_1 != '"')
                                parseOne();
                            if (La_1 != TokenConstants.Eof)
                                parseOne();
                        }
                        break;
                    case '*':
                        parseOne(MULT);
                        if (LastToken == NL)
                            parseSlComment();
                        else if (La_1 == '=')
                            parseOne(ASSIGN_MUL);
                        else if (La_1 == '*')
                        {
                            parseOne(EXP);
                            if (La_1 == '=')
                                parseOne(ASSIGN_EXP);
                        }
                        break;
                    case '?':
                        parseOne(QMARK);
                        if (La_1 == '?')
                            parseOne(QQMARK);
                        break;
                    case '=':
                        parseOne(EQ);
                        if (La_1 == '=')
                            parseOne(EEQ);
                        else if (La_1 == '>')
                            parseOne(UDCSEP);
                        break;
                    case '$':
                        parseOne(SUBSTR);
                        break;
                    case '!':
                        parseOne(NOT);
                        if (La_1 == '=')
                            parseOne(NEQ);
                        break;
                    case ';':
                        parseOne(SEMI);
                        do
                        {
                            while (La_1 == ' ' || La_1 == '\t')
                                parseOne();
                            if (La_1 == '/' && La_2 == '*')
                            {
                                parseOne();
                                parseOne();
                                parseMlComment();
                            }
                        } while (La_1 == ' ' || La_1 == '\t');
                        if (La_1 == '/' && La_2 == '/')
                            parseToEol();
                        else if (AllowOldStyleComments && La_1 == '&' && La_2 == '&')
                            parseToEol();
                        if (tryParseNewLine())
                        {
                            parseType(LINE_CONT);
                            _tokenChannel = TokenConstants.HiddenChannel;
                        }
                        if (parseType() == SEMI && _textSb.Length > 1)
                        {
                            _textSb.Remove(1, _textSb.Length - 1);
                            InputStream.Seek(_startCharIndex + 1);
                        }
                        break;
                    case '.':
                        if (La_2 >= '0' && La_2 <= '9')
                        {
                            goto case '0';
                        }
                        parseOne(DOT);
                        if (!_inDottedIdentifier)       // Do not translate .OR., .AND. etc Keywords that are part of a dotted identifier
                        {
                            if (La_2 == '.')
                            {
                                var c1 = La_1;
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
                            else if (La_3 == '.')
                            {
                                var c1 = La_1;
                                var c2 = La_2;
                                if ((c1 == 'O' || c1 == 'o') && (c2 == 'R' || c2 == 'r'))
                                {
                                    parseOne(LOGIC_OR);
                                    parseOne();
                                    parseOne();
                                }
                            }
                            else if (La_4 == '.')
                            {
                                var c1 = La_1;
                                var c2 = La_2;
                                var c3 = La_3;
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
                        if (tryParseNewLine()) parseType(NL);
                        break;
                    case '\t':
                    case ' ':
                        parseWhitespace();
                        break;
                    case 'c':
                    case 'C':
                        if (La_2 == '"' || La_2 == '\'') // char const
                        {
                            goto case '\'';
                        }
                        goto case 'a';
                    case 'e':
                    case 'E':
                        if (La_2 == '"') // escaped string
                        {
                            goto case '\'';
                        }
                        if ((La_2 == 'i' || La_2 == 'I') && La_3 == '"') // interpolated escaped string
                        {
                            goto case '\'';
                        }
                        goto case 'a';
                    case 'i':
                    case 'I':
                        if (La_2 == '"') // interpolated string
                        {
                            goto case '\'';
                        }
                        if ((La_2 == 'e' || La_2 == 'E') && La_3 == '"') // interpolated escaped string
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
                        parseSymbol();
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
                            var c = La_1;
                            if (c == '@')
                                c = La_3;
                            if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
                                    || (c >= '\u00C0' && c <= '\u00D6') || (c >= '\u00D8' && c <= '\u00F6')
                                    || (c >= '\u00F8' && c <= '\u02FF') || (c >= '\u0370' && c <= '\u037D')
                                    || (c >= '\u037F' && c <= '\u1FFF') || (c >= '\u200C' && c <= '\u200D'))
                                goto case 'a';
                        }
                        break;
                }
                if (parsingFailed())
                {
                    if (La_1 == TokenConstants.Eof)
                        parseType(Eof);
                    else
                        parseOne(UNRECOGNIZED);
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
            else if (type == REAL_CONST || type == INT_CONST || type == HEX_CONST || type == BIN_CONST)
            {
                t.Text = t.Text.Replace("_", "");
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
                    if (_isValidIdentifier(t) && La_1 == (int)'.')
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
            else if (!_currentLineHasEos && type == TokenConstants.Eof)
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
                    if (lastToken != EOS && lastToken != NL && lastToken != SEMI && lastToken != EXTERN && lastToken != RBRKT)
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
                    if (lastToken != EOS && lastToken != NL && lastToken != SEMI)
                    {
                        return ID;
                    }
                    break;
                case SWITCH:
                    if (lastToken != EOS && lastToken != NL && lastToken != SEMI && lastToken != BEGIN && lastToken != DO && lastToken != END)
                    {
                        return ID;
                    }
                    break;
                case IMPLIED:
                    // after the following tokens it is always IMPLIED
                    switch (lastToken)
                    {
                        case EOS:
                        case SEMI:
                        case LOCAL:
                        case STATIC:
                        case FOR:
                        case FOREACH:
                        case USING:
                            return IMPLIED;
                    }
                    return ID;
                case NAMESPACE:
                case SCOPE:
                case LOCK:
                    if (lastToken != BEGIN && lastToken != END)
                    {
                        return ID;
                    }
                    break;
                case PARAMETERS:
                    if (!AllowXBaseVariables)
                        return ID;
                    if (lastToken != EOS && lastToken != NL && lastToken != SEMI)
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
                        case FUNCTION:
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

        private IDictionary<string, int> _getIds(XSharpDialect dialect)
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
                    {"_MEMVAR", MEMVAR},
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
                {"_MEMVAR", MEMVAR},
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
            if (!dialect.AllowFourLetterAbbreviations() )
            {
                // These are predefined abbreviations of some keywords that are also valid in Vulcan
                if (!IsMacroLexer)
                {
                    voKeywords.Add("PROC", PROCEDURE);
                    voKeywords.Add("FUNC", FUNCTION);
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
                if (dialect.AllowFourLetterAbbreviations())
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

            if (dialect == XSharpDialect.XPP)
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


            if (dialect == XSharpDialect.FoxPro)
            {
                // Visual FoxPro Keywords
                var vfpKeywords = new Dictionary<string, int>
                {
                    // normal keywords
                };
                var vfpKeyWordAbbrev = new Dictionary<string, int>
                {
                    {"ENDDEFINE",   ENDDEFINE },
                    {"ENDFOR",   NEXT },
                    {"ENDFUNC",   ENDFUNC },
                    {"ENDPROC",   ENDPROC },
                    {"ENDWITH",   ENDWITH },        // duplicated here because it may be abbreviated
                    {"LPARAMETERS",   LPARAMETERS },
                    // text end text
                    {"TEXT",      TEXT },           // TEXT .. ENDTEXT is declared here because the Lexer needs to do some special magic
                    {"ENDTEXT",   ENDTEXT },        // it could also be implemented as UDC but we need the lexer support
                    {"ADDITIVE",  ADDITIVE } ,      // the various options are recognized in the parser and result in a special function call to
                    {"FLAGS",     FLAGS} ,          // process the literal string before it gets assigned.
                    {"PRETEXT",   PRETEXT},
                    {"NOSHOW",    NOSHOW},
                    {"TEXTMERGE", TEXTMERGE},
                };
                foreach (var kw in vfpKeywords)
                {
                    if (!ids.ContainsKey(kw.Key))
                        ids.Add(kw.Key, kw.Value);
                }
                foreach (var kw in vfpKeyWordAbbrev)
                {
                    var name = kw.Key;
                    while (true)
                    {
                        if (!ids.ContainsKey(name))
                        {
                            ids.Add(name, kw.Value);
                        }
                        if (name.Length == 4)
                            break;
                        name = name.Substring(0, name.Length - 1);
                    }
                }
            }

            //if (dialect == XSharpDialect.VO || dialect == XSharpDialect.Vulcan)
            //{
            //    ids.Add("ANY", USUAL);
            //}
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
                    // From VFP: WITH .. [END WITH| ENDWITH]
                    {"WITH",      WITH },
                    {"ENDWITH",   ENDWITH },

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
                    { "__DIALECT_XBASEPP__", MACRO},
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
                    { "__VO__", MACRO},
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
                    { "__VULCAN__", MACRO},
                    { "__WINDIR__", MACRO},
                    { "__WINDRIVE__", MACRO},
                    { "__XPP__", MACRO},
                    { "__XSHARP__", MACRO},
                    { "__XSHARP_RT__", MACRO},
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
                IDictionary<string, int> ids;
                lock (kwlock)
                {
                    ids = _kwids[(int)Dialect];
                }
                if (ids == null)
                {
                    ids = _getIds(Dialect);
                    lock (kwlock)
                    {
                        _kwids[(int)Dialect] = ids;
                    }
                }
                return ids;
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


