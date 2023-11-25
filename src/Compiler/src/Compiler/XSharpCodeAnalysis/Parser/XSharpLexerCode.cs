//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

#nullable disable
using System.Collections.Immutable;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp;
using System;
using Antlr4.Runtime;
using static Roslyn.Utilities.UnicodeCharacterUtilities;

namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser
{
    // Notes: If you want to add a dialect specific keyword then do this:
    // - add the keyword to XSharpLexer.g4 between FIRST_KEYWORD and LAST_KEYWORD
    // best choose a separate spot
    // - add it to the _GetIds() routine below for that dialect
    // - make sure that the FixPositionalKeyword code does not turn it back into an ID
    // - if the keyword has to appear before a DOT check the _inDottedIdentifier logic
    // - add it to the keyword<dialect> rule in XSharp.g4 (so it will become a positional keyword)
    // - add it to the grammar in XSharp.g4

    public class XSharpKeywords : Dictionary<string, int>
    {
        internal XSharpKeywords() : base(Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer)
        {

        }
    }

    public partial class XSharpLexer
    {

        #region constants
        public const int VO_AND = BIT_AND;
        public const int VO_NOT = BIT_NOT;
        public const int VO_OR  = BIT_OR ;
        public const int VO_XOR = BIT_XOR;
        #endregion                          
        #region Static Helper Methods
        // Several Help methods that can be used for colorizing in an editor
        public const int EOF = IntStreamConstants.Eof;
        /// <summary>
        /// Is the token IF, IIF, NAMEOF, TYPEOF or SIZEOF
        /// </summary>
        /// <param name="iToken"></param>
        /// <returns></returns>
        public static bool IsPseudoFunction(int iToken)
        {
            switch (iToken)
            {
                case IIF:
                case NAMEOF:
                case TYPEOF:
                case SIZEOF:
                case IF:
                    return true;
            }
            return false;
        }
        public static bool IsKeyword(int iToken)
        {
            return iToken > FIRST_KEYWORD && iToken < LAST_KEYWORD;
        }
        /// <summary>
        /// Returns TRUE when the token is a comparisons operator, logical operator
        /// but also when it is a symbol like LPAREN, LCURLY, COMMA, PIPE
        /// </summary>
        /// <param name="iToken"></param>
        /// <returns></returns>
        public static bool IsOperator(int iToken)
        {
            return (iToken > FIRST_OPERATOR && iToken < LAST_OPERATOR)
                || (iToken > PP_FIRST && iToken < PP_LAST)
                || iToken == SEMI;
        }
        public static bool IsConstant(int iToken)
        {
            return IsLiteral(iToken);
        }
        /// <summary>
        ///  Returns true If the token one of the literals, NULL_ variations or a MACRO like __ENTITY__
        /// </summary>
        /// <param name="iToken"></param>
        /// <returns></returns>
        public static bool IsLiteral(int iToken)
        {
            return (iToken > FIRST_CONSTANT && iToken < LAST_CONSTANT)
                || (iToken > FIRST_NULL && iToken < LAST_NULL)
                || iToken == MACRO;
        }
        public static bool IsIdentifier(int iToken)
        {
            return iToken == ID;
        }
        public static bool IsType(int iToken)
        {
            return (iToken > FIRST_TYPE && iToken < LAST_TYPE);
        }
        /// <summary>
        /// Return true when the keyword has been added recently and may also be used as identifier
        /// Examples are words like ABSTRACT, VIRTUAL, YIELD etc.
        /// </summary>
        /// <param name="iToken"></param>
        /// <returns></returns>
        public static bool IsPositionalKeyword(int iToken)
        {
            if (iToken > FIRST_POSITIONAL_KEYWORD && iToken < LAST_POSITIONAL_KEYWORD)
                return true;
            return false;
        }
        public static bool IsPPKeyword(int iToken)
        {
            if (iToken > PP_FIRST && iToken < PP_LAST)
                return true;
            return false;
        }
        /// <summary>
        /// Return true if the operator can be used between 2 identifiers, such as 
        /// DOT, COLON, ALIAS, COLONCOLON
        /// </summary>
        /// <param name="iToken"></param>
        /// <returns></returns>
        public static bool IsMemberOperator(int iToken)
        {
            switch (iToken)
            {
                case DOT:
                case COLON:
                case ALIAS:
                case COLONCOLON:
                    return true;
            }
            return false;
        }

        public static bool IsString(int iToken)
        {
            switch (iToken)
            {
                case CHAR_CONST:
                case STRING_CONST:
                case ESCAPED_STRING_CONST:
                case INTERPOLATED_STRING_CONST:
                case INCOMPLETE_STRING_CONST:
                case TEXT_STRING_CONST:
                case BRACKETED_STRING_CONST:
                    return true;
            }
            return false;
        }
        public static bool IsComment(int iToken)
        {
            return iToken == SL_COMMENT || iToken == ML_COMMENT || iToken == DOC_COMMENT;
        }

        #endregion

        #region Properties and Fields
        // Properties to set the behavior of the Lexer
        private readonly List<XSharpToken> pendingTokens = new();
        public CSharpParseOptions Options { get; set; }
        public XSharpDialect Dialect => Options.Dialect;
        private bool AllowOldStyleComments => Dialect.AllowOldStyleComments();
        private bool AllowFourLetterAbbreviations => Dialect.AllowFourLetterAbbreviations();
        private bool AllowSingleQuotedStrings => Dialect.AllowStringsWithSingleQuotes();
        private bool AllowXBaseVariables => Dialect.SupportsMemvars();
        public bool IsScript { get; set; }
        public bool IsMacroLexer { get; set; }
        // Properties that show what the contents of the Lexer buffer was
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
        public int OffSet { get; set; } = 0;
        internal IList<ParseErrorData> LexErrors => _lexErrors;
        int LastToken => _lastToken.Type;

        bool _inDottedIdentifier = false;
        bool _currentLineIsPreprocessorDefinition = false;
        bool _beginOfStatement = true;
        bool _onTextLine = false;
        XSharpToken _lastToken = new(NL);
        readonly IList<ParseErrorData> _lexErrors = new List<ParseErrorData>();
        readonly IList<XSharpToken> _trivia = new List<XSharpToken>();

        readonly System.Text.StringBuilder _textSb = new();
        int _startCharIndex;
        int _startColumn;
        int _startLine;
        int _tokenType;
        int _tokenChannel;
        bool _newLine = false;

        static readonly object kwlock = new();
        static IDictionary<XSharpDialect, XSharpKeywords> _kwids = null ;       // for each dialect its own _kwids
        static XSharpKeywords _symPPIds = null;

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
        #region Helper Methods

        new bool Eof()
        {
            return La(1) == EOF;
        }
        bool Expect(char c)
        {
            return La(1) == c;
        }
        bool Expect(char c1, char c2)
        {
            return La(1) == c1 && La(2) == c2;
        }
        static bool InRange(int c, int first, int last) => c >= first && c <= last;
        static bool InList(int c, int c1, int c2)
        {
            if (c == c1 || c == c2)
            {
                return true;
            }
            return false;
        }
        bool ExpectRange(char c1, char c2)
        {
            if (InRange(La(1), c1, c2))
            {
                return true;
            }
            return false;
        }
        bool ExpectAny(char c1, char c2)
        {
            var c = La(1);
            if (c == c1 || c == c2)
            {
                return true;
            }
            return false;
        }
        bool ExpectAny(char c1, char c2, char c3)
        {
            var c = La(1);
            if (c == c1 || c == c2 || c == c3)
            {
                return true;
            }
            return false;
        }
        bool ExpectAny(char c1, char c2, char c3, char c4)
        {
            var c = La(1);
            if (c == c1 || c == c2 || c == c3 || c == c4)
            {
                return true;
            }
            return false;
        }
        bool ExpectDelimited(string s)
        {
            if (La(1) == s[0])
            {
                // char 2 etc.
                // they may be delimited with spaces
                var j = 2;

                for (var i = 1; i < s.Length; i++)
                {
                    var c = La(j);
                    while (c == ' ' || c == '\t')
                    {
                        j += 1;
                        c = La(j);
                    }
                    if (c != s[i])
                        return false;
                    j += 1;
                }
                return true;
            }
            return false;
        }
        bool Expect(string s)
        {
            if (La(1) == s[0])
            {
                // char 2 etc.
                for (var i = 1; i < s.Length; i++)
                {
                    if (La(i + 1) != s[i])
                        return false;
                }
                return true;
            }
            return false;

        }
        bool ExpectLower(string s, bool skipWs = false)
        {
            var j = 1;
            // Skip leading Whitespace
            if (skipWs)
            {
                var c = La(j);
                while (c != EOF && (c == 32 || c == 9))
                {
                    j++;
                    c = La(j);
                }
            }
            if (char.ToLower((char)La(j)) == s[0])
            {
                // char 2 etc.
                for (var i = 1; i < s.Length; i++)
                {
                    var c = La(i + j);
                    if (c == EOF)
                    {
                        // this could happen when parsing a interpolated string with the contents "i"
                        return false;
                    }
                    if (char.ToLower((char)c) != s[i])
                        return false;
                }
                return true;
            }
            return false;
        }
        int La(int num)
        {
            return InputStream.La(num);
        }
        #endregion
        void parseInit()
        {
            _startCharIndex = InputStream.Index + OffSet;
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
            parseOne();
        }
        void parseOne()
        {
            _textSb.Append((char)InputStream.La(1));
            InputStream.Consume();
        }
        void parseTwo()
        {
            parseOne();
            parseOne();
        }
        void parseType(int type, int count)
        {
            parseType(type);
            for (var i = 0; i < count; i++)
                parseOne();
        }

        void parseFoxProDate()
        {
            // parse {^2019-01-04}
            //       ....5....0...
            parseType(DATE_CONST);
            parseOne();     // {
            parseOne();     // ^
            bool done = false;
            while (!done)
            {
                if (ExpectRange('0', '9') || Expect('-'))
                {
                    parseOne();
                }
                else if (ExpectAny('.', ':'))       // Dates do not have these
                {
                    parseType(DATETIME_CONST, 1);
                }
                else if (ExpectLower("am", true) || ExpectLower("pm", true))
                {
                    parseType(DATETIME_CONST, 2);
                }
                else if (Expect('}'))
                {
                    parseOne();
                    done = true;
                }
                else
                {
                    done = true;
                }
                if (this._textSb.Length > 13 && _tokenType == DATE_CONST)
                    done = true;
            }
            return;
        }


        void parseToEol()
        {
            var la1 = La(1);
            while (la1 != EOF && la1 != '\r' && la1 != '\n')
            {
                parseOne();
                la1 = La(1);
            }
        }

        bool tryParseNewLine()
        {
            if (ExpectAny('\n','\r'))
            {
                if (Expect('\r', '\n'))
                {
                    parseType(NL);
                    parseOne();
                }
                parseOne();
                _newLine = true;
                return true;
            }
            if (Eof())
            {
                parseType(EOF);
                return true;
            }
            return false;
        }

        void parseWhitespace()
        {
            parseOne(WS);
            _tokenChannel = TokenConstants.HiddenChannel;
            while (ExpectAny(' ', '\t'))
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
            while (!Eof())
            {
                if (Expect('*', '/'))
                    break;
                if (tryParseNewLine())
                {
                    HandleNewLine();
                }
                else
                {
                    parseOne();
                }
            }
             parseType(ML_COMMENT);
            if (!Eof())
            {
                // Eat the */
                parseTwo();
            }
        }

        void parseId()
        {
            bool allowDot = false;
            if (Options.SupportsMemvars && _lastToken.Type == AMP && _lastToken.Position == _startCharIndex - 1)
            {
                // macro-ed name can have dot, such as PUBLIC &varName.10 which would result in __MemVarDecl(varName+"10",...)
                allowDot = true;
            }
            parseType(ID);
            if (Expect("@@"))
            {
                parseTwo();
            }
            parseOne();
            var c = La(1);

            while (c > 0 && (IsIdentifierPartCharacter((char)c) || (c == '.' && allowDot)))
            {
                parseOne();
                if (c == '.')
                {
                    allowDot = false;
                }
                c = La(1);
            }
        }

        void parseSymbol()
        {
            // Todo: The VFP and Xbase++ dialect do not know
            // the symbol type. So they see
            // a#b as a # b
            // where VO see this as
            // a #b
            // These dialects however SHOULD support #ifdef etc and VFP also #null

            switch (_lastToken.Type)
            {
                case ID:
                case RPAREN:
                case RCURLY:
                case RBRKT:
                    if (_lastToken.StopIndex == _startCharIndex - 1)
                        return;
                    break;
            }



            var c = La(1);
            if (c > 0 && IsIdentifierStartCharacter((char)c))
            {
                parseOne(SYMBOL_CONST);
                c = La(1);
                while (c > 0 && IsIdentifierPartCharacter((char)c))
                {
                    parseOne();
                    c = La(1);
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
            }
        }

        void parseNumber()
        {
            parseType(La(1) == '.' ? REAL_CONST : INT_CONST);
            bool invalid = false;
            bool currency = false;
            if (Expect('$'))
            {
                parseOne();
                currency = true;
            }
            if (ExpectAny('.', '$'))
                parseOne();
            else if (Expect('0'))
            {
                parseOne();
                if (ExpectAny('X', 'x'))
                {
                    parseOne(HEX_CONST);
                    while (ExpectRange('0', '9') || ExpectRange('A', 'F') || ExpectRange('a', 'f') || Expect('_'))
                        parseOne();
                    if (_textSb[_textSb.Length - 1] == '_')
                        invalid = true;
                    if (ExpectAny('U', 'L', 'u', 'l'))
                        parseOne();
                    if (invalid)
                        parseType(INVALID_NUMBER);
                    return;
                }
                else if (ExpectAny('b', 'B'))
                {
                    parseOne(BIN_CONST);
                    while (ExpectAny('0', '1', '_'))
                        parseOne();
                    if (ExpectAny('U', 'u'))
                        parseOne();
                    if (invalid)
                        parseType(INVALID_NUMBER);
                    return;
                }
                else if (ExpectAny('h', 'H'))
                {
                    parseOne(BINARY_CONST);
                    while (ExpectRange('0', '9') || ExpectRange('A', 'F') || ExpectRange('a', 'f'))
                        parseOne();
                    return;
                }
            }
            // not a Hex, bin or Binary constant
            while (ExpectRange('0', '9') || Expect('_'))
                parseOne();
            if (_textSb[_textSb.Length - 1] == '_')
                invalid = true;
            if (parseType() == INT_CONST)
            {
                if (ExpectAny('U', 'u', 'L', 'l'))
                {
                    parseOne();
                    if (invalid)
                        parseType(INVALID_NUMBER);
                    return;
                }
                if (Expect('.') && (!ExpectLower(".and.") && !ExpectLower(".or.")))
                {
                    // The '.' should not be the start of .and. or .or.
                    parseOne(REAL_CONST);
                    if (ExpectRange('0', '9'))
                    {
                        while (ExpectRange('0', '9') || Expect('_'))
                            parseOne();
                        if (_textSb[_textSb.Length - 1] == '_')
                            invalid = true;
                    }
                }
            }
            if (parseType() == REAL_CONST)
            {
                if (Expect('.') && InRange(La(2), '0', '9') &&
                    (!InRange(La(3), '0', '9') ||
                    !InRange(La(4), '0', '9')))
                {
                    string s = _textSb.ToString();
                    int z0 = s.IndexOf('.');
                    if (z0 > 0 && z0 <= 4 && s.Length > z0 + 1 && s.Length <= z0 + 3 && !s.Contains("_"))
                    {
                        parseOne(DATE_CONST); // append dot
                                              // append day number
                        if (ExpectRange('0', '9'))
                            parseOne();
                        if (ExpectRange('0', '9'))
                            parseOne();
                        return;
                    }
                }
            }
            if (ExpectAny('M', 'm'))
                parseOne(REAL_CONST);
            if (ExpectAny('E', 'e'))
            {
                var c2 = La(2);
                if ((InList(c2, '+', '-') && InRange(La(3), '0', '9')) || InRange(c2, '0', '9'))
                {
                    parseOne(REAL_CONST);   // e
                    parseOne();             // +/-
                    while (ExpectRange('0', '9') || Expect('_'))
                        parseOne();
                    if (_textSb[_textSb.Length - 1] == '_')
                        invalid = true;
                }
            }
            if (ExpectAny('S', 'D', 's', 'd'))
                parseOne(REAL_CONST);
            if (invalid)
                parseType(INVALID_NUMBER);
            if (currency)
                parseType(REAL_CONST);
        }

        void parseString()
        {
            bool allow_esc = false;
            parseType(STRING_CONST);
            if (!AllowSingleQuotedStrings && Expect('\''))
            {
                parseType(CHAR_CONST);
                allow_esc = true;
            }
            else if (ExpectAny('C', 'c'))
            {
                parseOne(CHAR_CONST);
                allow_esc = true;
            }
            else if (Expect('['))
            {
                parseType(BRACKETED_STRING_CONST);
            }
            else
            {
                if (ExpectAny('E', 'e'))
                {
                    parseOne(ESCAPED_STRING_CONST);
                    allow_esc = true;
                }
                if (ExpectAny('I', 'i'))
                {
                    parseOne(INTERPOLATED_STRING_CONST);
                    if (ExpectAny('E', 'e'))
                    {
                        allow_esc = true;
                        parseOne();
                    }
                }
            }
            int q = La(1);
            if (_tokenType == BRACKETED_STRING_CONST)
            {
                q = ']';
            }
            parseOne();
            bool esc = false;
            bool eos = false;
            while (!eos)
            {
                switch (La(1))
                {
                    case EOF:
                    case 10:        // \n
                    case 13:        // \r
                        eos = true;
                        parseType(INCOMPLETE_STRING_CONST);
                        break;
                    default:
                        bool eat2 = false;
                        if (La(1) == q && !esc)
                        {
                            if (La(2) == q)              // allow 2 double quotes to be seen as a single double quote
                                eat2 = true;
                            else
                                eos = true;
                        }
                        if (allow_esc)
                        {
                            esc = !esc && La(1) == '\\';
                        }
                        parseOne();
                        if (eat2)
                        {
                            InputStream.Consume();
                        }
                        break;
                }
            }
        }

        private XSharpToken parseTextLine()
        {
            // parse a complete line into a TEST_STRING_CONST after \ or \\
            while (!Eof() && !ExpectAny('\r', '\n'))
                parseOne();
            parseType(TEXT_STRING_CONST);
            Interpreter.Column += (InputStream.Index - _startCharIndex);
            XSharpToken t = TokenFactory.Create(this.SourcePair, _tokenType, _textSb.ToString(), _tokenChannel, _startCharIndex, CharIndex - 1, _startLine, _startColumn) as XSharpToken;
            Emit(t);
            _onTextLine = false;
            return t;

        }

        private XSharpToken parseSemiColon()
        {
            // Semi colon can be:
            // - statement delimiter character. In that case its type is SEMI and it is on the normal token channel
            // - line continuation character. In that case its type is LINE_CONT and it is on the hidden channel.
            //   the second case happens when the token is followed by whitespace - EOL and optional comments between the SEMI and the EOL
            //   for the syntax highlighting in VS we want to mark the comments in the proper color, so we generate separate tokens for the
            //   comments. These tokens are added to a list, because we are collecting them while determining the true nature of the SEMI token.
            parseOne(SEMI);
            var t = TokenFactory.Create(this.SourcePair, _tokenType, _textSb.ToString(), _tokenChannel, _startCharIndex, CharIndex - 1, _startLine, _startColumn) as XSharpToken;
            parseInit();
            do
            {
                while (ExpectAny(' ', '\t'))
                    parseOne();
                // push possible whitespace token
                pushToken(WS, TokenConstants.HiddenChannel);
                if (Expect("/*"))
                {
                    parseTwo();
                    parseMlComment();
                    pushToken(ML_COMMENT, TokenConstants.HiddenChannel);
                }
            } while (ExpectAny(' ', '\t'));
            if (Expect("//"))
            {
                parseToEol();
                pushToken(SL_COMMENT, TokenConstants.HiddenChannel);

            }
            else if (AllowOldStyleComments && Expect("&&"))
            {
                parseToEol();
                pushToken(SL_COMMENT, TokenConstants.HiddenChannel);
            }
            if (tryParseNewLine())
            {
                // make sure to push the CRLF as WS, so it will not be recognized as EOS
                pushToken(WS, TokenConstants.HiddenChannel);
                t.Type = LINE_CONT;
                t.Channel = TokenConstants.HiddenChannel;
                HandleNewLine();
                return t;
            }
            // when we get here then the semi colon was a statement delimiter
            if (_beginOfStatement)
            {
                t.Channel = t.OriginalChannel = TokenConstants.HiddenChannel;
            }
            else
            {
                _beginOfStatement = true;
                t.Type = EOS;
            }
            _inDottedIdentifier = false;
            return t;

        }

#if VSPARSER
        int regionLine = -1;
#endif
        private void handleTrivia(XSharpToken t)
        {
            if (t.IsTrivia)
            {
                _trivia.Add(t);
            }
#if VSPARSER
            else if (t.Channel == PREPROCESSORCHANNEL)
            {
                if (t.Type == PP_REGION)
                {
                    // Make sure #region and #endregion lines are compiled as trivia
                    _trivia.Add(t);
                    regionLine = t.Line;
                }
                else if (t.Type == PP_ENDREGION || t.Line == regionLine)
                {
                    _trivia.Add(t);
                }
                else
                {
                    // skip other preprocessor tokens
                }
            }
#endif
            else if (t.CanHaveTrivia && _trivia.Count > 0)
            {
                t.Trivia = _trivia.ToImmutableArray();
                _trivia.Clear();
            }
        }
        private void handleSpecialFunctions()
        {
            // Handle function names that are the same as keywords
            // This gets called when the LPAREN token is found
            // This prevents the tokens to be seen as keyword and also makes
            // Sure that in the editor the case of the tokens is not changed
            switch (LastToken)
            {
                case FOR:                               // For ()
                case FIELD:                             // Field()
                    if (Dialect == XSharpDialect.FoxPro)
                    {
                        _lastToken.Type = ID;
                    }
                    break;
                case DATETIME:                          // DateTime (....)
                case ARRAY:                             // Array (...)
                    _lastToken.Type = ID;
                    break;
            }
        }

        private void HandleNewLine()
        {
            if (_newLine)
            {
                Interpreter.Line += 1;
                Interpreter.Column = 0;
                _newLine = false;
            }
        }
        private void setLastToken(XSharpToken t)
        {
            if (t.Channel == TokenConstants.DefaultChannel)
                _lastToken = t;
        }

        public override IToken NextToken()
        {
            if (pendingTokens.Count > 0)
            {
                var token = popToken();
                handleTrivia(token);
                if (_beginOfStatement && (token.Channel == DefaultTokenChannel || token.Channel == PREPROCESSORCHANNEL))
                    _beginOfStatement = false;
                setLastToken(token);
                return token;
            }
            XSharpToken t;
            {
                parseInit();
                if (_onTextLine)
                {
                    return parseTextLine();
                }
                if (La(1) == '\uFEFF')
                    parseSkip();
                switch (La(1))
                {
                    case '(':
                        parseOne(LPAREN);
                        handleSpecialFunctions();   // Handle function names that are the same as keywords
                        break;
                    case ')':
                        parseOne(RPAREN);
                        break;
                    case '{':
                        if (Dialect == XSharpDialect.FoxPro)
                        {
                            if (ExpectDelimited("{//}") ||
                                ExpectDelimited("{--}") ||
                                ExpectDelimited("{..}"))
                            {
                                parseType(NULL_DATE);
                                while (!Expect('}'))
                                {
                                    parseOne();
                                }
                                parseOne();
                                break;
                            }
                        }
                        if (La(2) == '^' && La(3) >= '0' && La(3) <= '9')
                        {
                            parseFoxProDate();
                        }
                        else
                        {
                            parseOne(LCURLY);
                        }
                        break;
                    case '}':
                        parseOne(RCURLY);
                        break;
                    case '[':
                        if (this.Dialect == XSharpDialect.Core)
                        {
                            parseOne(LBRKT);
                            break;
                        }
                        switch (LastToken)
                        {
                            case ID:
                            case RPAREN:
                            case RCURLY:
                            case RBRKT:
                                parseOne(LBRKT);
                                break;
                            case RETURN:
                            case GET:
                                parseString();
                                break;
                            default:
                                if (Options.MacroScript)
                                {
                                    parseString();
                                }
                                else if (StartOfLine(LastToken) || IsKeyword(LastToken) || _currentLineIsPreprocessorDefinition)
                                {
                                    parseOne(LBRKT);
                                }
                                else
                                {
                                    parseString();
                                }
                                //parseOne(LBRKT);
                                break;
                        }
                        break;
                    case ']':
                        parseOne(RBRKT);
                        break;
                    case ':':
                        parseOne(COLON);
                        if (Expect(':'))
                            parseOne(COLONCOLON);
                        else if (Expect('='))
                            parseOne(ASSIGN_OP);
                        break;
                    case ',':
                        parseOne(COMMA);
                        break;
                    case '\\':       // used inside #command to escape '<'
                        parseOne(BACKSLASH);
                        if (StartOfLine(LastToken) && Options.Dialect == XSharpDialect.FoxPro)
                        {
                            if (Expect('\\'))
                                parseOne(BACKBACKSLASH);
                            _onTextLine = true;
                        }
                        break;
                    case '|':
                        parseOne(PIPE);
                        if (Expect('|'))
                            parseOne(OR);
                        else if (Expect('='))
                            parseOne(ASSIGN_BITOR);
                        break;
                    case '&':
                        parseOne(AMP);
                        if (Dialect.AllowOldStyleComments() && Expect('&'))
                            parseSlComment();
                        else if (Expect('&'))
                            parseOne(AND);
                        else if (Expect('='))
                            parseOne(ASSIGN_BITAND);
                        break;
                    case '@':
                        if (La(2) == '@')
                            goto default;
                        parseOne(ADDROF);
                        break;
                    case '-':
                        parseOne(MINUS);
                        if (Expect('>'))
                            parseOne(ALIAS);
                        else if (Expect('-'))
                            parseOne(DEC);
                        else if (Expect('='))
                            parseOne(ASSIGN_SUB);
                        break;
                    case '+':
                        parseOne(PLUS);
                        if (Expect('+'))
                            parseOne(INC);
                        else if (Expect('='))
                            parseOne(ASSIGN_ADD);
                        break;
                    case '/':
                        parseOne(DIV);
                        if (Expect('/'))
                        {
                            parseOne(SL_COMMENT);
                            if (Expect('/'))
                                parseDocComment();
                            else
                                parseSlComment();
                        }
                        else if (Expect('*'))
                        {
                            parseOne(ML_COMMENT);
                            parseMlComment();
                            break;
                        }
                        else if (Expect('='))
                            parseOne(ASSIGN_DIV);
                        break;
                    case '%':
                        parseOne(MOD);
                        if (Expect('='))
                            parseOne(ASSIGN_MOD);
                        break;
                    case '^':
                        parseOne(EXP);
                        if (Expect('='))
                            parseOne(ASSIGN_EXP);
                        break;
                    case '<':
                        parseOne(LT);
                        if (Expect('<'))
                        {
                            parseOne(LSHIFT);
                            if (Expect('='))
                                parseOne(ASSIGN_LSHIFT);
                        }
                        else if (Expect('='))
                            parseOne(LTE);
                        else if (Expect('>'))
                            parseOne(NEQ);
                        break;
                    case '>':
                        parseOne(GT);
                        // GreaterThanGreaterThanToken is synthesized in the parser since it is ambiguous (with closing nested type parameter lists)
                        if (Expect(">="))
                        {
                            parseOne(); // >
                            parseOne(ASSIGN_RSHIFT); // =
                        }
                        else if (Expect('='))
                            parseOne(GTE);
                        break;
                    case '~':
                        parseOne(TILDE);
                        if (Expect('='))
                            parseOne(ASSIGN_XOR);
                        else if (Expect('"'))           // Old Style Pragma like ~"ONLYEARLY+", treat it as whitespace
                        {
                            parseOne(WS);
                            _tokenChannel = TokenConstants.HiddenChannel;
                            while (!Eof() && !Expect('"'))
                                parseOne();
                            if (!Eof())
                                parseOne();
                        }
                        break;
                    case '*':
                        parseOne(MULT);
                        if (StartOfLine(LastToken))
                            parseSlComment();
                        else if (Expect('='))
                            parseOne(ASSIGN_MUL);
                        else if (Expect('*'))
                        {
                            parseOne(EXP);
                            if (Expect('='))
                                parseOne(ASSIGN_EXP);
                        }
                        break;
                    case '?':
                        parseOne(QMARK);
                        if (Expect('?'))
                        {
                            parseOne(QQMARK);
                            if (Expect('='))
                                parseOne(ASSIGN_QQMARK);
                        }
                        break;

                    case '=':
                        parseOne(EQ);
                        if (Expect('='))
                            parseOne(EEQ);
                        else if (Expect('>'))
                            parseOne(UDCSEP);
                        break;
                    case '$':
                        switch (La(2))
                        {
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
                            case '.':
                                parseNumber();
                                break;
                            default:
                                parseOne(SUBSTR);
                                break;
                        }
                        break;
                    case '!':
                        parseOne(NOT);
                        if (Expect('='))
                            parseOne(NEQ);
                        break;
                    case ';':
                        t = parseSemiColon();
                        handleTrivia(t);
                        setLastToken(t);
                        return t;
                    case '.':
                        if (InRange(La(2), '0', '9'))
                        {
                            goto case '0';
                        }
                        parseOne(DOT);
                        if (La(2) == '.' && !_inDottedIdentifier) // literals cannot happen between IDs
                        {
                            if (ExpectAny('F', 'f', 'N', 'n'))
                            {
                                parseType(FALSE_CONST, 2);
                            }
                            else if (ExpectAny('T', 't', 'Y', 'y'))
                            {
                                parseType(TRUE_CONST, 2);
                            }
                            else if (Expect('.'))
                            {
                                parseType(ELLIPSIS, 2);
                            }
                        }
                        else if (La(3) == '.') // a.or.b should be allowed, so no check for _inDottedIdentifier
                        {
                            if (ExpectLower("or"))
                            {
                                parseType(LOGIC_OR, 3);
                                _inDottedIdentifier = false;
                            }
                        }
                        else if (La(4) == '.') // a.and.b should be allowed, so no check for _inDottedIdentifier
                        {
                            if (ExpectLower("and"))
                            {
                                parseType(LOGIC_AND, 4);
                                _inDottedIdentifier = false;
                            }
                            else if (ExpectLower("not"))
                            {
                                parseType(LOGIC_NOT, 4);
                                _inDottedIdentifier = false;
                            }
                            else if (ExpectLower("xor"))
                            {
                                parseType(LOGIC_XOR, 4);
                                _inDottedIdentifier = false;
                            }
                        }
                        else if (La(5) == '.' && !_inDottedIdentifier && Dialect == XSharpDialect.FoxPro)
                        {
                            // map .NULL. => NULL_FOX => DBNull.Value
                            if (ExpectLower("null"))
                            {
                                parseType(NULL_FOX, 5);
                                _inDottedIdentifier = false;
                            }
                        }
                        break;
                    case '\r':
                    case '\n':
                        if (tryParseNewLine())
                            parseType(NL);
                        break;
                    case '\t':
                    case ' ':
                        parseWhitespace();
                        break;
                    case 'c':
                    case 'C':
                        if (InList(La(2), '"', '\'')) // char const
                        {
                            goto case '\'';
                        }
                        goto case 'a';
                    case 'e':
                    case 'E':
                        if (La(2) == '"') // escaped string
                        {
                            goto case '\"';
                        }
                        if (ExpectLower("ei\"")) // escaped interpolated string
                        {
                            goto case '\"';
                        }
                        goto case 'a';
                    case 'i':
                    case 'I':
                        if (La(2) == '"') // interpolated string
                        {
                            goto case '\'';
                        }
                        if (ExpectLower("ie\"")) // interpolated escaped string
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
                            var c = La(1);
                            if (c == '@')
                                c = La(3);
                            if (c > 0 && IsIdentifierStartCharacter((char)c))
                                goto case 'a';
                        }
                        break;
                }
                if (parsingFailed())
                {
                    if (Eof())
                        parseType(EOF);
                    else
                        parseOne(UNRECOGNIZED);
                }
                Interpreter.Column += (InputStream.Index - _startCharIndex);
                t = TokenFactory.Create(this.SourcePair, _tokenType, _textSb.ToString(), _tokenChannel, _startCharIndex, CharIndex - 1, _startLine, _startColumn) as XSharpToken;
                Emit(t);
                HandleNewLine();
                if (t.Type == ML_COMMENT)
                {
                    if (!t.Text.EndsWith("*/"))
                    {
                        _lexErrors.Add(new ParseErrorData(t, ErrorCode.ERR_OpenEndedComment));
                    }
                }
            }
            var type = t.Type;
            if (findKeyWord(t, LastToken))
            {
                type = t.Type;
            }
            else if (type == REAL_CONST || type == INT_CONST || type == HEX_CONST || type == BIN_CONST)
            {
                t.Text = t.Text.Replace("_", "");
            }
            else if (type == SYMBOL_CONST && (StartOfLine(LastToken) || LastToken == UDCSEP))
            {
                if (SymPPIds.TryGetValue(t.Text, out var symtype))
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
                        case PP_TEXT:
                        case PP_ENDTEXT:
                        case PP_PRAGMA:
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
            if (type != EOF)
            {
                if (!_inDottedIdentifier)
                {
                    // Check if the current token is a valid Identifier (starts with A..Z or _) and is followed by a DOT
                    // In that case we change the type from Keyword to ID
                    if (_isValidIdentifier(t) && Expect('.'))
                    {
                        if (t.Type != SELF && t.Type != SUPER && t.Type != FOX_M)
                        {
                            t.Type = ID;
                        }
                        _inDottedIdentifier = true;
                    }
                    else if (type == ID)
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
                    else if (type != DOT && type != ID)
                    {
                        _inDottedIdentifier = false;
                    }
                }
            }
            if (type == NL)    // Semi colon EOS is handled in parseSemi()
            {
                if (_beginOfStatement)
                {
                    t.Channel = t.OriginalChannel = TokenConstants.HiddenChannel;
                }
                else
                {
                    t.Type = EOS;
                    _beginOfStatement = true;
                }
            }
            else if (_beginOfStatement && t.Channel == TokenConstants.DefaultChannel)
            {
                _beginOfStatement = false;
            }
            else if (!_beginOfStatement && type == EOF)
            {
                t.Type = EOS;
                _beginOfStatement = true;
            }
            if (t.Type == ALIAS)
            {
                // ALIAS after keyword, then the keyword should be the name of a workarea. For example EVENT->DATE
                if (IsKeyword(LastToken) && LastToken != FIELD && LastToken != MEMVAR)
                {
                    _lastToken.Type = ID;
                }
            }
            setLastToken(t);
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
                    if (type == NL || type == EOF)
                    {
                        // end of line ends the preprocessor definition
                        _currentLineIsPreprocessorDefinition = false;
                    }
                }
            }
            handleTrivia(t);
            return t;
        }

        private XSharpToken popToken()
        {
            if (pendingTokens.Count > 0)
            {
                var t = pendingTokens[0];
                pendingTokens.RemoveAt(0);
                return t;
            }
            return null;

        }

        private void pushToken(int type, int channel)
        {
            if (_textSb.Length > 0)
            {
                var token = TokenFactory.Create(this.SourcePair, type, _textSb.ToString(), channel, _startCharIndex, CharIndex - 1, _startLine, _startColumn) as XSharpToken;
                pendingTokens.Add(token);
                parseInit();
            }
        }
        private static bool StartOfLine(int iToken)
        {
            switch (iToken)
            {
                case EOS:
                case -1:
                case NL:
                case SEMI:
                case UDCSEP:        // first keyword in UDC after '=>' is considered to be at start of line
                    return true;
            }
            return false;
        }

        public static bool IsModifier(int iToken)
        {
            // Make sure this stays in sync with the various modifier rules in XSharp.g4
            switch (iToken)
            {
                case ABSTRACT:
                case ASYNC:
                case CONST:
                case EXPORT:
                case EXTERN:
                case FIXED:
                case HIDDEN:
                case INITONLY:
                case INSTANCE:
                case INTERNAL:
                case NEW:
                case OVERRIDE:
                case PARTIAL:
                case PRIVATE:
                case PROTECTED:
                case PUBLIC:
                case SEALED:
                case STATIC:
                case UNSAFE:
                case VIRTUAL:
                case VOLATILE:

                // Xbase++ modifiers

                case CLASS:
                case DEFERRED:
                case FINAL:
                case FREEZE:
                case INLINE:
                case INTRODUCE:
                case SYNC:
                    return true;
            }
            return false;
        }

        #region Keywords and Preprocessor Lookup
        private int fixPositionalKeyword(int keyword, int lastToken, string text)
        {
            // after the following tokens we treat everything as ID
            if (IsMemberOperator(lastToken))
            {
                return ID;
            }
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
                // Next tokens only at Start of Line
                case FOREACH:
                case FINALLY:
                case CATCH:
                case REPEAT:
                case UNTIL:
                case YIELD:
                case ENDDEFINE:
                case ENDCLASS:
                case DIMENSION:
                case DECLARE:
                case LPARAMETERS:
                case NOP:
                    if (!StartOfLine(lastToken))
                    {
                        return ID;
                    }
                    break;
                case FUNCTION:
                    if (text.Length == 4)
                    {
                        // if specified as FUNC then we want to make sure that it is not a type
                        if (!StartOfLine(lastToken) && !IsModifier(lastToken)
                            && lastToken != RBRKT && lastToken != END
                            && lastToken != LOCAL && lastToken != DLL)
                        {
                            return ID;
                        }
                    }
                    break;
                case ENUM:          //Should appear after EOS, END, Modifier, LBRKT or RBRKT
                                    // Potential problem in the editor because Enum is also a type.
                    if (!StartOfLine(lastToken) && !IsModifier(lastToken)
                        && lastToken != RBRKT && lastToken != LBRKT && lastToken != END)
                    {
                        return ID;
                    }
                    break;
                case DEFAULT:
                    if (StartOfLine(lastToken))     // DEFAULT is not a keyword when at the start of a line like in Default(@someVar, someValue)
                    {
                        return ID;
                    }
                    break;
                // Next tokens only at Start of Line or after STATIC or INTERNAL
                case DEFINE:
                    if (!StartOfLine(lastToken) && !IsModifier(lastToken) && lastToken != END)
                    {
                        return ID;
                    }
                    break;
                // Next token only at Start of Line or after BEGIN, DO and END
                case SWITCH:
                    if (!StartOfLine(lastToken) && lastToken != BEGIN && lastToken != DO && lastToken != END)
                    {
                        return ID;
                    }
                    break;
                // Next tokens only at Start of Line or after END
                case TRY:
                    if (!StartOfLine(lastToken) && lastToken != END)
                    {
                        return ID;
                    }
                    break;
                // Next token only at Start of Line or after BEGIN, DO and END
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
                // Next tokens only after BEGIN and END
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
                    if (!(StartOfLine(lastToken)))
                    {
                        return ID;
                    }
                    break;
                // These entity types at start of line, after modifier or attribute, after DLL or after END
                case CONSTRUCTOR:
                case DESTRUCTOR:
                case EVENT:
                case PROPERTY:
                case INTERFACE:
                case VOSTRUCT:
                case UNION:
                case OPERATOR:
                    if (!StartOfLine(lastToken) && !IsModifier(lastToken) && lastToken != DLL && lastToken != LBRKT && lastToken != RBRKT && lastToken != END)
                    {
                        return ID;
                    }
                    break;
                // These modifiers at start of line, after modifier or attribute
                case INITONLY:
                case PARTIAL:
                case SEALED:
                case ABSTRACT:
                // XBase++ modifiers
                case FREEZE:
                case FINAL:
                case INTRODUCE:
                case SYNC:
                case DEFERRED:
                case INLINE:    // should not appear after modifier ...
                    if (!StartOfLine(lastToken) && !IsModifier(lastToken) && lastToken != RBRKT)
                    {
                        return ID;
                    }
                    break;
                // This modifiers at start of line, after modifier or attribute and also after BEGIN or END
                case UNSAFE:
                    if (!StartOfLine(lastToken) && !IsModifier(lastToken) && lastToken != RBRKT && lastToken != BEGIN && lastToken != END)
                    {
                        return ID;
                    }
                    break;
                case EACH:                   // FoxPro dialect and only after FOR
                    if (lastToken != FOR)
                        return ID;
                    else
                        return keyword;
                case FOX_M:
                    return keyword;
            }
            // when all else fails...
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

                case DEFINE:
                    if (Dialect == XSharpDialect.FoxPro && keyword == CLASS)   // FoxPro uses DEFINE CLASS
                        return keyword;
                    else
                        return ID;

                case LOCAL:
                    if (keyword == FUNCTION || keyword == PROCEDURE || keyword == ARRAY)    // local function and procedure statement and local array
                        return keyword;
                    return ID;

                case GLOBAL:
                    if (keyword == CONST)    // GLOBAL CONST Id
                        return keyword;
                    return ID;

                // After these keywords we expect an ID
                // Some of these also have a possible SELF, DIM, CONST or STATIC clause but these have been excluded above
                case STRUCTURE when keyword == WHERE:
                    return WHERE;
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
                case ENUM:
                case MEMBER:
                case DIM:
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
            return keyword;
        }
        private bool findKeyWord(XSharpToken token, int lastToken)
        {
            if (token.Type == ID && token.Channel == Lexer.DefaultTokenChannel)
            {
                if (KwIds.TryGetValue(token.Text, out var kwtype))
                {
                    if (IsPositionalKeyword(kwtype) && (lastToken == COLON || lastToken == DOT))
                    {
                        ; // do nothing, no new keywords after colon or dot
                    }
                    else
                    {
                        kwtype = fixPositionalKeyword(kwtype, lastToken, token.Text);
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
        private static bool _isValidIdentifier(IToken t)
        {
            if (t == null || t.Text?.Length == 0 || t.Type == EOF)
                return false;

            switch (t.Channel)
            {
                case XSharpLexer.Hidden: // 1
                case XSharpLexer.XMLDOCCHANNEL:  // 2
                case XSharpLexer.DEFOUTCHANNEL: // 3
                    return false;
                case XSharpLexer.PREPROCESSORCHANNEL:  // 4
                case TokenConstants.DefaultChannel: // 0
                default:
                    char fc = t.Text?[0] ?? (char)0;
                    return fc == '_' || InRange(fc, 'A', 'Z') || InRange(fc, 'a', 'z');
            }
        }

        private XSharpKeywords _getIds(XSharpDialect dialect)
        {
            var ids = new XSharpKeywords(); ;

            XSharpKeywords voKeywords;
            if (IsMacroLexer)
            {
                // short list of keywords used in Macro Expressions
                // keywords for statements and entities are not included
                voKeywords = new XSharpKeywords
                {
                    {"_AND", BIT_AND},
                    {"BREAK", BREAK},
                    {"_CAST", CAST},
                    {"EXPORT", EXPORT},
                    {"_FIELD", FIELD},
                    {"IF", IF},
                    {"IIF", IIF},
                    {"IS", IS},
                    {"MEMVAR", MEMVAR},
                    {"_MEMVAR", MEMVAR},
                    {"_NOT", BIT_NOT},
                    {"_OR", BIT_OR},
                    {"_XOR", BIT_XOR},

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
                voKeywords = new XSharpKeywords
            {
                {"ACCESS", ACCESS},
                {"ALIGN", ALIGN},
                {"_AND", BIT_AND},
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
                {"_NOT", BIT_NOT},
                {"_OR", BIT_OR},
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
                {"THIS", SELF},
                {"THISCALL", THISCALL},
                {"TO", TO},
                //{"_TYPEOF", TYPEOF},  VO does not allow 4 letter abbreviations
                {"UNION", UNION},
                {"UPTO", UPTO},
                {"USING", USING},
                {"_WINCALL", WINCALL},
                {"WHILE", WHILE},
                {"_XOR", BIT_XOR},

                // Predefined types
                {"ARRAY", ARRAY},
                {"BINARY", BINARY},
                {"BYTE", BYTE},
                {"CODEBLOCK", CODEBLOCK},
                {"CURRENCY", CURRENCY},
                {"DATE", DATE},
                {"DATETIME", DATETIME},
                {"DWORD", DWORD},
                {"DECIMAL", DECIMAL},
                {"FLOAT", FLOAT},
                {"INT", INT},
                {"LOGIC", LOGIC},
                {"LONGINT", LONGINT},
                {"NINT", NINT},
                {"NUINT", NUINT},
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
            if (!dialect.AllowFourLetterAbbreviations())
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
                // Xbase++ Keywords
                var xppKeywords = new XSharpKeywords
                {
                    // normal keywords
                    // {"ENDSEQUENCE", END }, Xbase++ redefines ENDSEQUENCE to END in STD.CH
                    // We handle that in XBasePPCmd.xh
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
                var xppKeyWordAbbrev = new XSharpKeywords
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
            if (dialect == XSharpDialect.FoxPro)
            {
                // Visual FoxPro Keywords
                var vfpKeywords = new XSharpKeywords
                {
                    // normal keywords
                    {"THIS_ACCESS",THISACCESS },
                    {"HELPSTRING",HELPSTRING },
                    {"DIMENSION",DIMENSION},
                    {"AND", FOX_AND},
                    {"OR", FOX_OR},
                    {"NOT", FOX_NOT},
                    {"THEN", THEN},
                    {"XOR", FOX_XOR},
                    {"EACH", EACH },                // Only after FOR
                    {"M", FOX_M }                   // FoxPro allows LOCAL M.Name and PRIVATE M.Name
                };
                var vfpKeyWordAbbrev = new XSharpKeywords
                {
                    {"ENDDEFINE", ENDDEFINE },
                    {"LPARAMETERS",   LPARAMETERS },
                    {"EXCLUDE", EXCLUDE },
                    {"OLEPUBLIC", OLEPUBLIC },
                    {"NOINIT", NOINIT },
                };
                foreach (var kw in vfpKeywords)
                {
                    if (!ids.ContainsKey(kw.Key))
                        ids.Add(kw.Key, kw.Value);
                    else
                        ids[kw.Key] = kw.Value;         // replace abbreviated 'thiscall' with 'this'
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
            XSharpKeywords keywords;
            if (IsMacroLexer)
            {
                keywords = new XSharpKeywords
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
                keywords = new XSharpKeywords
                {
                    // VO keywords that cannot be abbreviated

                    { "_INIT1", INIT1},
                    { "_INIT2", INIT2},
                    { "_INIT3", INIT3},
                    { "INIT1", INIT1},
                    { "INIT2", INIT2},
                    { "INIT3", INIT3},
                    { "_SIZEOF", SIZEOF},
                    { "_TYPEOF", TYPEOF},

                    // Vulcan keywords
                    { "ABSTRACT", ABSTRACT},
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
                    { "TUPLE", TUPLE},
                    { "TYPEOF", TYPEOF},
                    { "UNTIL", UNTIL},
                    { "VALUE", VALUE},
                    { "VIRTUAL", VIRTUAL},
                    { "VOSTRUCT", VOSTRUCT},

                    // XSharp keywords
                    { "__ARGLIST", ARGLIST},
                    { "ADD", ADD},
                    { "ASCENDING", ASCENDING},
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
                    { "INIT", INIT},
                    { "INTO", INTO},
                    { "JOIN", JOIN},
                    { "LET", LET},
                    { "NOP", NOP},
                    { "NAMEOF", NAMEOF},
                    { "ORDERBY", ORDERBY},
                    { "OVERRIDE", OVERRIDE},
                    { "REMOVE", REMOVE},
                    { "SELECT", SELECT},
                    { "STACKALLOC", STACKALLOC},
                    { "SWITCH", SWITCH},
                    { "UNCHECKED", UNCHECKED},
                    { "UNSAFE", UNSAFE},
                    { "VAR", VAR},
                    { "VOLATILE", VOLATILE},
                    { "WHEN", WHEN},
                    { "WHERE", WHERE},
                    { "YIELD", YIELD},
                    // From FoxPro: WITH .. [END WITH]
                    {"WITH",      WITH },

                    // Vulcan types
                    { "INT64", INT64},
                    { "UINT64", UINT64},

                    // XSharp types
                    { "DYNAMIC", DYNAMIC},
                    { "NINT", NINT },
                    { "NUINT", NUINT },

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
                    { "__DIALECT_FOXPRO__", MACRO},
                    { "__MEMVAR__", MACRO},
                    { "__UNDECLARED__", MACRO},
                    { "__UNSAFE__", MACRO},
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
                    { "__VO17__", MACRO},
                    { "__VULCAN__", MACRO},
                    { "__WINDIR__", MACRO},
                    { "__WINDRIVE__", MACRO},
                    { "__XPP__", MACRO},
                    { "__XSHARP__", MACRO},
                    { "__XSHARP_RT__", MACRO},
                    { "__XPP1__", MACRO},
                    { "__XPP2__", MACRO},
                    { "__FOX1__", MACRO},
                    { "__FOX2__", MACRO},
                };

            }
            // These keywords are inserted without abbreviations
            foreach (var text in keywords.Keys)
            {
                var token = keywords[text];
                // Better safe than sorry
                if (!ids.ContainsKey(text))
                    ids.Add(text, token);
            }
            return ids;
        }

        public XSharpKeywords KwIds
        {
            get
            {
                XSharpKeywords ids = null;
                lock (kwlock)
                {
                    if (_kwids == null)
                        _kwids = new Dictionary<XSharpDialect, XSharpKeywords>();
                    if (_kwids.ContainsKey(Dialect))
                        ids = _kwids[Dialect] ;
                }
                if (ids == null)
                {
                    ids = _getIds(Dialect);
                    lock (kwlock)
                    {
                        _kwids[Dialect] = ids;
                    }
                }
                return ids;
            }
        }

        private XSharpKeywords _getppIds()
        {
            // Macro lexer has no preprocessor keywords
            var symIds = new XSharpKeywords()
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
                {"#YCOMMAND", PP_COMMAND},		// #ycommand   <matchPattern> => <resultPattern>  // alias for #xcommand   , case sensitive
                {"#YTRANSLATE", PP_TRANSLATE},	// #ytranslate <matchPattern> => <resultPattern>  // alias for #ytranslate , case sensitive
                {"#IF", PP_IF},	                // #if <expression>
                {"#STDOUT", PP_STDOUT },        // #stdout [Message]
                {"#TEXT", PP_TEXT },            // #text const [, optionalfunc] or #text linefunc, endfunc
                {"#PRAGMA", PP_PRAGMA },        // #pragma options.... or #pragma warnings
                {"#ENDTEXT",  PP_ENDTEXT },      // endtext
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
                    var symFour = new XSharpKeywords();
                    foreach (var entry in symIds)
                    {
                        var name = entry.Key;       // '#' name
                        while (name.Length > 5)
                        {
                            name = name.Substring(0, name.Length - 1);
                            if (!symIds.ContainsKey(name) && !symFour.ContainsKey(name))
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
            return symIds ;
        }

        public XSharpKeywords SymPPIds
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
        public static XSharpLexer Create(string text, string fileName, CSharpParseOptions options = null)
        {
            var stream = new AntlrInputStream(text)
            {
                name = fileName
            };
            var lexer = new XSharpLexer(stream)
            {
                TokenFactory = XSharpTokenFactory.Instance
            };

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
