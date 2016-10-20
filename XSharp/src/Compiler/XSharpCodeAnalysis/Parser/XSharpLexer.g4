/*
   Copyright 2016 XSharp B.V.

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
lexer grammar XSharpLexer;

/*
 * Lexer Rules
*/

@lexer::members
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
        public static bool IsComment(int iToken)
        {
            return iToken == XSharpLexer.SL_COMMENT || iToken == XSharpLexer.ML_COMMENT || iToken == XSharpLexer.DOC_COMMENT;
        }

	public const int COMMENT = 1;

	bool _inId = false;
	bool _inPp = false;
	bool _hasEos = true;
	private bool isKw(IToken t) {
		char fc = Char.ToUpper(t.Text?[0] ?? (Char)0);
		return fc == '_' || (fc >= 'A' && fc <= 'Z');
	}
	int _lastToken = NL;
    System.Text.StringBuilder _textSb = new System.Text.StringBuilder();
	public override IToken NextToken()
	{
        CommonToken t;
        {
            var _startCharIndex = InputStream.Index;
            var _startColumn = Interpreter.Column;
            var _startLine = Interpreter.Line;
			int _type = -1;
			int _channel = TokenConstants.DefaultChannel;
            int c = InputStream.La(1);
			switch (c) {
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
					if (c == ':') {
						_type = COLONCOLON;
						_textSb.Append((char)c);
						InputStream.Consume();
					}
					else if (c == '=') {
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
					if (c == '|') {
						_type = OR;
						_textSb.Append((char)c);
						InputStream.Consume();
					}
					else if (c == '=') {
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
					if (c == '&') {
						_type = AND;
						_textSb.Append((char)c);
						InputStream.Consume();
					}
					else if (c == '=') {
						_type = ASSIGN_BITAND;
						_textSb.Append((char)c);
						InputStream.Consume();
					}
					break;
				case '@':
					if (InputStream.La(2) == '@') {
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
					if (c == '>') {
						_type = ALIAS;
						_textSb.Append((char)c);
						InputStream.Consume();
					}
					else if (c == '-') {
						_type = DEC;
						_textSb.Append((char)c);
						InputStream.Consume();
					}
					else if (c == '=') {
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
					if (c == '+') {
						_type = INC;
						_textSb.Append((char)c);
						InputStream.Consume();
					}
					else if (c == '=') {
						_type = ASSIGN_ADD;
						_textSb.Append((char)c);
						InputStream.Consume();
					}
					break;

				case '/':
					if (InputStream.La(2) == '/' || InputStream.La(2) == '*') {
						break;
					}
					_type = DIV;
					_textSb.Clear();
					_textSb.Append((char)c);
					InputStream.Consume();
					c = InputStream.La(1);
					if (c == '=') {
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
					if (c == '=') {
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
					if (c == '=') {
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
					if (c == '<') {
						_type = LSHIFT;
						_textSb.Append((char)c);
						InputStream.Consume();
						c = InputStream.La(1);
						if (c == '=') {
							_type = ASSIGN_LSHIFT;
							_textSb.Append((char)c);
							InputStream.Consume();
						}
					}
					else if (c == '=') {
						_type = LTE;
						_textSb.Append((char)c);
						InputStream.Consume();
					}
					else if (c == '>') {
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
						_textSb.Append((char)c);	// >
						InputStream.Consume();
						c = InputStream.La(1);
						_textSb.Append((char)c);	// =
						InputStream.Consume();
						_type = ASSIGN_RSHIFT;
						InputStream.Consume();
					}
					else if (c == '=') {
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
					if (c == '=') {
						_type = ASSIGN_XOR;
						_textSb.Append((char)c);
						InputStream.Consume();
					}
					break;
				case '*':
					if (/*_OldComment && */LastToken == NL)
						break;
					_type = MULT;
					_textSb.Clear();
					_textSb.Append((char)c);
					InputStream.Consume();
					c = InputStream.La(1);
					if (c == '=') {
						_type = ASSIGN_MUL;
						_textSb.Append((char)c);
						InputStream.Consume();
					}
					else if (c == '*') {
						_type = EXP;
						_textSb.Append((char)c);
						InputStream.Consume();
						c = InputStream.La(1);
						if (c == '=') {
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
					if (c == '?') {
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
					if (c == '=') {
						_type = EEQ;
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
					if (c == '=') {
						_type = NEQ;
						_textSb.Append((char)c);
						InputStream.Consume();
					}
					break;
				case '.':
					if (InputStream.La(2) >= '0' && InputStream.La(2) <= '9') {
						break;
					}
					_type = DOT;
					_textSb.Clear();
					_textSb.Append((char)c);
					InputStream.Consume();
					if (!_inId) {
						if (InputStream.La(2) == '.') {
							c = InputStream.La(1);
							if (c == 'F' || c == 'N' || c == 'f' || c == 'n') {
								_type = FALSE_CONST;
								_textSb.Append((char)c);
								InputStream.Consume();
								_textSb.Append('.');
								InputStream.Consume();
							}
							else if (c == 'T' || c == 'Y' || c == 't' || c == 'y') {
								_type = TRUE_CONST;
								_textSb.Append((char)c);
								InputStream.Consume();
								_textSb.Append('.');
								InputStream.Consume();
							}
							else if (c == '.' ) {
								_type = ELLIPSIS;
								_textSb.Append('.');
								InputStream.Consume();
								_textSb.Append('.');
								InputStream.Consume();
							}
						}
						else if (InputStream.La(3) == '.') {
							c = InputStream.La(1);
							var c2 = InputStream.La(2);
							if ((c == 'O' || c == 'o') && (c2 == 'R' || c2 == 'r')) {
								_type = LOGIC_OR;
								_textSb.Append((char)c);
								InputStream.Consume();
								_textSb.Append((char)c2);
								InputStream.Consume();
								_textSb.Append('.');
								InputStream.Consume();
							}
						}
						else if (InputStream.La(4) == '.') {
							c = InputStream.La(1);
							var c2 = InputStream.La(2);
							var c3 = InputStream.La(3);
							if ((c == 'A' || c == 'a') && (c2 == 'N' || c2 == 'n') && (c3 == 'D' || c3 == 'd')) {
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
							else if ((c == 'N' || c == 'n') && (c2 == 'O' || c2 == 'o') && (c3 == 'T' || c3 == 't')) {
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
							else if ((c == 'X' || c == 'x') && (c2 == 'O' || c2 == 'o') && (c3 == 'R' || c3 == 'r')) {
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
                    if (c == '\r' && InputStream.La(1) == '\n') {
						c = InputStream.La(1);
						_textSb.Append((char)c);
						InputStream.Consume();
                    }
                    Interpreter.Line += 1;
					c = InputStream.La(1);
					while (c == '\r' || c == '\n') {
						_textSb.Append((char)c);
						InputStream.Consume();
                        if (c == '\r' && InputStream.La(1) == '\n') {
							c = InputStream.La(1);
							_textSb.Append((char)c);
							InputStream.Consume();
                        }
						Interpreter.Line += 1;
						c = InputStream.La(1);
					}
                    Interpreter.Column = 1 - (InputStream.Index - _startCharIndex);
					break;
				case '\t':
				case ' ':
					_type = WS;
					_channel = TokenConstants.HiddenChannel;
					_textSb.Clear();
					_textSb.Append((char)c);
					InputStream.Consume();
					c = InputStream.La(1);
					while (c == ' ' || c == '\t') {
						_textSb.Append((char)c);
						InputStream.Consume();
						c = InputStream.La(1);
					}
					break;
				case 'e':
                case 'E':
					if (InputStream.La(2) == '"') // escaped string
					{
						break;
					}
					if ((InputStream.La(2) == 'i' ||  InputStream.La(2) == 'I') && InputStream.La(3) == '"') // interpolated escaped string
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
					if ((InputStream.La(2) == 'e' ||  InputStream.La(2) == 'E') && InputStream.La(3) == '"') // interpolated escaped string
					{
						break;
					}
                    goto case 'a';
                case 'a': case 'b': case 'c': case 'd': case 'f': case 'g': case 'h': case 'j':
                case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
                case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
                case 'A': case 'B': case 'C': case 'D': case 'F': case 'G': case 'H': case 'J':
                case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
                case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
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
							) {
						_textSb.Append((char)c);
						InputStream.Consume();
						c = InputStream.La(1);
					}
                    break;
			}
            if (_type >= 0) {
                Interpreter.Column += (InputStream.Index - _startCharIndex);
                t = TokenFactory.Create(TokenFactorySourcePair, _type, _textSb.ToString(), _channel, _startCharIndex, CharIndex - 1, _startLine, _startColumn) as CommonToken;
                Emit(t);
            }
            else
			    t = base.NextToken() as CommonToken;
        }
		//System.Diagnostics.Debug.WriteLine("T[{0},{1}]:{2}",t.Line,t.Column,t.Text);
		var type = t.Type;
		if (type == ID) {
			int kwtype;
			if (kwIds.TryGetValue(t.Text,out kwtype)) {
				t.Type = kwtype;
			}
		}
		/*else if (type == KWID) {
			t.Type = ID;
		}*/
		else if (type == SYMBOL_CONST && LastToken == NL) {
			int symtype;
			if (symIds.TryGetValue(t.Text,out symtype)) {
				t.Type = symtype;
				if (symtype != PRAGMA && symtype != HASHUSING) {
					_inPp = true;
				}
			}
		}
		if (!_inId) {
			if (isKw(t) && InputStream.La(1) == (int)'.') {
				t.Type = ID;
				_inId = true;
			}
			else if (type == ID || type == KWID)
				_inId = true;
		}
		else {
			if (isKw(t))
				t.Type = ID;
			else if (type != DOT && type != ID && type != KWID)
				_inId = false;
		}
		if (type == NL || type == SEMI) {
			if (_hasEos) {
				t.Channel = TokenConstants.HiddenChannel;
			}
			else {
				t.Type = EOS;
				_hasEos = true;
			}
		}
		else if (_hasEos && t.Channel == TokenConstants.DefaultChannel) {
			_hasEos = false;
		}
		else if (!_hasEos && type == Eof) {
			t.Type = EOS;
			_hasEos = true;
		}
		if (t.Channel == TokenConstants.DefaultChannel)
			_lastToken = type; // nvk: Note that this is the type before any modifications!!!
		if (_inPp && t.Channel == TokenConstants.DefaultChannel) {
			t.Channel = PREPROCESSOR;
			if (type == NL || type == Eof)
				_inPp = false;
		}
		return t;
	}
	int LastToken
	{
		get {return _lastToken;}
	}
	bool _Four = false;
	public bool AllowFourLetterAbbreviations
	{
		get {return _Four;}
		set {_Four = value;}
	}
	bool _OldComment = false;
	public bool AllowOldStyleComments
	{
		get {return _OldComment;}
		set {_OldComment = value;}
	}

	System.Collections.Generic.Dictionary<string,int> _kwIds;

	System.Collections.Generic.Dictionary<string,int> kwIds {
		get {
			if (_kwIds == null) {
				_kwIds = new System.Collections.Generic.Dictionary<string,int>(Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer);

				var VoKeywords = new System.Collections.Generic.Dictionary<string,int>
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
					{"_INIT1", INIT1},
					{"_INIT2", INIT2},
					{"_INIT3", INIT3},
					{"INSTANCE", INSTANCE},
					{"IS", IS},
					{"LOCAL", LOCAL},
					{"LOOP", LOOP},
					{"MEMBER", MEMBER},
					{"MEMVAR", MEMVAR},
					{"METHOD", METHOD},
					{"NAMEOF", NAMEOF},
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
					{"_SIZEOF", SIZEOF},
					{"STATIC", STATIC},
					{"STEP", STEP},
					{"STRICT", STRICT},
					{"SUPER", SUPER},
					{"THISCALL", THISCALL},
					{"TO", TO},
					{"_TYPEOF", TYPEOF},
					{"UNION", UNION},
					{"UPTO", UPTO},
					{"USING", USING},
					{"WINCALL", WINCALL},
					{"WHILE", WHILE},
					{"_XOR", VO_XOR},

					// Predefined types
					{"ARRAY", ARRAY},
					{"BYTE", BYTE},
					{"_CODEBLOCK", CODEBLOCK},
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

				if (! _Four)
				{
                    // These are predefined abbreviations of some keywords that are also valid in Vulcan
					VoKeywords.Add("PROC", PROCEDURE);
					VoKeywords.Add("FUNC", FUNCTION);
					VoKeywords.Add("PROTECT", PROTECTED);
					VoKeywords.Add("SHORT", SHORTINT);
					VoKeywords.Add("LONG", LONGINT);
				}
				foreach (var text in VoKeywords.Keys) {
					var token = VoKeywords[text];
					_kwIds.Add(text,token);
					if (_Four) {
						var s = text;
						while (s.Length > 4) {
							s = s.Substring(0,s.Length-1);
							if (!_kwIds.ContainsKey(s))
								_kwIds.Add(s,token);
						}
					}
				}

				var Keywords = new System.Collections.Generic.Dictionary<string,int>
				{
					// Vulcan keywords
					{"ABSTRACT", ABSTRACT},
					{"ANSI", ANSI},
					{"AUTO", AUTO},
					{"CATCH", CATCH},
					{"CHAR", CHAR},
					{"CONSTRUCTOR", CONSTRUCTOR},
					{"CONST", CONST},
					{"DEFAULT", DEFAULT},
					{"DELEGATE", DELEGATE},
					{"DESTRUCTOR", DESTRUCTOR},
					{"ENUM", ENUM},
					{"EVENT", EVENT},
					{"EXPLICIT", EXPLICIT},
					{"FINALLY", FINALLY},
					{"FOREACH", FOREACH},
					{"GET", GET},
					{"IMPLEMENTS", IMPLEMENTS},
					{"IMPLICIT", IMPLICIT},
					{"IMPLIED", IMPLIED},
					{"INITONLY", INITONLY},
					{"INTERFACE", INTERFACE},
					{"INTERNAL", INTERNAL},
					{"LOCK", LOCK},
					{"NAMESPACE", NAMESPACE},
					{"NEW", NEW},
					{"OFF", OFF},
					{"ON", ON},
					{"OPERATOR", OPERATOR},
					{"OPTIONS", OPTIONS},
					{"OUT", OUT},
					{"PARAMS", PARAMS},
					{"PARTIAL", PARTIAL},
					{"POP", POP},
					{"PUSH", PUSH},
					{"PROPERTY", PROPERTY},
					{"REPEAT", REPEAT},
					{"SCOPE", SCOPE},
					{"SEALED", SEALED},
					{"SET", SET},
					{"SIZEOF", SIZEOF},
					{"STRUCTURE", STRUCTURE},
					{"STRUCT", STRUCTURE},
					{"THROW", THROW},
					{"TRY", TRY},
					{"TYPEOF", TYPEOF},
					{"UNICODE", UNICODE},
					{"UNTIL", UNTIL},
					{"VALUE", VALUE},
					{"VIRTUAL", VIRTUAL},
					{"VOSTRUCT", VOSTRUCT},
					{"WARNINGS", WARNINGS},

					// XSharp keywords
					{"__ARGLIST", ARGLIST},
                    {"ADD", ADD},
					{"ASCENDING", ASCENDING},
					{"ASSEMBLY", ASSEMBLY},
					{"ASYNC", ASYNC},
					{"AWAIT", AWAIT},
					{"BY", BY},
					{"CHECKED", CHECKED},
					{"DESCENDING", DESCENDING},
					{"EQUALS", EQUALS},
					{"EXTERN", EXTERN},
					{"FIXED", FIXED},
					{"FROM", FROM},
					{"GROUP", GROUP},
					{"INTO", INTO},
					{"JOIN", JOIN},
					{"LET", LET},
					{"NOP", NOP},
					{"MODULE", MODULE},
					{"ORDERBY", ORDERBY},
					{"OVERRIDE", OVERRIDE},
                    {"REMOVE", REMOVE},
					{"SELECT", SELECT},
					{"SWITCH", SWITCH},
					{"UNCHECKED", UNCHECKED},
					{"UNSAFE", UNSAFE},
					{"VAR", VAR},
					{"VOLATILE", VOLATILE},
					{"WHERE", WHERE},
					{"YIELD", YIELD},

					// Vulcan types
					{"INT64", INT64},
					{"UINT64", UINT64},

					// XSharp types
					{"DYNAMIC", DYNAMIC},

					// Macros
					{"__ARRAYBASE__", MACRO},
					{"__CLR2__", MACRO},
					{"__CLR4__", MACRO},
					{"__CLRVERSION__", MACRO},
					{"__DATE__", MACRO},
					{"__DATETIME__", MACRO},
					{"__DEBUG__", MACRO},
					{"__DIALECT__", MACRO},
					{"__DIALECT_CORE__", MACRO},
					{"__DIALECT_VO__", MACRO},
					{"__DIALECT_VULCAN__", MACRO},
					{"__ENTITY__", MACRO},
					{"__FILE__", MACRO},
					{"__LINE__", MACRO},
					{"__MODULE__", MACRO},
					{"__SIG__", MACRO},
					{"__SRCLOC__", MACRO},
					{"__SYSDIR__", MACRO},
					{"__TIME__", MACRO},
					{"__UTCTIME__", MACRO},
					{"__VERSION__", MACRO},
					{"__VO1__", MACRO},
					{"__VO2__", MACRO},
					{"__VO3__", MACRO},
					{"__VO4__", MACRO},
					{"__VO5__", MACRO},
					{"__VO6__", MACRO},
					{"__VO7__", MACRO},
					{"__VO8__", MACRO},
					{"__VO9__", MACRO},
					{"__VO10__", MACRO},
					{"__VO11__", MACRO},
					{"__VO12__", MACRO},
					{"__VO13__", MACRO},
					{"__VO14__", MACRO},
					{"__VO15__", MACRO},
                    {"__VO16__", MACRO},
					{"__WINDIR__", MACRO},
					{"__WINDRIVE__", MACRO},
					{"__XSHARP__", MACRO},
				};
                // These keywords are inserted without abbreviations
				foreach (var text in Keywords.Keys) {
					var token = Keywords[text];
					_kwIds.Add(text,token);
				}
			}
			return _kwIds;
		}
	}

	System.Collections.Generic.Dictionary<string,int> _symIds;

	System.Collections.Generic.Dictionary<string,int> symIds {
		get {
			if (_symIds == null) {
				_symIds = new System.Collections.Generic.Dictionary<string,int>(Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer);

				var PpSymbols = new System.Collections.Generic.Dictionary<string,int>
				{
					{"#PRAGMA", PRAGMA},
					{"#USING", HASHUSING},
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

				foreach (var text in PpSymbols.Keys) {
					var token = PpSymbols[text];
					_symIds.Add(text,token);
				}
			}
			return _symIds;
		}
	}
}

options	{
			language=CSharp;
		}



channels {
XMLDOC,
DEFOUT,
PREPROCESSOR
}

tokens {

// Keywords
// Old (VO) Keywords can have 4 letter abbreviations. This can be enabled/disabled with the
// AllowFourLetterAbbreviations property of the Lexer, which sets the protected field _Four.
// New (Vulcan) Keywords only full names
//
FIRST_KEYWORD,
ACCESS,ALIGN,AS,ASPEN,ASSIGN,BEGIN,BREAK,CALLBACK,CASE,CAST,CLASS,CLIPPER,DECLARE,DEFINE,DIM,DLL,DLLEXPORT,DO,DOWNTO,ELSE,ELSEIF,END,ENDCASE,ENDDO,ENDIF,EXIT,EXPORT,FASTCALL,FIELD,FIELD_,
FIELD,FOR,FUNCTION,FUNCTION,GLOBAL,HIDDEN,IF,IIF,INHERIT,INIT1,INIT2,INIT3,INSTANCE,IS,IN,LOCAL,LOOP,MEMBER,MEMVAR,METHOD,NAMEOF,NEXT,OTHERWISE,PARAMETERS,PASCAL,
PRIVATE,PROCEDURE,PROTECTED,PUBLIC,RECOVER,RETURN,SELF,SEQUENCE,SIZEOF,SIZEOF,STATIC,STEP,STRICT,SUPER,THISCALL,TO,TYPEOF,UNION,
UPTO,USING,WHILE,WINCALL,

// Vulcan keywords that are not part of the identifier rule
// to prevent parser disambiguities
// (These keywords were NOT contextual in Vulcan either)
//
CATCH,FINALLY,THROW,

// New Vulcan Keywords (no 4 letter abbreviations)
// Should also all be part of the identifier rule
//
ABSTRACT,ANSI,AUTO,CONSTRUCTOR,CONST,DEFAULT,DELEGATE,DESTRUCTOR,ENUM,EVENT,EXPLICIT,FOREACH,GET,IMPLEMENTS,IMPLICIT,IMPLIED,INITONLY,INTERFACE,INTERNAL,
LOCK,NAMESPACE,NEW,OPERATOR,OUT,PARTIAL,PROPERTY,REPEAT,SCOPE,SEALED,SET,STRUCTURE,TRY,UNICODE,UNTIL,VALUE,VIRTUAL,VOSTRUCT,
// Pragma keywords
OFF,ON,OPTIONS,WARNINGS,PUSH, POP,

// New XSharp Keywords (no 4 letter abbreviations)
// Should also all be part of the identifier rule
//
ADD,ARGLIST,ASCENDING,ASSEMBLY,ASYNC,AWAIT,BY,CHECKED,DESCENDING,EQUALS,EXTERN,FIXED,FROM,GROUP,INTO,JOIN,LET,NOP,MODULE,ORDERBY,OVERRIDE,PARAMS,
REMOVE,SELECT,SWITCH, UNCHECKED,UNSAFE,VAR,VOLATILE,WHERE,YIELD,


// Predefined types
ARRAY,BYTE,CODEBLOCK,DATE,DWORD,FLOAT,INT,LOGIC,LONGINT,OBJECT,PSZ,PTR,REAL4,REAL8,REF,SHORTINT,STRING,SYMBOL,USUAL,VOID,WORD,

// Vulcan Types
CHAR,INT64,UINT64,

// XSharp Types
DYNAMIC,

LAST_KEYWORD,

// UDC Keyword, can be any word. The PP sets this type to tokens that are matched with keywords inside an UDC
// So they can get a special color in the editor
PP_UDC,


// Null values
FIRST_NULL,

NIL,NULL,NULL_ARRAY,NULL_CODEBLOCK,NULL_DATE,NULL_OBJECT,NULL_PSZ,NULL_PTR,NULL_STRING,NULL_SYMBOL,

LAST_NULL,

// Relational operators
FIRST_OPERATOR,
LT,LTE,GT,GTE,EQ,EEQ,SUBSTR,NEQ,NEQ2,

// Prefix and postfix Operators
INC,DEC,

// Unary & binary operators
PLUS,MINUS,DIV,MOD,EXP,LSHIFT,RSHIFT,TILDE,MULT,QQMARK,QMARK,

// Boolean operators
AND,OR,NOT,

// VO Bitwise operators
VO_NOT, VO_AND, VO_OR, VO_XOR,

// Assignments
ASSIGN_OP,ASSIGN_ADD,ASSIGN_SUB,ASSIGN_EXP,ASSIGN_MUL,ASSIGN_DIV,
ASSIGN_MOD,ASSIGN_BITAND,ASSIGN_BITOR,ASSIGN_LSHIFT,ASSIGN_RSHIFT,
ASSIGN_XOR,

// Logics
FALSE_CONST,TRUE_CONST,

// Operators
LOGIC_AND,LOGIC_OR,LOGIC_NOT,LOGIC_XOR,

// Symbols
LPAREN,RPAREN,LCURLY,RCURLY,LBRKT,RBRKT,COLON,COMMA,PIPE,AMP,ADDROF,ALIAS,DOT,COLONCOLON,BACKSLASH,ELLIPSIS,

LAST_OPERATOR,

FIRST_CONSTANT,
// Consts
HEX_CONST,BIN_CONST,INT_CONST,DATE_CONST,REAL_CONST,SYMBOL_CONST,CHAR_CONST,STRING_CONST,ESCAPED_STRING_CONST,INTERPOLATED_STRING_CONST,

LAST_CONSTANT,

// Pre processor symbols
PP_FIRST,
PRAGMA,HASHUSING,
PP_COMMAND,PP_DEFINE,PP_ELSE,PP_ENDIF,PP_ENDREGION,PP_ERROR,PP_IFDEF,PP_IFNDEF,PP_INCLUDE,PP_LINE,PP_REGION,PP_TRANSLATE,PP_UNDEF,PP_WARNING,
PP_LAST,

// PP constant
MACRO,

// Ids
ID,KWID,

// Comments
DOC_COMMENT,SL_COMMENT,ML_COMMENT,

// Separators
LINE_CONT,LINE_CONT_OLD,
SEMI,WS,NL,EOS,

// Error
UNRECOGNIZED

}

/*
 * Lexer Rules
 */

// Numeric & date constants
HEX_CONST	: '0' X ( HEX_DIGIT )+ ( U | L )?;
BIN_CONST	: '0' B ( [0-1] )+ ( U )?;
INT_CONST	:  ( DIGIT )+ ( U | L )? ;
DATE_CONST	: ( DIGIT ( DIGIT ( DIGIT ( DIGIT )? )? )? )? '.' DIGIT ( DIGIT )? '.' DIGIT ( DIGIT )?;			// 2015.07.15
REAL_CONST	: ( ( DIGIT )+ ( '.' ( DIGIT )* )? | '.' ( DIGIT )+ ) ( E ( '+' | '-' )? ( DIGIT )+ )? ( S | D )? // normal, exponential with optional Single or Double specifier
            | ( ( DIGIT )+ ( '.' ( DIGIT )* )? | '.' ( DIGIT )+ ) M // decimals cannot have exponential notation
            ;

// Preprocessor symbols are in the handwritten part above as well as #pragma and #using

SYMBOL_CONST     : '#' IDStartChar (IDChar)*;

NEQ2			 : '#'			// Alternatine NEQ but also use in _DLL rule for the DLL Hint
				 ;


CHAR_CONST  : '\'' ESCAPED_CHARACTER '\'';

STRING_CONST: '"' ( ~( '"' | '\n' | '\r' ) )* '"'			// Double quoted string
			| '\'' ( ~( '\'' | '\n' | '\r' ) )* '\''		// Single quoted string
			;


INTERPOLATED_STRING_CONST: I E? '"' ( ~( '"' | '\n' | '\r' ) )* '"'		// i "..." or ie"..."
			| E I '"' ( ~( '"' | '\n' | '\r' ) )* '"'					// ei"...."
            ;

ESCAPED_STRING_CONST
			: E '"' (ESCAPED_STRING_CHARACTER )* '"'			// Escaped double quoted string
			;

// When a semi colon is followed by optional whitespace and optional two or three slash comments then skip the line including the end of line character
LINE_CONT   :   SEMI (' ' |  '\t')* ( '/' '/' '/'? ( ~(  '\n' | '\r' ) )* )?  ('\r' '\n'? | '\n')             ->channel(HIDDEN)
            ;

LINE_CONT_OLD: {_OldComment}? SEMI (' ' |  '\t')* ( '&' '&' ( ~(  '\n' | '\r' ) )* )?  ('\r' '\n'? | '\n')     ->channel(HIDDEN)
            ;

SEMI		: ';'
			;

// Old Dbase Style Comments &&  and * at begin of line can be enabled with
// the Lexer Property AllowOldStyleComments

DOC_COMMENT :  '/' '/' '/' ( ~(  '\n' | '\r' ) )*	-> channel(XMLDOC)
			;

SL_COMMENT	:( '/' '/' ( ~(  '\n' | '\r' ) )*
			| {_OldComment}? '&' '&' ( ~(  '\n' | '\r' ) )*
			| {LastToken == NL }? '*' ( ~(  '\n' | '\r' ) )*)	-> channel(HIDDEN)
			;


ML_COMMENT  : ('/' '*' .*? '*' '/'
			| '/' '*' .*? EOF		// TODO: Generate an error 'missing End of Comment'
			)	-> channel(HIDDEN)
			;

// The ID rule must be last to make sure that it does not 'eat' the keywords

ID						: ID_PART
						;

KWID					: '@' '@' ID_PART // {Text = Text.Substring(2, Text.Length-2);}
						;


UNRECOGNIZED			: . ;

// Lexer fragments

fragment
ESCAPED_CHARACTER       : ~( '\'' | '\\' | '\r' | '\n' )		// this differs from the ESCAPED_STRING_CHARACTER rule because this has a single quote and not a double quote
						| SIMPLE_ESCAPE_SEQUENCE
						| HEX_ESCAPE_SEQUENCE
						| UNICODE_ESCAPE_SEQUENCE
						;

fragment
ESCAPED_STRING_CHARACTER: ~( '\"' | '\\' | '\r' | '\n' )		// this differs from the ESCAPED_CHARACTER rule because this has a double quote and not a single quote
						| SIMPLE_ESCAPE_SEQUENCE
						| HEX_ESCAPE_SEQUENCE
						| UNICODE_ESCAPE_SEQUENCE
						;

fragment
SIMPLE_ESCAPE_SEQUENCE	: '\\\''	// Single quote
						| '\\"'		// Double quote
						| '\\\\'	// \\
						| '\\0'		// Null
						| '\\' A	// Alert
						| '\\' B	// backspace
						| '\\' F	// formfeed
						| '\\' N	// newline
						| '\\' R	// linefeed
						| '\\' T	// tab
						| '\\' V	// vertical tab
						;

fragment
HEX_ESCAPE_SEQUENCE		: '\\' X  HEX_DIGIT (HEX_DIGIT (HEX_DIGIT (HEX_DIGIT)?)?)?		// \x+ 1 -4 digits
						;

fragment
UNICODE_ESCAPE_SEQUENCE : '\\' U HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT (HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT)?	// \u 4 hex or \u 8 hex
						;



fragment DIGIT			: [0-9];
fragment HEX_DIGIT		: [0-9a-fA-F];
fragment ID_PART		: IDStartChar IDChar*
						;

fragment IDChar			: IDStartChar
						| '0'..'9'
						| '\u00B7'
						| '\u0300'..'\u036F'
						| '\u203F'..'\u2040'
						;

fragment IDStartChar	: 'A'..'Z' | 'a'..'z'
						| '_'
						| '\u00C0'..'\u00D6'
						| '\u00D8'..'\u00F6'
						| '\u00F8'..'\u02FF'
						| '\u0370'..'\u037D'
						| '\u037F'..'\u1FFF'
						| '\u200C'..'\u200D'
						;
// these are no longer used for keywords but some are still used in the escape sequence rules and numeric constant rules
fragment A	: 'a' | 'A';
fragment B	: 'b' | 'B';
fragment C	: 'c' | 'C';
fragment D	: 'd' | 'D';
fragment E	: 'e' | 'E';
fragment F	: 'f' | 'F';
fragment G	: 'g' | 'G';
fragment H	: 'h' | 'H';
fragment I	: 'i' | 'I';
fragment J	: 'j' | 'J';
fragment K	: 'k' | 'K';
fragment L	: 'l' | 'L';
fragment M	: 'm' | 'M';
fragment N	: 'n' | 'N';
fragment O	: 'o' | 'O';
fragment P	: 'p' | 'P';
fragment Q	: 'q' | 'Q';
fragment R	: 'r' | 'R';
fragment S	: 's' | 'S';
fragment T	: 't' | 'T';
fragment U	: 'u' | 'U';
fragment V	: 'v' | 'V';
fragment W	: 'w' | 'W';
fragment X	: 'x' | 'X';
fragment Y	: 'y' | 'Y';
fragment Z	: 'z' | 'Z';

