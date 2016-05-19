lexer grammar XSharpLexer;

/*
 * Lexer Rules
*/ 

@lexer::members
{

	public const int COMMENT = 1;

	bool _inId = false;
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
					if (InputStream.La(2) == '"') {
						break;
					}
					_type = ID;
					_textSb.Clear();
					_textSb.Append((char)c);
					InputStream.Consume();
					c = InputStream.La(1);
					while ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_') {
						_textSb.Append((char)c);
						InputStream.Consume();
						c = InputStream.La(1);
					}
					break;
                case 'a': case 'b': case 'c': case 'd': case 'f': case 'g': case 'h': case 'i': case 'j':
                case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
                case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
                case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
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
				if (t.Type == _SKIP) {
					t.Channel = TokenConstants.HiddenChannel;
				}
			}
		}
		/*else if (type == KWID) {
			t.Type = ID;
		}*/
		else if (type == SYMBOL_CONST && LastToken == NL) {
			int symtype;
			if (symIds.TryGetValue(t.Text,out symtype)) {
				t.Type = symtype;
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
		return t;
	}
	int LastToken
	{
		get {return _lastToken;}
	}

	System.Collections.Generic.Dictionary<string,int> _kwIds;

	System.Collections.Generic.Dictionary<string,int> kwIds { 
		get {
			if (_kwIds == null) {
				_kwIds = new System.Collections.Generic.Dictionary<string,int>(Microsoft.CodeAnalysis.CaseInsensitiveComparison.Comparer);

				var VoKeywords = new System.Collections.Generic.Dictionary<string,int>
				{
					{"_CO", _SKIP},			
					{"_NC", _SKIP},
					{"ACCESS", ACCESS},
					{"ALIGN", ALIGN},
					{"_AND", VO_AND},
					{"AS", AS},
					{"ASSIGN", ASSIGN},
					{"BEGIN", BEGIN},
					{"BREAK", BREAK},
					{"CASE", CASE},
					{"_CAST", CAST},
					{"CLASS", CLASS},
					{"CLIPPER", CLIPPER},
					{"DEFINE", DEFINE},
					{"DIM", DIM},
					{"_DLL", DLL},
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
					{"RETURN", RETURN},
					{"SELF", SELF},
					{"SEQUENCE", SEQUENCE},
					{"SIZEOF", SIZEOF},
					{"_SIZEOF", SIZEOF},
					{"STATIC", STATIC},
					{"STEP", STEP},
					{"STRICT", STRICT},
					{"SUPER", SUPER},
					{"THISCALL", THISCALL},
					{"TO", TO},
					{"TYPEOF", TYPEOF},
					{"_TYPEOF", TYPEOF},
					{"UNION", UNION},
					{"UPTO", UPTO},
					{"USING", USING},
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
					{"REF", REF},
					{"SHORTINT", SHORTINT},
					{"STRING", STRING},
					{"SYMBOL", SYMBOL},
					{"USUAL", USUAL},
					{"VOID", VOID},
					{"WORD", WORD},
				};

				foreach (var text in VoKeywords.Keys) {
					var token = VoKeywords[text];
					_kwIds.Add(text,token);
					{
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

					// Macros
					{"__AEFDIR__", MACRO},
					{"__APPLICATION__", MACRO},
					{"__APPWIZDIR__", MACRO},
					{"__CAVODIR__", MACRO},
					{"__CAVOBINDIR__", MACRO},
					{"__CAVOSAMPLESROOTDIR__", MACRO},
					{"__DATE__", MACRO},
					{"__DATETIME__", MACRO},
					{"__DEBUG__", MACRO},
					{"__ENTITY__", MACRO},
					{"__ENTITYSYM__", MACRO},
					{"__EXECUTABLEDIR__", MACRO},
					{"__FILENAME__", MACRO},
					{"__LINE__", MACRO},
					{"__MDFFILENAME__", MACRO},
					{"__MEFDIR__", MACRO},
					{"__MODULE__", MACRO},
					{"__OS__", MACRO},
					{"__PROJECT__", MACRO},
					{"__PRGDIR__", MACRO},
					{"__RESOURCEDIR__", MACRO},
					{"__SRCLOC__", MACRO},
					{"__SYSDIR__", MACRO},
					{"__TIME__", MACRO},
					{"__VERSION__", MACRO},
					{"__WINDIR__", MACRO},
					{"__WINDRIVE__", MACRO},
					{"__VO__", MACRO},
					{"#IFDEF", PP_COMMAND},			// #ifdef <identifier>   <statements>...[#else]   <statements>...#endif
					{"#IFNDEF", PP_COMMAND},			// #ifndef <identifier>   <statements>...[#else]   <statements>...#endif
					{"#ELSE", PP_COMMAND},				// #ifdef <identifier>   <statements>...[#else]   <statements>...#endif
					{"#ENDIF", PP_COMMAND},			// #ifdef <identifier>   <statements>...[#else]   <statements>...#endif
					{"#ERROR", PP_COMMAND},			// #error [errorMessage]
					{"#IFINCL", PP_COMMAND},			// #ifincl
					{"#IFNINCL", PP_COMMAND},		// #ifnincl
					{"#WARNING", PP_COMMAND},		// #warning [warningMessage]
					{"VTRACE", PP_COMMAND},		// #warning [warningMessage]

				};
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



tokens {

// Keywords
// Old (VO) Keywords can have 4 letter abbreviations. This can be enabled/disabled with the
// AllowFourLetterAbbreviations property of the Lexer, which sets the protected field _Four.
// New (Vulcan) Keywords only full names
//
_SKIP,
FIRST_KEYWORD,
ACCESS,ALIGN,AS,ASSIGN,BEGIN,BREAK,CASE,CAST,CLASS,CLIPPER,DEFINE,DIM,DLL,DO,DOWNTO,ELSE,ELSEIF,END,ENDCASE,ENDDO,ENDIF,EXIT,EXPORT,FASTCALL,FIELD,
FIELD,FOR,FUNCTION,FUNCTION,GLOBAL,HIDDEN,IF,IIF,INHERIT,INIT1,INIT2,INIT3,INSTANCE,IS,LOCAL,LOOP,MEMBER,MEMVAR,METHOD,NAMEOF,NEXT,OTHERWISE,PARAMETERS,PASCAL,
PRIVATE,PROCEDURE,PROTECTED,PUBLIC,RECOVER,RETURN,SELF,SEQUENCE,SIZEOF,SIZEOF,STATIC,STEP,STRICT,SUPER,THISCALL,TO,TYPEOF,UNION,
UPTO,USING,WHILE,

// Predefined types
ARRAY,BYTE,CHAR,CODEBLOCK,DATE,DWORD,FLOAT,INT,LOGIC,LONGINT,OBJECT,PSZ,PTR,REAL4,REAL8,REF,SHORTINT,STRING,SYMBOL,USUAL,VOID,WORD,

LAST_KEYWORD,


// Null values
FIRST_NULL,

NIL,NULL,NULL_ARRAY,NULL_CODEBLOCK,NULL_DATE,NULL_OBJECT,NULL_PSZ,NULL_PTR,NULL_STRING,NULL_SYMBOL,

LAST_NULL,

// Relational operators
FIRST_OPERATOR,
LT,LTE,GT,GTE,EQ,EEQ,SUBSTR,NEQ,

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
LPAREN,RPAREN,LCURLY,RCURLY,LBRKT,RBRKT,COLON,COMMA,PIPE,AMP,ADDROF,ALIAS,DOT,COLONCOLON,

LAST_OPERATOR,

FIRST_CONSTANT,
// Consts
HEX_CONST,BIN_CONST,INT_CONST,DATE_CONST,REAL_CONST,SYMBOL_CONST,CHAR_CONST,STRING_CONST,ESCAPED_STRING_CONST,
PRAGMA

LAST_CONSTANT,

// Various
PP_COMMAND, MACRO, UDC_COMMAND

// Ids
ID,KWID,

// Comments
SL_COMMENT,ML_COMMENT,

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
REAL_CONST	: ( ( DIGIT )+ ( '.' ( DIGIT )* )? | '.' ( DIGIT )+ ) ( 'e' ( '+' | '-' )? ( DIGIT )+ )? ( S | D | M )?;


// VO Style pragmas - are skipped

PRAGMA		: '~' '"' [a-z_A-Z]+ ('+'|'-') '"'	->channel(HIDDEN)
			;

// Preprocessor symbols handled by the Lexer

SYMBOL_CONST     : '#' [a-z_A-Z] ([a-z_A-Z0-9])*;

CHAR_CONST  : '\'' ESCAPED_CHARACTER '\'';

STRING_CONST: '"' ( ~( '"' | '\n' | '\r' ) )* '"'			// Double quoted string
			| '\'' ( ~( '\'' | '\n' | '\r' ) )* '\''		// Single quoted string
			| '\[' ( ~( '\]' | '\n' | '\r' ) )* '\]'		// Bracketed  string
			;


// When a semi colon is followed by optional whitespace and optional two or three slash comments then skip the line including the end of line character                            
LINE_CONT   :   ';' (' ' |  '\t')* ( '/' '/' '/'? ( ~(  '\n' | '\r' ) )* )?  ('\r' '\n'? | '\n')             ->channel(HIDDEN)
            ;

LINE_CONT_OLD: {_OldComment}? ';' (' ' |  '\t')* ( '&' '&' ( ~(  '\n' | '\r' ) )* )?  ('\r' '\n'? | '\n')     ->channel(HIDDEN)
            ;

SEMI		: ';' 
			;

// Old Dbase Style Comments &&  and * at begin of line can be enabled with
// the Lexer Property AllowOldStyleComments


SL_COMMENT	:( '/' '/' ( ~(  '\n' | '\r' ) )*
			| {_OldComment}? '&' '&' ( ~(  '\n' | '\r' ) )*
			| {/*_OldComment && */LastToken == NL }? '*' ( ~(  '\n' | '\r' ) )*)	-> channel(HIDDEN)
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

ORDINAL					: '#' ( DIGIT )+ ;

UNRECOGNIZED			: . ;

// Lexer fragments

fragment DIGIT			: [0-9];
fragment HEX_DIGIT		: [0-9a-fA-F];
fragment ID_PART		: IDStartChar IDChar*
						;

fragment IDChar			: IDStartChar
						| '0'..'9'
						;

fragment IDStartChar	: 'A'..'Z' | 'a'..'z'
						| '_'
						;

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
