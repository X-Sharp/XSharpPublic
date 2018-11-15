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
lexer grammar XSharpLexer;

/*
 * Lexer Rules
*/

options	{
		}


channels {
			XMLDOCCHANNEL,
			DEFOUTCHANNEL,
			PREPROCESSORCHANNEL,
			PRAGMACHANNEL
		}

tokens {

// Keywords
// Old (VO) Keywords can have 4 letter abbreviations. This can be enabled/disabled with the
// AllowFourLetterAbbreviations property of the Lexer, which sets the protected field _Four.
// New (Vulcan) Keywords only full names
//
FIRST_KEYWORD,
ACCESS,ALIGN,AS,ASPEN,ASSIGN,BEGIN,BREAK,CALLBACK,CASE,CAST,CLASS,CLIPPER,DECLARE,DEFINE,DIM,DLL,DLLEXPORT,DO,DOWNTO,ELSE,ELSEIF,END,ENDCASE,ENDDO,ENDIF,EXIT,EXPORT,FASTCALL,FIELD,
FOR,FUNC,FUNCTION,GLOBAL,HIDDEN,IF,IIF,INHERIT,INIT1,INIT2,INIT3,INSTANCE,IS,IN,LOCAL,LOOP,MEMBER,MEMVAR,METHOD,NAMEOF,NEXT,OTHERWISE,PARAMETERS,PASCAL,
PRIVATE,PROC,PROCEDURE,PROTECTED,PUBLIC,RECOVER,RETURN,SELF,SEQUENCE,SIZEOF,STATIC,STEP,STRICT,SUPER,THISCALL,TO,TYPEOF,UNION,
UPTO,USING,WHILE,WINCALL,

// Vulcan keywords that are not part of the identifier rule
// to prevent parser disambiguities
// (These keywords were NOT contextual in Vulcan either)
//
CATCH,FINALLY,THROW,


FIRST_POSITIONAL_KEYWORD,
// New Vulcan Keywords (no 4 letter abbreviations)
// Should also all be part of the identifier rule
//
ABSTRACT,ANSI,AUTO,CASTCLASS,CONSTRUCTOR,CONST,DEFAULT,DELEGATE,DESTRUCTOR,ENUM,EVENT,EXPLICIT,FOREACH,GET,IMPLEMENTS,IMPLICIT,IMPLIED,INITONLY,INTERFACE,INTERNAL,
LOCK,NAMESPACE,NEW,OPERATOR,OUT,PARTIAL,PROPERTY,REPEAT,SCOPE,SEALED,SET,STRUCTURE,TRY,UNICODE,UNTIL,VALUE,VIRTUAL,VOSTRUCT,


// New XSharp Keywords (no 4 letter abbreviations)
// Should also all be part of the identifier rule
//
ADD,ARGLIST,ASCENDING,ASSEMBLY,ASYNC,ASTYPE,AWAIT,BY,CHECKED,DESCENDING,EQUALS,EXTERN,FIXED,FROM,GROUP,INTO,JOIN,LET,NOP,MODULE,OF,ON,ORDERBY,OVERRIDE,PARAMS,
REMOVE,SELECT,SWITCH, UNCHECKED,UNSAFE,VAR,VOLATILE,WHERE,YIELD,
LAST_POSITIONAL_KEYWORD,

// Predefined types
FIRST_TYPE,
ARRAY,BYTE,CODEBLOCK,DATE,DWORD,FLOAT,INT,LOGIC,LONGINT,OBJECT,PSZ,PTR,REAL4,REAL8,REF,SHORTINT,STRING,SYMBOL,USUAL,VOID,WORD,

// Vulcan Types
CHAR,INT64,UINT64,

// XSharp Types
DYNAMIC, DECIMAL, DATETIME,
LAST_TYPE,

// Vulcan UDCs
//WAIT, ACCEPT, CANCEL, QUIT,

// UDC Tokens that should be shown in the keyword color
UDC_KEYWORD,

// Scripting directives (pseudo-preprocessor handling)
SCRIPT_REF, SCRIPT_LOAD,

// XPP Keywords
ASSIGNMENT, DEFERRED, ENDCLASS, EXPORTED, FREEZE, FINAL, INLINE, INTRODUCE, NOSAVE, READONLY, SHARING, SHARED, SYNC,  

LAST_KEYWORD,

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


// Operators
LOGIC_AND,LOGIC_OR,LOGIC_NOT,LOGIC_XOR,

// Symbols
LPAREN,RPAREN,LCURLY,RCURLY,LBRKT,RBRKT,COLON,COMMA,PIPE,AMP,ADDROF,ALIAS,DOT,COLONCOLON,BACKSLASH,ELLIPSIS,

LAST_OPERATOR,

FIRST_CONSTANT,
// Logics
FALSE_CONST,TRUE_CONST,
// Consts
HEX_CONST,BIN_CONST,INT_CONST,DATE_CONST,REAL_CONST,SYMBOL_CONST,CHAR_CONST,STRING_CONST,ESCAPED_STRING_CONST,INTERPOLATED_STRING_CONST,INCOMPLETE_STRING_CONST,

LAST_CONSTANT,

// Pre processor symbols
PP_FIRST,
PP_COMMAND,PP_DEFINE,PP_ELSE,PP_ENDIF,PP_ENDREGION,PP_ERROR,PP_IFDEF,PP_IFNDEF,PP_INCLUDE,PP_LINE,PP_REGION,PP_TRANSLATE,
PP_UNDEF,PP_WARNING,
PP_LAST,

// PP constant
MACRO,	  // __term__
UDCSEP, // =>

// Ids
ID,KWID,

// Pragma
PRAGMA,

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
HEX_CONST		: '0' X HEX_DIGIT+ ( U | L )?;

BIN_CONST		: '0' B BIN_DIGIT+ ( U )?;

INT_CONST		:  DIGIT+ ( U | L )? ;

DATE_CONST		:  DIGIT DIGIT? DIGIT? DIGIT? '.' DIGIT DIGIT? '.' DIGIT  DIGIT?;			// 2015.07.15

REAL_CONST		: ( DIGIT+ ( '.' DIGIT* )? | '.' DIGIT+ ) ( E ( '+' | '-' )? DIGIT+ )? ( S | D )? // normal, exponential with optional Single or Double specifier
				| ( DIGIT+ ( '.' DIGIT* )? | '.' DIGIT+ ) M // decimals cannot have exponential notation
				;

USING			: NUMSIGN U S I N G ;

PRAGMA			: {LastToken == NL }? NUMSIGN P R A G M A WHITESPACE  NOT_NEW_LINE	-> channel(PRAGMACHANNEL)
				;

SYMBOL_CONST    : NUMSIGN IDStartChar (IDChar)* ;

NEQ2			: NUMSIGN ;			// Alternatine NEQ but also use in _DLL rule for the DLL Hint


// Char_Const is parsed as STRING_CONST in all but the core & vulcan dialect
CHAR_CONST		: {!AllowSingleQuotedStrings}? '\''   ESCAPED_CHARACTER '\''
                | C '"'  ESCAPED_STRING_CHARACTER '"'    
                | C '\'' ESCAPED_CHARACTER '\''
                | C '\'' NOT_SINGLE '\''		// an error will be produced later
                | C '"'  NOT_DOUBLE '"'			// an error will be produced later
                ;
                

STRING_CONST	: '"'  NOT_DOUBLE '"'			// Double quoted string
				| '\'' NOT_SINGLE '\''			// Single quoted string
				;


INTERPOLATED_STRING_CONST: 
				  I   '"' NOT_DOUBLE			    '"'			// i "..." 
				| I E '"' ESCAPED_STRING_CHARACTER* '"'			// ie"...."
				| E I '"' ESCAPED_STRING_CHARACTER* '"'			// ei"...."
				;

ESCAPED_STRING_CONST
				: E '"' ESCAPED_STRING_CHARACTER* '"'			// Escaped double quoted string
				;

// The next rule is used to match incomplete strings and allows to show a proper error message 
INCOMPLETE_STRING_CONST	: '"'		NOT_DOUBLE					// Double quoted string with  missing end of string
						| '\''		NOT_SINGLE					// Single quoted string with  missing end of string
						| I	'"'		NOT_DOUBLE					// interpolated string with  missing end of string
						| I E  '"'	ESCAPED_STRING_CHARACTER*	// interpolated escaped string with  missing end of string
						| E I? '"'	ESCAPED_STRING_CHARACTER*	// escaped or interpolated string with  missing end of string
						;


// When a semi colon is followed by optional whitespace and optional two or three slash comments then skip the line including the end of line character
LINE_CONT		:   SEMI WHITESPACE  ('/' '/' '/'?  NOT_NEW_LINE)?			NEW_LINE     ->channel(HIDDEN)
				;

LINE_CONT_OLD	: {AllowOldStyleComments}? SEMI WHITESPACE  ('&' '&' NOT_NEW_LINE)?  NEW_LINE     ->channel(HIDDEN)
				;

SEMI			: SEMICOLON ;

// Old Dbase Style Comments &&  and * at begin of line can be enabled with
// the Lexer Property AllowOldStyleComments

DOC_COMMENT		:  '/' '/' '/'					NOT_NEW_LINE	-> channel(XMLDOCCHANNEL)
				;

SL_COMMENT		:( '/' '/'						NOT_NEW_LINE
				| {AllowOldStyleComments}?		'&' '&' NOT_NEW_LINE
				| {LastToken == NL }?	'*'		NOT_NEW_LINE)	-> channel(HIDDEN)
				;


ML_COMMENT		: ('/' '*' .*? '*' '/'
				| '/' '*' .*? EOF		// 'missing End of Comment' is generated in the NextToken() method above
				)	-> channel(HIDDEN)
				;

// The ID rule must be last to make sure that it does not 'eat' the keywords

ID				: ID_PART ;

KWID			: '@' '@' ID_PART  ; // {Text = Text.Substring(2, Text.Length-2);}

UNRECOGNIZED	: . ;

// Lexer fragments

fragment
ESCAPED_CHARACTER       : NOT_ESCAPE_SINGLE		
						| SIMPLE_ESCAPE_SEQUENCE
						| HEX_ESCAPE_SEQUENCE
						| UNICODE_ESCAPE_SEQUENCE
						;

fragment
ESCAPED_STRING_CHARACTER: NOT_ESCAPE_DOUBLE		
						| SIMPLE_ESCAPE_SEQUENCE
						| HEX_ESCAPE_SEQUENCE
						| UNICODE_ESCAPE_SEQUENCE
						;

fragment 
NOT_ESCAPE_SINGLE		: ~( '\'' | '\\' | '\r' | '\n' )  ;

fragment 
NOT_ESCAPE_DOUBLE		: ~( '\"' | '\\' | '\r' | '\n' ) ;

fragment NOT_DOUBLE		: ( ~( '"' | '\n' | '\r' ) )* ;
fragment NOT_SINGLE		: ( ~( '\'' | '\n' | '\r' ) )* ;
fragment NOT_NEW_LINE	: ( ~(  '\n' | '\r' ) )* ;
fragment CR_OR_LF		: ( '\n' | '\r' )  ;
fragment NEW_LINE		: ('\r' '\n'? | '\n') ;
fragment WHITESPACE		: (' ' |  '\t')* ;

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
fragment BIN_DIGIT		: [0-1];
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

fragment NUMSIGN: '#' ;
fragment SEMICOLON: ';' ;
