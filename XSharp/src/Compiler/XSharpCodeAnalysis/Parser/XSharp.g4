grammar XSharp;

/*
 * Parser Rules
*/

options	{ language=CSharp; }


source				: (eos)? (entity)* eof
					;

entity              : function
					;

function            : FUNCTION Id=identifier ParamList=parameterList AS Type=datatype eos StmtBlk=statementBlock
					;

procedure           : PROCEDURE Id=identifier ParamList=parameterList eos StmtBlk=statementBlock
					;

parameterList		: LPAREN (Params+=parameter (COMMA Params+=parameter)*)? RPAREN
					;

parameter			: Id=identifier (ASSIGN_OP Default=expression)? Modifiers+=(AS | REF | OUT) Modifiers+=CONST? Type=datatype
					;

statementBlock      : (Stmts+=statement)*
					;

/*
: localdecl
| casestmt 
| whilestmt 
| repeatstmt
| forstmt
| foreachstmt
| ifstmt 
| retstmt 
| seqstmt 
| breakstmt 
| throwstmt
| exprstmt 
| exitstmt 
| loopstmt 
| qoutstmt
| tryblock
| { LA(1) == BEGIN && LA(2) == LOCK }? lockstmt
| { LA(1) == BEGIN && LA(2) == SCOPE }? scopestmt
| fieldstmt */

statement           : DO WHILE Expr=expression eos 
					  StmtBlk=statementBlock END DO eos							#whileStmt
					| WHILE Expr=expression eos 
					  StmtBlk=statementBlock END eos							#whileStmt
					| REPEAT eos 
					  StmtBlk=statementBlock 
					  UNTIL Expr=expression eos									#repeatStmt
					| FOR Iter=expression ASSIGN_OP InitExpr=expression 
					  Dir=(TO | UPTO | DOWNTO) FinalExpr=expression 
					  (STEP Step=expression)? eos 
					  StmtBlk=statementBlock NEXT eos							#forStmt
					| FOREACH 
					  (IMPLIED Id=identifier | Id=identifier AS Type=datatype)
					  IN Container=expression eos 
					  StmtBlk=statementBlock NEXT eos							#foreachStmt
					| IF CondBlock+=conditionalBlock
					  (ELSEIF CondBlock+=conditionalBlock)*
					  (ELSE eos ElseBlock+=statementBlock)?
					  (END IF? | ENDIF) eos										#condStmt
					| DO CASE eos
					  (CASE CondBlock+=conditionalBlock)*
					  (OTHERWISE eos ElseBlock+=statementBlock)?
					  (END CASE? | ENDCASE) eos									#condStmt
					| EXIT eos													#exitStmt
					| LOOP eos													#loopStmt
					| Exprs+=expression (COMMA Exprs+=expression)* eos			#expressionStmt
					| BREAK Expr=expression? eos								#breakStmt
					| THROW Expr=expression? eos								#throwStmt
					| TRY eos StmtBlk=statementBlock
					  (CATCH CatchBlock+=catchBlock)*
					  (FINALLY eos FinBlock=statementBlock)?
					  END TRY? eos												#tryStmt
					| RETURN (VOID | Expr=expression)? eos						#returnStmt
					| BEGIN LOCK Expr=expression eos
					  StmtBlk=statementBlock
					  END LOCK? eos												#lockStmt
					| BEGIN SCOPE eos
					  StmtBlk=statementBlock
					  END SCOPE? eos											#scopeStmt
					;

conditionalBlock	: Cond=expression eos StmtBlk=statementBlock
					;

catchBlock			: Id=identifier AS Type=datatype eos StmtBlk=statementBlock
					;

// The operators in VO have the following precedence level:
//    lowest (13)  assignment           := *= /= %= ^= += -= <<= >>=
//           (12)  logical or           .OR.
//           (11)  logical and          .AND.
//           (10)  logical negation     .NOT. !
//           ( 9)  bitwise or           | 
//           ( 8)  bitwise xor          ~
//           ( 7)  bitwise and          &
//           ( 6)  relational           < <= > >= = == <> # != $
//           ( 5)  shift                << >>
//           ( 4)  additive             + -
//           ( 3)  multiplicative       * / %
//           ( 2)  exponentation        ^ **
//           ( 1)  unary                + - ++ -- ~

expression			: Left=expression Op=(DOT | COLON) Right=identifierName		#accessMember           // member access
					| Expr=expression Op=(INC | DEC)							#postfixExpression		// expr ++/--
					| Op=(PLUS | MINUS | TILDE| ADDROF | INC | DEC) 
					  Expr=expression											#prefixExpression		// +/-/~/&/++/-- expr
					| Left=expression Op=EXP Right=expression					#binaryExpression		// expr ^ expr
					| Left=expression Op=(MULT | DIV | MOD) Right=expression	#binaryExpression		// expr * expr
					| Left=expression Op=(PLUS | MINUS) Right=expression		#binaryExpression		// expr +/- expr
					| Left=expression Op=(LSHIFT| RSHIFT) Right=expression		#binaryExpression		// expr >> expr (shift)
					| Left=expression 
					  Op=( LT | LTE | GT | GTE | EQ | EEQ 
							| SUBSTR | NEQ )
					  Right=expression											#binaryExpression		// expr >= expr (relational)
					| Left=expression Op=AMP Right=expression					#binaryExpression		// expr & expr (bitwise and)
					| Left=expression Op=TILDE Right=expression					#binaryExpression		// expr ~ expr (bitwise xor)
					| Left=expression Op=PIPE Right=expression					#binaryExpression		// expr | expr (bitwise or)
					| Op=(LOGIC_NOT | LOGIC_XOR | NOT) Expr=expression			#prefixExpression		// .not. expr (logical not)
					| Left=expression Op=LOGIC_AND Right=expression				#binaryExpression		// expr .and. expr (logical and)
					| Left=expression Op=LOGIC_XOR Right=expression				#binaryExpression		// expr .xor. expr (logical xor)
					| Left=expression Op=LOGIC_OR  Right=expression				#binaryExpression		// expr .or. expr (logical or)
					| Left=expression 
					  Op=( ASSIGN_OP | ASSIGN_ADD | ASSIGN_EXP 
							| ASSIGN_MUL | ASSIGN_DIV | ASSIGN_MOD 
							| ASSIGN_BITAND | ASSIGN_BITOR | ASSIGN_LSHIFT 
							| ASSIGN_RSHIFT | ASSIGN_XOR )
					  Right=expression											#assignmentExpression	// expr := expr
					| Expr=expression LPAREN ArgList=argumentList? RPAREN		#methodCall				// method call
					| Expr=expression LBRKT ArgList=expressionList? RBRKT		#arrayAccess			// Array element access
					| Type=datatype LCURLY ArgList=argumentList? RCURLY			#ctorCall				// id{ [expr [, expr...] }
					| Literal=literalValue										#literalExpression		// literals
					| LiteralArray=literalArray									#literalArrayExpression	// { expr [, expr] }
					| CbExpr=codeblock											#codeblockExpression	// {| [id [, id...] | expr [, expr...] }
					| LPAREN Type=datatype RPAREN Expr=expression				#typeCast			    // (typename) expr
					| TYPEOF LPAREN Type=datatype RPAREN						#typeOfExpression		// typeof( typeORid )
					| SIZEOF LPAREN Type=datatype RPAREN						#sizeOfExpression		// sizeof( typeORid )
					| Name=name													#nameExpression			// generic name
					| Type=nativeType											#typeExpression			// ARRAY, CODEBLOCK, etc.
					| Expr=iif													#iifExpression			// iif( expr, expr, expr )
					| LPAREN Expr=expression RPAREN								#parenExpression		// ( expr )
//					| PTR LPAREN nativeType COMMA expression RPAREN				#oldcast				// PTR( typeName, expr )
//					| nativeType LPAREN CAST COMMA expression RPAREN			#oldcast2				// typename(_CAST, expr )
//					| nativeType LPAREN expression RPAREN						#conversion				// nativetype( expr )
//					| aliasedField												#aliasfield				//  ALIAS->FIELD
//					| aliasedExpr												#aliasexpr				// ALIAS->(expr)
//					| aliasedFuncCall											#aliasfunccall			//  foo->bar()
//					| extendedaliasExpr											#aliasextended			// (expr) -> ...
//					| AMP LPAREN expression RPAREN								#macroexpr				// &( expr )
//					| AMP identifierName										#macrovar				// &id
					;

expressionList		: Exprs+=expression (COMMA Exprs+=expression)*
					;

argumentList		: Args+=argument (COMMA Args+=argument?)*
					;

argument			: Expr=expression
					;

iif					: IIF LPAREN Cond=expression COMMA TrueExpr=expression COMMA FalseExpr=expression RPAREN
					;

name				: Left=name Op=DOT Right=identifier							#qualifiedName
					| Id=identifier	GenericArgList=genericArgumentList			#genericName
					| Id=identifier												#simpleName
					;

genericArgumentList : LT GenericArgs+=datatype (COMMA GenericArgs+=datatype)* GT
					;

identifierName		: Id=identifier
					;

datatype			: TypeName=typeName PTR											#ptrDatatype
					| TypeName=typeName (Ranks+=arrayRank)*							#arrayDatatype
					| TypeName=typeName												#simpleDatatype
					;

arrayRank			: LBRKT (COMMA)* RBRKT
					;

typeName			: Name=name
					| NativeType=nativeType
					;

literalArray		: (LT Type=datatype GT)? LCURLY (ExprList=expressionList)? RCURLY
					;

//aliasedField		:	FIELD ALIAS Id=identifierName									// _FIELD->NAME
//					|	Left=identifierName ALIAS Right=identifierName					// CUSTOMER->NAME
//					|   FIELD ALIAS Left=identifierName ALIAS Right=identifierName		// _FIELD->CUSTOMER->NAME
//					;

//aliasedExpr			:	Id=identifierName ALIAS LPAREN Expr=expression RPAREN
//					;

//aliasedFuncCall		:	i=identifierName a=ALIAS m=staticMethodCall 
//					;

//extendedaliasExpr	:	l1=LPAREN e1=expression r1=RPAREN a=ALIAS
//						( i=identifierName								// (expr) -> ID
//						| l2=LPAREN e2=expression r2=RPAREN				// (expr) -> (expr)
//						| m=staticMethodCall							// (expr) -> func(..)
//						)
//					;

codeblock			: LCURLY (OR | PIPE CbParamList=codeblockParamList? PIPE) Expr=expression? RCURLY
					;

codeblockParamList	: Ids+=identifier (COMMA Ids+=identifier)*
					;

identifier			: Token=ID
					;

nativeType			: Token=
					( ARRAY    
					| BYTE    
					| CODEBLOCK
					| DATE     
					| DWORD   
					| FLOAT    
					| SHORTINT 
					| INT      
					| INT64
					| LOGIC   
					| LONGINT
					| OBJECT   
					| PSZ     
					| PTR      
					| REAL4    
					| REAL8    
					| STRING   
					| SYMBOL   
					| USUAL
					| UINT64    
					| WORD    
					| VOID )   
					;

literalValue		: Token=
					( TRUE_CONST
					| FALSE_CONST
					| STRING_CONST
					| SYMBOL_CONST
					| HEX_CONST 
					| BIN_CONST 
					| REAL_CONST 
					| INT_CONST 
					| DATE_CONST
					| NIL
					| NULL   
					| NULL_ARRAY
					| NULL_CODEBLOCK
					| NULL_DATE
					| NULL_OBJECT
					| NULL_PSZ 
					| NULL_PTR 
					| NULL_STRING
					| NULL_SYMBOL )
					;

accessModifier		: Token=
					( PUBLIC
					| PRIVATE
					| INTERNAL
					| PROTECTED
					| EXPORT
					| HIDDEN )
					;

/*assignOperator		: Token=
					( ASSIGN_OP 
					| ASSIGN_ADD 
					| ASSIGN_EXP 
					| ASSIGN_MUL 
					| ASSIGN_DIV 
					| ASSIGN_MOD 
					| ASSIGN_BITAND 
					| ASSIGN_BITOR
					| ASSIGN_LSHIFT
					| ASSIGN_RSHIFT
					| ASSIGN_XOR )
					;*/

/*relationOperator	: Token=
					( LT
					| LTE
					| GT
					| GTE
					| EQ
					| EEQ
					| SUBSTR
					| NEQ )
					;*/

eos                 : (NL)* (NL|EOF)
					;

eof                 : EOF
					;

/*
 * Lexer Rules
 */

// Keywords
ABSTRACT			: A B S T R A C T ;
ACCESS				: A C C E S S ;
ALIGN				: A L I G N ;
AS					: A S ;
ASSIGN				: A S S I G N ;
AUTO				: A U T O ;
BEGIN				: B E G I N ;
BREAK				: B R E A K ;
CASE				: C A S E;
CAST				: '_' C A S T ;
CATCH				: C A T C H ;
CLASS				: C L A S S ;
CLIPPER				: C L I P P E R;
CONSTRUCTOR			: C O N S T R U C T O R ;
CONST				: C O N S T ;
DELEGATE			: D E L E G A T E ;           // only after EOS, INTERNAL or EXPORT 
DESTRUCTOR			: D E S T R U C T O R ;					// only after EOS
DIM					: D I M ;
DLL					: '_' D L L ;
DO					: D O ;
DOWNTO				: D O W N T O ;
ELSE				: E L S E;
ELSEIF				: E L S E I F ;
END					: E N D ;
ENDCASE				: E N D C A S E ;
ENDDO				: E N D D O ;
ENDIF				: E N D I F ;
ENUM				: E N U M ;
EVENT				: E V E N T ;              // only after EOS
EXIT				: E X I T ;
EXPLICIT			: E X P L I C I T ;
EXPORT				: E X P O R T ;
FASTCALL			: F A S T C A L L ;
FIELD				: '_'? F I E L D ;
FINALLY				: F I N A L L Y ;
FOR					: F O R ;
FOREACH				: F O R E A C H ;            // only after EOS
FUNCTION			: F U N C T I O N ;
GET					: G E T ;                // only inside PROPERTY after EOS or END
GLOBAL				: G L O B A L ;
HIDDEN				: H I D D E N ;
IF					: I F ;
IIF					: I I F;
IMPLEMENTS			: I M P L E M E N T S ;
IMPLICIT			: I M P L I C I T ;
IMPLIED				: I M P L I E D ;
INHERIT				: I N H E R I T ;
INITONLY			: I N I T O N L Y ;
INSTANCE			: I N S T A N C E ;
INTERFACE			: I N T E R F A C E ;
INTERNAL			: I N T E R N A L ;
IN					: I N ;
IS					: I S ;
LOCAL				: L O C A L ;
LOCK				: L O C K ;               // only after BEGIN or END
LOOP				: L O O P ;
MEMBER				: M E M B E R ;
METHOD				: M E T H O D ;
NAMESPACE			: N A M E S P A C E ;          // only after BEGIN or END
NEW					: N E W ;                // only **before** METHOD
NEXT				: N E X T ;
OPERATOR			: O P E R A T O R ;
OTHERWISE			: O T H E R W I S E ;
OUT					: O U T ;
PARTIAL				: P A R T I A L ;
PASCAL				: P A S C A L ;
PRIVATE				: P R I V A T E ;
PROCEDURE			: P R O C (E D U R E)? ;
PROTECTED			: P R O T E C T (E D)? ;
PROPERTY			: P R O P E R T Y ;           // only after EOS or END
PUBLIC				: P U B L I C ;
RECOVER				: R E C O V E R ;
REPEAT				: R E P E A T ;              // only after EOS
RETURN				: R E T U R N ;
SCOPE				: S C O P E ;              // only after BEGIN or END
SEALED				: S E A L E D ;
SELF				: S E L F ;
SEQUENCE			: S E Q U E N C E ;
SET					: S E T ;                // only inside PROPERTY after EOS or END
SIZEOF				: '_'? S I Z E O F ;
STATIC				: S T A T I C ;
STEP				: S T E P ;
STRICT				: S T R I C T ;
STRUCTURE			: S T R U C T (U R E)? ;
SUPER				: S U P E R ;
THISCALL			: T H I S C A L L ;
THROW				: T H R O W ;
TO					: T O ;
TRY					: T R Y ;                // only after EOS or END
TYPEOF				: '_'? T Y P E O F ;
UNION				: U N I O N ;
UNTIL				: U N T I L ;              // only after EOS
UPTO				: U P T O ;
USING				: U S I N G ;
VIRTUAL				: V I R T U A L ;
WHILE				: W H I L E ;

// Predefined types
ARRAY				: A R R A Y ;
BYTE				: B Y T E ;
CHAR				: C H A R ;
CODEBLOCK			: C O D E B L O C K ;
DATE				: D A T E ;
DWORD				: D W O R D ;
FLOAT				: F L O A T ;
INT					: I N T ;
INT64				: I N T '6' '4' ;
LOGIC				: L O G I C ;
LONGINT				: L O N G I N T ;
OBJECT				: O B J E C T ;
PSZ					: P S Z ;
PTR					: P T R ;
REAL4				: R E A L '4' ;
REAL8				: R E A L '8' ;
REF					: R E F ;
SHORTINT			: S H O R T (I N T)? ;
STRING				: S T R I N G ;
SYMBOL				: S Y M B O L ;
USUAL				: U S U A L ;
UINT64				: U I N T '6' '4' ;
VOID				: V O I D ;
WORD				: W O R D ;
 
// Null values
NIL					: N I L ;
NULL				: N U L L ;
NULL_ARRAY			: N U L L '_' A R R A Y ;
NULL_CODEBLOCK		: N U L L '_' C O D E B L O C K ;
NULL_DATE			: N U L L '_' D A T E ;
NULL_OBJECT			: N U L L '_' O B J E C T ;
NULL_PSZ			: N U L L '_' P S Z ;
NULL_PTR			: N U L L '_' P T R  ;
NULL_STRING			: N U L L '_' S T R I N G ;
NULL_SYMBOL			: N U L L '_' S Y M B O L ;
 
// Logics
FALSE_CONST			: F A L S E 
					| '.' F '.' 
					| '.' N '.' 
					;
			
TRUE_CONST			: T R U E 
					| '.' T '.' 
					| '.' Y '.' 
					;

// Operators
LOGIC_AND			: '.' A N D '.' ;
LOGIC_OR			: '.' O R '.'   ;
LOGIC_NOT			: '.' N O T '.' ;
LOGIC_XOR			: '.' X O R '.' ;

// Prefix and postfix Operators
INC					: '++' ;
DEC					: '--' ;

// Boolean operators
NOT					: '!'  ;
AND					: '&&' ;
OR					: '||' ;

// Unary & binary operators
PLUS				: '+';
MINUS				: '-' ;
DIV					: '/' ;
MOD					: '%';
EXP					: '^';
LSHIFT				: '<<' ;
RSHIFT				: '>>' ;
TILDE				: '~';
MULT				: '*' ;

// Assignments
ASSIGN_OP     : ':=' ;
ASSIGN_ADD    : '+=' ;
ASSIGN_SUB    : '-=' ;
ASSIGN_EXP    : '^=' ;
ASSIGN_MUL    : '*=' ;
ASSIGN_DIV    : '/=' ;
ASSIGN_MOD    : '%=' ;
ASSIGN_BITAND : '&=';
ASSIGN_BITOR  : '|=';
ASSIGN_LSHIFT : '<<=';
ASSIGN_RSHIFT : '>>=';
ASSIGN_XOR    : '~=';

// Relational operators
LT			: '<' ;      
LTE			: '<=' ;
GT			: '>' ;
GTE			: '>=' ;
EQ			: '=' ;
EEQ			: '==' ;
SUBSTR		 : '$'	;
NEQ			: '<>'  
			| '!='  
			;

// Misc Lexical Elements
LPAREN      : '(' ;
RPAREN		: ')' ;
LCURLY		: '{' ;
RCURLY		: '}' ;
LBRKT		: '[' ;
RBRKT		: ']' ;
COLON		: ':' ;
COMMA		: ',' ;
PIPE		: '|' ;
AMP			: '&' ;
ADDROF		: '@' ;
ALIAS		: '->';
DOT			: '.' ;

// Numeric & date constants
HEX_CONST	: '0' X ( HEX_DIGIT )+ ( U | L )?;
BIN_CONST	: '0' B ( [0-1] )+ ( U )?;
INT_CONST	:  ( DIGIT )+ ( U | L )? ;
DATE_CONST  : ( DIGIT ( DIGIT ( DIGIT ( DIGIT )? )? )? )? DOT DIGIT ( DIGIT )? DOT DIGIT ( DIGIT )?;			// 2015.07.15
REAL_CONST  : ( ( DIGIT )+ )? DOT ( DIGIT )* ( 'e' ( '+' | '-' )? ( DIGIT )+ )? ( S | D | M )?;

//DECIMAL_CONST;                         // a literal floating point number followed by 'm'

SYMBOL_CONST: '#' [a-z_A-Z] ([a-z_A-Z0-9])*;


STRING_CONST: '"' ( ~( '"' | '\n' | '\r' ) )* '"'			// Double quoted string
			| '\'' ( ~( '\'' | '\n' | '\r' ) )* '\''		// Single quoted string
			;

WS			:	(' ' |  '\t' |	'\r') -> channel(HIDDEN)
			;


SL_COMMENT	: '/' '/' ( ~(  '\n' | '\r' ) )*  -> channel(HIDDEN)
			;

NL         : '\n' ;

// The ID rule must be last to make sure that it does not 'eat' the keywords

ID			:  ID_PART ;

// Lexer fragments

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

fragment A: 'a' | 'A';
fragment B: 'b' | 'B';
fragment C: 'c' | 'C';
fragment D: 'd' | 'D';
fragment E: 'e' | 'E';
fragment F: 'f' | 'F';
fragment G: 'g' | 'G';
fragment H: 'h' | 'H';
fragment I: 'i' | 'I';
fragment J: 'j' | 'J';
fragment K: 'k' | 'K';
fragment L: 'l' | 'L';
fragment M: 'm' | 'M';
fragment N: 'n' | 'N';
fragment O: 'o' | 'O';
fragment P: 'p' | 'P';
fragment Q: 'q' | 'Q';
fragment R: 'r' | 'R';
fragment S: 's' | 'S';
fragment T: 't' | 'T';
fragment U: 'u' | 'U';
fragment V: 'v' | 'V';
fragment W: 'w' | 'W';
fragment X: 'x' | 'X';
fragment Y: 'y' | 'Y';
fragment Z: 'z' | 'Z';
