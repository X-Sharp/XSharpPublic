grammar XSharp;

/*
 * Parser Rules
*/

// Known issues:
// - ALIAS (->) operator
// - preprocessor , #region, #using etc

@parser::members
{

} 


options	{ 
		language=CSharp; 
		tokenVocab=XSharpLexer;
		}


source				: (Entities+=entity)* EOF
					;

entity              : class_
					| structure_
                    | function                  
					| procedure                 
					| method                
					| voglobal				
					| define				
					| struct_				
					| union				
					| vodll					
					;

function            : STATIC? FUNCTION Id=identifier (ParamList=parameterList)?
					   (AS Type=datatype)? 
					   (CallingConvention=callingconvention)? EOS 
					   StmtBlk=statementBlock
					;

procedure           : STATIC? PROCEDURE Id=identifier (ParamList=parameterList)?
					   (CallingConvention=callingconvention)? Init=(INIT1|INIT2|INIT3)? EOS 
					   StmtBlk=statementBlock
					;

callingconvention	: Convention=(CLIPPER | STRICT | PASCAL) 
					;


vodll				: STATIC? DLL 
					  ( T=FUNCTION Id=identifier ParamList=parameterList (AS Type=datatype)?
					  | T=PROCEDURE Id=identifier ParamList=parameterList )
					  (CallingConvention=dllcallconv)? COLON 
					  Dll=identifierString ( DOT Entrypoint=identifierString | Ordinal=ORDINAL )
					  EOS
                    ;

dllcallconv         : Cc=( CLIPPER | STRICT | PASCAL | THISCALL | FASTCALL)
                    ;


parameterList		: LPAREN (Params+=parameter (COMMA Params+=parameter)*)? RPAREN
					;

parameter			: Id=identifier (ASSIGN_OP Default=expression)? (Modifiers=parameterDeclMods Type=datatype)?
					;

parameterDeclMods   : Tokens+=(AS | REF|  IS ) 
					;

statementBlock      : (Stmts+=statement)*
					;


voglobal			: STATIC? GLOBAL (Const=CONST)? Vars=classVarList EOS
					;

// Separate method/access/assign with Class name -> convert to partial class with just one method
// And when Class is outside of assembly, convert to Extension Method?
// nvk: we have no knowledge of whether a class is outside of the assembly at the parser stage!
method				: (Modifiers=memberModifiers)?
					  T=methodtype  Id=identifier (ParamList=parameterList)? (AS Type=datatype)? 
					  (CallingConvention=callingconvention)? CLASS ClassId=identifier EOS 
					  StmtBlk=statementBlock		
					;

methodtype			: Token=(METHOD | ACCESS | ASSIGN)
					;

// Convert to constant on Globals class. Expression must be resolvable at compile time
define				: STATIC? DEFINE Id=identifier ASSIGN_OP Expr=expression (AS DataType=nativeType)? EOS
					;

struct_				: STATIC? (VOSTRUCT|STRUCTURE) Id=identifier (ALIGN Alignment=INT_CONST)? EOS
					  (Members+=sumember)+
					;

union				: STATIC? UNION Id=identifier EOS
					  (Members+=sumember)+
					;

sumember			: MEMBER Dim=DIM Id=identifier LBRKT ArraySub=arraysub RBRKT ((AS | IS) DataType=datatype)? EOS
					| MEMBER Id=identifier ((AS | IS) DataType=datatype)? EOS
					;

class_				: CLASS Id=identifier (INHERIT BaseType=datatype)? EOS         
					  (Members+=classmember)*
					;

pragma				: PRAGMA EOS
					;

pp_line				: PP_COMMAND (~EOS)* EOS
					;

udc_line			: UDC (~EOS)* EOS
					;

classvars			: (Modifiers=classvarModifiers)? Vars=classVarList EOS
					; 

classvarModifiers	: ( Tokens+=(INSTANCE| STATIC | HIDDEN | PROTECTED | EXPORT ) )+
					;

classVarList		: Var+=classvar (COMMA Var+=classvar)* ((AS | IS) DataType=datatype)?
					;

classvar			: (Dim=DIM)? Id=identifier (LBRKT ArraySub=arraysub RBRKT)? (ASSIGN_OP Initializer=expression)?
					;


declaration			: DECLARE ( ACCESS | ASSIGN | METHOD ) Ids+=identifier ( COMMA Ids+=Identifier )* eos
					;

arraysub			: ArrayIndex+=expression (RBRKT LBRKT ArrayIndex+=expression)+		// x][y
					| ArrayIndex+=expression (COMMA ArrayIndex+=expression)+			// x,y
					| ArrayIndex+=expression
					;

classmember			: Member=classvars									#clsvars
					| Member=declaration								#clsdeclaration
					| Member=(pragma|pp_line|udc_line)					#clsother
					;

memberModifiers		: ( Tokens+=(HIDDEN | PROTECTED | EXPORT ) )+
					;


statement           : Decl=localdecl                                            #declarationStmt
					| xbasedecl													#xbasedeclStmt
					| DO? WHILE Expr=expression EOS
					  StmtBlk=statementBlock (END DO? | ENDDO) EOS				#whileStmt
					| FOR ForIter=identifier ASSIGN_OP Expr=expression
					  Dir=(TO | UPTO | DOWNTO) FinalExpr=expression
					  (STEP Step=expression)? EOS
					  StmtBlk=statementBlock NEXT EOS							#forStmt
					| IF IfStmt=ifElseBlock
					  (END IF? | ENDIF)  EOS									#ifStmt	
					| DO CASE EOS
					  CaseStmt=caseBlock?
					  (END CASE? | ENDCASE) EOS									#caseStmt
					| EXIT EOS												#exitStmt
					| LOOP EOS												#loopStmt
					| BREAK Expr=expression? EOS							#breakStmt
					| RETURN (VOID | Expr=expression)? EOS					#returnStmt
					| Q=(QMARK | QQMARK) Exprs=expressionlist? EOS			#qoutStmt
					| BEGIN SEQUENCE EOS
					  StmtBlk=statementBlock
					  (RECOVER RecoverBlock=recoverBlock)?
					  END (SEQUENCE)? EOS										#seqStmt
					| Exprs=expressionlist EOS								#expressionStmt
					| pragma													#pragmaStmt
					| pp_line													#ppStmt
					| udc_line													#udcStmt
					;

ifElseBlock			: Cond=expression EOS StmtBlk=statementBlock
					  (ELSEIF ElseIfBlock=ifElseBlock | ELSE EOS ElseBlock=statementBlock)?
					;

caseBlock			: Key=CASE Cond=expression EOS StmtBlk=statementBlock NextCase=caseBlock?
					| Key=OTHERWISE EOS StmtBlk=statementBlock
					;

recoverBlock		: (USING Id=identifier)? EOS StmtBlock=statementBlock
					;

variableDeclaration	: (LOCAL? Var=IMPLIED | Var=VAR) Decl+=variableDeclarator (COMMA Decl+=variableDeclarator)*
					| LOCAL Decl+=variableDeclarator (COMMA Decl+=variableDeclarator)* (AS Type=datatype)?
					;

variableDeclarator	: Id=identifier ASSIGN_OP Expr=expression
					;

// Variable declarations
// There are many variations in the declarations
// LOCAL a,b                        // USUAL in most languages
// LOCAL a as STRING
// LOCAL a,b as STRING
// LOCAL a,b as STRING, c as INT
// LOCAL a AS STRING, c as INT
// LOCAL a := "Foo" as STRING
// LOCAL a := "Foo" as STRING, c := 123 as INT

// Each Var may have a assignment and/or type
// When the type is missing and the following element has a type
// then the type of the following element propagates forward until for all elements without type

localdecl          : (Static=STATIC LOCAL? | LOCAL)
					 LocalVars+=localvar (COMMA LocalVars+=localvar)*						EOS    #commonLocalDecl	// STATIC LOCAL or LOCAL
				   ;

localvar           : (Const=CONST)? ( Dim=DIM )? Id=identifier (LBRKT ArraySub=arraysub RBRKT)? 
					 (ASSIGN_OP Expression=expression)? ((AS | IS) DataType=datatype)?
				   ;
					  

// Old Style xBase declarations

xbasedecl        : T=(PRIVATE												// PRIVATE Foo, Bar
					  |PUBLIC												// PUBLIC  Foo, Bar
					  |MEMVAR												// MEMVAR  Foo, Bar
					  |PARAMETERS											// PARAMETERS Foo, Bar
					 )   Vars+=identifier (COMMA Vars+=identifier)* EOS       
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

expressionlist		: (Exprs+=expression (COMMA Exprs+=expression)*)
					;

expression			: Expr=expression Op=(DOT | COLON) Name=simpleName			#accessMember			// member access The ? is new
					| Expr=expression LPAREN ArgList=argumentList? RPAREN		#methodCall				// method call
					| Expr=expression LBRKT ArgList=bracketedArgumentList? RBRKT #arrayAccess			// Array element access
					| Expr=expression Op=(INC | DEC)							#postfixExpression		// expr ++/--
					| Op=(PLUS | MINUS | TILDE| ADDROF | INC | DEC)
					  Expr=expression											#prefixExpression		// +/-/~/&/++/-- expr
					| Left=expression Op=EXP Right=expression					#binaryExpression		// expr ^ expr
					| Left=expression Op=(MULT | DIV | MOD) Right=expression	#binaryExpression		// expr * expr
					| Left=expression Op=(PLUS | MINUS) Right=expression		#binaryExpression		// expr +/- expr
					| Left=expression Op=LSHIFT Right=expression				#binaryExpression		// expr << expr (shift)
					| Left=expression Op=GT	Gt=GT Right=expression				#binaryExpression		// expr >> expr (shift)
					| Left=expression
					  Op=( LT | LTE | GT | GTE | EQ | EEQ
							| SUBSTR | NEQ )
					  Right=expression											#binaryExpression		// expr >= expr (relational)
					| Left=expression Op=AMP Right=expression					#binaryExpression		// expr & expr (bitwise and)
					| Left=expression Op=TILDE Right=expression					#binaryExpression		// expr ~ expr (bitwise xor)
					| Left=expression Op=PIPE Right=expression					#binaryExpression		// expr | expr (bitwise or)
					| Op=(LOGIC_NOT|NOT) Expr=expression						#prefixExpression		// .not. expr (logical not)  also  !
					| Left=expression Op=(LOGIC_AND | AND) Right=expression		#binaryExpression		// expr .and. expr (logical and) also &&
					| Left=expression Op=LOGIC_XOR Right=expression				#binaryExpression		// expr .xor. expr (logical xor) 
					| Left=expression Op=(LOGIC_OR | OR) Right=expression		#binaryExpression		// expr .or. expr (logical or)  also || 
					| <assoc=right> Left=expression
					  Op=( ASSIGN_OP | ASSIGN_ADD | ASSIGN_SUB | ASSIGN_EXP
							| ASSIGN_MUL | ASSIGN_DIV | ASSIGN_MOD
							| ASSIGN_BITAND | ASSIGN_BITOR | ASSIGN_LSHIFT
							| ASSIGN_RSHIFT | ASSIGN_XOR )
					  Right=expression											#assignmentExpression	// expr := expr
					| Expr=primary												#primaryExpression
					;

					// Primary expressions
primary				: Key=SELF													#selfExpression
					| Key=SUPER													#superExpression
					| Literal=literalValue										#literalExpression		// literals
					| LiteralArray=literalArray									#literalArrayExpression	// { expr [, expr] }
					| CbExpr=codeblock											#codeblockExpression	// {| [id [, id...] | expr [, expr...] }
					| Type=datatype LCURLY ArgList=argumentList? RCURLY			#ctorCall				// id{ [expr [, expr...] }
					| TYPEOF LPAREN Type=datatype RPAREN						#typeOfExpression		// typeof( typeORid )
					| SIZEOF LPAREN Type=datatype RPAREN						#sizeOfExpression		// sizeof( typeORid )
					| DEFAULT LPAREN Type=datatype RPAREN						#defaultExpression		// default( typeORid )
					| Name=simpleName											#nameExpression			// generic name
					| Type=nativeType LPAREN Expr=expression RPAREN				#voConversionExpression	// nativetype( expr )
					| Type=datatype LPAREN CAST COMMA Expr=expression RPAREN	#voCastExpression		// typename(_CAST, expr )
					| PTR LPAREN Type=datatype COMMA Expr=expression RPAREN		#voCastPtrExpression	// PTR( typeName, expr )
					| Type=nativeType											#typeExpression			// Standard DotNet Types
					| Expr=iif													#iifExpression			// iif( expr, expr, expr )
					| LPAREN ( Expr=expression ) RPAREN							#parenExpression		// ( expr )
					| Op=(VO_AND | VO_OR | VO_XOR | VO_NOT) LPAREN Exprs=expressionlist RPAREN							#intrinsicExpression	// _Or(expr, expr, expr)
					| aliasedField												#aliasfield				//  ALIAS->FIELD
					| aliasedExpr												#aliasexpr				// ALIAS->(expr)
					| aliasedFuncCall											#aliasfunccall			//  foo->bar()
					| extendedaliasExpr											#aliasextended			// (expr) -> ...
					| AMP LPAREN expression RPAREN								#macroexpr				// &( expr )
					| AMP identifierName										#macrovar				// &id
					;

boundExpression		: Expr=boundExpression Op=(DOT | COLON) Name=simpleName		#boundAccessMember		// member access The ? is new
					| Expr=boundExpression LPAREN ArgList=argumentList? RPAREN	#boundMethodCall		// method call
					| Expr=boundExpression 
					  LBRKT ArgList=bracketedArgumentList? RBRKT				#boundArrayAccess		// Array element access
					| <assoc=right> Left=boundExpression
					| Op=(DOT | COLON) Name=simpleName							#bindMemberAccess
					| LBRKT ArgList=bracketedArgumentList? RBRKT				#bindArrayAccess
					;

bracketedArgumentList
					: Args+=argument (COMMA Args+=argument?)*
					;

argumentList		: Args+=argument (COMMA Args+=argument?)*
					;

argument			: ( COLON Name=identifierName ASSIGN_OP )? ( RefOut=(REF | OUT) )? Expr=expression
					;

iif					: IIF LPAREN Cond=expression COMMA TrueExpr=expression COMMA FalseExpr=expression RPAREN
					| IF LPAREN Cond=expression COMMA TrueExpr=expression COMMA FalseExpr=expression RPAREN
					;


name				: Left=name Op=DOT Right=simpleName								#qualifiedName
					| Name=aliasedName												#simpleOrAliasedName
					;

aliasedName			: Alias=identifierName Op=COLONCOLON Right=simpleName			#aliasQualifiedName
					| Global=GLOBAL Op=COLONCOLON Right=simpleName					#globalQualifiedName
					| Name=simpleName												#identifierOrGenericName
					;

simpleName			: Id=identifier	GenericArgList=genericArgumentList?
					;


identifierName		: Id=identifier
					;

datatype			: TypeName=typeName PTR											#ptrDatatype
					| TypeName=typeName (Ranks+=arrayRank)+							#arrayDatatype
					| TypeName=typeName 											#simpleDatatype
					;

arrayRank			: LBRKT (Commas+=COMMA)* RBRKT
					;

typeName			: NativeType=nativeType
					| Name=name
					;

literalArray		: (LT Type=datatype GT)? LCURLY (Exprs=expressionlist)? RCURLY
					;

					;
aliasmethodCall		: Expr=expression LPAREN ArgList=argumentList? RPAREN
					;

aliasedField		:	FIELD ALIAS Id=identifierName									// _FIELD->NAME
					|	Left=identifierName ALIAS Right=identifierName					// CUSTOMER->NAME
					|   FIELD ALIAS Left=identifierName ALIAS Right=identifierName		// _FIELD->CUSTOMER->NAME
					;

aliasedExpr			:	Id=identifierName ALIAS LPAREN Expr=expression RPAREN		// CUSTOMER->(<Expression>)
					;

aliasedFuncCall		:	i=identifierName a=ALIAS m=aliasmethodCall					// Customer->DoSomething()
					;

extendedaliasExpr	:	l1=LPAREN e1=expression r1=RPAREN a=ALIAS
						( i=identifierName								// (expr) -> ID
						| l2=LPAREN e2=expression r2=RPAREN				// (expr) -> (expr)
						| m=aliasmethodCall									// (expr) -> func(..)
						)
					;

codeblock			: LCURLY (OR | PIPE CbParamList=codeblockParamList? PIPE)
					  ( Expr=expression?
					  | EOS StmtBlk=statementBlock 
					  | ExprList=codeblockExprList )
					  RCURLY
					;

codeblockParamList	: Ids+=identifier (COMMA Ids+=identifier)*
					;

codeblockExprList	: (Exprs+=expression COMMA)+ ReturnExpr=expression
					;

identifierString	: Token=(ID | KWID | STRING_CONST)
					;

nativeType			: Token=
					( ARRAY					// VO types start here
					| CODEBLOCK
					| DATE 
					| FLOAT 
					| PSZ 
					| SYMBOL 
					| USUAL
					| BYTE					// .Net types start here
					| DWORD
					| SHORTINT
					| INT
					| LOGIC
					| LONGINT
					| OBJECT
					| PTR
					| REAL4
					| REAL8
					| STRING
					| WORD
					| VOID )
					;

literalValue		: Token=
					( TRUE_CONST
					| FALSE_CONST
					| CHAR_CONST
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


keyword             :  Token=(ACCESS | ALIGN | AS | ASSIGN | BEGIN | BREAK | CASE | CAST | CLASS | CLIPPER | DEFINE | DIM | DLL | DO | DOWNTO
					| ELSE | ELSEIF | END | ENDCASE | ENDDO | ENDIF | EXIT | EXPORT | FASTCALL | FIELD | FOR | FUNCTION | GLOBAL
					| HIDDEN | IF | IIF | INHERIT | INSTANCE |  IS | LOCAL | LOOP | MEMBER | METHOD | NEXT | OTHERWISE 
					| PASCAL | PRIVATE | PROCEDURE | PROTECTED | PTR | PUBLIC | RECOVER | RETURN | SELF| SEQUENCE | SIZEOF | STEP | STRICT | SUPER
					| THISCALL | TO | TYPEOF | UNION | UPTO | USING | WHILE | VO_AND| VO_NOT| VO_OR| VO_XOR
					| STRUCTURE ) 
					;

