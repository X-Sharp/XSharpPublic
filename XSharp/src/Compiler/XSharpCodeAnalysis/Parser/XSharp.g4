grammar XSharp;

/*
 * Parser Rules
*/

// Known issues:
// - ALIAS (->) operator
// - preprocessor , #region, #using etc

@parser::members
{
	bool _VOSyntax = true;
	public bool VOSyntax
	{
		get {return _VOSyntax;}
		set {_VOSyntax = value;}
	}
	bool _ClsFunc = true;
	public bool AllowFunctionInsideClass
	{
		get {return _ClsFunc;}
		set {_ClsFunc = value;}
	}
	bool _xBaseVars = true;
	public bool AllowXBaseVariables
	{
		get {return _xBaseVars;}
		set {_xBaseVars = value;}
	}
}


options	{ 
		language=CSharp; 
		tokenVocab=XSharpLexer;
		}


source				: (eos)? (Entities+=entity)* eof
					;

entity              : namespace_
					| class_
					| structure_
					| interface_
					| delegate_
					| event_
					| enum_
                    | function                  // This will become part of the 'Globals' class
					| procedure                 // This will become part of the 'Globals' class
					| method                    // Method xxx Class xxx syntax
					| globalAttributes          // Assembly attributes, Module attributes etc.
                    | using_                    // Using Namespace
                    | pragma                    // Compiler pragma
					| {_VOSyntax}? voglobal     // This will become part of the 'Globals' class
					| {_VOSyntax}? vodefine     // This will become part of the 'Globals' class
					| {_VOSyntax}? vostruct     // Compatibility (unsafe) structure
					| {_VOSyntax}? vounion      // Compatibility (unsafe) structure with members aligned at FieldOffSet 0
					| {_VOSyntax}? vodll        // External method of the Globals class
					;

function            : (Attributes=attributes)? (Modifiers=funcprocModifiers)? 
						FUNCTION Id=identifier TypeParameters=typeparameters? (ParamList=parameterList)?
					   (AS Type=datatype)? 
					   (ConstraintsClauses+=typeparameterconstraintsclause)*
					   (CallingConvention=callingconvention)? eos 
					   StmtBlk=statementBlock
					;

procedure           : (Attributes=attributes)? (Modifiers=funcprocModifiers)? 
					   PROCEDURE Id=identifier TypeParameters=typeparameters? (ParamList=parameterList)?
					   (ConstraintsClauses+=typeparameterconstraintsclause)*
					   (CallingConvention=callingconvention)? Init=(INIT1|INIT2|INIT3)? eos 
					   StmtBlk=statementBlock
					;

callingconvention	: Convention=(CLIPPER | STRICT | PASCAL) 
					;


vodll				: (Modifiers=funcprocModifiers)? DLL 
					  ( T=FUNCTION Id=identifier ParamList=parameterList (AS Type=datatype)?
					  | T=PROCEDURE Id=identifier ParamList=parameterList )
					  (CallingConvention=dllcallconv)? COLON 
					  Dll=identifierString DOT Entrypoint=identifierString //(NEQ Ordinal=INT_CONST)? 
                    ;

dllcallconv         : Cc=( CLIPPER | STRICT | PASCAL | THISCALL | FASTCALL)
                    ;


parameterList		: LPAREN (Params+=parameter (COMMA Params+=parameter)*)? RPAREN
					;

// Compared with C# PARAMS is not supported. This can be achived by setting [ParamArrayAttribute] on the parameter: [ParamArrayAttribute] args as OBJECT[] 
parameter			: (Attributes=attributes)? Id=identifier (ASSIGN_OP Default=expression)? Modifiers=parameterDeclMods Type=datatype
					;

parameterDeclMods   : Tokens+=(AS | REF | OUT | IS ) Tokens+=SELF? Tokens+=CONST?
					;

statementBlock      : (Stmts+=statement)*
					;


funcprocModifiers	: ( Tokens+=(STATIC | INTERNAL | PUBLIC | EXPORT | UNSAFE) )+
					;


using_              : HASHUSING (Static=STATIC)? (Alias=identifierName ASSIGN_OP)? Name=name     eos
                    ;

// nvk: roslyn treats #pragma directives as trivia attached to parse nodes. The parser does not handle them directly.
pragma              : PRAGMA OPTIONS    LPAREN Compileroption=STRING_CONST COMMA Switch=pragmaswitch RPAREN eos         #pragmaOptions
                    | PRAGMA WARNINGS   LPAREN WarningNumber=INT_CONST     COMMA Switch=pragmaswitch RPAREN eos         #pragmaWarnings
                    ;

pragmaswitch        : ON | OFF | DEFAULT
                    ;

voglobal			: (Attributes=attributes)? (Modifiers=funcprocModifiers)? GLOBAL (Const=CONST)? Vars=classVarList eos
					;


// Separate method/access/assign with Class name -> convert to partial class with just one method
// And when Class is outside of assembly, convert to Extension Method?
// nvk: we have no knowledge of whether a class is outside of the assembly at the parser stage!
method				: (Attributes=attributes)? (Modifiers=memberModifiers)?
					  T=methodtype (ExplicitIface=name DOT)? Id=identifier TypeParameters=typeparameters? (ParamList=parameterList)? (AS Type=datatype)? 
					  (ConstraintsClauses+=typeparameterconstraintsclause)*
					  (CallingConvention=callingconvention)? (CLASS ClassId=identifier)? eos 
					  StmtBlk=statementBlock		
					;

methodtype			: Token=(METHOD | ACCESS | ASSIGN)
					;

// Convert to constant on Globals class. Expression must be resolvable at compile time
vodefine			: DEFINE Id=identifier ASSIGN_OP Expr=expression
					;

vostruct			: VOSTRUCT Id=identifier (ALIGN Alignment=INT_CONST)? eos
					  (Members+=vostructmember)+
					;

vounion				: UNION  Id=identifier  eos
					  (Members+=vostructmember)+
					;


vostructmember		: MEMBER Dim=DIM Id=identifier LBRKT ArraySub=arraysub RBRKT (AS | IS) DataType=datatype eos
					| MEMBER Id=identifier (AS | IS) DataType=datatype eos
					;

namespace_			: BEGIN NAMESPACE Name=name eos
					  (Entities+=entity)*
					  END NAMESPACE eos
					;

interface_			: (Attributes=attributes)? (Modifiers=interfaceModifiers)?
					  INTERFACE Id=identifier TypeParameters=typeparameters?
					  ((INHERIT|COLON) Parents+=datatype)? (COMMA Parents+=datatype)*
					  (ConstraintsClauses+=typeparameterconstraintsclause)* eos         // Optional typeparameterconstraints for Generic Class
					  (Members+=classmember)*
					  END INTERFACE eos
					;

interfaceModifiers	: ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE | PARTIAL) )+
					;

class_				: (Attributes=attributes)? (Modifiers=classModifiers)?
					  CLASS Id=identifier TypeParameters=typeparameters?						// TypeParameters indicate Generic Class
					  (INHERIT BaseType=datatype)?
					  (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)?
					  (ConstraintsClauses+=typeparameterconstraintsclause)* eos         // Optional typeparameterconstraints for Generic Class
					  (Members+=classmember)*
					  END CLASS  eos
					;

classModifiers		: ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | ABSTRACT | SEALED | STATIC | UNSAFE | PARTIAL) )+
					;

// Start Extensions for Generic Classes
typeparameters      : LT TypeParams+=typeparameter (COMMA attributes? TypeParams+=typeparameter)* GT 
					;

typeparameter       : Attributes=attributes? VarianceKeyword=(IN | OUT)? Id=identifier
					;

typeparameterconstraintsclause
					: WHERE Name=identifierName IS Constraints+=typeparameterconstraint (COMMA Constraints+=typeparameterconstraint)*
					;
							  
typeparameterconstraint: Type=typeName						#typeConstraint				//  Class Foo<t> WHERE T IS Customer
					   | Key=(CLASS|STRUCTURE)				#classOrStructConstraint	//  Class Foo<t> WHERE T IS (CLASS|STRUCTURE)
                       | NEW LPAREN RPAREN					#constructorConstraint		//  Class Foo<t> WHERE T IS NEW()
					   ; 
							  
// End of Extensions for Generic Classes

structure_			: (Attributes=attributes)? (Modifiers=structureModifiers)?
					  STRUCTURE Id=identifier TypeParameters=typeparameters?
					  (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)?
					  (ConstraintsClauses+=typeparameterconstraintsclause)* eos
					  (Members+=classmember)+
					  END STRUCTURE eos
					;

structureModifiers	: ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE | PARTIAL) )+
					;


delegate_			: (Attributes=attributes)? (Modifiers=delegateModifiers)?
					  DELEGATE Id=identifier TypeParameters=typeparameters?
					  ParamList=parameterList? AS Type=datatype
					  (ConstraintsClauses+=typeparameterconstraintsclause)* eos
					;

delegateModifiers	: ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE) )+
					;


enum_				: (Attributes=attributes)? (Modifiers=enumModifiers)?
					  ENUM Id=identifier (AS Type=datatype)? eos
					  (Members+=enummember)+
					  END (ENUM)? eos
					;

enumModifiers		: ( Tokens+=(NEW | PUBLIC| EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN) )+
					;

enummember			: (Attributes=attributes)? MEMBER? Id=identifier (ASSIGN_OP Expr=expression)? eos
					;

event_				:  (Attributes=attributes)? (Modifiers=eventModifiers)?
					   EVENT (ExplicitIface=name DOT)? Id=identifier AS Type=datatype eos
					;

eventModifiers		: ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | STATIC | VIRTUAL | SEALED | ABSTRACT | UNSAFE) )+
					;



classvars			: (Attributes=attributes)? (Modifiers=classvarModifiers)? Vars=classVarList eos
					; 

classvarModifiers	: ( Tokens+=(INSTANCE| STATIC | CONST | INITONLY | PRIVATE | HIDDEN | PROTECTED | PUBLIC | EXPORT | INTERNAL | VOLATILE | UNSAFE) )+
					;

classVarList		: Var+=classvar (COMMA Var+=classvar)* ((AS | IS) DataType=datatype)?
					;

classvar			: (Dim=DIM)? Id=identifier (LBRKT ArraySub=arraysub RBRKT)? (ASSIGN_OP Initializer=expression)?
					;

arraysub			: ArrayIndex+=expression (RBRKT LBRKT ArrayIndex+=expression)+		// x][y
					| ArrayIndex+=expression (COMMA ArrayIndex+=expression)+			// x,y
					| ArrayIndex+=expression
					;

property			: (Attributes=attributes)? (Modifiers=memberModifiers)? 
					  PROPERTY (SELF ParamList=propertyParameterList | (ExplicitIface=name DOT)? Id=identifier) AS Type=datatype 
					  ( Auto=AUTO (AutoAccessors+=propertyAutoAccessor)* (ASSIGN_OP Initializer=expression)? eos	// Auto
					  | (LineAccessors+=propertyLineAccessor)+ eos													// Single Line
					  | Multi=eos (Accessors+=propertyAccessor)+  END PROPERTY? eos									// Multi Line
					  )
					;

propertyParameterList
					: LBRKT (Params+=parameter (COMMA Params+=parameter)*)? RBRKT
					;

propertyAutoAccessor: Attributes=attributes? Modifiers=memberModifiers? Key=(GET|SET)
					;

propertyLineAccessor: Attributes=attributes? Modifiers=memberModifiers? 
					  ( Key=GET Expr=expression?
					  | Key=SET ExprList=expressionListStmt? )
					;

expressionListStmt	: Exprs+=expression (COMMA Exprs+=expression)*
					;

propertyAccessor    : Attributes=attributes? Modifiers=memberModifiers? 
					  ( Key=GET eos StmtBlk=statementBlock END GET?
					  | Key=SET eos StmtBlk=statementBlock END SET? )
					  eos
					;

classmember			: Member=method										#clsmethod
					| (Attributes=attributes)?
					  (Modifiers=constructorModifiers)? 
					  CONSTRUCTOR (ParamList=parameterList)? eos 
					  (Chain=(SELF | SUPER) 
						LPAREN ArgList=argumentList? RPAREN  eos)?
					  StmtBlk=statementBlock							#clsctor
					| (Attributes=attributes)? 
					  (Modifiers=destructorModifiers)?
					  DESTRUCTOR (LPAREN RPAREN)?  eos 
					  StmtBlk=statementBlock							#clsdtor
					| Member=classvars									#clsvars
					| Member=property									#clsproperty
					| Member=operator_									#clsoperator
					| Member=structure_									#nestedStructure
					| Member=class_										#nestedClass
					| Member=delegate_									#nestedDelegate
					| Member=enum_										#nestedEnum
					| Member=event_										#nestedEvent
					| Member=interface_									#nestedInterface
//                    | using_										#nestedUsing	// nvk: C# does not allow using directives within a class!
                    | Pragma=pragma										#nestedPragma
					| {_ClsFunc}? Member=function						#clsfunction		// Equivalent to method
					| {_ClsFunc}? Member=procedure						#clsprocedure		// Equivalent to method
					;


constructorModifiers: ( Tokens+=( PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | EXTERN | STATIC ) )+
					;

destructorModifiers : ( Tokens+=EXTERN )+
					;


overloadedOps		: Token=(INC | DEC | NOT | PLUS | MINUS | MULT | DIV | MOD | AND | OR| LSHIFT| RSHIFT| EEQ
					| GT  | LT | NEQ | GTE| LTE | TRUE_CONST | FALSE_CONST
					| TILDE | AMP   | PIPE )
					;

conversionOps		: Token=( IMPLICIT | EXPLICIT )
					;

operator_			: Attributes=attributes? Modifiers=operatorModifiers? 
					  OPERATOR (Operation=overloadedOps | Conversion=conversionOps)
					  ParamList=parameterList AS Type=datatype eos StmtBlk=statementBlock
					;

operatorModifiers	: ( Tokens+=(PUBLIC | STATIC | EXTERN) )+
					;

memberModifiers		: ( Tokens+=(NEW | PRIVATE | HIDDEN | PROTECTED | PUBLIC | EXPORT | INTERNAL | STATIC | VIRTUAL | SEALED | ABSTRACT | ASYNC | UNSAFE | EXTERN) )+
					;

attributes			: ( AttrBlk+=attributeBlock )+
					;

attributeBlock		: LBRKT Target=attributeTarget? Attributes+=attribute (COMMA Attributes+=attribute)* RBRKT
					;

attributeTarget		: Id=identifier COLON
					| Kw=keyword COLON
					;

attribute			: Name=name (LPAREN (Params+=attributeParam (COMMA Params+=attributeParam)* )? RPAREN ) ?
					;

attributeParam		: (Name=identifierName ASSIGN_OP)? Expr=expression					#propertyAttributeParam
					| Expr=expression													#exprAttributeParam
					;

globalAttributes    : LBRKT Target=globalAttributeTarget Attributes+=attribute (COMMA Attributes+=attribute) RBRKT
					;

globalAttributeTarget : Token=(ASSEMBLY | MODULE) COLON
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

statement           : Decl=localdecl                                            #declarationStmt
					| {_xBaseVars}? xbasedecl									#xbasedeclStmt
					| DO WHILE Expr=expression eos
					  StmtBlk=statementBlock END DO eos							#whileStmt
					| WHILE Expr=expression eos
					  StmtBlk=statementBlock END eos							#whileStmt
					| FOR Iter=expression ASSIGN_OP InitExpr=expression
					  Dir=(TO | UPTO | DOWNTO) FinalExpr=expression
					  (STEP Step=expression)? eos
					  StmtBlk=statementBlock NEXT eos							#forStmt
					| IF IfStmt=ifElseBlock
					  (END IF? | ENDIF)  eos									#ifStmt	
					| DO CASE eos
					  CaseStmt=caseBlock?
					  (END CASE? | ENDCASE) eos									#caseStmt
					| EXIT eos													#exitStmt
					| LOOP eos													#loopStmt
					| Exprs+=expression (COMMA Exprs+=expression)* eos			#expressionStmt
					| BREAK Expr=expression? eos								#breakStmt
					| RETURN (VOID | Expr=expression)? eos						#returnStmt
					| Q=(QMARK | QQMARK)
					   Exprs+=expression (COMMA Exprs+=expression)* eos			#qoutStmt
					| BEGIN SEQUENCE eos
					  StmtBlk=statementBlock
					  (RECOVER RecoverBlock=recoverBlock)?
					  (FINALLY eos FinBlock=statementBlock)?
					  END (SEQUENCE)? eos										#seqStmt
					//
					// New in Vulcan
					//
					| REPEAT eos
					  StmtBlk=statementBlock
					  UNTIL Expr=expression eos									#repeatStmt
					| FOREACH
					  (IMPLIED Id=identifier | Id=identifier AS Type=datatype| VAR Id=identifier)
					  IN Container=expression eos
					  StmtBlk=statementBlock NEXT eos							#foreachStmt
					| THROW Expr=expression? eos								#throwStmt
					| TRY eos StmtBlk=statementBlock
					  (CATCH CatchBlock+=catchBlock)*
					  (FINALLY eos FinBlock=statementBlock)?
					  END TRY? eos												#tryStmt
					| BEGIN LOCK Expr=expression eos
					  StmtBlk=statementBlock
					  END LOCK? eos												#lockStmt
					| BEGIN SCOPE eos
					  StmtBlk=statementBlock
					  END SCOPE? eos											#scopeStmt
					//
					// New XSharp Statements
					//
					| YIELD RETURN (VOID | Expr=expression)? eos				#yieldStmt
					| SWITCH Expr=expression eos
					  (SwitchBlock+=switchBlock)*
					  END SWITCH?  eos											#switchStmt
					| BEGIN USING Expr=expression eos
						Stmtblk=statementBlock
					  END USING? eos											#usingStmt
					| BEGIN UNSAFE eof
					  StmtBlk=statementBlock
					  END UNSAFE? eos											#unsafeStmt
					| BEGIN Ch=CHECKED eof
					  StmtBlk=statementBlock
					  END CHECKED? eos											#checkedStmt
					| BEGIN Ch=UNCHECKED eof
					  StmtBlk=statementBlock
					  END UNCHECKED? eos										#checkedStmt

					;

ifElseBlock			: Cond=expression eos StmtBlk=statementBlock
					  (ELSEIF ElseIfBlock=ifElseBlock | ELSE eos ElseBlock=statementBlock)?
					;

caseBlock			: Key=CASE Cond=expression eos StmtBlk=statementBlock NextCase=caseBlock?
					| Key=OTHERWISE eos StmtBlk=statementBlock
					;

// Note that literalValue is not enough. We also need to support members of enums
switchBlock         : (Key=CASE Const=expression | Key=(OTHERWISE|DEFAULT)) eos StmtBlk=statementBlock			 
					;

catchBlock			: Id=identifier AS Type=datatype eos StmtBlk=statementBlock
					;

recoverBlock		: (USING Id=identifier)? eos StmtBlock=statementBlock
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
					 LocalVars+=localvar (COMMA LocalVars+=localvar)*						eos   #commonLocalDecl	// STATIC LOCAL or LOCAL
				   | (Static=STATIC LOCAL? IMPLIED | LOCAL IMPLIED | Static=STATIC? VAR)							// LOCAL IMPLIED
				     ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*               eos   #varLocalDecl		// VAR special for Robert !
				   ;

localvar           : (Const=CONST)? ( Dim=DIM )? Id=identifier (LBRKT ArraySub=arraysub RBRKT)? 
					 (ASSIGN_OP Expression=expression)? ((AS | IS) DataType=datatype)?
				   ;
					  
impliedvar         : (Const=CONST)? Id=identifier ASSIGN_OP Expression=expression 
				   ;


// Old Style xBase declarations

xbasedecl        : T=(PRIVATE												// PRIVATE Foo, Bar
					  |PUBLIC												// PUBLIC  Foo, Bar
					  |MEMVAR												// MEMVAR  Foo, Bar
					  |PARAMETERS											// PARAMETERS Foo, Bar
					 )   Vars+=identifier (COMMA Vars+=identifier)* eos       
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

expression			: Left=expression Op=(DOT | COLON) Right=identifierName		#accessMember			// member access The ? is new
					| Expr=expression LPAREN ArgList=argumentList? RPAREN		#methodCall				// method call
					| Expr=expression LBRKT ArgList=bracketedArgumentList? RBRKT #arrayAccess			// Array element access
					| Left=expression Op=QMARK Right=expression					#condAccessExpr			// expr ? expr
					| Expr=expression Op=(INC | DEC)							#postfixExpression		// expr ++/--
					| LPAREN Type=datatype RPAREN Expr=expression				#typeCast			    // (typename) expr
					| Op=AWAIT Expr=expression									#awaitExpression		// AWAIT expr
					| Op=(PLUS | MINUS | TILDE| ADDROF | INC | DEC)
					  Expr=expression											#prefixExpression		// +/-/~/&/++/-- expr
					| Expr=expression IS Type=datatype							#typeCheckExpression	// expr IS typeORid
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
					| Left=expression Op=(LOGIC_AND | AND) Right=expression		#binaryExpression		// expr .and. expr (logical and)
					| Left=expression Op=LOGIC_XOR Right=expression				#binaryExpression		// expr .xor. expr (logical xor)
					| Left=expression Op=(LOGIC_OR | OR) Right=expression		#binaryExpression		// expr .or. expr (logical or)
					| Left=expression Op=DEFAULT Right=expression				#binaryExpression		// expr DEFAULT expr 
					| <assoc=right> Left=expression
					  Op=( ASSIGN_OP | ASSIGN_ADD | ASSIGN_SUB | ASSIGN_EXP
							| ASSIGN_MUL | ASSIGN_DIV | ASSIGN_MOD
							| ASSIGN_BITAND | ASSIGN_BITOR | ASSIGN_LSHIFT
							| ASSIGN_RSHIFT | ASSIGN_XOR )
					  Right=expression											#assignmentExpression	// expr := expr
					| Key=SELF													#selfExpression
					| Key=SUPER													#superExpression
					| Literal=literalValue										#literalExpression		// literals
					| LiteralArray=literalArray									#literalArrayExpression	// { expr [, expr] }
					| CbExpr=codeblock											#codeblockExpression	// {| [id [, id...] | expr [, expr...] }
                    | Query=linqQuery											#queryExpression        // LINQ
					| Type=datatype LCURLY ArgList=argumentList? RCURLY			#ctorCall				// id{ [expr [, expr...] }
					| ch=CHECKED LPAREN ( Expr=expression ) RPAREN				#checkedExpression		// checked( expression )
					| ch=UNCHECKED LPAREN ( Expr=expression ) RPAREN			#checkedExpression		// unchecked( expression )
					| TYPEOF LPAREN Type=datatype RPAREN						#typeOfExpression		// typeof( typeORid )
					| SIZEOF LPAREN Type=datatype RPAREN						#sizeOfExpression		// sizeof( typeORid )
					| Name=identifierName										#nameExpression			// generic name
					| Type=nativeType											#typeExpression			// ARRAY, CODEBLOCK, etc.
					| Expr=iif													#iifExpression			// iif( expr, expr, expr )
					| Op=(DOT | COLON) Right=identifierName						#bindMemberAccess
					| LBRKT ArgList=bracketedArgumentList? RBRKT				#bindArrayAccess
					| LPAREN ( Expr=expression ) RPAREN							#parenExpression		// ( expr )
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

name				: Left=name Op=DOT Right=identifier							#qualifiedName
					| Id=identifier	GenericArgList=genericArgumentList			#genericName
					| Id=identifier												#simpleName
					| Alias=identifierName Op=COLONCOLON Right=identifierName	#aliasQualifiedName
					| Global=GLOBAL Op=COLONCOLON Right=identifierName			#globalQualifiedName
					;

genericArgumentList : LT GenericArgs+=datatype (COMMA GenericArgs+=datatype)* GT
					;

identifierName		: Id=identifier
					;

datatype			: TypeName=typeName PTR											#ptrDatatype
					| TypeName=typeName (Ranks+=arrayRank)+							#arrayDatatype
					| TypeName=typeName 											#simpleDatatype
					| TypeName=typeName QMARK 										#nullableDatatype
					;

arrayRank			: LBRKT (COMMA)* RBRKT
					;

typeName			: NativeType=nativeType
					| Name=name
					;

literalArray		: (LT Type=datatype GT)? LCURLY (Exprs+=expression (COMMA Exprs+=expression)*)? RCURLY
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


// LINQ Support

linqQuery			: From=fromClause Body=queryBody
                    ;

fromClause          : FROM Id=identifier (AS Type=typeName)? IN Expr=expression
                    ;

queryBody           : (Bodyclauses+=queryBodyClause)* SorG=selectOrGroupclause (Continuation=queryContinuation)?
                    ;

queryBodyClause     : From=fromClause                                                                                           #fromBodyClause
                    | LET Id=identifier ASSIGN_OP Expr=expression                                                               #letClause
                    | WHERE Expr=expression                                                                                     #whereClause        // expression must be Boolean
                    | JOIN Id=identifier (AS Type=typeName)? IN Expr=expression ON OnExpr=expression EQUALS EqExpr=expression
					  Into=joinIntoClause?																						#joinClause
                    | ORDERBY Orders+=ordering (COMMA Orders+=ordering)*                                                        #orderbyClause
                    ;

joinIntoClause		: INTO Id=identifier
					;

ordering            : Expr=expression Direction=(ASCENDING|DESCENDING)?
                    ;

selectOrGroupclause : SELECT Expr=expression                                #selectClause
                    | GROUP Expr=expression BY ByExpr=expression            #groupClause
                    ;

queryContinuation   : INTO Id=identifier Body=queryBody
                    ;
// -- End of LINQ


// All New Vulcan and X# keywords can also be recognized as Identifier
identifier			: Token=ID  
					| VnToken=keywordvn 
					| XsToken=keywordxs
					;

identifierString	: Token=(ID | STRING_CONST)
					| VnToken=keywordvn 
					| XsToken=keywordxs
					;

nativeType			: Token=
					( ARRAY
					| BYTE
					| CODEBLOCK
					| DATE
					| DWORD
					| DYNAMIC
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
					| VOID 
					| CHAR )
					;

literalValue		: Token=
					( TRUE_CONST
					| FALSE_CONST
					| CHAR_CONST
					| STRING_CONST
					| ESCAPED_STRING_CONST
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
					| NULL_SYMBOL 
					| MACRO )
					;

eos                 : (NL)* (NL|EOF)
                    | SEMI ~NL
					;

eof                 : EOF
					;


keyword             : (KwVo=keywordvo | KwVn=keywordvn | KwXs=keywordxs) ;

keywordvo           : Token=(ACCESS | ALIGN | AS | ASSIGN | BEGIN | BREAK | CASE | CAST | CLASS | CLIPPER | DEFINE | DIM | DLL | DO | DOWNTO
					| ELSE | ELSEIF | END | ENDCASE | ENDDO | ENDIF | EXIT | EXPORT | FASTCALL | FIELD | FOR | FUNCTION | GLOBAL
					| HIDDEN | IF | IIF | INHERIT | IN | INSTANCE |  IS | LOCAL | LOOP | MEMBER | METHOD | NEXT | OTHERWISE 
					| PASCAL | PRIVATE | PROCEDURE | PROTECTED | PUBLIC | RECOVER | RETURN | SELF| SEQUENCE | SIZEOF | STEP | STRICT | SUPER
					| THISCALL | TO | TYPEOF | UNION | UPTO | USING | WHILE )
					;

keywordvn           : Token=(ABSTRACT | AUTO | CATCH | CONSTRUCTOR | CONST | DEFAULT | DELEGATE | DESTRUCTOR	| ENUM | EVENT
					| EXPLICIT | FINALLY | FOREACH | GET | IMPLEMENTS | IMPLICIT | IMPLIED | INITONLY | INTERFACE | INTERNAL 
					| LOCK | NAMESPACE | NEW | OPERATOR	| OPTIONS | OUT | PARTIAL | PROPERTY | REPEAT | SCOPE | SEALED | SET | STRUCTURE			
					| THROW | TRY | UNTIL | VALUE | VIRTUAL | VOSTRUCT | WARNINGS)
					;

keywordxs           : Token=( ASCENDING | ASSEMBLY | ASYNC | AWAIT | BY | CHECKED | DESCENDING | DYNAMIC | EQUALS | EXTERN | FROM | 
                              GROUP | INTO | JOIN | LET | MODULE | ORDERBY | SELECT | SWITCH | UNCHECKED | UNSAFE | VAR | VOLATILE | WHERE | YIELD | CHAR)
					;
