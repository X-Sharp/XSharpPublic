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


source				: (Entities+=entity)* EOF
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
					   (CallingConvention=callingconvention)? EOS 
					   StmtBlk=statementBlock
					;

procedure           : (Attributes=attributes)? (Modifiers=funcprocModifiers)? 
					   PROCEDURE Id=identifier TypeParameters=typeparameters? (ParamList=parameterList)?
					   (ConstraintsClauses+=typeparameterconstraintsclause)*
					   (CallingConvention=callingconvention)? Init=(INIT1|INIT2|INIT3)? EOS 
					   StmtBlk=statementBlock
					;

callingconvention	: Convention=(CLIPPER | STRICT | PASCAL) 
					;


vodll				: (Modifiers=funcprocModifiers)? DLL 
					  ( T=FUNCTION Id=identifier ParamList=parameterList (AS Type=datatype)?
					  | T=PROCEDURE Id=identifier ParamList=parameterList )
					  (CallingConvention=dllcallconv)? COLON 
					  Dll=identifierString DOT Entrypoint=identifierString //(NEQ Ordinal=INT_CONST)? 
					  EOS
                    ;

dllcallconv         : Cc=( CLIPPER | STRICT | PASCAL | THISCALL | FASTCALL)
                    ;


parameterList		: LPAREN (Params+=parameter (COMMA Params+=parameter)*)? RPAREN
					;

// Compared with C# PARAMS is not supported. This can be achived by setting [ParamArrayAttribute] on the parameter: [ParamArrayAttribute] args as OBJECT[] 
parameter			: (Attributes=attributes)? Self=SELF? Id=identifier (ASSIGN_OP Default=expression)? Modifiers=parameterDeclMods Type=datatype
					;

parameterDeclMods   : Tokens+=(AS | REF | OUT | IS ) Tokens+=CONST?
					;

statementBlock      : (Stmts+=statement)*
					;


funcprocModifiers	: ( Tokens+=(STATIC | INTERNAL | PUBLIC | EXPORT | UNSAFE) )+
					;


using_              : (HASHUSING|USING) (Static=STATIC)? (Alias=identifierName ASSIGN_OP)? Name=name     EOS
                    ;

// nvk: roslyn treats #pragma directives as trivia attached to parse nodes. The parser does not handle them directly.
pragma              : PRAGMA OPTIONS    LPAREN Compileroption=STRING_CONST COMMA Switch=pragmaswitch RPAREN EOS         #pragmaOptions
                    | PRAGMA WARNINGS   LPAREN WarningNumber=INT_CONST     COMMA Switch=pragmaswitch RPAREN EOS         #pragmaWarnings
                    ;

pragmaswitch        : ON | OFF | DEFAULT
                    ;

voglobal			: (Attributes=attributes)? (Modifiers=funcprocModifiers)? GLOBAL (Const=CONST)? Vars=classVarList EOS
					;


// Separate method/access/assign with Class name -> convert to partial class with just one method
// And when Class is outside of assembly, convert to Extension Method?
// nvk: we have no knowledge of whether a class is outside of the assembly at the parser stage!
method				: (Attributes=attributes)? (Modifiers=memberModifiers)?
					  T=methodtype (ExplicitIface=nameDot)? Id=identifier TypeParameters=typeparameters? (ParamList=parameterList)? (AS Type=datatype)? 
					  (ConstraintsClauses+=typeparameterconstraintsclause)*
					  (CallingConvention=callingconvention)? (CLASS (Namespace=nameDot)? ClassId=identifier)? EOS 
					  StmtBlk=statementBlock		
					;

methodtype			: Token=(METHOD | ACCESS | ASSIGN)
					;

// Convert to constant on Globals class. Expression must be resolvable at compile time
vodefine			: DEFINE Id=identifier ASSIGN_OP Expr=expression
					;

vostruct			: (Modifiers=votypeModifiers)? 
					  VOSTRUCT (Namespace=nameDot)? Id=identifier (ALIGN Alignment=INT_CONST)? EOS
					  (Members+=vostructmember)+
					;

vostructmember		: MEMBER Dim=DIM Id=identifier LBRKT ArraySub=arraysub RBRKT (AS | IS) DataType=datatype EOS
					| MEMBER Id=identifier (AS | IS) DataType=datatype EOS
					;


vounion				: (Modifiers=votypeModifiers)? 
					  UNION (Namespace=nameDot)? Id=identifier EOS
					  (Members+=vounionmember)+
					;

votypeModifiers		: ( Tokens+=(INTERNAL | PUBLIC | EXPORT | UNSAFE) )+
					;


vounionmember		: MEMBER Dim=DIM Id=identifier LBRKT ArraySub=arraysub RBRKT (AS | IS) DataType=datatype EOS
					| MEMBER Id=identifier (AS | IS) DataType=datatype EOS
					;

namespace_			: BEGIN NAMESPACE Name=name EOS
					  (Entities+=entity)*
					  END NAMESPACE EOS
					;

interface_			: (Attributes=attributes)? (Modifiers=interfaceModifiers)?
					  INTERFACE (Namespace=nameDot)? Id=identifier TypeParameters=typeparameters?
					  ((INHERIT|COLON) Parents+=datatype)? (COMMA Parents+=datatype)*
					  (ConstraintsClauses+=typeparameterconstraintsclause)* EOS         // Optional typeparameterconstraints for Generic Class
					  (Members+=classmember)*
					  END INTERFACE EOS
					;

interfaceModifiers	: ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE | PARTIAL) )+
					;

class_				: (Attributes=attributes)? (Modifiers=classModifiers)?
					  CLASS (Namespace=nameDot)? Id=identifier TypeParameters=typeparameters?						// TypeParameters indicate Generic Class
					  (INHERIT BaseType=datatype)?
					  (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)?
					  (ConstraintsClauses+=typeparameterconstraintsclause)* EOS         // Optional typeparameterconstraints for Generic Class
					  (Members+=classmember)*
					  END CLASS  EOS
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

typeparameterconstraint: Key=(CLASS|STRUCTURE)				#classOrStructConstraint	//  Class Foo<t> WHERE T IS (CLASS|STRUCTURE)
					   | Type=typeName						#typeConstraint				//  Class Foo<t> WHERE T IS Customer		
                       | NEW LPAREN RPAREN					#constructorConstraint		//  Class Foo<t> WHERE T IS NEW()
					   ; 

// End of Extensions for Generic Classes

structure_			: (Attributes=attributes)? (Modifiers=structureModifiers)?
					  STRUCTURE (Namespace=nameDot)? Id=identifier TypeParameters=typeparameters?
					  (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)?
					  (ConstraintsClauses+=typeparameterconstraintsclause)* EOS
					  (Members+=classmember)+
					  END STRUCTURE EOS
					;

structureModifiers	: ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE | PARTIAL) )+
					;


delegate_			: (Attributes=attributes)? (Modifiers=delegateModifiers)?
					  DELEGATE (Namespace=nameDot)? Id=identifier TypeParameters=typeparameters?
					  ParamList=parameterList? AS Type=datatype
					  (ConstraintsClauses+=typeparameterconstraintsclause)* EOS
					;

delegateModifiers	: ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE) )+
					;


enum_				: (Attributes=attributes)? (Modifiers=enumModifiers)?
					  ENUM (Namespace=nameDot)? Id=identifier (AS Type=datatype)? EOS
					  (Members+=enummember)+
					  END (ENUM)? EOS
					;

enumModifiers		: ( Tokens+=(NEW | PUBLIC| EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN) )+
					;

enummember			: (Attributes=attributes)? MEMBER? Id=identifier (ASSIGN_OP Expr=expression)? EOS
					;

event_				:  (Attributes=attributes)? (Modifiers=eventModifiers)?
					   EVENT (ExplicitIface=nameDot)? Id=identifier AS Type=datatype EOS
					;

eventModifiers		: ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | STATIC | VIRTUAL | SEALED | ABSTRACT | UNSAFE) )+
					;



classvars			: (Attributes=attributes)? (Modifiers=classvarModifiers)? Vars=classVarList EOS
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
					  PROPERTY (SELF ParamList=propertyParameterList | (ExplicitIface=nameDot)? Id=identifier) (ParamList=propertyParameterList)?  AS Type=datatype 
					  ( Auto=AUTO (AutoAccessors+=propertyAutoAccessor)* (ASSIGN_OP Initializer=expression)? EOS	// Auto
					  | (LineAccessors+=propertyLineAccessor)+ EOS													// Single Line
					  | Multi=EOS (Accessors+=propertyAccessor)+  END PROPERTY? EOS									// Multi Line
					  )
					;

propertyParameterList
					: LBRKT  (Params+=parameter (COMMA Params+=parameter)*)? RBRKT
					| LPAREN (Params+=parameter (COMMA Params+=parameter)*)? RPAREN			// Allow Parentheses as well
					;

propertyAutoAccessor: Attributes=attributes? Modifiers=memberModifiers? Key=(GET|SET)
					;

propertyLineAccessor: Attributes=attributes? Modifiers=memberModifiers? 
					  ( {InputStream.La(2) != SET}? Key=GET Expr=expression?
					  | {InputStream.La(1) != GET}? Key=SET ExprList=expressionListStmt?
					  | Key=(GET|SET) )
					;

expressionListStmt	: Exprs+=expression (COMMA Exprs+=expression)*
					;

propertyAccessor    : Attributes=attributes? Modifiers=memberModifiers? 
					  ( Key=GET EOS StmtBlk=statementBlock END GET?
					  | Key=SET EOS StmtBlk=statementBlock END SET? )
					  EOS
					;

classmember			: Member=method										#clsmethod
					| (Attributes=attributes)?
					  (Modifiers=constructorModifiers)? 
					  CONSTRUCTOR (ParamList=parameterList)? EOS 
					  (Chain=(SELF | SUPER) 
						LPAREN ArgList=argumentList? RPAREN  EOS)?
					  StmtBlk=statementBlock							#clsctor
					| (Attributes=attributes)? 
					  (Modifiers=destructorModifiers)?
					  DESTRUCTOR (LPAREN RPAREN)?  EOS 
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


overloadedOps		: Token=(INC | DEC | PLUS | MINUS | MULT | DIV | MOD | AND | OR| LSHIFT| RSHIFT| EEQ
					| GT  | LT | NEQ | GTE| LTE | TRUE_CONST | FALSE_CONST
					| TILDE | AMP   | PIPE )
					;

conversionOps		: Token=( IMPLICIT | EXPLICIT )
					;

operator_			: Attributes=attributes? Modifiers=operatorModifiers? 
					  OPERATOR (Operation=overloadedOps | Conversion=conversionOps)
					  ParamList=parameterList AS Type=datatype EOS StmtBlk=statementBlock
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

attribute			: Name=name (LPAREN (Params+=attributeParam (COMMA Params+=attributeParam)* )? RPAREN )?
					;

attributeParam		: Name=identifierName ASSIGN_OP Expr=expression						#propertyAttributeParam
					| Expr=expression													#exprAttributeParam
					;

globalAttributes    : LBRKT Target=globalAttributeTarget Attributes+=attribute (COMMA Attributes+=attribute)* RBRKT EOS
					;

globalAttributeTarget : Token=(ASSEMBLY | MODULE) COLON
					;

statement           : Decl=localdecl                                            #declarationStmt
					| {_xBaseVars}? xbasedecl									#xbasedeclStmt
					| DO? WHILE Expr=expression EOS
					  StmtBlk=statementBlock (END DO? | ENDDO) EOS				#whileStmt
					| FOR 
						( AssignExpr=expression
						| (LOCAL? ForDecl=IMPLIED | ForDecl=VAR) ForIter=identifier ASSIGN_OP Expr=expression
						| ForDecl=LOCAL ForIter=identifier ASSIGN_OP Expr=expression AS Type=datatype
						)
					  Dir=(TO | UPTO | DOWNTO) FinalExpr=expression
					  (STEP Step=expression)? EOS
					  StmtBlk=statementBlock NEXT EOS							#forStmt
					| IF IfStmt=ifElseBlock
					  (END IF? | ENDIF)  EOS									#ifStmt	
					| DO CASE EOS
					  CaseStmt=caseBlock?
					  (END CASE? | ENDCASE) EOS									#caseStmt
					| EXIT EOS													#exitStmt
					| LOOP EOS													#loopStmt
					| BREAK Expr=expression? EOS								#breakStmt
					| RETURN (VOID | Expr=expression)? EOS						#returnStmt
					| Q=(QMARK | QQMARK)
					   (Exprs+=expression (COMMA Exprs+=expression)*)? EOS		#qoutStmt
					| BEGIN SEQUENCE EOS
					  StmtBlk=statementBlock
					  (RECOVER RecoverBlock=recoverBlock)?
					  (FINALLY EOS FinBlock=statementBlock)?
					  END (SEQUENCE)? EOS										#seqStmt
					//
					// New in Vulcan
					//
					| REPEAT EOS
					  StmtBlk=statementBlock
					  UNTIL Expr=expression EOS									#repeatStmt
					| FOREACH
					  (IMPLIED Id=identifier | Id=identifier AS Type=datatype| VAR Id=identifier)
					  IN Container=expression EOS
					  StmtBlk=statementBlock NEXT EOS							#foreachStmt
					| THROW Expr=expression? EOS								#throwStmt
					| TRY EOS StmtBlk=statementBlock
					  (CATCH CatchBlock+=catchBlock?)*
					  (FINALLY EOS FinBlock=statementBlock)?
					  END TRY? EOS												#tryStmt
					| BEGIN LOCK Expr=expression EOS
					  StmtBlk=statementBlock
					  END LOCK? EOS												#lockStmt
					| BEGIN SCOPE EOS
					  StmtBlk=statementBlock
					  END SCOPE? EOS											#scopeStmt
					//
					// New XSharp Statements
					//
					| YIELD RETURN (VOID | Expr=expression)? EOS				#yieldStmt
					| SWITCH Expr=expression EOS
					  (SwitchBlock+=switchBlock)+
					  END SWITCH?  EOS											#switchStmt
					| BEGIN USING Expr=expression EOS
						Stmtblk=statementBlock
					  END USING? EOS											#usingStmt
					| BEGIN UNSAFE EOS
					  StmtBlk=statementBlock
					  END UNSAFE? EOS											#unsafeStmt
					| BEGIN Ch=CHECKED EOS
					  StmtBlk=statementBlock
					  END CHECKED? EOS											#checkedStmt
					| BEGIN Ch=UNCHECKED EOS
					  StmtBlk=statementBlock
					  END UNCHECKED? EOS										#checkedStmt
					| {InputStream.La(2) != LPAREN ||
					   (InputStream.La(1) != CONSTRUCTOR && InputStream.La(1) != DESTRUCTOR) }?
					  Exprs+=expression (COMMA Exprs+=expression)* EOS			#expressionStmt
					;

ifElseBlock			: Cond=expression EOS StmtBlk=statementBlock
					  (ELSEIF ElseIfBlock=ifElseBlock | ELSE EOS ElseBlock=statementBlock)?
					;

caseBlock			: Key=CASE Cond=expression EOS StmtBlk=statementBlock NextCase=caseBlock?
					| Key=OTHERWISE EOS StmtBlk=statementBlock
					;

// Note that literalValue is not enough. We also need to support members of enums
switchBlock         : (Key=CASE Const=expression | Key=(OTHERWISE|DEFAULT)) EOS StmtBlk=statementBlock			 
					;

catchBlock			: (Id=identifier AS Type=datatype)? EOS StmtBlk=statementBlock
					;

recoverBlock		: (USING Id=identifier)? EOS StmtBlock=statementBlock
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
					 LocalVars+=localvar (COMMA LocalVars+=localvar)*						EOS   #commonLocalDecl	// STATIC LOCAL or LOCAL
				   | (Static=STATIC LOCAL? IMPLIED | LOCAL IMPLIED | Static=STATIC? VAR)							// LOCAL IMPLIED
				     ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*               EOS   #varLocalDecl		// VAR special for Robert !
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

expression			: Expr=expression Op=(DOT | COLON) Name=simpleName			#accessMember			// member access The ? is new
					| Expr=expression LPAREN ArgList=argumentList? RPAREN		#methodCall				// method call
					| Expr=expression LBRKT ArgList=bracketedArgumentList? RBRKT #arrayAccess			// Array element access
					| Left=expression Op=QMARK Right=boundExpression			#condAccessExpr			// expr ? expr
					| LPAREN Type=datatype RPAREN Expr=expression				#typeCast			    // (typename) expr
					| Expr=expression Op=(INC | DEC)							#postfixExpression		// expr ++/--
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
					| Op=(LOGIC_NOT|NOT) Expr=expression						#prefixExpression		// .not. expr (logical not)  also  !
					| Left=expression Op=(LOGIC_AND | AND) Right=expression		#binaryExpression		// expr .and. expr (logical and) also &&
					| Left=expression Op=LOGIC_XOR Right=expression				#binaryExpression		// expr .xor. expr (logical xor) 
					| Left=expression Op=(LOGIC_OR | OR) Right=expression		#binaryExpression		// expr .or. expr (logical or)  also || 
					| Left=expression Op=DEFAULT Right=expression				#binaryExpression		// expr DEFAULT expr 
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
                    | Query=linqQuery											#queryExpression        // LINQ
					| Type=datatype LCURLY Obj=expression COMMA
					  ADDROF Func=name LPAREN RPAREN RCURLY						#delegateCtorCall		// delegate{ obj , @func() }
					| Type=datatype LCURLY ArgList=argumentList? RCURLY			#ctorCall				// id{ [expr [, expr...] }
					| ch=CHECKED LPAREN ( Expr=expression ) RPAREN				#checkedExpression		// checked( expression )
					| ch=UNCHECKED LPAREN ( Expr=expression ) RPAREN			#checkedExpression		// unchecked( expression )
					| TYPEOF LPAREN Type=datatype RPAREN						#typeOfExpression		// typeof( typeORid )
					| SIZEOF LPAREN Type=datatype RPAREN						#sizeOfExpression		// sizeof( typeORid )
					| DEFAULT LPAREN Type=datatype RPAREN						#defaultExpression		// sizeof( typeORid )
					| Name=simpleName											#nameExpression			// generic name
					| Type=nativeType LPAREN Expr=expression RPAREN				#voConversionExpression	// nativetype( expr )
					| XType=xbaseType LPAREN Expr=expression RPAREN				#voConversionExpression	// xbaseType( expr )
					| Type=datatype LPAREN CAST COMMA Expr=expression RPAREN	#voCastExpression		// typename(_CAST, expr )
					| PTR LPAREN Type=datatype COMMA Expr=expression RPAREN		#voCastPtrExpression	// PTR( typeName, expr )
					| Type=nativeType											#typeExpression			// Standard DotNet Types
					| XType=xbaseType											#typeExpression			// ARRAY, CODEBLOCK, etc.
					| Expr=iif													#iifExpression			// iif( expr, expr, expr )
					| LPAREN ( Expr=expression ) RPAREN							#parenExpression		// ( expr )
					| Op=(VO_AND | VO_OR | VO_XOR | VO_NOT) LPAREN Exprs+=expression 
					  (COMMA Exprs+=expression)* RPAREN							#intrinsicExpression	// _Or(expr, expr, expr)
//					| aliasedField												#aliasfield				//  ALIAS->FIELD
//					| aliasedExpr												#aliasexpr				// ALIAS->(expr)
//					| aliasedFuncCall											#aliasfunccall			//  foo->bar()
//					| extendedaliasExpr											#aliasextended			// (expr) -> ...
//					| AMP LPAREN expression RPAREN								#macroexpr				// &( expr )
//					| AMP identifierName										#macrovar				// &id
					;

boundExpression		: Expr=boundExpression Op=(DOT | COLON) Name=simpleName		#boundAccessMember		// member access The ? is new
					| Expr=boundExpression LPAREN ArgList=argumentList? RPAREN	#boundMethodCall		// method call
					| Expr=boundExpression 
					  LBRKT ArgList=bracketedArgumentList? RBRKT				#boundArrayAccess		// Array element access
					| <assoc=right> Left=boundExpression
					  Op=QMARK Right=boundExpression							#boundCondAccessExpr	// expr ? expr
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

nameDot				: Left=nameDot Right=simpleName DOT								#qualifiedNameDot
					| Name=aliasedName DOT											#simpleOrAliasedNameDot
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

genericArgumentList : LT GenericArgs+=datatype (COMMA GenericArgs+=datatype)* GT
					;

identifierName		: Id=identifier
					;

datatype			: TypeName=typeName PTR											#ptrDatatype
					| TypeName=typeName (Ranks+=arrayRank)+							#arrayDatatype
					| TypeName=typeName 											#simpleDatatype
					| TypeName=typeName QMARK 										#nullableDatatype
					;

arrayRank			: LBRKT (Commas+=COMMA)* RBRKT
					;

typeName			: NativeType=nativeType
					| XType=xbaseType
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

// remove Vulcan types for now
//					

xbaseType			: Token=
					( ARRAY 
					| CODEBLOCK
					| DATE 
					| FLOAT 
					| PSZ 
					| SYMBOL 
					| USUAL)
					;

nativeType			: Token=
					( BYTE
					| DWORD
					| DYNAMIC
					| SHORTINT
					| INT
					| INT64
					| LOGIC
					| LONGINT
					| OBJECT
					| PTR
					| REAL4
					| REAL8
					| STRING
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
					| NULL_SYMBOL ) 
//					| MACRO )
					;


keyword             : (KwVo=keywordvo | KwVn=keywordvn | KwXs=keywordxs) ;

keywordvo           : Token=(ACCESS | ALIGN | AS | ASSIGN | BEGIN | BREAK | CASE | CAST | CLASS | CLIPPER | DEFINE | DIM | DLL | DO | DOWNTO
					| ELSE | ELSEIF | END | ENDCASE | ENDDO | ENDIF | EXIT | EXPORT | FASTCALL | FIELD | FOR | FUNCTION | GLOBAL
					| HIDDEN | IF | IIF | INHERIT | INSTANCE |  IS | LOCAL | LOOP | MEMBER | METHOD | NEXT | OTHERWISE 
					| PASCAL | PRIVATE | PROCEDURE | PROTECTED | PTR | PUBLIC | RECOVER | RETURN | SELF| SEQUENCE | SIZEOF | STEP | STRICT | SUPER
					| THISCALL | TO | TYPEOF | UNION | UPTO | USING | WHILE | CATCH | FINALLY | TRY |VO_AND| VO_NOT| VO_OR| VO_XOR)
					;

keywordvn           : Token=(ABSTRACT | AUTO | CONSTRUCTOR | CONST | DEFAULT | DELEGATE | DESTRUCTOR | ENUM | EVENT
					| EXPLICIT | FOREACH | GET | IMPLEMENTS | IMPLICIT | IMPLIED | IN | INITONLY | INTERFACE | INTERNAL 
					| LOCK | NAMESPACE | NEW | OPERATOR	| OPTIONS | OUT | PARTIAL | PROPERTY | REPEAT | SCOPE | SEALED | SET | STRUCTURE			
					|  TRY | UNTIL | VALUE | VIRTUAL | VOSTRUCT | WARNINGS)
					;

keywordxs           : Token=( ASCENDING | ASSEMBLY | ASYNC | AWAIT | BY | CHECKED | DESCENDING | DYNAMIC | EQUALS | EXTERN | FROM | 
                              GROUP | INTO | JOIN | LET | MODULE | ORDERBY | SELECT | SWITCH | UNCHECKED | UNSAFE | VAR | VOLATILE | WHERE | YIELD | CHAR |
							  MEMVAR | PARAMETERS // Added as XS keywords to allow them to be treated as IDs
							)
					;
