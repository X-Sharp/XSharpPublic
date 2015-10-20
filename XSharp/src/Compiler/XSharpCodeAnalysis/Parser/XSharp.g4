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


source				: (eos)? (entity)* eof
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
					| globalattributes          // Assembly attributes, Module attributes etc.
                    | using_                    // Using Namespace
                    | pragma                    // Compiler pragma
					| {_VOSyntax}? voglobal     // This will become part of the 'Globals' class
					| {_VOSyntax}? vodefine     // This will become part of the 'Globals' class
					| {_VOSyntax}? vostruct     // Compatibility (unsafe) structure
					| {_VOSyntax}? vounion      // Compatibility (unsafe) structure with members aligned at FieldOffSet 0
					| {_VOSyntax}? vodllproc    // External method of the Globals class
					| {_VOSyntax}? vodllfunc    // External method of the Globals class
					;

function            : (Attributes=attributes)? (Modifiers+=funcprocmodifier)* 
                       FUNCTION Id=identifier (ParamList=parameterList )?
					   (AS Type=datatype)? 
					   (CallingConvention=callingconvention)? eos 
					   StmtBlk=statementBlock
					;

procedure           : (Attributes=attributes)? (Modifiers+=funcprocmodifier)* 
                      PROCEDURE Id=identifier (ParamList=parameterList)?
					   (CallingConvention=callingconvention)? Init=(INIT1|INIT2|INIT3)? eos 
					   StmtBlk=statementBlock
					;

callingconvention	: Convention=(CLIPPER | STRICT | PASCAL) 
					;


vodllfunc			: (Modifiers+=funcprocmodifier)* DLL FUNCTION Id=identifier ParamList=parameterList 
					   (AS Type=datatype)? (CallingConvention=dllcallconv)? COLON 
					   ( Dll=identifier DOT Entrypoint=identifier //(NEQ Ordinal=INT_CONST)? 
					   | Dll=identifier DOT EntrypointString=STRING_CONST //(NEQ Ordinal=INT_CONST)? 
						) 
					;

vodllproc			:  (Modifiers+=funcprocmodifier)* DLL PROCEDURE Id=identifier ParamList=parameterList 
					   (CallingConvention=dllcallconv)? COLON 
					   ( EntryPoint=name   (NEQ Ordinal=INT_CONST)? 
						| Dll=identifier DOT EntrypointString=STRING_CONST (NEQ Ordinal=INT_CONST)? 
						) 

					;

dllcallconv         : Cc=( CLIPPER | STRICT | PASCAL | THISCALL | FASTCALL)
					;


parameterList		: LPAREN (Params+=parameter (COMMA Params+=parameter)*)? RPAREN
					;

// Compared with C# PARAMS is not supported. This can be achived by setting [ParamArrayAttribute] on the parameter: [ParamArrayAttribute] args as OBJECT[] 
parameter			: (Attributes=attributes)? Id=identifier (ASSIGN_OP Default=expression)? Modifiers+=(AS | REF | OUT| IS ) Modifiers+=CONST? Type=datatype
					;

statementBlock      : (Stmts+=statement)*
					;


funcprocmodifier	: Token = (STATIC | INTERNAL | PUBLIC | EXPORT | UNSAFE)
					;


using_              : HASHUSING (Alias=identifier ASSIGN_OP)? Namespace=name     
                    ;

pragma              : PRAGMA OPTIONS    LPAREN Compileroption=STRING_CONST Switch=pragmaswitch RPAREN eos         #pragmaOptions
                    | PRAGMA WARNINGS   LPAREN WarningNumber=INT_CONST     Switch=pragmaswitch RPAREN eos         #pragmaWarnings
                    ;

pragmaswitch        : ON | OFF | DEFAULT
                    ;

voglobal			: (Modifiers+=funcprocmodifier)*  GLOBAL Vars += globalvar (COMMA Vars += globalvar)* (VarType = vartype)? eos
					;

globalvar			: (DIM)? Id=identifier (LBRKT ArraySub=arraysub RBRKT)? (ASSIGN_OP Initializer=expression)?
					;


// Separate method/access/assign with Class name -> convert to partial class with just one method
// And when Class is outside of assembly, convert to Extension Method?
method				: (Attributes=attributes)? (Modifiers+= membermodifier)*
					  T=methodtype Id=identifier (ParamList=parameterList)? (AS Type=datatype)? 
					  (CallingConvention=callingconvention)? (CLASS ClassId=name)? eos 
					  StmtBlk=statementBlock		
					;

methodtype			: METHOD | ACCESS | ASSIGN
					;

// Convert to constant on Globals class. Expression must be resolvable at compile time
vodefine			: DEFINE Id=identifier ASSIGN_OP Expr=expression
					;

vostruct			: VOSTRUCT Id=identifier (ALIGN Alignment=INT_CONST)? eos
					  (Members += vostructmember)+
					;

vounion				: UNION  Id=identifier  eos
					  (Members += vostructmember)+
					;


vostructmember		: MEMBER DIM Id=identifier LBRKT ArraySub=arraysub RBRKT  MemberType = vartype eos
					| MEMBER Id=identifier MemberType = vartype eos
					;

namespace_			: BEGIN NAMESPACE Id=name eos
					  (entity)*
					  END NAMESPACE eos
					;

interface_			: (Attributes=attributes)? (Modifiers+= interfacemodifier)*
					  INTERFACE Id=name
					  Parents=interfaceparents
					  (Members+=classmember)+
					  END INTERFACE eos
					;

interfacemodifier	: Token=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE | PARTIAL)
					;

interfaceparents	: INHERIT Interfaces+= datatype (COMMA Interfaces+= datatype)* eos
					| COLON   Interfaces+= datatype (COMMA Interfaces+= datatype)* eos
					| eos
					;

class_				: (Attributes=attributes)? (Modifiers += classmodifier)*
					  CLASS Id=name TypeParameters=typeparameters?                        // TypeParameters indicate Generic Class
					  Parents=classparents? Interfaces=interfacetypelist? 
					  (ConstraintsClauses +=typeparameterconstraintsclause)* eos                 // Optional typeparameterconstraints for Generic Class
					  (Members +=classmember)*
					  END CLASS  eos
					;

classmodifier		: Token=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | ABSTRACT | SEALED | STATIC | UNSAFE | PARTIAL)
					;

classparents        : INHERIT ParentClass=datatype
					;

interfacetypelist	: (IMPLEMENTS (Interfaces+= datatype) (COMMA Interfaces+= datatype)*)
					;

// Start Extensions for Generic Classes
typeparameters      : LT TypeParams+=typeparameter (COMMA attributes? TypeParams+=typeparameter)* GT 
					;

typeparameter       : Attributes=attributes?  Id=identifier
					;

typeparameterconstraintsclause
					: WHERE identifier IS typeparameterconstraints
					;
							  
typeparameterconstraints
					  : Constraints+=typeparameterconstraint (COMMA Constraints+=typeparameterconstraint)*
					  ;

typeparameterconstraint:  typeName                          //  Class Foo<t> WHERE T IS Customer
					   | typeparameter                      //  Class Foo<T,U> WHERE U IS T
					   | typeparameterconstrainttypes       //  Class Foo<t> WHERE T IS CLASS
                                                            //  Class Foo<t> WHERE T IS STRUCTURE
                                                            //  Class Foo<t> WHERE T IS NEW()
					   ; 
typeparameterconstrainttypes: CLASS
                            | STRUCTURE
                            | NEW LPAREN RPAREN
                            ;

// End of Extensions for Generic Classes

structure_			: (Attributes=attributes)? (Modifiers += structuremodifier)*
					  STRUCTURE Id=name
					  Interfaces=interfacetypelist? eos
					  (Members +=classmember)+
					  END STRUCTURE eos
					;

structuremodifier	: Token=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE | PARTIAL)
					;


delegate_			: (Attributes=attributes)? (Modifiers += delegatemodifier)*
					  DELEGATE Id=identifier
					  ParamList=parameterList AS Type=datatype eos
					;

delegatemodifier	: Token= (NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE)
					;


enum_				: (Attributes=attributes)? (Modifiers += enummodifier)*
					  ENUM Id=identifier (AS Type=datatype)? eos
					  (Members += enummember)+
					  END (ENUM)? eos
					;

enummodifier		: Token= (NEW | PUBLIC| EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN)
					;

enummember			: MEMBER? Id=identifier (ASSIGN_OP expression)? eos
					;

event_				:  (Attributes=attributes)? (Modifiers += eventmodifier)*
					   EVENT Id=identifier AS Type=datatype eos
					;

eventmodifier		: Token=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | STATIC | VIRTUAL | SEALED | ABSTRACT | UNSAFE)
					;



classvars			: (Attributes=attributes)? (Modifiers += classvarmodifiers)*
					  Var += classvar (COMMA Var += classvar) VarType = vartype
					;

classvarmodifiers	: Token=(INSTANCE| STATIC | CONST | INITONLY | PRIVATE | HIDDEN | PROTECTED | PUBLIC | EXPORT | INTERNAL | VOLATILE | UNSAFE)
					;


classvar			: (DIM)? Id=identifier (LBRKT ArraySub=arraysub RBRKT)? (ASSIGN_OP Initializer=expression)?
					;

arraysub			: ArrayIndex+=expression (RBRKT LBRKT ArrayIndex+=expression)+		// x][y
					| ArrayIndex+=expression (COMMA ArrayIndex+=expression)+			// x,y
					| ArrayIndex+=expression
					;

vartype				: (AS | IS) DataType = datatype
					;


property			: (Attributes=attributes)? (Modifiers+= membermodifier)* 
					  PROPERTY Id=identifier (ParamList=parameterList)? AS Type=datatype 
					  ( Auto=propertyauto													// Auto
					  | SingleLine=propertysingleline                                       // Single Line
					  | eos (Get=propertyget) (Set=propertyset)?  END PROPERTY? eos         // Multi Line GET SET?
					  | eos (Set=propertyset) (Get=propertyget)?  END PROPERTY? eos         // Multi Line SET GET?
					  )
					;

propertyauto        : AUTO ((GetModifiers+= membermodifier)* GET) ((SetModifiers+= membermodifier)* SET)? (ASSIGN_OP Initializer = expression)? eos  // AUTO GET SET? Initializer? eos
					| AUTO ((SetModifiers+= membermodifier)* SET) ((GetModifiers+= membermodifier)* GET)? (ASSIGN_OP Initializer = expression)? eos  // AUTO SET GET? Initializer? eos
					| AUTO (ASSIGN_OP Initializer = expression)? eos																				 // AUTO Initializer? eos
					;

propertysingleline  : (GetModifiers+= membermodifier)* GET GetExpression=expression     ((SetModifiers+= membermodifier)* SET SetExpression=expressionList)? eos  // GET SET? eos
					| (SetModifiers+= membermodifier)* SET SetExpression=expressionList ((GetModifiers+= membermodifier)* GET GetExpression=expression)?     eos  // SET GET? eos
					;

propertyget         : (GetModifiers+= membermodifier)* GET eos GetStmtBlk=statementBlock END GET? eos // GET Stmts END GET?
					;

propertyset         : (SetModifiers+= membermodifier)* SET eos SetStmtBlk=statementBlock END SET? eos	// SET Stmts END SET?
					;

classmember			: method															#clsmethod
					| (Attributes=attributes)? (Modifiers+= constructormodifier)* 
					  CONSTRUCTOR (ParamList=parameterList)? eos StmtBlk=statementBlock	#clsctor
					| (Attributes=attributes)? (Modifiers+= EXTERN )*       
					  DESTRUCTOR (LPAREN RPAREN)?  eos StmtBlk=statementBlock           #clsdtor
					| classvars									#clsvars
					| property									#clsproperty
					| operator_									#clsoperator
					| structure_								#nestedStructure
					| class_									#nestedClass
					| delegate_									#nestedDelegate
					| enum_										#nestedEnum
					| event_									#nestedEvent
					| interface_								#nestedInterface
                    | using_                                    #nestedUsing
                    | pragma                                    #nestedPragma
					| {_ClsFunc}? function						#clsfunction		// Equivalent to method
					| {_ClsFunc}? procedure						#clsprocedure		// Equivalent to method
					;


constructormodifier : Token=( PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | EXTERN | STATIC )
					;



overloadedops		: Token=(INC | DEC | NOT | PLUS | MINUS | MULT | DIV | MOD | AND | OR| LSHIFT| RSHIFT| EEQ
					| GT  | LT | NEQ | GTE| LTE | IMPLICIT | EXPLICIT | TRUE_CONST | FALSE_CONST
					| TILDE | AMP   | PIPE )
					;


operator_			: OPERATOR Operation=overloadedops
					  ParamList=parameterList AS Type=datatype eos StmtBlk=statementBlock
					;


membermodifier		: Token = (NEW | PRIVATE | HIDDEN | PROTECTED | PUBLIC | EXPORT | INTERNAL | STATIC | VIRTUAL | SEALED | ABSTRACT | ASYNC | UNSAFE | EXTERN)
					;


attributes			: ( attributeblock )+
					;

attributeblock		: LBRKT attributetarget? Attributes+=attribute (COMMA Attributes+=attribute) RBRKT
					;

attributetarget		: Id=identifier COLON
					| Kw=keyword COLON
					;

attribute			: Id=identifier (LPAREN (Params +=attributeParam (COMMA Params+=attributeParam)* )? RPAREN ) ?
					;

attributeParam		: Expr=expression
					;


globalattributes    : LBRKT globallattributetarget Attributes+=attribute (COMMA Attributes+=attribute) RBRKT
					;

globallattributetarget : ASSEMBLY COLON
					   | MODULE COLON
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

statement           : localdecl                                                 #declarationStmt
					| {_xBaseVars}? xbasedecl									#xbasedeclStmt
					| DO WHILE Expr=expression eos
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
					  (IMPLIED Id=identifier | Id=identifier AS Type=datatype| VAR Id=identifier)
					  IN Container=expression eos
					  StmtBlk=statementBlock NEXT eos							#foreachStmt
					| IF CondBlock+=conditionalBlock
					  (ELSEIF CondBlock+=conditionalBlock)*
					  (ELSE eos ElseBlock+=statementBlock)?
					  (END IF? | ENDIF) eos										#condStmt
					| DO CASE eos
					  (CASE CondBlock+=conditionalBlock)+
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
					| BEGIN LOCK Expr=expression eos
					  StmtBlk=statementBlock
					  END LOCK? eos												#lockStmt
					| BEGIN SCOPE eos
					  StmtBlk=statementBlock
					  END SCOPE? eos											#scopeStmt
					| RETURN (VOID | Expr=expression)? eos						#returnStmt
					// New XSharp Statements
					| YIELD RETURN (VOID | Expr=expression)? eos				#yieldStmt
					| SWITCH Expr=expression eos
					  (CASE SwitchBlock+=switchBlock)+
					  ((OTHERWISE|DEFAULT) eos ElseBlock+=statementBlock)?
					  END SWITCH?  eos											#switchStmt
					| BEGIN USING Expr=expression eos
						Stmtblk=statementBlock
					  END USING? eos											#usingStmt
					| Q1=QMARK (Q2=QMARK)? Exprs+=expression (COMMA Exprs+=expression)* eos	#qoutStmt
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


conditionalBlock	: Cond=expression eos StmtBlk=statementBlock
					;

// Note that literalValue is not enough. We also need to support members of enums
switchBlock         : Const=expression eos StmtBlk=statementBlock			 
					;

catchBlock			: Id=identifier AS Type=datatype eos StmtBlk=statementBlock
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

localdecl          : LOCAL                 LocalVars +=localvar (COMMA LocalVars+=localvar)*              // LOCAL
				   | Static=STATIC LOCAL?  LocalVars +=localvar (COMMA LocalVars+=localvar)*              // STATIC LOCAL or LOCAL
				   | (LOCAL)? IMPLIED      ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*       // LOCAL IMPLIED or simply IMPLIED
				   | VAR                   ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*       // VAR special for Robert !
				   ;

localvar           : (Const=CONST)? ( Dim=DIM )? Id=identifier (LBRKT Arraysub=arraysub RBRKT)? (ASSIGN_OP Initializer=expression)? ( AsIs=(AS | IS) Datatype=datatype)?
				   ;
					  
impliedvar         : (Const=CONST)? Id=identifier ASSIGN_OP Expression=expression 
				   ;


// Old Style xBase declarations

xbasedecl        : T=PRIVATE      Vars+=identifier (COMMA Vars+=identifier)* eos       // PRIVATE Foo, Bar
				 | T=PUBLIC       Vars+=identifier (COMMA Vars+=identifier)* eos       // PUBLIC Foo, Bar
				 | T=MEMVAR       Vars+=identifier (COMMA Vars+=identifier)* eos       // MEMVAR Foo, Bar
				 | T=PARAMETERS   Vars+=identifier (COMMA Vars+=identifier)* eos       // PARAMETERS Foo, Bar
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

expression			: Left=expression Q=QMARK Op=(DOT | COLON) Right=identifierName		#accessMember           // member access
					| Expr=expression Op=(INC | DEC)							#postfixExpression		// expr ++/--
					| Op=AWAIT Expr=expression									#awaitExpression		// AWAIT expr
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
					| Left=expression Op=DEFAULT Right=expression				#binaryExpression		// expr DEFAULT expr 
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
					| ch=CHECKED LPAREN Expr=expression RPAREN					#checkedExpression		// checked( expression )
					| ch=UNCHECKED LPAREN Expr=expression RPAREN				#checkedExpression		// unchecked( expression )
					| Expr=expression  IS Type=datatype							#typeCheckExpression	// expr IS typeORid
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
					| IF LPAREN Cond=expression COMMA TrueExpr=expression COMMA FalseExpr=expression RPAREN
					;

name				: Left=identifier Op=DOT Right=name							#qualifiedName
					| Id=identifier	GenericArgList=genericArgumentList			#genericName
					| Id=identifier												#simpleName
					;

genericArgumentList : LT GenericArgs+=datatype (COMMA GenericArgs+=datatype)* GT
					;

identifierName		: Id=identifier
					;

datatype			: TypeName=typeName 											#simpleDatatype
					| TypeName=typeName (Ranks+=arrayRank)*							#arrayDatatype
					| TypeName=typeName PTR											#ptrDatatype
					;

arrayRank			: LBRKT (COMMA)* RBRKT
					;

typeName			: NativeType=nativeType
					| Name=name
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

// All New Vulcan and X# keywords can also be recognized as Identifier
identifier			: Token=ID  
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

eos                 : (NL)* (NL|EOF)
					;

eof                 : EOF
					;


keyword             : (keywordvo | keywordvn | keywordxs) ;

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

keywordxs           : Token=(ASSEMBLY | ASYNC | AWAIT | CHECKED | DYNAMIC | EXTERN | MODULE | SWITCH | UNCHECKED | UNSAFE | VAR | VOLATILE | WHERE | YIELD)
					;
