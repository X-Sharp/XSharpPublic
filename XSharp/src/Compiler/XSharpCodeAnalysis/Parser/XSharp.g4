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
grammar XSharp;

/*
 * Parser Rules
*/

// Known issues
// Local a as long
//  a := 10
//  ? (a) + 1     <-- parser sees this as typecast and not as paren expression

options {
        tokenVocab=XSharpLexer;
        contextSuperClass=XSharpParserRuleContext;
        }

script              : ( SCRIPT_LOAD Includes+=STRING_CONST EOS | SCRIPT_REF References+=STRING_CONST EOS )*
                    ( Entities+=scriptEntity )*
                    EOF
                    ;

scriptEntity        : Stmt=statement
                    | Entity=entity
                    | Expr=expression
                    ;

macroScript         : ( CbExpr=codeblock | Code=codeblockCode ) EOS
                      EOF
                    ;

source              : eos? (Entities+=entity )*
                      EOF
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
                    | constructor               // Constructor Class xxx syntax
                    | destructor                // Destructor Class xxx syntax
                    | globalAttributes          // Assembly attributes, Module attributes etc.
                    | using_                    // Using Namespace
                    | vodefine                  // This will become part of the 'Globals' class
                    | voglobal                  // This will become part of the 'Globals' class
                    | vodll                     // External method of the Globals class
                    | vostruct                  // Compatibility (unsafe) structure
                    | vounion                   // Compatibility (unsafe) structure with members aligned at FieldOffSet 0
                    ;


eos                 : EOS+ 
                    ;

function            : (Attributes=attributes)? (Modifiers=funcprocModifiers)?   
                      (FUNCTION|FUNC) Id=identifier                             
                      TypeParameters=typeparameters?                            
                      (ParamList=parameterList)?                                
                      (AS Type=datatype)?                                       
                      (ConstraintsClauses+=typeparameterconstraintsclause)*     
                      (CallingConvention=callingconvention)?                    
                      (EXPORT LOCAL)?                                           // Optional (ignored)
                      (DLLEXPORT STRING_CONST)?                                 // Optional (ignored)
                      end=eos   
                     StmtBlk=statementBlock
                    ;

procedure           : (Attributes=attributes)? (Modifiers=funcprocModifiers)?   
                      (PROCEDURE|PROC) Id=identifier                            
                      TypeParameters=typeparameters?                            
                      (ParamList=parameterList)?                                
                      (AS VOID)?                                                // As Void is allowed but ignored
                      (ConstraintsClauses+=typeparameterconstraintsclause)*     
                      (CallingConvention=callingconvention)?                    
                      InitExit=(INIT1|INIT2|INIT3|EXIT)?                        
                      (EXPORT LOCAL)?                                           // Optional (ignored)
                      (DLLEXPORT STRING_CONST)?                                 // Optional (ignored)
                      end=eos                       
                      StmtBlk=statementBlock
                    ;

callingconvention	: Convention=(CLIPPER | STRICT | PASCAL | ASPEN | WINCALL | CALLBACK | FASTCALL | THISCALL)
                    ;


                    // there are many variations
                    // Simple:
                    // _DLL FUNCTION SetDebugErrorLevel( dwLevel AS DWORD) AS VOID PASCAL:USER32.SetDebugErrorLevel
                    // With Extension
                    // _DLL FUNC HTMLHelp(hwndCaller AS PTR, pszFile AS PSZ, uCommand AS LONG, dwData AS LONG) AS LONG PASCAL:HHCTRL.OCX.HtmlHelpA
                    // With Hint (which is ignored by Vulcan too)
                    // _DLL FUNC InternetHangUp(dwConnection AS DWORD, dwReserved AS DWORD) AS DWORD PASCAL:WININET.InternetHangUp#247
                    // And with numeric entrypoint, which is supported by VO but not by .NET
                    // We parse the numeric entrypoint here but we will throw an error during the tree transformation
                    // _DLL FUNCTION SetDebugErrorLevel( dwLevel AS DWORD) AS VOID PASCAL:USER32.123
                    // and Finally we also parse the @Num
                    // 

vodll               : (Attributes=attributes)? (Modifiers=funcprocModifiers)? // Optional
                      DLL
                      (
                        T=(FUNCTION|FUNC)   Id=identifier ParamList=parameterList (AS Type=datatype)? |
                        T=(PROCEDURE|PROC)  Id=identifier ParamList=parameterList
                      )
                      (CallingConvention=dllcallconv)? COLON
                      Dll=identifierString (DOT Extension=identifierString)?
                      (	Ordinal=REAL_CONST 
                       |  DOT Entrypoint=identifierString Address=ADDROF? Number=INT_CONST? (NEQ2 INT_CONST)? 
                      )
                       
                      ( CharSet=(AUTO | ANSI | UNICODE) )?
                      eos
                    ;

dllcallconv         : Cc=( CLIPPER | STRICT | PASCAL | THISCALL | FASTCALL | ASPEN | WINCALL | CALLBACK)
                    ;


parameterList       : LPAREN (Params+=parameter (COMMA Params+=parameter)*)? RPAREN
                    ;

parameter           : (Attributes=attributes)? Self=SELF? Id=identifier (ASSIGN_OP Default=expression)? (Modifiers=parameterDeclMods Type=datatype)?
                    | Ellipsis=ELLIPSIS
                    ;

parameterDeclMods   : Tokens+=(AS | REF | OUT | IS | PARAMS) /*Tokens+=*/CONST?
                    ;

statementBlock      : (Stmts+=statement)*
                    ;


funcprocModifiers   : ( Tokens+=(STATIC | INTERNAL | PUBLIC | EXPORT | UNSAFE) )+
                    ;


using_              : USING (Static=STATIC)? (Alias=identifierName ASSIGN_OP)? Name=name eos
                    ;


                    // [STATIC] GLOBAL [CONST] Identifier [:= Expression] [, Identifier2 [:= Expression2] ] [AS Type]
                    // STATIC          [CONST] Identifier [:= Expression] [, Identifier2 [:= Expression2] ] [AS Type]
voglobal            : (Attributes=attributes)? (Modifiers=funcprocModifiers)? GLOBAL (Const=CONST)? Vars=classVarList end=eos 
                    | (Attributes=attributes)? STATIC (Const=CONST)? Vars=classVarList end=eos 
                    ;


// Separate method/access/assign with Class name -> convert to partial class with just one method
// And when Class is outside of assembly, convert to Extension Method?
// nvk: we have no knowledge of whether a class is outside of the assembly at the parser stage!
method              : (Attributes=attributes)? (Modifiers=memberModifiers)?
                      T=methodtype (ExplicitIface=nameDot)? Id=identifier
                      TypeParameters=typeparameters?                        // Optional Type parameters for generic method
                      (ParamList=parameterList)?                            // Optional Parameters
                      (AS Type=datatype)?                                   // Optional Return Type
                      (ConstraintsClauses+=typeparameterconstraintsclause)* // Optional Type constraints for generic method
                      (CallingConvention=callingconvention)?                // Optional Calling convention
                      (CLASS (Namespace=nameDot)? ClassId=identifier)?      // CLass Clause will be allowed but should match class inside which we are defined
                      (EXPORT LOCAL)?                                       // Export Local exists in VO but is ignored in X#
                      (DLLEXPORT STRING_CONST)?                             // The DLLEXPORT clause exists in VO but is ignored in X#
                      end=eos
                      StmtBlk=statementBlock
                    ;

methodtype          : Token=(METHOD | ACCESS | ASSIGN)
                    ;

// Convert to constant on Globals class. Expression must be resolvable at compile time
vodefine            : (Modifiers=funcprocModifiers)?
                      DEFINE Id=identifier ASSIGN_OP Expr=expression (AS DataType=typeName)? eos
                    ;

vostruct            : (Modifiers=votypeModifiers)?
                      VOSTRUCT (Namespace=nameDot)? Id=identifier (ALIGN Alignment=INT_CONST)? e=eos
                      (Members+=vostructmember)+
                    ;

vostructmember      : MEMBER Dim=DIM Id=identifier LBRKT ArraySub=arraysub RBRKT (As=(AS | IS) DataType=datatype)? eos
                    | MEMBER Id=identifier (As=(AS | IS) DataType=datatype)? eos
                    ;


vounion             : (Modifiers=votypeModifiers)?
                      UNION (Namespace=nameDot)? Id=identifier e=eos
                      (Members+=vostructmember)+
                    ;

votypeModifiers     : ( Tokens+=(INTERNAL | PUBLIC | EXPORT | UNSAFE | STATIC ) )+
                    ;


namespace_          : BEGIN NAMESPACE Name=name e=eos
                      (Entities+=entity)*
                      END NAMESPACE Ignored=name?  eos
                    ;

interface_          : (Attributes=attributes)? (Modifiers=interfaceModifiers)?            
                      INTERFACE (Namespace=nameDot)? Id=identifier                        
                      TypeParameters=typeparameters?                                      // TypeParameters indicate Generic Interface
                      ((INHERIT|COLON) Parents+=datatype)? (COMMA Parents+=datatype)*
                      (ConstraintsClauses+=typeparameterconstraintsclause)*              // Optional typeparameterconstraints for Generic Interface
                      e=eos
                      (Members+=classmember)*
                      END INTERFACE Ignored=identifier?   eos
                    ;

interfaceModifiers  : ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE | PARTIAL) )+
                    ;

class_              : (Attributes=attributes)? (Modifiers=classModifiers)?              
                      CLASS (Namespace=nameDot)? Id=identifier                          
                      TypeParameters=typeparameters?                                    // TypeParameters indicate Generic Class
                      (INHERIT BaseType=datatype)?                                  
                      (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)?
                      (ConstraintsClauses+=typeparameterconstraintsclause)*             // Optional typeparameterconstraints for Generic Class
                      e=eos
                      (Members+=classmember)*
                      END CLASS Ignored=identifier?
                      eos
                    ;

classModifiers      : ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | ABSTRACT | SEALED | STATIC | UNSAFE | PARTIAL) )+
                    ;





// Start Extensions for Generic Classes
typeparameters      : LT TypeParams+=typeparameter (COMMA attributes? TypeParams+=typeparameter)* GT
                    ;

typeparameter       : Attributes=attributes? VarianceKeyword=(IN | OUT)? Id=identifier
                    ;

typeparameterconstraintsclause
                    : WHERE Name=identifierName IS Constraints+=typeparameterconstraint (COMMA Constraints+=typeparameterconstraint)*
                    ;

typeparameterconstraint: Key=(CLASS|STRUCTURE)                    #classOrStructConstraint	//  Class Foo<t> WHERE T IS (CLASS|STRUCTURE)
                       | Type=typeName                            #typeConstraint				//  Class Foo<t> WHERE T IS Customer
                       | NEW LPAREN RPAREN                        #constructorConstraint		//  Class Foo<t> WHERE T IS NEW()
                       ;

// End of Extensions for Generic Classes

structure_          : (Attributes=attributes)? (Modifiers=structureModifiers)?
                      STRUCTURE (Namespace=nameDot)? Id=identifier
                      TypeParameters=typeparameters?
                      (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)?
                      (ConstraintsClauses+=typeparameterconstraintsclause)* e=eos
                      (Members+=classmember)*
                      END STRUCTURE Ignored=identifier?
                      eos
                    ;

structureModifiers  : ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE | PARTIAL) )+
                    ;


delegate_           : (Attributes=attributes)? (Modifiers=delegateModifiers)?
                      DELEGATE (Namespace=nameDot)? Id=identifier
                      TypeParameters=typeparameters?
                      ParamList=parameterList?
                      (AS Type=datatype)?
                      (ConstraintsClauses+=typeparameterconstraintsclause)*
                      e=eos
                    ;

delegateModifiers   : ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE) )+
                    ;


enum_               : (Attributes=attributes)? (Modifiers=enumModifiers)?
                      ENUM (Namespace=nameDot)? Id=identifier ((AS|INHERIT) Type=datatype)? e=eos
                      (Members+=enummember)+
                      END ENUM? Ignored=identifier?
                      eos
                    ;

enumModifiers       : ( Tokens+=(NEW | PUBLIC| EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN) )+
                    ;

enummember          : (Attributes=attributes)? MEMBER? Id=identifier (ASSIGN_OP Expr=expression)? eos
                    ;

event_              : (Attributes=attributes)? (Modifiers=eventModifiers)?
                       EVENT (ExplicitIface=nameDot)? Id=identifier (AS Type=datatype)?
                       ( end=eos
                        | (LineAccessors += eventLineAccessor)+ end=eos
                        | Multi=eos (Accessors+=eventAccessor)+ END EVENT? Ignored=identifier? eos
                       )
                    ;

eventModifiers          : ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | STATIC | VIRTUAL | SEALED | ABSTRACT | UNSAFE) )+
                        ;


eventLineAccessor   : Attributes=attributes? Modifiers=accessorModifiers?
                      ( {InputStream.La(2) != REMOVE}? Key=ADD ExprList=expressionList?
                      | {InputStream.La(2) != ADD}?    Key=REMOVE ExprList=expressionList?
                      | Key=(ADD|REMOVE) )
                    ;
eventAccessor       : Attributes=attributes? Modifiers=accessorModifiers?
                      ( Key=ADD     end=eos StmtBlk=statementBlock END ADD?
                      | Key=REMOVE  end=eos StmtBlk=statementBlock END REMOVE? )
                      end=eos
                    ;



classvars           : (Attributes=attributes)? (Modifiers=classvarModifiers)?
                      Vars=classVarList
                      eos
                    ;

classvarModifiers   : ( Tokens+=(INSTANCE| STATIC | CONST | INITONLY | PRIVATE | HIDDEN | PROTECTED | PUBLIC | EXPORT | INTERNAL | VOLATILE | UNSAFE | FIXED) )+
                    ;

classVarList        : Var+=classvar (COMMA Var+=classvar)* (As=(AS | IS) DataType=datatype)?
                    ;

classvar            : (Dim=DIM)? Id=identifier (LBRKT ArraySub=arraysub RBRKT)? (ASSIGN_OP Initializer=expression)?
                    ;

arraysub            : ArrayIndex+=expression (RBRKT LBRKT ArrayIndex+=expression)+		// x][y
                    | ArrayIndex+=expression (COMMA ArrayIndex+=expression)+			// x,y
                    | ArrayIndex+=expression
                    ;

property            : (Attributes=attributes)? (Modifiers=memberModifiers)?
                      PROPERTY (SELF ParamList=propertyParameterList | (ExplicitIface=nameDot)? Id=identifier)
                      (ParamList=propertyParameterList)?
                      (AS Type=datatype)?
                      ( Auto=AUTO (AutoAccessors+=propertyAutoAccessor)* (ASSIGN_OP Initializer=expression)? end=eos	// Auto
                        | (LineAccessors+=propertyLineAccessor)+ end=eos													// Single Line
                        | Multi=eos (Accessors+=propertyAccessor)+  END PROPERTY? Ignored=identifier?  eos				// Multi Line
                      )
                    ;

propertyParameterList
                    : LBRKT  (Params+=parameter (COMMA Params+=parameter)*)? RBRKT
                    | LPAREN (Params+=parameter (COMMA Params+=parameter)*)? RPAREN		// Allow Parentheses as well
                    ;

propertyAutoAccessor: Attributes=attributes? Modifiers=accessorModifiers? Key=(GET|SET)
                    ;

propertyLineAccessor: Attributes=attributes? Modifiers=accessorModifiers?
                      ( {InputStream.La(2) != SET}? Key=GET Expr=expression?
                      | {InputStream.La(2) != GET}? Key=SET ExprList=expressionList?
                      | Key=(GET|SET) )
                    ;

accessorModifiers	: ( Tokens+=(PRIVATE | HIDDEN | PROTECTED | PUBLIC | EXPORT | INTERNAL ) )+
                    ;

expressionList	    : Exprs+=expression (COMMA Exprs+=expression)*
                    ;

propertyAccessor    : Attributes=attributes? Modifiers=accessorModifiers?
                      ( Key=GET end=eos StmtBlk=statementBlock END GET?
                      | Key=SET end=eos StmtBlk=statementBlock END SET? )
                      end=eos
                    ;

classmember         : Member=method                                 #clsmethod
                    | decl=declare                                  #clsdeclare
                    | Member=constructor                            #clsctor
                    | Member=destructor                             #clsdtor
                    | Member=classvars                              #clsvars
                    | Member=property                               #clsproperty
                    | Member=operator_                              #clsoperator
                    | Member=structure_                             #nestedStructure
                    | Member=class_                                 #nestedClass
                    | Member=delegate_                               #nestedDelegate
                    | Member=enum_                                  #nestedEnum
                    | Member=event_                                 #nestedEvent
                    | Member=interface_                             #nestedInterface
                    | {AllowFunctionInsideClass}? Member=function                   #clsfunction      // Equivalent to static method
                    | {AllowFunctionInsideClass}? Member=procedure                  #clsprocedure     // Equivalent to static method
                    ;

constructor         :  (Attributes=attributes)? (Modifiers=constructorModifiers)?
                      CONSTRUCTOR (ParamList=parameterList)? (AS VOID)? // As Void is allowed but ignored
                        (CallingConvention=callingconvention)? 
                        (CLASS (Namespace=nameDot)? ClassId=identifier)?		
                        end=eos
                      (Chain=(SELF | SUPER)
                      (
                          (LPAREN RPAREN)
                        | (LPAREN ArgList=argumentList RPAREN)
                      ) eos)?
                      StmtBlk=statementBlock
                    ;

constructorModifiers: ( Tokens+=( PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | EXTERN | STATIC ) )+
                    ;

declare             : DECLARE (ACCESS | ASSIGN | METHOD )  Ids+=identifier (COMMA Ids+=identifier)* eos
                    ;

destructor          : (Attributes=attributes)? (Modifiers=destructorModifiers)?
                      DESTRUCTOR (LPAREN RPAREN)? 
                      (CLASS (Namespace=nameDot)? ClassId=identifier)?
                      end=eos
                      StmtBlk=statementBlock
                    ;

destructorModifiers : ( Tokens+=EXTERN )+
                    ;
/*
    From the C# syntax guide:
    overloadable-unary-operator:  one of
    +   -   !  ~   ++   --   true   false
    overloadable-binary-operator:
    + - * / % & | ^  << right-shift == != > < >= <=
    // note in C# ^ is binary operator XOR and ~ is bitwise negation (Ones complement)
    // in VO ~is XOR AND bitwise negation. ^is EXP and should not be used for overloaded ops
    // VO uses ^ for Exponent

*/
overloadedOps       : Token= (PLUS | MINUS | NOT | TILDE | INC | DEC | TRUE_CONST | FALSE_CONST |
                              MULT | DIV | MOD | AMP | PIPE | LSHIFT | RSHIFT | EEQ | NEQ | NEQ2 |
                              GT | LT | GTE | LTE |
                              AND | OR )  // these two do not exist in C# and are mapped to & and |
                    ;

conversionOps		: Token=( IMPLICIT | EXPLICIT )
                    ;

operator_           : Attributes=attributes? Modifiers=operatorModifiers?
                      OPERATOR (Operation=overloadedOps | Conversion=conversionOps) Gt=GT?
                      ParamList=parameterList
                      (AS Type=datatype)?
                      end=eos
                      StmtBlk=statementBlock
                    ;

operatorModifiers   : ( Tokens+=(PUBLIC | STATIC | EXTERN) )+
                    ;

memberModifiers     : ( Tokens+=(NEW | PRIVATE | HIDDEN | PROTECTED | PUBLIC | EXPORT | INTERNAL | STATIC | VIRTUAL | SEALED | ABSTRACT | ASYNC | UNSAFE | EXTERN | OVERRIDE) )+
                    ;

attributes          : ( AttrBlk+=attributeBlock )+
                    ;

attributeBlock      : LBRKT Target=attributeTarget? Attributes+=attribute (COMMA Attributes+=attribute)* RBRKT
                    ;

attributeTarget     : Id=identifier COLON
                    | Kw=keyword COLON
                    ;

attribute           : Name=name (LPAREN (Params+=attributeParam (COMMA Params+=attributeParam)* )? RPAREN )?
                    ;

attributeParam      : Name=identifierName ASSIGN_OP Expr=expression     #propertyAttributeParam
                    | Expr=expression                                   #exprAttributeParam
                    ;

globalAttributes    : LBRKT Target=globalAttributeTarget Attributes+=attribute (COMMA Attributes+=attribute)* RBRKT eos
                    ;

globalAttributeTarget : Token=(ASSEMBLY | MODULE) COLON
                      ;

statement           : Decl=localdecl                        #declarationStmt
                    | {AllowXBaseVariables}? xbasedecl               #xbasedeclStmt
                    | Decl=fielddecl                        #fieldStmt
                    | DO? WHILE Expr=expression end=eos
                      StmtBlk=statementBlock 
                      ((e=END (DO|WHILE)? | e=ENDDO) Ignored=expression?  eos)?	#whileStmt
                    | NOP (LPAREN RPAREN )? end=eos								#nopStmt
                    | FOR
                        ( AssignExpr=expression
                        | (LOCAL? ForDecl=IMPLIED | ForDecl=VAR) ForIter=identifier Op=(ASSIGN_OP|EQ) Expr=expression
                        | ForDecl=LOCAL ForIter=identifier Op=(ASSIGN_OP|EQ) Expr=expression (AS Type=datatype)?
                        )
                      Dir=(TO | UPTO | DOWNTO) FinalExpr=expression
                      (STEP Step=expression)? end=eos
                      StmtBlk=statementBlock (e=NEXT Ignored=identifier? eos)?	#forStmt
                    | IF IfStmt=ifElseBlock
                      ((e=END IF? | e=ENDIF)   Ignored=expression? eos)?			#ifStmt
                    | DO CASE end=eos
                      CaseStmt=caseBlock?
                      ((e=END CASE? | e=ENDCASE)   eos)?                  #caseStmt
                    | Key=EXIT end=eos                                    #jumpStmt
                    | Key=LOOP end=eos                                    #jumpStmt
                    | Key=BREAK Expr=expression? end=eos                  #jumpStmt
                    | RETURN (Void=VOID|Expr=expression)? end=eos         #returnStmt
                    | Q=(QMARK | QQMARK)
                       (Exprs+=expression (COMMA Exprs+=expression)*)?
                       end=eos                                            #qoutStmt
                    | BEGIN SEQUENCE end=eos
                      StmtBlk=statementBlock
                      (RECOVER RecoverBlock=recoverBlock)?
                      (FINALLY eos FinBlock=statementBlock)?
                      (e=END (SEQUENCE)? eos)?                              #seqStmt
                    //
                    // New in Vulcan
                    //
                    | REPEAT end=eos
                      StmtBlk=statementBlock
                      UNTIL Expr=expression
                      eos                                                       #repeatStmt
                    | FOREACH
                      (IMPLIED Id=identifier | Id=identifier AS Type=datatype| VAR Id=identifier)
                      IN Container=expression end=eos
                      StmtBlk=statementBlock (e=NEXT Ignored=identifier? eos)?  #foreachStmt
                    | Key=THROW Expr=expression? end=eos                        #jumpStmt
                    | TRY end=eos StmtBlk=statementBlock
                      (CATCH CatchBlock+=catchBlock?)*
                      (FINALLY eos FinBlock=statementBlock)?
                      (e=END TRY? eos)?								#tryStmt
                    | BEGIN Key=LOCK Expr=expression end=eos
                      StmtBlk=statementBlock
                      (e=END LOCK? eos)?						#blockStmt
                    | BEGIN Key=SCOPE end=eos
                      StmtBlk=statementBlock
                      (e=END SCOPE? eos)?						#blockStmt
                    //
                    // New XSharp Statements
                    //
                    | YIELD RETURN (VOID | Expr=expression)? end=eos			#yieldStmt
                    | YIELD Break=(BREAK|EXIT) end=eos							#yieldStmt
                    | (BEGIN|DO)? SWITCH Expr=expression end=eos
                      (SwitchBlock+=switchBlock)+
                      (e=END SWITCH? eos)?					#switchStmt
                    | BEGIN Key=USING ( Expr=expression | VarDecl=variableDeclaration ) end=eos
                        StmtBlk=statementBlock
                      (e=END USING? eos)?						#blockStmt
                    | BEGIN Key=UNSAFE end=eos
                      StmtBlk=statementBlock
                      (e=END UNSAFE? eos)?							#blockStmt
                    | BEGIN Key=CHECKED end=eos
                      StmtBlk=statementBlock
                      (e=END CHECKED? eos)?						#blockStmt
                    | BEGIN Key=UNCHECKED end=eos
                      StmtBlk=statementBlock
                      (e=END UNCHECKED? eos)?					#blockStmt
                    | BEGIN Key=FIXED ( VarDecl=variableDeclaration ) end=eos
                      StmtBlk=statementBlock
                      (e=END FIXED? eos)?						#blockStmt

                      // NOTE: The ExpressionStmt rule MUST be last, even though it already existed in VO
                      // The first ExpressonStmt rule matches a single expression
                      // The second Rule matches a single expression with an extraneous RPAREN RCURLY or RBRKT
                      // The third rule matches more than one expression
                    | {validExpressionStmt()}?
                      Exprs+=expression  end=eos									#expressionStmt
                    | {validExpressionStmt()}?
                      Exprs+=expression t=(RPAREN|RCURLY|RBRKT)  end=eos {eosExpected($t);}		#expressionStmt
                    | {validExpressionStmt()}?
                      Exprs+=expression (COMMA Exprs+=expression)+  end=eos			#expressionStmt
	                ;


ifElseBlock         : Cond=expression end=eos StmtBlk=statementBlock
                      ( ELSEIF ElseIfBlock=ifElseBlock 
                    | ELSE eos ElseBlock=statementBlock)?
                    ;

caseBlock           : Key=CASE Cond=expression end=eos StmtBlk=statementBlock NextCase=caseBlock?
                    | Key=OTHERWISE end=eos StmtBlk=statementBlock
                    ;

// Note that literalValue is not enough. We also need to support members of enums
switchBlock         : (Key=CASE Const=expression | Key=OTHERWISE) end=eos StmtBlk=statementBlock
                    ;

catchBlock          : (Id=identifier)? (AS Type=datatype)? end=eos StmtBlk=statementBlock
                    ;

recoverBlock        : (USING Id=identifier)? end=eos StmtBlock=statementBlock
                    ;

variableDeclaration	: (LOCAL? Var=IMPLIED | Var=VAR) Decl+=variableDeclarator (COMMA Decl+=variableDeclarator)*
                    | LOCAL Decl+=variableDeclarator (COMMA Decl+=variableDeclarator)* (AS Type=datatype)?
                    ;

variableDeclarator  : Id=identifier Op=(ASSIGN_OP|EQ) Expr=expression
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

localdecl          : LOCAL (Static=STATIC)? LocalVars+=localvar (COMMA LocalVars+=localvar)*			end=eos #commonLocalDecl	
                   | Static=STATIC LOCAL    LocalVars+=localvar (COMMA LocalVars+=localvar)*			end=eos #commonLocalDecl	
                   | {!XSharpLexer.IsKeyword(InputStream.La(2))}?   // STATIC Identifier , but not STATIC <Keyword>
                     Static=STATIC          LocalVars+=localvar (COMMA LocalVars+=localvar)*			end=eos #commonLocalDecl	
                   // The following rules allow STATIC in the parser, 
                   // but the treetransformation will produce an error 9044 for STATIC implied
                   | Static=STATIC? VAR           ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*  end=eos #varLocalDecl
                   | Static=STATIC LOCAL? IMPLIED ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*  end=eos #varLocalDecl
                   | LOCAL Static=STATIC? IMPLIED ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*  end=eos #varLocalDecl
                   ;

localvar           : (Const=CONST)? ( Dim=DIM )? Id=identifier (LBRKT ArraySub=arraysub RBRKT)?
                     (ASSIGN_OP Expression=expression)? (As=(AS | IS) DataType=datatype)?
                   ;

impliedvar         : (Const=CONST)? Id=identifier Op=(ASSIGN_OP|EQ) Expression=expression
                   ;

fielddecl          : FIELD Fields+=identifierName (COMMA Fields+=identifierName)* (IN Alias=identifierName)? end=eos
                   ;

// Old Style xBase declarations

xbasedecl           : T=(PRIVATE                               // PRIVATE Foo, Bar
                      |PUBLIC                               // PUBLIC  Foo, Bar
                      |MEMVAR                               // MEMVAR  Foo, Bar
                      |PARAMETERS                           // PARAMETERS Foo, Bar
                      ) Vars+=identifierName (COMMA Vars+=identifierName)*
                      end=eos
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

expression          : Expr=expression Op=(DOT | COLON) Name=simpleName          #accessMember           // member access
                    |                 Op=COLONCOLON   Name=simpleName           #accessMember           // XPP & Harbour SELF member access
                    | Left=expression Op=(DOT | COLON) AMP LPAREN Right=expression RPAREN  #accessMemberLate // aa:&(Expr). Expr must evaluate to a string which is the ivar name
                                                                                                        // can become IVarGet() or IVarPut when this expression is the LHS of an assignment
                    | Left=expression Op=(DOT | COLON) AMP Name=identifierName  #accessMemberLateName   // aa:&Name  Expr must evaluate to a string which is the ivar name
                    | Expr=expression LPAREN                       RPAREN       #methodCall             // method call, no params
                    | Expr=expression LPAREN ArgList=argumentList  RPAREN       #methodCall             // method call, params
                    | Expr=expression LBRKT ArgList=bracketedArgumentList RBRKT #arrayAccess            // Array element access
                    | Left=expression Op=QMARK Right=boundExpression            #condAccessExpr         // expr ? expr
                    | LPAREN Type=datatype RPAREN Expr=expression               #typeCast               // (typename) expr
                    | Expr=expression Op=(INC | DEC)                            #postfixExpression      // expr ++/--
                    | Op=AWAIT Expr=expression                                  #awaitExpression        // AWAIT expr
                    | Op=(PLUS | MINUS | TILDE| ADDROF | INC | DEC) Expr=expression #prefixExpression   // +/-/~/&/++/-- expr
                    | Expr=expression Op=IS Type=datatype                       #typeCheckExpression    // expr IS typeORid
                    | Expr=expression Op=ASTYPE Type=datatype                   #typeCheckExpression    // expr AS TYPE typeORid
                    | Left=expression Op=EXP Right=expression                   #binaryExpression       // expr ^ expr
                    | Left=expression Op=(MULT | DIV | MOD) Right=expression    #binaryExpression       // expr * expr
                    | Left=expression Op=(PLUS | MINUS) Right=expression        #binaryExpression       // expr +/- expr
                    | Left=expression Op=LSHIFT Right=expression                #binaryExpression       // expr << expr (shift)
                    | Left=expression Op=GT Gt=GT Right=expression              #binaryExpression       // expr >> expr (shift)
                    | Left=expression Op=( LT | LTE | GT | GTE | EQ | EEQ | 
                                          SUBSTR | NEQ | NEQ2) Right=expression #binaryExpression       // expr >= expr (relational)
                    | Left=expression Op=AMP Right=expression                   #binaryExpression       // expr & expr (bitwise and)
                    | Left=expression Op=TILDE Right=expression                 #binaryExpression       // expr ~ expr (bitwise xor)
                    | Left=expression Op=PIPE Right=expression                  #binaryExpression       // expr | expr (bitwise or)
                    | Op=(LOGIC_NOT|NOT) Expr=expression                        #prefixExpression       // .not. expr (logical not)  also  !
                    | Left=expression Op=(LOGIC_AND | AND) Right=expression     #binaryExpression       // expr .and. expr (logical and) also &&
                    | Left=expression Op=LOGIC_XOR Right=expression             #binaryExpression       // expr .xor. expr (logical xor)
                    | Left=expression Op=(LOGIC_OR | OR) Right=expression       #binaryExpression       // expr .or. expr (logical or)  also ||
                    | Left=expression Op=DEFAULT Right=expression               #binaryExpression       // expr DEFAULT expr
                    | <assoc=right> Left=expression
                      Op=( ASSIGN_OP | ASSIGN_ADD | ASSIGN_SUB | ASSIGN_EXP
                            | ASSIGN_MUL | ASSIGN_DIV | ASSIGN_MOD
                            | ASSIGN_BITAND | ASSIGN_BITOR | ASSIGN_LSHIFT
                            | ASSIGN_RSHIFT | ASSIGN_XOR )
                      Right=expression                                          #assignmentExpression	// expr := expr, also expr += expr etc.
                    | Expr=primary                                              #primaryExpression
                    ;

                    // Primary expressions
                    // Note: No need to check for extra ) } or ] tokens. The expression rule does that already
primary             : Key=SELF                                                  #selfExpression
                    | Key=SUPER                                                 #superExpression
                    | Literal=literalValue                                      #literalExpression		// literals
                    | LiteralArray=literalArray                                 #literalArrayExpression	// { expr [, expr] }
                    | AnonType=anonType                                         #anonTypeExpression		// { .id := expr [, .id := expr] }
                    | CbExpr=codeblock                                          #codeblockExpression	// {| [id [, id...] | expr [, expr...] }
                    | AnoExpr=anonymousMethodExpression                         #codeblockExpression	// DELEGATE (x as Foo) { DoSomething(Foo) }
                    | Query=linqQuery                                           #queryExpression        // LINQ
                    | Type=datatype LCURLY Obj=expression COMMA
                      ADDROF Func=name LPAREN RPAREN RCURLY                     #delegateCtorCall		// delegate{ obj , @func() }
                    | Type=datatype LCURLY RCURLY  Init=objectOrCollectioninitializer?  #ctorCall   // id{  } with optional { Name1 := Expr1, [Name<n> := Expr<n>]}
                    | Type=datatype LCURLY ArgList=argumentList  RCURLY	
                                                   Init=objectOrCollectioninitializer?  #ctorCall				// id{ expr [, expr...] } with optional { Name1 := Expr1, [Name<n> := Expr<n>]}
                    | ch=CHECKED LPAREN ( Expr=expression ) RPAREN              #checkedExpression		// checked( expression )
                    | ch=UNCHECKED LPAREN ( Expr=expression ) RPAREN            #checkedExpression		// unchecked( expression )
                    | TYPEOF LPAREN Type=datatype RPAREN                        #typeOfExpression		// typeof( typeORid )
                    | SIZEOF LPAREN Type=datatype RPAREN                        #sizeOfExpression		// sizeof( typeORid )
                    | DEFAULT LPAREN Type=datatype RPAREN                       #defaultExpression		// default( typeORid )
                    | Name=simpleName                                           #nameExpression			// generic name
                    | Type=nativeType LPAREN Expr=expression RPAREN             #voConversionExpression	// nativetype( expr )
                    | XType=xbaseType LPAREN Expr=expression RPAREN             #voConversionExpression	// xbaseType( expr )
                    | Type=nativeType LPAREN CAST COMMA Expr=expression RPAREN  #voCastExpression		// nativetype(_CAST, expr )
                    | XType=xbaseType LPAREN CAST COMMA Expr=expression RPAREN  #voCastExpression		// xbaseType(_CAST, expr )
                    | CASTCLASS LPAREN Type = nativeType COMMA Expr=expression RPAREN #voCastExpression		// __CastClass( nativetype, expression)
                    | CASTCLASS LPAREN XType = xbaseType COMMA Expr=expression RPAREN #voCastExpression		// __CastClass( xbaseType, expression)
                    | PTR LPAREN Type=datatype COMMA Expr=expression RPAREN     #voCastPtrExpression	// PTR( typeName, expr )
                    | Name=usualTypeName                                        #usualTypeNameExpression	// LONG, STRING etc., used as NUMERIC in expressions
                    | Type=typeName                                             #typeExpression			// Standard DotNet Types
                    | Expr=iif                                                  #iifExpression			// iif( expr, expr, expr )
                    | Op=(VO_AND | VO_OR | VO_XOR | VO_NOT) LPAREN Exprs+=expression
                      (COMMA Exprs+=expression)* RPAREN                         #intrinsicExpression	// _Or(expr, expr, expr)
                    | FIELD ALIAS (Alias=identifier ALIAS)? Field=identifier    #aliasedField		      // _FIELD->CUSTOMER->NAME is equal to CUSTOMER->NAME
                    | FIELD ALIAS (Alias=identifier ALIAS)? AMP Expr=expression #aliasedFieldLate     // _FIELD->CUSTOMER->&expression expression must evaluate to a string. 
                                                                                                      // Expression can of course be a parenExpression. And can also be LHS of an assigment !
                    | MEMVAR ALIAS VarName=identifier                           #aliasedMemvar        // MEMVAR->Name
                    | {InputStream.La(4) != LPAREN}?                            // this makes sure that CUSTOMER->NAME() is not matched
                          Alias=identifier ALIAS Field=identifier               #aliasedField		      // CUSTOMER->NAME
                    | Id=identifier ALIAS Expr=expression                       #aliasedExpr          // id -> expr       // when id = 'M' then redirect to aliasedMemvar
                    | LPAREN Alias=expression RPAREN ALIAS Expr=expression      #aliasedExpr          // (expr) -> expr   // when expression = 'M' then redirect to aliasedMemvar
                    | AMP LPAREN Expr=expression RPAREN                         #macro					      // &(expr)          // parens are needed because otherwise &(string) == Foo will match everything until Foo
                    | AMP Name=identifierName                                   #macroName			      // &name            // macro with a variable name
                    | LPAREN Exprs+=expression (COMMA Exprs+=expression)* RPAREN  #parenExpression		  // ( expr,expr,expr )
                    | Key=ARGLIST                                               #argListExpression		// __ARGLIST
                    ;

boundExpression		: Expr=boundExpression Op=(DOT | COLON) Name=simpleName               #boundAccessMember	// member access The ? is new
                    | Expr=boundExpression LPAREN                       RPAREN          #boundMethodCall	// method call, no params
                    | Expr=boundExpression LPAREN ArgList=argumentList RPAREN           #boundMethodCall	// method call, with params
                    | Expr=boundExpression LBRKT ArgList=bracketedArgumentList RBRKT    #boundArrayAccess	// Array element access
                    | <assoc=right> Left=boundExpression Op=QMARK Right=boundExpression #boundCondAccessExpr	// expr ? expr
                    | Op=(DOT | COLON) Name=simpleName                                  #bindMemberAccess
                    | LBRKT ArgList=bracketedArgumentList RBRKT                         #bindArrayAccess
                    ;

// Initializers

objectOrCollectioninitializer : ObjInit=objectinitializer
                              | CollInit=collectioninitializer
                              ;

objectinitializer   : LCURLY (Members+=memberinitializer (COMMA Members+=memberinitializer)*)? RCURLY
                    ;

memberinitializer   : Name=identifierName ASSIGN_OP Expr=initializervalue
                    ;

initializervalue    : Init=objectOrCollectioninitializer // Put this first to make sure we are not matching a literal array for { expr [, expr] }
                    | Expr=expression
                    ;

collectioninitializer : LCURLY Members+=expression (COMMA Members+=expression)* RCURLY
                      ;

bracketedArgumentList : Args+=unnamedArgument (COMMA Args+=unnamedArgument)*
                      ;

                      // NOTE: Separate rule for bracketedarguments because they cannot use identifierName syntax
unnamedArgument     :  Expr=expression?
                    ;
                    // NOTE: Optional argumentlist is handled in the rules that use this rule
argumentList        :  Args+=namedArgument (COMMA Args+=namedArgument)*
                    ;

                    // NOTE: Expression is optional so we can skip arguments for VO/Vulcan compatibility
namedArgument       :  {AllowNamedArgs}?  Name=identifierName ASSIGN_OP  ( RefOut=(REF | OUT) )? Expr=expression?
                    |  ( RefOut=(REF | OUT) )? Expr=expression?
                    ;


iif                 : (IIF|IF) LPAREN Cond=expression COMMA TrueExpr=expression? COMMA FalseExpr=expression? RPAREN
                    ;

nameDot             : Left=nameDot Right=simpleName DOT                         #qualifiedNameDot
                    | Name=aliasedName DOT                                      #simpleOrAliasedNameDot
                    ;

name                : Left=name Op=DOT Right=simpleName                         #qualifiedName
                    | Name=aliasedName                                          #simpleOrAliasedName
                    ;

aliasedName         : Global=GLOBAL Op=COLONCOLON Right=simpleName              #globalQualifiedName
                    | Alias=identifierName Op=COLONCOLON Right=simpleName       #aliasQualifiedName
                    | Name=simpleName                                           #identifierOrGenericName
                    ;

simpleName          : Id=identifier	GenericArgList=genericArgumentList?
                    ;

genericArgumentList : LT GenericArgs+=datatype (COMMA GenericArgs+=datatype)* GT
                    ;

identifierName      : Id=identifier
                    ;

datatype            : ARRAY OF TypeName=typeName                                    #arrayOfType
                    | TypeName=typeName PTR                                         #ptrDatatype
                    | TypeName=typeName (Ranks+=arrayRank)+                         #arrayDatatype
                    | TypeName=typeName                                             #simpleDatatype
                    | TypeName=typeName QMARK                                       #nullableDatatype
                    ;

arrayRank           : LBRKT (Commas+=COMMA)* RBRKT
                    ;

typeName            : NativeType=nativeType
                    | XType=xbaseType
                    | Name=name
                    ;

usualTypeName       : NativeType=nativeType					// just type typenames that are allowed for USUAL variables
                    | XType=xbaseType				
                    ;

                    // Separate rule for Array with zero elements, to prevent entering the first arrayElement rule
                    // with a missing Expression which would not work for the core dialect
literalArray        : (LT Type=datatype GT)? LCURLY RCURLY															// {}
                    | (LT Type=datatype GT)? LCURLY Elements+=arrayElement (COMMA Elements+=arrayElement)* RCURLY   // {e,e,e} or {e,,e} or {,e,} etc
                    ;

arrayElement        : Expr=expression?      // VO Array elements are optional
                    ;

// Anonymous Types

anonType            : CLASS LCURLY (Members+=anonMember (COMMA Members+=anonMember)*)? RCURLY
                    ;

anonMember          : Name=identifierName ASSIGN_OP Expr=expression
                    | Expr=expression
                    ;


// Codeblocks & Lambda Expressions

codeblock       : LCURLY (Or=OR | P1=PIPE LambdaParamList=lambdaParameterList? P2=PIPE)	// Old codeblock Syntax
                    Code=codeblockCode RCURLY
                | LCURLY (Or=OR | P1=PIPE? LambdaParamList=lambdaParameterList? P2=PIPE?) lambda=UDCSEP		// Alternative Lambda Syntax with pips that will trigger an error
                   Code=codeblockCode RCURLY
                    ;

codeblockCode     : Expr=expression? 
                  | eos StmtBlk=statementBlock
                  | ExprList=codeblockExprList 
                  ;

lambdaParameterList : ImplicitParams=codeblockParamList 
                    | ExplicitParams=explicitAnonymousFunctionParamList
                    ;

codeblockParamList  : Ids+=identifier (COMMA Ids+=identifier)*
                    ;

codeblockExprList   : (Exprs+=expression? COMMA)+ ReturnExpr=expression
                    ;

// Anonymous methods 
anonymousMethodExpression : (Async=ASYNC)? Delegate=DELEGATE (LPAREN ParamList=explicitAnonymousFunctionParamList? RPAREN)? 
                            LCURLY Code=codeblockCode RCURLY 
                          ;

explicitAnonymousFunctionParamList  : Params+=explicitAnonymousFunctionParameter (COMMA Params+=explicitAnonymousFunctionParameter)*
                                    ;

explicitAnonymousFunctionParameter      : Id=identifier Mod=anonymousfunctionParameterModifier Type=datatype
                                        ;

anonymousfunctionParameterModifier      : Tokens+= (AS | REF | OUT)
                                        ;



// LINQ Support

linqQuery           : From=fromClause Body=queryBody
                    ;

fromClause          : FROM Id=identifier (AS Type=typeName)? IN Expr=expression
                    ;

queryBody           : (Bodyclauses+=queryBodyClause)* SorG=selectOrGroupclause (Continuation=queryContinuation)?
                    ;

queryBodyClause     : From=fromClause                                                                    #fromBodyClause
                    | LET Id=identifier ASSIGN_OP Expr=expression                                        #letClause
                    | WHERE Expr=expression                                                              #whereClause        // expression must be Boolean
                    | JOIN Id=identifier (AS Type=typeName)?
                      IN Expr=expression ON OnExpr=expression EQUALS EqExpr=expression
                      Into=joinIntoClause?                                                              #joinClause
                    | ORDERBY Orders+=ordering (COMMA Orders+=ordering)*                                #orderbyClause
                    ;

joinIntoClause      : INTO Id=identifier
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
identifier          : Token=(ID  | KWID)
                    | VnToken=keywordvn
                    | XsToken=keywordxs
                    | XppToken=keywordxpp
                    ;

identifierString    : Token=(ID | KWID | STRING_CONST)
                    | VnToken=keywordvn
                    | XsToken=keywordxs
                    | XppToken=keywordxpp
                    ;


// xBaseTypes are NOT available in the Core dialect and therefore separated here.
xbaseType           : Token=	// Aphabetical order
                    ( ARRAY
                    | CODEBLOCK
                    | DATE
                    | FLOAT
                    | PSZ
                    | SYMBOL
                    | USUAL)
                    ;

nativeType			: Token=		// Aphabetical order
                    ( BYTE
                    | CHAR
                    | DATETIME
                    | DECIMAL
                    | DWORD
                    | DYNAMIC
                    | INT
                    | INT64
                    | LOGIC
                    | LONGINT
                    | OBJECT
                    | PTR
                    | REAL4
                    | REAL8
                    | SHORTINT
                    | STRING
                    | UINT64
                    | VOID                  
                    | WORD
                     )
                    ;

literalValue        : Token=
                    ( TRUE_CONST
                    | FALSE_CONST
                    | CHAR_CONST
                    | STRING_CONST
                    | ESCAPED_STRING_CONST
                    | INTERPOLATED_STRING_CONST
                    | INCOMPLETE_STRING_CONST
                    | SYMBOL_CONST
                    | HEX_CONST
                    | BIN_CONST
                    | REAL_CONST
                    | INT_CONST
                    | INVALID_NUMBER
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


keyword             : (KwVo=keywordvo | KwVn=keywordvn | KwXs=keywordxs) ;

keywordvo           : Token=(ACCESS | AS | ASSIGN | BEGIN | BREAK | CASE | CAST | CLASS | DLL | DO 
                    | ELSE | ELSEIF | END | ENDCASE | ENDDO | ENDIF | EXIT | EXPORT | FOR | FUNCTION 
                    | HIDDEN | IF | IIF | IS | LOCAL | LOOP | MEMBER | METHOD | NEXT | OTHERWISE
                    | PRIVATE | PROCEDURE | PROTECTED | PTR | PUBLIC | RECOVER | RETURN | SELF| SIZEOF | SUPER
                    | TYPEOF | WHILE | TRY | VO_AND | VO_NOT | VO_OR | VO_XOR
                    // The following new keywords cannot be in the keywordVN list because it will match an expression when used on their own
                    | REPEAT | CONSTRUCTOR | CATCH | DESTRUCTOR | FINALLY 
                    )
                    ;


keywordvn           : Token=(ABSTRACT | ANSI | AUTO | CHAR | CONST |  DEFAULT | EXPLICIT | FOREACH | GET | IMPLEMENTS | IMPLICIT | IMPLIED | INITONLY | INTERNAL
                            | LOCK | NAMESPACE | NEW | OUT | PARTIAL | SCOPE | SEALED | SET |  TRY | UNICODE |  VALUE | VIRTUAL  
                            )
                    ;

keywordxs           : Token=( ADD | ARGLIST | ASCENDING | ASSEMBLY | ASTYPE | ASYNC | AWAIT | BY | CHECKED | DESCENDING | DYNAMIC | EQUALS | EXTERN | FIXED | FROM 
                    | GROUP | INTO | JOIN | LET | MODULE | NAMEOF | NOP | OF | ON | ORDERBY | OVERRIDE |PARAMS | REMOVE 
                    | SELECT | SWITCH | UNCHECKED | UNSAFE | VAR | VOLATILE | WHERE | YIELD | CHAR  | DECIMAL | DATETIME 
                    | MEMVAR | PARAMETERS  // Added as XS keywords to allow them to be treated as IDs
                    // the following entity keywords will be never used 'alone' and can therefore be safely defined as identifiers
                    | DEFINE| DELEGATE | ENUM | GLOBAL | INHERIT | INTERFACE | OPERATOR	| PROPERTY | STRUCTURE | VOSTRUCT   
                    // The following 'old' keywords are never used 'alone' and are harmless as identifiers
                    | ALIGN | CALLBACK | CLIPPER  | DECLARE | DIM | DOWNTO | DLLEXPORT | EVENT 
                    | FASTCALL | FUNC | IN | INSTANCE | PASCAL | PROC | SEQUENCE 
                    | STEP | STRICT | TO | THISCALL | UNION | UNTIL | UPTO | USING | WINCALL 
                    )
                    ;
					
/// XBase++ Parser definities					
					
keywordxpp         : Token=(ENDCLASS| FREEZE| FINAL| SHARING| SHARED| INLINE| SYNC| ASSIGNMENT| EXPORTED| READONLY| NOSAVE| INTRODUCE)
                   ;

xppsource           : eos? (Entities+=xppentity )*
                      EOF
                    ;

xppnamespace        : BEGIN NAMESPACE Name=name e=eos
                      (Entities+=xppentity)*
                      END NAMESPACE Ignored=name?  eos
                    ;
					
xppentity           : xppnamespace
                    | xppclass
                    | structure_
                    | interface_
                    | delegate_
                    | event_
                    | enum_
                    | function                  // This will become part of the 'Globals' class
                    | procedure                 // This will become part of the 'Globals' class
                    | xppmethod                 // Method xxx Class xxx syntax
                    | globalAttributes          // Assembly attributes, Module attributes etc.
                    | using_                    // Using Namespace
                    | vodefine                  // This will become part of the 'Globals' class
                    | voglobal                  // This will become part of the 'Globals' class
                    | vodll                     // External method of the Globals class
                    | vostruct                  // Compatibility (unsafe) structure
                    | vounion                   // Compatibility (unsafe) structure with members aligned at FieldOffSet 0
                    | xppmemvar                 // global memvar outside of code 
                    ;

xppclass           :  (Attributes=attributes)?                                // NEW Optional Attributes
                      (Modifiers=xppclassModifiers)?                          // [STATIC|FREEZE|FINAL] 
                       CLASS (Namespace=nameDot)? Id=identifier               // CLASS <ClassName>
                       (
                          From=(FROM| SHARING) BaseTypes+=datatype (COMMA BaseTypes+=datatype)*  // [FROM <SuperClass,...>] ; 
                       )?                                                                   // [SHARING <SuperClass,...>]
                       (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)? // NEW Implements
                       // No type parameters and type parameter constraints
                      e=eos
                      (Members+=xppclassMember)*
                      ENDCLASS Ignored=identifier?
                      eos
                    ;

xppclassModifiers   : ( Tokens+=(STATIC | FREEZE | FINAL) )+
                    ;

xppclassMember      : Member=xppmethodvis                           #xppclsvisibility
                    | Member=xppclassvars                           #xppclsvars
                    | Member=xppinlineMethod                        #xppclsinlinemethod
                    | Member=xppdeclareMethod                       #xppclsdeclaremethod
                    | Member=xppproperty                            #xppclsproperty
                    ;

xppmethodvis        : Vis=xppvisibility COLON eos
                    ;

xppvisibility       : Token=(HIDDEN | PROTECTED | EXPORTED)         
                    ;

xppdeclareMethod    : (Modifiers=xppdeclareModifiers)?                            // [DEFERRED |FINAL | INTRODUCE | OVERRIDE] [CLASS] 
                      METHOD Methods+=identifier                                   // METHOD <MethodName,...> 
                      (
                        xppisin                                                   //  [IS <Name>] [IN <SuperClass>] 
                        | (COMMA Methods+=identifier)*                             // or an optional comma seperated list of other names
                      )
                      eos
                    ;


xppisin             : IS Id=identifier (IN SuperClass=identifier)?                //  IS <Name> [IN <SuperClass>] 
                    | IN SuperClass=identifier								                    //  IN <SuperClass> without IS clause
                    ;

                    

xppdeclareModifiers : ( Tokens+=( DEFERRED | FINAL | INTRODUCE | OVERRIDE | CLASS | SYNC ) )+
                    ;

xppclassvars        : (Modifiers=xppmemberModifiers)?                             // [CLASS] 
                      VAR Vars+=identifier                                        // VAR <VarName> 
                      (
                        Is=xppisin                                                // [IS <Name>] [IN <SuperClass>] 
                        | ((COMMA Vars+=identifier)*                              // <,...> 
                        (AS DataType=datatype)?  )                                // Optional data type

                      )
                      (Shared=SHARED)?                                            // [SHARED]
                      (ReadOnly=READONLY)?                                        // [READONLY] 
                      (Assignment=xppvarassignment)?                              // [ASSIGNMENT HIDDEN | PROTECTED | EXPORTED] 
                      (Nosave= NOSAVE)?                                           // [NOSAVE] 
                      eos
                    ;


xppvarassignment    : ASSIGNMENT xppvisibility                                    // [ASSIGNMENT HIDDEN | PROTECTED | EXPORTED] 
                    ;

xppproperty         : (Attributes=attributes)?                                    // NEW Optional Attributes
                      (   Access=ACCESS Assign=ASSIGN?                            // ACCESS | ASSIGN  | ACCESS ASSIGN | ASSIGN ACCESS
                        | Assign=ASSIGN Access=ACCESS?
                      ) 
                      Modifiers=xppmemberModifiers?                               // [CLASS]
                      METHOD Id=identifier                                        // METHOD <MethodName>
                      (VAR VarName=identifier)?                                   // [VAR <VarName>]
                      (AS Type=datatype)?                                         // NEW Optional data type
                      eos
                    ;


xppmethod           : (Attributes=attributes)?                              // NEW Optional Attributes
                      (MethodType=(ACCESS|ASSIGN))?                         // Optional Access or Assign
                      (Modifiers=xppmemberModifiers)?                       // [CLASS]
                      METHOD (ClassId=identifier COLON)? Id=identifier      // [<ClassName>:] <MethodName>
                      // no type parameters 
                      (ParamList=parameterList)?                            // Optional Parameters
                      (AS Type=datatype)?                                   // NEW Optional return type
                      // no type constraints
                      // no calling convention
                      end=eos
                      StmtBlk=statementBlock
                    ;

xppinlineMethod     : (Attributes=attributes)?                               // NEW Optional Attributes
                      INLINE  
                      (Modifiers=xppmemberModifiers)?                        // [CLASS]
                      METHOD  Id=identifier                                  // METHOD <MethodName>
                      // no type parameters 
                      (ParamList=parameterList)?                            // Optional Parameters
                      (AS Type=datatype)?                                   // NEW Optional return type
                      // no type constraints
                      // no calling convention
                      end=eos
                      StmtBlk=statementBlock
                    ;

xppmemberModifiers  : ( Tokens+=( CLASS | STATIC) )+
                    ;

xppmemvar           : MEMVAR Vars+=identifierName (COMMA Vars+=identifierName)*
                      end=eos
                    ;
