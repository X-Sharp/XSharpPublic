//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// This file is bested edited with VS Code and the ANTLR4 grammar extension mike-lischke.vscode-antlr4

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

source              :  (Entities+=entity )* EOF
                    ;

foxsource           :  ({HasMemVars}? MemVars += filewidememvar)*
                       StmtBlk=statementBlock
                       (Entities+=entity )* EOF
                    ;

entity              : namespace_
                    // types
                    | class_
                    | {IsFox}? foxclass                  // FoxPro Class definition*/
                    | {IsXPP}? xppclass                  // XPP Class definition
                    | structure_
                    | interface_
                    | delegate_
                    | event_
                    | enum_
                    | globalAttributes          // Assembly attributes, Module attributes etc.
                    | {IsVO}? vostruct          // VO Compatibility (unsafe) structure
                    | {IsVO}? vounion           // VO Compatibility (unsafe) structure with members aligned at FieldOffSet 0
                    // members of the functions class
                    | funcproc                  // This will become part of the 'Globals' class
                    | using_                    // Using Namespace
                    | vodefine                  // This will become part of the 'Globals' class
                    | voglobal                  // This will become part of the 'Globals' class
                    | vodll                     // External method of the Globals class
                    // methods outside of class .. endclass
                    | {!IsXPP}? method          // Method xxx Class xxx syntax, has to be excluded for XPP because of different semantics
                    | {IsXPP}? xppmethod        // XPP method, will be linked to XPP Class
                    | constructor               // Constructor Class xxx syntax
                    | destructor                // Destructor Class xxx syntax
                    | {HasMemVars}? filewidememvar  // memvar declared at file level
                    | {IsFox}? foxdll           // FoxPro style of declaring Functions in External DLLs
                    | eos                       // Blank Lines between entities
                    ;

eos                 : EOS+
                    ;

funcproc              : (Attributes=attributes)? (Modifiers=funcprocModifiers)?
                        T=funcproctype Sig=signature
                        InitExit=(INIT1|INIT2|INIT3|EXIT)?
                        vodummyclauses
                        end=eos
                        StmtBlk=statementBlock
                        (END T2=funcproctype EOS )?
                       |
                        // Clipper/XBase++ INIT PROC. InitExit is not optional
                        // otherwise this may be ambigous with alternative 1
                        // also there are no modifiers here
                        (Attributes=attributes)? 
                        InitExit=(INIT|EXIT) T=funcproctype
                        Sig=signature
                        end=eos
                        StmtBlk=statementBlock 
                        (END T2=funcproctype EOS )?
                      ;

funcproctype          : Token=(FUNCTION|PROCEDURE)
                      ;

signature             : Id=identifier
                        TypeParameters=typeparameters?
                        (ParamList=parameterList)?
                        (AS Type=datatype)?
                        (ConstraintsClauses+=typeparameterconstraintsclause)*
                        (CallingConvention=callingconvention)?
                        (UDCSEP ExpressionBody=expression)?                       // New: Expression Body
                      ;


vodummyclauses      :(EXPORT LOCAL)?                                       // Export Local exists in VO but is ignored in X#
                     (DLLEXPORT STRING_CONST)?                             // The DLLEXPORT clause exists in VO but is ignored in X#
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
                      D=DLL T=funcproctype Id=identifier (ParamList=parameterList)? (AS Type=datatype)?
                      (CallingConvention=dllcallconv)? COLON
                      Dll=identifierString (DOT Extension=identifierString)?
                      (	Ordinal=REAL_CONST
                       |  DOT Entrypoint=identifierString Address=ADDROF? Number=INT_CONST? (NEQ2 INT_CONST)?
                      )
                      ( CharSet=(AUTO | ID) )?  // ID must be either ANSI or UNICODE
                      EOS
                    ;

dllcallconv         : Cc=( CLIPPER | STRICT | PASCAL | THISCALL | FASTCALL | ASPEN | WINCALL | CALLBACK)
                    ;

                    // Note that when an alias is specified (AS) then that is the name that we should use and then the Id is the entrypoint
foxdll              : (Attributes=attributes)? (Modifiers=funcprocModifiers)? // Optional
                      DECLARE (Type=datatype)? Id=identifier IN Dll=identifier (DOT Extension=identifierString)? (AS Alias=identifier)?
                      ( Params+=foxdllparam (COMMA Params+=foxdllparam)* )?
                      EOS
                    ;

foxdllparam         : (Attributes=attributes)? Type=datatype (Address=ADDROF)? (Name=identifier)?
                    ;


// _DLL access Allpages :rp2DSN32.PtrDevice:Allpages:Access
// _DLL method ImportCfg(cId,hFile) :rp2DSN32.PtrDevice:ImportCfg
// _DLL assign LandScape(lValue) :rp2DSN32.PtrDevice:LandScape:Assign
// _DLL CONSTRUCTOR(oWinOwner,oRptOwner) :rp2DSN32.PtrDevice:Init
// _DLL DESTRUCTOR :rp2DSN32.PtrDevice:Axit

vodllmethod         : D=DLL T=(METHOD|ACCESS|ASSIGN|CONSTRUCTOR|DESTRUCTOR) (~EOS)? EOS
                    ;


parameterList       : LPAREN (Params+=parameter (COMMA Params+=parameter)*)? RPAREN
                    ;

parameter           : (Attributes=attributes)? Self=SELF? Id=identifier (Op=assignoperator Default=expression)? (Modifiers=parameterDeclMods Type=datatype)?
                    | Ellipsis=ELLIPSIS
                    ;

parameterDeclMods   : Tokens+=(AS | REF | OUT | IS | PARAMS | IN ) /*Tokens+=*/CONST?
                    ;

statementBlock      : (Stmts+=statement)*
                    ;


funcprocModifiers   : ( Tokens+=(STATIC | INTERNAL | PUBLIC | EXPORT | UNSAFE | ASYNC ) )+
                    ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs


using_              : USING (Static=STATIC)? (Alias=identifierName Op=assignoperator)? Name=name EOS
                    ;


                    // [STATIC] GLOBAL [CONST] Identifier [:= Expression] [, Identifier2 [:= Expression2] ] [AS Type]
                    // STATIC          [CONST] Identifier [:= Expression] [, Identifier2 [:= Expression2] ] [AS Type]
voglobal            : (Attributes=attributes)? (Modifiers=funcprocModifiers)? Global=GLOBAL (Const=CONST)?
                      Vars+=classvar (COMMA Vars+=classvar)* end=EOS
                    | (Attributes=attributes)? Static=STATIC (Const=CONST)?
                      Vars+=classvar (COMMA Vars+=classvar)* end=EOS
                    ;



// method rule used inside and outside class members rule
method              : (Attributes=attributes)? (Modifiers=memberModifiers)?
                      T=methodtype (ExplicitIface=nameDot)? Sig=signature
                      (CLASS ClassId=identifier)?      // Class Clause needed when entity and allowed when class member
                      vodummyclauses
                      end=eos
                      StmtBlk=statementBlock
                      (END T2=methodtype End=EOS)?
                    ;


methodtype          : Token=(METHOD | ACCESS | ASSIGN )
                    ;


// Convert to constant on Globals class. Expression must be resolvable at compile time
vodefine            : (Modifiers=funcprocModifiers)?
                      D=DEFINE Id=identifier Op=assignoperator Expr=expression (AS DataType=typeName)? end=EOS
                    ;

vostruct            : (Modifiers=votypeModifiers)?
                      V=VOSTRUCT (Namespace=nameDot)? Id=identifier (ALIGN Alignment=INT_CONST)? e=eos
                      (Members+=vostructmember)+
                      (END VOSTRUCT End=EOS)?
                    ;

vostructmember      : MEMBER Dim=DIM Id=identifier LBRKT ArraySub=arraysub RBRKT (As=(AS | IS) DataType=datatype)? eos
                    | MEMBER Id=identifier (As=(AS | IS) DataType=datatype)? eos
                    ;


vounion             : (Modifiers=votypeModifiers)?
                      U=UNION (Namespace=nameDot)? Id=identifier e=eos
                      (Members+=vostructmember)+
                      (END UNION End=EOS)?
                    ;

votypeModifiers     : ( Tokens+=(INTERNAL | PUBLIC | EXPORT | UNSAFE | STATIC ) )+
                    ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs


namespace_          : BEGIN NAMESPACE Name=name e=eos
                      (Entities+=entity)*
                      (END NAMESPACE End=EOS)?
                    ;

interface_          : (Attributes=attributes)? (Modifiers=classModifiers)?
                      I=INTERFACE (Namespace=nameDot)? Id=identifier
                      TypeParameters=typeparameters?                                      // TypeParameters indicate Generic Interface
                      ((INHERIT|COLON) Parents+=datatype)? (COMMA Parents+=datatype)*
                      (ConstraintsClauses+=typeparameterconstraintsclause)*              // Optional typeparameterconstraints for Generic Interface
                      e=eos
                      (Members+=classmember)*
                      // Do not make the next line optional. The parser will not know when a nested type starts or the next type
                      // as a result the parser will become VERY slow
                      END INTERFACE End=EOS
                    ;


class_              : (Attributes=attributes)? (Modifiers=classModifiers)?
                      C=CLASS (Namespace=nameDot)? Id=identifier
                      TypeParameters=typeparameters?                                    // TypeParameters indicate Generic Class
                      (INHERIT BaseType=datatype)?
                      (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)?
                      (ConstraintsClauses+=typeparameterconstraintsclause)*             // Optional typeparameterconstraints for Generic Class
                      e=eos
                      (Members+=classmember)*
                      // Do not make the next line optional. The parser will not know when a nested type starts or the next type
                      // as a result the parser will become VERY slow
                      END CLASS End=EOS
                    ;

classModifiers      : ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | ABSTRACT | SEALED | STATIC | UNSAFE | PARTIAL) )+
                    ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs




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

structure_          : (Attributes=attributes)? (Modifiers=classModifiers)?
                      S=STRUCTURE (Namespace=nameDot)? Id=identifier
                      TypeParameters=typeparameters?
                      (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)?
                      (ConstraintsClauses+=typeparameterconstraintsclause)* e=eos
                      (Members+=classmember)*
                      // Do not make the next line optional. The parser will not know when a nested type starts or the next type
                      // as a result the parser will become VERY slow
                      END STRUCTURE End=EOS
                    ;



delegate_           : (Attributes=attributes)? (Modifiers=classModifiers)?
                      D=DELEGATE (Namespace=nameDot)? Id=identifier
                      TypeParameters=typeparameters?
                      ParamList=parameterList?
                      (AS Type=datatype)?
                      (ConstraintsClauses+=typeparameterconstraintsclause)*
                      (CallingConvention=callingconvention)?
                      e=EOS
                    ;


enum_               : (Attributes=attributes)? (Modifiers=classModifiers)?
                      E=ENUM (Namespace=nameDot)? Id=identifier ((AS|INHERIT) Type=datatype)? e=eos
                      (Members+=enummember)+
                      END ENUM? End=EOS
                    ;

enummember          : (Attributes=attributes)? MEMBER? Id=identifier (Op=assignoperator Expr=expression)? eos
                    ;

event_              : (Attributes=attributes)? (Modifiers=memberModifiers)?
                       E=EVENT (ExplicitIface=nameDot)? Id=identifier (AS Type=datatype)?
                       ( end=EOS
                        | (LineAccessors += eventLineAccessor)+ end=EOS
                        | Multi=eos (Accessors+=eventAccessor)+ END EVENT? EOS
                       )
                    ;


eventLineAccessor   : Attributes=attributes? Modifiers=accessorModifiers?
                      ( {InputStream.La(2) != REMOVE}? Key=ADD ExprList=expressionList?
                      | {InputStream.La(2) != ADD}?    Key=REMOVE ExprList=expressionList?
                      | Key=(ADD|REMOVE) )
                    ;

                    /*
                      EVENT Foo as STRING
                        ADD
                          statements
                        END ADD
                        REMOVE
                          statements
                        END REMOVE
                      END EVENT
                      but also
                      EVENT Foo as STRING
                        ADD    => ExpressionBody
                        REMOVE => ExpressionBody
                      END EVENT
                    */

eventAccessor       : Attributes=attributes? Modifiers=accessorModifiers?
                      ( Key=ADD     end=eos StmtBlk=statementBlock END ADD?
                      | Key=ADD     UDCSEP ExpressionBody=expression              // New: Expression Body
                      | Key=REMOVE  end=eos StmtBlk=statementBlock END REMOVE?
                      | Key=REMOVE  UDCSEP ExpressionBody=expression              // New: Expression Body
                      )
                      end=eos
                    ;



classvars           : (Attributes=attributes)? Modifiers=classvarModifiers
                      Vars+=classvar (COMMA Vars+=classvar)*
                      eos
                    ;

classvarModifiers   : ( Tokens+=(INSTANCE| STATIC | CONST | INITONLY | PRIVATE | HIDDEN | PROTECTED | PUBLIC | EXPORT | INTERNAL | VOLATILE | UNSAFE | FIXED | NEW) )+
                    ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs

classvar            : (Dim=DIM)? Id=identifier (LBRKT ArraySub=arraysub RBRKT)?
                      (Op=assignoperator Initializer=expression)?
                      (As=(AS | IS) DataType=datatype)?
                    ;

arraysub            : ArrayIndex+=expression (RBRKT LBRKT ArrayIndex+=expression)+		// x][y
                    | ArrayIndex+=expression (COMMA ArrayIndex+=expression)+			// x,y
                    | ArrayIndex+=expression
                    ;

                    /*
                    There are basically 3 variations of properties
                    1) Normal multi line
                    PROPERTY Foo as STRING
                      // Accessors
                    END PROPERTY

                    2) Single Line
                    PROPERTY Foo as STRING GET "42"
                    // You can specify Accessors with single expression.

                    3) Auto property, generates a backing field
                    PROPERTY Foo AS STRING AUTO
                    - You can specify GET/SET/INIT accessor on the same line
                    PROPERTY Foo AS STRING AUTO GET SET

                    They all start with [attributes] [modifiers] PROPERTY (SELF|Id) [params] AS [Type]
                    The variation is after as TYPE:
                    1) AUTO ... (on the same line)
                    2) GET/SET LineAccessorsSingle (on the same line)
                    3) Multi Line Accessors (recognized by an EOS after the type clause)
                    We have merged this in one rule.
                    */

property            : (Attributes=attributes)? (Modifiers=memberModifiers)?
                      P=PROPERTY (ExplicitIface=nameDot)? (Self=SELF | Id=identifier)
                      (ParamList=propertyParameterList)? 
                      (AS Type=datatype)?
                      ( Auto=AUTO (AutoAccessors+=propertyAutoAccessor)* (Op=assignoperator Initializer=expression)? end=EOS	// Auto
                        | (LineAccessors+=propertyLineAccessor)+ end=EOS                     // Single Line
                        | Multi=eos (Accessors+=propertyAccessor)+  END PROPERTY? EOS        // Multi Line
                      )
                    ;

propertyParameterList
                    : L=LBRKT  (Params+=parameter (COMMA Params+=parameter)*)? R=RBRKT
                    // Parentheses are parsed but may generate an error in the future
                    | L=LPAREN (Params+=parameter (COMMA Params+=parameter)*)? R=RPAREN		
                    ;
                    /*
                     Examples of Auto Accessors
                     PROPERTY Foo AS STRING AUTO      // Get/Set
                     PROPERTY Foo AS STRING AUTO GET  // Get only
                     PROPERTY Foo AS STRING AUTO GET SET // Get Set
                     PROPERTY Foo AS STRING AUTO SET  // Set only
                     PROPERTY Foo AS STRING AUTO INIT // Init only
                    */

propertyAutoAccessor: Attributes=attributes? Modifiers=accessorModifiers? Key=(GET|SET|INIT)
                    ;
                    /*
                       Examples of Line Accessors
                       PROPERTY Foo AS STRING GET "42" SET SELF:_Store(value)
                       PROPERTY Foo AS STRING => "42"
                       PROPERTY Foo AS STRING GET SET // no expression, this generates an AUTO property. And warning 9047
                       PROPERTY Foo as STRING SET     // Generates warning 9051 that get accessor is created
                    */

propertyLineAccessor: Attributes=attributes? Modifiers=accessorModifiers?
                      ( {InputStream.La(2) != SET && InputStream.La(2) != INIT}?     Key=(GET|UDCSEP)    Expr=expression?
                      | {InputStream.La(2) != GET && InputStream.La(2) != UDCSEP}?   Key=(SET|INIT)      ExprList=expressionList?
                      | Key=(GET|SET|INIT)
                      )
                    ;

accessorModifiers	: ( Tokens+=(PRIVATE | HIDDEN | PROTECTED | PUBLIC | EXPORT | INTERNAL ) )+
                    ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs

expressionList	    : Exprs+=expression (COMMA Exprs+=expression)*
                    ;

                    /*
                      Example of Multi Line Accessors
                      PROPERTY Foo as STRING
                        GET
                          statements
                        END GET
                        SET
                          statements
                        END SET
                      END PROPERTY
                      but also
                      PROPERTY Foo as STRING
                        GET => ExpressionBody
                        SET => ExpressionBody
                      END PROPERTY
                    */

propertyAccessor    : Attributes=attributes? Modifiers=accessorModifiers?
                      ( Key=GET end=eos StmtBlk=statementBlock END Key2=GET?
                      | Key=GET UDCSEP ExpressionBody=expression              // New: Expression Body
                      | Key=(SET|INIT) end=eos StmtBlk=statementBlock END Key2=(SET | INIT)?
                      | Key=(SET|INIT) UDCSEP ExpressionBody=expression              // New: Expression Body
                      )
                      end=eos
                    ;

classmember         : Member=method                                 #clsmethod
                    | decl=vodeclare                                #clsdeclare
                    | Member=constructor                            #clsctor
                    | Member=destructor                             #clsdtor
                    | Member=classvars                              #clsvars
                    | Member=property                               #clsproperty
                    | Member=operator_                              #clsoperator
                    | Member=structure_                             #nestedStructure
                    | Member=class_                                 #nestedClass
                    | Member=delegate_                              #nestedDelegate
                    | Member=enum_                                  #nestedEnum
                    | Member=event_                                 #nestedEvent
                    | Member=interface_                             #nestedInterface
                    | {IsVO}? Member=vodllmethod                    #clsvodllmethod   //  old style declaration of methods inside external DLL
                    | eos                                           #clseos// Blank Lines between entities
                    ;

constructor         :  (Attributes=attributes)? (Modifiers=constructorModifiers)?
                      c1=CONSTRUCTOR (ParamList=parameterList)? (AS VOID)? // As Void is allowed but ignored
                        (CallingConvention=callingconvention)?
                        (CLASS ClassId=identifier)?
                        (UDCSEP ExpressionBody=expression)?               // New: Expression Body
                        end=eos
                      (Chain=constructorchain)?
                      StmtBlk=statementBlock
                      (END c2=CONSTRUCTOR End=EOS)?
                    ;

constructorchain    : (SELF | SUPER) LPAREN ArgList=argumentList RPAREN eos
                    ;

constructorModifiers: ( Tokens+=( PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | EXTERN | STATIC ) )+
                    ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs

vodeclare           : DECLARE (ACCESS | ASSIGN | METHOD )  (~EOS)+ eos
                    ;

destructor          : (Attributes=attributes)? (Modifiers=destructorModifiers)?
                      d1=DESTRUCTOR (LPAREN RPAREN)?
                      (CLASS ClassId=identifier)?
                      (UDCSEP ExpressionBody=expression)?               // New: Expression Body
                      end=eos
                      StmtBlk=statementBlock
                      (END d2=DESTRUCTOR End=EOS)?
                    ;

destructorModifiers : ( Tokens+=EXTERN )+
                    ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs
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
                      o1=OPERATOR (Operation=overloadedOps | Conversion=conversionOps) Gt=GT?
                      ParamList=parameterList
                      (AS Type=datatype)?
                      (UDCSEP ExpressionBody=expression)?             // New: Expression Body
                      end=eos
                      StmtBlk=statementBlock
                     (END o1=OPERATOR End=EOS)?

                    ;

operatorModifiers   : ( Tokens+=(PUBLIC | STATIC | EXTERN) )+
                    ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs

memberModifiers     : ( Tokens+=(NEW | PRIVATE | HIDDEN | PROTECTED | PUBLIC | EXPORT | INTERNAL | STATIC | VIRTUAL | SEALED | ABSTRACT | ASYNC | UNSAFE | EXTERN | OVERRIDE) )+
                    ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs

attributes          : ( AttrBlk+=attributeBlock )+
                    ;

attributeBlock      : LBRKT Target=attributeTarget? Attributes+=attribute (COMMA Attributes+=attribute)* RBRKT
                    | String=BRACKETED_STRING_CONST
                    ;

attributeTarget     : Token=(ID | CLASS | CONSTRUCTOR | DELEGATE | ENUM | EVENT | FIELD | INTERFACE | METHOD | PROPERTY  | RETURN | STRUCTURE ) COLON
                    ;

attribute           : Name=name (LPAREN (Params+=attributeParam (COMMA Params+=attributeParam)* )? RPAREN )?
                    ;

attributeParam      : Name=identifierName Op=assignoperator Expr=expression     #propertyAttributeParam
                    | Expr=expression                                           #exprAttributeParam
                    ;

globalAttributes    : LBRKT Target=globalAttributeTarget Attributes+=attribute (COMMA Attributes+=attribute)* RBRKT EOS
                    ;

globalAttributeTarget : Token=ID COLON      // We'll Check for ASSEMBLY and MODULE later
                      ;


filewidememvar      : Token=MEMVAR Vars+=identifierName (COMMA Vars+=identifierName)* end=EOS
                    | {!IsFox }? Token=PUBLIC XVars+=memvar[$Token] (COMMA XVars+=memvar[$Token])*  end=EOS 
                    | {IsFox  }? Token=PUBLIC FoxVars+=foxmemvar[$Token] (COMMA FoxVars+=foxmemvar[$Token])*  end=EOS
                    ;

 
statement           : Decl=localdecl                            #declarationStmt
                    | {IsFox}? Decl=foxlparameters              #foxlparametersStmt    // LPARAMETERS
                    | Decl=localfuncproc                        #localFunctionStmt
                    | {!IsFox && HasMemVars}? Decl=memvardecl   #memvardeclStmt  // Memvar declarations, not for FoxPro
                    | Decl=fielddecl                            #fieldStmt
                    | {IsFox && HasMemVars}?  Decl=foxmemvardecl #foxmemvardeclStmt    // Memvar declarations FoxPro specific
                    | {IsFox}? Decl=foxdimvardecl                #foxdimvardeclStmt        // DIMENSION this.Field(10)
                    | DO? w=WHILE Expr=expression end=eos
                      StmtBlk=statementBlock
                      (e=END (DO|WHILE)? | e=ENDDO) eos	#whileStmt
                    | NOP (LPAREN RPAREN )? end=eos					#nopStmt

                    | f=FOR
                        ( AssignExpr=expression
                        | (LOCAL? ForDecl=IMPLIED | ForDecl=VAR) ForIter=identifier Op=assignoperator Expr=expression
                        | ForDecl=LOCAL ForIter=identifier Op=assignoperator Expr=expression (AS Type=datatype)?
                        )
                      Dir=(TO | UPTO | DOWNTO) FinalExpr=expression
                      (STEP Step=expression)? end=eos
                      StmtBlk=statementBlock
                      (e = NEXT | e = END FOR) eos	                  #forStmt

                    | i=IF IfBlocks += condBlock[$i]
                      (e=ELSEIF IfBlocks += condBlock[$e])*
                      (el=ELSE eos ElseStmtBlk=statementBlock)? 
                      (e=END IF? | e=ENDIF)   eos                      #ifStmt

                    | DO CASE end=eos
                      (c=CASE CaseBlocks +=condBlock[$c])*
                      (oth=OTHERWISE end=eos OtherwiseStmtBlk=statementBlock)?
                      (e=END CASE? | e=ENDCASE)   eos                   #caseStmt

                    | Key=EXIT end=eos                                  #jumpStmt
                    | Key=LOOP end=eos                                  #jumpStmt
                    | Key=BREAK Expr=expression? end=eos                #jumpStmt
                    | R=RETURN (Void=VOID|Expr=expression)? end=eos     #returnStmt
                    | Q=(QMARK | QQMARK)
                       (Exprs+=expression (COMMA Exprs+=expression)*)?
                       end=eos                                          #qoutStmt

                    | BEGIN SEQUENCE end=eos
                      StmtBlk=statementBlock
                      (RECOVER RecoverBlock=recoverBlock)?
                      (F=FINALLY eos FinBlock=statementBlock)?
                      e=END (SEQUENCE)? eos                             #seqStmt
                    //
                    // New in Vulcan 
                    //
                    | r=REPEAT end=eos
                      StmtBlk=statementBlock
                      UNTIL Expr=expression
                      eos                                               #repeatStmt

                    | f=FOREACH
                      a=AWAIT?
                      ( V=IMPLIED Id=varidentifier
                      | Id=varidentifier (AS Type=datatype)?
                      | V=VAR Id=varidentifier
                      )
                      IN Container=expression end=eos
                      StmtBlk=statementBlock
                      (e=NEXT |e=END FOR) eos	                          #foreachStmt

                    | Key=THROW Expr=expression? end=eos                #jumpStmt

                    | T=TRY end=eos StmtBlk=statementBlock
                      (CATCH CatchBlock+=catchBlock?)*
                      (F=FINALLY eos FinBlock=statementBlock)?
                      e=END TRY? eos								                    #tryStmt

                    | BEGIN Key=LOCK Expr=expression end=eos
                      StmtBlk=statementBlock
                      e=END LOCK? eos						                        #blockStmt
                    //
                    // New XSharp Statements
                    //
                    | Y=YIELD R=RETURN (VOID | Expr=expression)? end=eos  #yieldStmt
                    | Y=YIELD Break=(BREAK|EXIT) end=eos							    #yieldStmt
                    | (BEGIN|DO)? S=SWITCH Expr=expression end=eos
                      (SwitchBlock+=switchBlock)+
                      e=END SWITCH? eos					                          #switchStmt
                    | BEGIN Key=USING ( Expr=expression | VarDecl=variableDeclaration ) end=eos
                        StmtBlk=statementBlock
                      e=END USING? eos						                        #blockStmt
                    | BEGIN Key=FIXED ( VarDecl=variableDeclaration ) end=eos
                      StmtBlk=statementBlock
                      e=END FIXED? eos						                        #blockStmt

                    | WITH Expr=expression (As=AS DataType=datatype foxclasslib?  )?  end=eos
                      StmtBlk=statementBlock
                      e=END WITH? eos                                  #withBlock

                    | BEGIN Key1=blockTokens end=eos
                      StmtBlk=statementBlock
                      e=END (Key2=blockTokens)? eos	                      #blockStmt

                    // some statements that are only valid in FoxPro dialect
                    | Eq=EQ Exprs+=expression  end=eos		                          #foxexpressionStmt
                    | B=(BACKSLASH | BACKBACKSLASH) String=TEXT_STRING_CONST end=EOS  #foxtextoutStmt
                    | D=DO Amp=AMP? Id=varidentifierName (WITH ArgList=argumentList)?  end=eos      #doStmt

                      // NOTE: The ExpressionStmt rule MUST be last, even though it already existed in VO
                      // validExpressionStmt check  for CONSTRUCTOR( or DESTRUCTOR(
                    | {validExpressionStmt()}? Exprs+=expression (COMMA Exprs+=expression)*  end=eos  #expressionStmt
	                ;

blockTokens          : Token=(SCOPE|CHECKED|UNCHECKED|UNSAFE)
                     ;

condBlock[IToken st] : Cond=expression Then=THEN? end=eos StmtBlk=statementBlock
                         ;


// Note that literalValue is not enough. We also need to support members of enums
switchBlock         : (
                      Key=CASE Const=expression (W=WHEN whenexpr=expression)?
                    | Key=CASE Id=varidentifier AS DataType=datatype (W=WHEN whenexpr=expression)?
                    | Key=OTHERWISE
                    )
                    end=eos StmtBlk=statementBlock
                    ;

catchBlock          : (TO)? Id=varidentifier? (AS Type=datatype)? (W=WHEN whenexpr=expression)? end=eos StmtBlk=statementBlock
                    ;

recoverBlock        : (USING Id=varidentifier)? end=eos StmtBlock=statementBlock
                    ;

variableDeclaration	: (LOCAL? Var=IMPLIED | Var=VAR) Decl+=variableDeclarator (COMMA Decl+=variableDeclarator)*
                    | LOCAL Decl+=variableDeclarator (COMMA Decl+=variableDeclarator)* (AS Type=datatype)?
                    ;

variableDeclarator  : Id=varidentifier Op=assignoperator Expr=expression
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

// FoxPro allows LOCAL M.Name The M is only lexed in FoxPro dialect
// We parse the M. Prefix in the localvar rule but ignore it, it is not relevant here.

localdecl          : LOCAL (Static=STATIC)? LocalVars+=localvar (COMMA LocalVars+=localvar)*			end=eos #commonLocalDecl
                   | Static=STATIC LOCAL    LocalVars+=localvar (COMMA LocalVars+=localvar)*			end=eos #commonLocalDecl
                   | {!XSharpLexer.IsKeyword(InputStream.La(2))}?   // STATIC Identifier , but not STATIC <Keyword>
                     Static=STATIC          LocalVars+=localvar (COMMA LocalVars+=localvar)*			end=eos #commonLocalDecl
                   // The following rules allow STATIC in the parser,
                   // but the treetransformation will produce an error 9044 for STATIC implied
                   | Static=STATIC? VAR           ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*   end=eos #varLocalDecl
                   | Static=STATIC LOCAL? IMPLIED ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*   end=eos #varLocalDecl
                   | LOCAL Static=STATIC? IMPLIED ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*   end=eos #varLocalDecl
                   | Using=USING Static=STATIC? VAR ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)* end=eos #varLocalDecl
                   | Using=USING Static=STATIC? LOCAL? IMPLIED ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*  end=eos #varLocalDecl
                   | ( VAR | LOCAL? IMPLIED ) Designation=designationExpr
                      Op=assignoperator Expression=expression end=eos                                                #varLocalDesignation // VAR ( ID[, ID, ...] ) := EXPR
                   | LOCAL DesignationType=designationTypeExpr
                      Op=assignoperator Expression=expression end=eos                                                #typeLocalDesignation // LOCAL ( ID AS TYPE[,ID AS TYPE,...] ) := EXPR
                   ;

localvar           : (Const=CONST)? ( Dim=DIM )? Id=varidentifier (LBRKT ArraySub=arraysub RBRKT)?
                     (Op=assignoperator Expression=expression)?
                     (As=(AS | IS) DataType=datatype foxclasslib?  )?
                     // FoxPro: LOCAL arrayName(10) as array.
                     // does not support CONST, DIM and Initializer
                   | {IsFox}? Id=varidentifier LPAREN ArraySub=arraysub RPAREN
                     (Op=assignoperator Expression=expression)?
                     (As=(AS | IS) DataType=datatype foxclasslib?  )?
                   ;

impliedvar         : (Const=CONST)? Id=varidentifier Op=assignoperator Expression=expression
                   ;


fielddecl          : FIELD Fields+=identifierName (COMMA Fields+=identifierName)* (IN Alias=identifierName)? end=eos
                   ;

                    // Old Style xBase declarations
                    // FoxPro allows PRIVATE M.Name The M is only lexed in the FoxPro dialect. The varidentifierrule has the M DOT clause
                    //  Not for FoxPro !
                    //  NOTE: The parent rule already filters out so this is not called when MEMVARS are not enabled
                    // This is only the list of names
                      // This includes the optional initializer or array dimension
memvardecl         : T=(MEMVAR|PARAMETERS|PRIVATE|PUBLIC)
                      Vars+=memvar[$T] (COMMA Vars+=memvar[$T] )* end=eos  // MEMVAR  Foo, Bar
                   ;

// For the variable list for Private and Public
memvar[IToken T]  : (Amp=AMP)?  Id=varidentifierName (LBRKT ArraySub=arraysub RBRKT)? (Op=assignoperator Expression=expression)?
                   ;


//
// Only for the FoxPro dialect
// DIMENSION  may have AS Type clause and either parens or brackets (see DimensionVar)
// The parent rule already filters out so this is not called when MEMVARS are not enabled

                                        
foxmemvardecl       :  T=( MEMVAR |PARAMETERS | PRIVATE | PUBLIC ) FoxVars+=foxmemvar[$T]  (COMMA FoxVars+=foxmemvar[$T])*  end=eos 
                    ;

                    // This includes array indices and optional type per name
foxdimvardecl       :  T=(DIMENSION | DECLARE )  DimVars += foxdimvar[$T] (COMMA DimVars+=foxdimvar[$T])* end=eos
                    // This has names and dimensions
                    |  T=PUBLIC (ARRAY)? DimVars += foxdimvar[$T] (COMMA DimVars+=foxdimvar[$T])*    end=eos
                    |  T=LOCAL ARRAY     DimVars += foxdimvar[$T] (COMMA DimVars+=foxdimvar[$T])*    end=eos
                    ;


                    // This includes array indices and optional type per name
foxlparameters      : T=LPARAMETERS LParameters+=foxlparameter[$T] (COMMA LParameters+=foxlparameter[$T] )* end=eos
                    // This has names and optional ampersands
                    ;

                    // FoxPro dimension statement allows the AS Type per variable name
                    // AS Type OF ClassLib is ignored in FoxPro too, except when calling COM components
                    // second variation to redim class members
foxdimvar[IToken T]  : (Amp=AMP)? Id=varidentifierName
                        ( LBRKT  Dims+=expression (COMMA Dims+=expression)* RBRKT
                        | LPAREN Dims+=expression (COMMA Dims+=expression)* RPAREN )
                        XT=foxtypedecl?
                    | Expr=expression 
                        ( LBRKT  Dims+=expression (COMMA Dims+=expression)* RBRKT
                        | LPAREN Dims+=expression (COMMA Dims+=expression)* RPAREN )
                        XT=foxtypedecl?
                    ;
                    
foxclasslib        : Of=OF ClassLib=identifierName
                   ;

foxlparameter[IToken T] : Name=varidentifierName XT=foxtypedecl?
                        ;

                      // parsed but ignored . FoxPro uses this only for intellisense. We can/should do that to in the editor
foxtypedecl         : As=AS Type=datatype foxclasslib?
                    ;

                       // For the variable list for Private and Public
                       // We have added the initializer that FoxPro does not have
foxmemvar[IToken T] : (Amp=AMP)?  Id=varidentifierName
                      (Op=assignoperator Expression=expression)?
                      XT=foxtypedecl?  // is ignored in FoxPro too
                   ;

localfuncproc       :  (Modifiers=localfuncprocModifiers)?
                        LOCAL T=funcproctype Sig=signature
                        end=eos
                        StmtBlk=statementBlock
                        END T2=funcproctype  EOS
                     ;

localfuncprocModifiers : ( Tokens+=(UNSAFE | ASYNC) )+
                       ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs


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


assignoperator      : Op = (ASSIGN_OP | EQ)
                    ;

expression          : Expr=expression Op=(DOT|COLON) Name=simpleName          #accessMember           // member access.
                    | Op=(DOT|COLON|COLONCOLON)     Name=simpleName           #accessMember            // XPP & Harbour SELF member access or inside WITH
                    | Left=expression Op=(DOT|COLON) LPAREN Rigth=expression RPAREN #accessMemberWith // member access with left expression.
                    // Latebound member access with a ampersand and a name or an expression that evaluates to a string
                    | Left=expression Op=(DOT|COLON) AMP 
                      ( Name=identifierName | LPAREN Right=expression RPAREN)  #accessMemberLate   // aa:&Name  Expr must evaluate to a string which is the ivar name
                    | Op=(DOT|COLON|COLONCOLON) AMP 
                        ( Name=identifierName | LPAREN Right=expression RPAREN) #accessMemberLate   // .&Name  XPP & Harbour Late member access or inside WITH
                    | Expr=expression LPAREN ArgList=argumentList RPAREN        #methodCall             // method call, params
                    | XFunc=xbaseFunc LPAREN ArgList=argumentList RPAREN        #xFunctionExpression    // Array(...) or Date(...) params
                    | Expr=expression LBRKT ArgList=bracketedArgumentList RBRKT #arrayAccess            // Array element access
                    | Left=expression Op=QMARK Right=boundExpression            #condAccessExpr         // expr ? expr
                    // The IsTypeCastAllowed function prevents the following from being seen as a typecast on an expression inside a with block: (n):ToString().
                    // it checks for a DOT or COLON after the RPAREN. When it finds that then IsTypeCastAllowed() return false.
                    | {IsTypeCastAllowed() }? LPAREN Type=datatype RPAREN Expr=expression  #typeCast    // (typename) expr
                    | Expr=expression Op=(INC | DEC)                            #postfixExpression      // expr ++/--
                    | Op=AWAIT Expr=expression                                  #awaitExpression        // AWAIT expr
                    // The predicate prevents STACKALLOC(123) from being parsed as a STACKALLOC <ParenExpression>
                    | {InputStream.La(2) != LPAREN }? Op=STACKALLOC Expr=expression  #stackAllocExpression   // STACKALLOC expr 
                    | Op=(PLUS | MINUS | TILDE| ADDROF | INC | DEC | EXP) Expr=expression #prefixExpression   // +/-/~/&/++/-- expr
                    | Expr=expression Op=IS Type=datatype (VAR Id=varidentifier)? #typeCheckExpression    // expr IS typeORid [VAR identifier]
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
                    | Op=(LOGIC_NOT|NOT|FOX_NOT) Expr=expression                #prefixExpression       // .not. expr (logical not)  also  !
                    | Left=expression Op=(LOGIC_AND |AND |FOX_AND) Right=expression #binaryExpression       // expr .and. expr (logical and) also &&
                    | Left=expression Op=(LOGIC_XOR |FOX_XOR) Right=expression  #binaryExpression       // expr .xor. expr (logical xor)
                    | Left=expression Op=(LOGIC_OR |OR|FOX_OR) Right=expression #binaryExpression       // expr .or. expr (logical or)  also ||
                    | Left=expression Op=(DEFAULT|QQMARK) Right=expression               #binaryExpression       // expr DEFAULT expr
                    | <assoc=right> Left=expression
                      Op=( ASSIGN_OP | ASSIGN_ADD | ASSIGN_SUB | ASSIGN_EXP
                            | ASSIGN_MUL | ASSIGN_DIV | ASSIGN_MOD
                            | ASSIGN_BITAND | ASSIGN_BITOR | ASSIGN_LSHIFT
                            | ASSIGN_RSHIFT | ASSIGN_XOR | ASSIGN_QQMARK)
                      Right=expression                                          #assignmentExpression	// expr := expr, also expr += expr etc.
                    | Expr=primary                                              #primaryExpression
                    ;

                    // Primary expressions
                    // Note: No need to check for extra ) } or ] tokens. The expression rule does that already
primary             : Key=SELF                                                  #selfExpression
                    | Key=SUPER                                                 #superExpression
                    | Literal=literalValue                                      #literalExpression		// literals
                    | Literal=parserLiteralValue                                #parserLiteralExpression		// literals created by the preprocessor
                    | LiteralArray=literalArray                                 #literalArrayExpression	// { expr [, expr] }
                    | AnonType=anonType                                         #anonTypeExpression		// CLASS { id := expr [, id := expr] }
                    | TupleExpr=tupleExpr                                       #tupleExpression      // TUPLE { id := expr [, id := expr] }
                    | CbExpr=codeblock                                          #codeblockExpression	// {| [id [, id...] | expr [, expr...] }
                    | AnoExpr=anonymousMethodExpression                         #codeblockExpression	// DELEGATE (x as Foo) { DoSomething(Foo) }
                    | Query=linqQuery                                           #queryExpression        // LINQ
                    | {ExpectToken(LCURLY)}? Type=datatype LCURLY Obj=expression COMMA
                      ADDROF Func=name LPAREN RPAREN RCURLY                     #delegateCtorCall		// delegate{ obj , @func() }
                    | {ExpectToken(LCURLY)}? Type=datatype LCURLY ArgList=argumentList  RCURLY
                                                   Init=objectOrCollectioninitializer?  #ctorCall				// id{ expr [, expr...] } with optional { Name1 := Expr1, [Name<n> := Expr<n>]}
                    | ch=(CHECKED|UNCHECKED) LPAREN Expr=expression  RPAREN     #checkedExpression		// checked( expression )
                    | TYPEOF LPAREN Type=datatype RPAREN                        #typeOfExpression		// typeof( typeORid )
                    | SIZEOF LPAREN Type=datatype RPAREN                        #sizeOfExpression		// sizeof( typeORid )
                    | DEFAULT LPAREN Type=datatype RPAREN                       #defaultExpression		// default( typeORid )
                    | Name=simpleName                                           #nameExpression			// generic name
                    | {ExpectToken(LPAREN)}? Type=nativeType LPAREN Expr=expression RPAREN             #voConversionExpression	// nativetype( expr )
                    | {ExpectToken(LPAREN)}? XType=xbaseType LPAREN Expr=expression RPAREN             #voConversionExpression	// xbaseType( expr )
                    | {ExpectToken(LPAREN)}? Type=nativeType LPAREN CAST COMMA Expr=expression RPAREN  #voCastExpression		// nativetype(_CAST, expr )
                    | {ExpectToken(LPAREN)}? XType=xbaseType LPAREN CAST COMMA Expr=expression RPAREN  #voCastExpression		// xbaseType(_CAST, expr )
                    | CASTCLASS LPAREN Type = nativeType COMMA Expr=expression RPAREN #voCastExpression		// __CastClass( nativetype, expression)
                    | CASTCLASS LPAREN XType = xbaseType COMMA Expr=expression RPAREN #voCastExpression		// __CastClass( xbaseType, expression)
                    | PTR LPAREN Type=datatype COMMA Expr=expression RPAREN     #voCastPtrExpression	// PTR( typeName, expr )
                    | Name=usualTypeName                                        #usualTypeNameExpression	// LONG, STRING etc., used as NUMERIC in expressions
                    | Type=typeName                                             #typeExpression			// Standard DotNet Types
                    | Expr=iif                                                  #iifExpression			// iif( expr, expr, expr )
                    |  Op=(BIT_AND | BIT_OR | BIT_XOR | BIT_NOT) LPAREN Exprs+=expression
                      (COMMA Exprs+=expression)* RPAREN                         #intrinsicExpression	// _Or(expr, expr, expr)
                    | {ExpectToken(ALIAS)}? Expr=aliasExpression                                      #aliasedExpression    // Handles all expressions with the ALIAS operator
                    | AMP LPAREN Expr=expression RPAREN                         #macro					      // &(expr)          // parens are needed because otherwise &(string) == Foo will match everything until Foo
                    | AMP Name=identifierName                                   #macroName			      // &name            // macro with a variable name
                    | LPAREN Exprs+=expression (COMMA Exprs+=expression)* RPAREN #parenExpression		// ( expr[,expr,..] )
                    | Key=ARGLIST                                               #argListExpression		// __ARGLIST
                    ;

boundExpression		  : Expr=boundExpression Op=(DOT | COLON) Name=simpleName             #boundAccessMember	// member access The ? is new
                    | Expr=boundExpression LPAREN ArgList=argumentList RPAREN           #boundMethodCall	// method call, with params
                    | Expr=boundExpression LBRKT ArgList=bracketedArgumentList RBRKT    #boundArrayAccess	// Array element access
                    | <assoc=right> Left=boundExpression Op=QMARK Right=boundExpression #boundCondAccessExpr	// expr ? expr
                    | Op=(DOT | COLON) Name=simpleName                                  #bindMemberAccess
                    | LBRKT ArgList=bracketedArgumentList RBRKT                         #bindArrayAccess
                    ;

aliasExpression     : {HasMemVars}? MEMVAR ALIAS VarName=identifier                           #aliasedMemvar        // MEMVAR->Name
                    | FIELD ALIAS (Alias=identifier ALIAS)? Field=identifier    #aliasedField		      // _FIELD->CUSTOMER->NAME
                    | {InputStream.La(4) != LPAREN}?                            // this makes sure that CUSTOMER->NAME() is not matched, this is matched by aliasedExpr later
                      Alias=identifier ALIAS Field=identifier                   #aliasedField		      // CUSTOMER->NAME
                    // The next rule makes sure that single fields are processed before expressions.
                    // otherwise the following would not parse properly.
                    // DbSeek((nArea)->Field1 + (nArea)->Field2)
                    // The seek expression would be seen as  "(nArea)->(Field1+(nArea)->Field2)"
                    | {InputStream.La(6) != LPAREN}?
                      LPAREN Area=identifier RPAREN ALIAS Field=identifier      #aliasedField		      // (nCust)->NAME
                    | Alias=identifier              ALIAS AMP Field=expression  #aliasedFieldLate	    // CUSTOMER->&fldName
                    | FIELD ALIAS (Alias=identifier ALIAS)? AMP Field=expression #aliasedFieldLate	  // _FIELD->CUSTOMER->&fldName or _FIELD->&fldName
                    | LPAREN Area=identifier RPAREN ALIAS AMP Field=expression  #aliasedFieldLate	  // (nCust)->&fldName
                    // Note that the LPAREN .. RPAREN has been added to make sure that the expression does not match the Paren Expression
                    // !(CUSTOMER)->(Eof()) .and. SomeOtherCondition
                    // Because the subrule with LPAREN and RPAREN is listed first then only Eof() will be evaluated in the workarea.
                    | ( Id=identifier | LPAREN Alias=expression RPAREN)
                       ALIAS ( (LPAREN Expr=expression RPAREN) | Expr=expression )  #aliasedExpr          // id -> expr   or (expr) -> expr
                    ;

// Initializers

objectOrCollectioninitializer : ObjInit=objectinitializer
                              | CollInit=collectioninitializer
                              ;

objectinitializer   : LCURLY (Members+=memberinitializer (COMMA Members+=memberinitializer)*)? RCURLY
                    ;

memberinitializer   : Name=identifierName Op=assignoperator Expr=initializervalue
                    ;

initializervalue    : Init=objectOrCollectioninitializer // Put this first to make sure we are not matching a literal array for { expr [, expr] }
                    | Expr=expression
                    ;

complexInitExpr     : LCURLY Members+=initializerMember (COMMA Members+=initializerMember)* RCURLY
                    ;

initializerMember   : Init=complexInitExpr
                    | Expr=expression
                    ;

collectioninitializer : LCURLY Members+=initializerMember (COMMA Members+=initializerMember)* RCURLY
                      ;

bracketedArgumentList : Args+=unnamedArgument (COMMA Args+=unnamedArgument)* 
                      ;

                      // NOTE: Separate rule for bracketedarguments because they cannot use identifierName syntax
unnamedArgument     :  Expr=expression
                    ;

                    // NOTE: Optional argumentlist is handled in the rules that use this rule
argumentList        :  Args+=namedArgument (COMMA Args+=namedArgument)*
                    ;

                    // NOTE: Expression is optional so we can skip arguments for VO/Vulcan compatibility
namedArgument       :  {AllowNamedArgs}? Name=identifierName Op=ASSIGN_OP  ( RefOut=(REF | OUT) )? Expr=expression
                    |   RefOut=OUT Var=VAR Id=varidentifier
                    |   RefOut=OUT Id=varidentifier AS Type=datatype
                    |   RefOut=OUT Null=NULL
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

varidentifier       : (FOX_M DOT)? Id=identifier
                    ;

varidentifierName   : (FOX_M DOT)? Id=identifierName
                    ;

datatype            : ARRAY OF TypeName=typeName                                    #arrayOfType
                    | TypeName=typeName PTR                                         #ptrDatatype
                    | TypeName=typeName (Ranks+=arrayRank)+                         #arrayDatatype
                    | TypeName=typeName                                             #simpleDatatype
                    | TypeName=typeName QMARK                                       #nullableDatatype
                    | TupleType=tupleType                                           #tupleDatatype
                    ;

arrayRank           : LBRKT (Commas+=COMMA)* RBRKT
                    | String=BRACKETED_STRING_CONST
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

anonMember          : Name=identifierName Op=assignoperator Expr=expression
                    | Expr=expression
                    ;

// Tuples

tupleType           : TUPLE LCURLY (Elements+=tupleTypeElement (COMMA Elements+=tupleTypeElement)*)? RCURLY
                    ;

tupleTypeElement    : (identifierName AS)? datatype
                    ;

tupleExpr           : TUPLE LCURLY (Args+=tupleExprArgument (COMMA Args+=tupleExprArgument)*)? RCURLY
                    ;

tupleExprArgument   : Name=identifierName Op=assignoperator Expr=expression
                    | Expr=expression
                    ;

designationExpr     : LPAREN Ids+=varidentifier (COMMA Ids+=varidentifier)* RPAREN
                    ;

designationTypeExpr : LPAREN Locals+=localDesignation (COMMA Locals+=localDesignation)* RPAREN
                    ;

localDesignation    : Id=varidentifier AS Type=datatype
                    ;

// Codeblocks & Lambda Expressions

codeblock         : LCURLY Or=OR lambda=UDCSEP? Code=codeblockCode RCURLY                                                   // Lambda char will trigger warning
                  | LCURLY P1=PIPE LambdaParamList=lambdaParameterList? P2=PIPE lambda=UDCSEP? Code=codeblockCode RCURLY    // Lambda char will trigger warning
                  | LCURLY LambdaParamList=lambdaParameterList? lambda=UDCSEP Code=codeblockCode RCURLY                     // True Lambda expression
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

explicitAnonymousFunctionParameter      : Id=identifier Mod=parameterDeclMods Type=datatype
                                        ;


// LINQ Support

linqQuery           : From=fromClause Body=queryBody
                    ;

fromClause          : FROM Id=identifier (AS Type=typeName)? IN Expr=expression
                    ;

queryBody           : (Bodyclauses+=queryBodyClause)* SorG=selectOrGroupclause (Continuation=queryContinuation)?
                    ;

queryBodyClause     : From=fromClause                                                                   #fromBodyClause
                    | L=LET Id=identifier Op=assignoperator Expr=expression                              #letClause
                    | W=WHERE Expr=expression                                                            #whereClause        // expression must be Boolean
                    | J=JOIN Id=identifier (AS Type=typeName)?
                      I=IN Expr=expression O=ON OnExpr=expression E=EQUALS EqExpr=expression
                      Into=joinIntoClause?                                                               #joinClause
                    | O=ORDERBY Orders+=ordering (COMMA Orders+=ordering)*                               #orderbyClause
                    ;

joinIntoClause      : I=INTO Id=identifier
                    ;

ordering            : Expr=expression Direction=(ASCENDING|DESCENDING)?
                    ;

selectOrGroupclause : S=SELECT Expr=expression                                #selectClause
                    | G=GROUP Expr=expression B=BY ByExpr=expression          #groupClause
                    ;

queryContinuation   : I=INTO Id=identifier Body=queryBody
                    ;
// -- End of LINQ


// All New Vulcan and X# keywords can also be recognized as Identifier
identifier          : ID            // No rule names, we use the Start property to access the token
                    | keywordxs
                    | keywordxpp
                    | keywordfox
                    | {!IsCoreVO}? xbaseType     // VO, Vulcan and Core allow TYPE(something)
                    | {!IsCoreVO}? nativeType    // VO, Vulcan and Core allow TYPE(something)
                    ;

identifierString    : ID            // No rule names, we use the Start property to access the token
                    | STRING_CONST
                    | keywordxs
                    | keywordxpp
                    | keywordfox
                    | xbaseType    
                    | nativeType   
                    ;


// xBase types that are also available as runtime function
xbaseFunc           : Token=	// Aphabetical order
                    ( ARRAY | DATE | DATETIME)
                    ;

// xBaseTypes are NOT available in the Core dialect and therefore separated here.
xbaseType           : Token=	// Aphabetical order
                    ( ARRAY
                    | BINARY
                    | CODEBLOCK
                    | CURRENCY
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
                    | NINT
                    | NUINT
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
                    | BRACKETED_STRING_CONST
                    | SYMBOL_CONST
                    | HEX_CONST
                    | BIN_CONST
                    | BINARY_CONST
                    | REAL_CONST
                    | INT_CONST
                    | INVALID_NUMBER
                    | DATE_CONST
                    | DATETIME_CONST
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
                    | NULL_FOX )
                    ;

                    // The following rule matches DateTime literals that are the result of a preprocessor rule
parserLiteralValue  : Year=INT_CONST DOT Month=INT_CONST DOT Day=INT_CONST
                    | LCURLY EXP Year=INT_CONST MINUS Month=INT_CONST MINUS Day=INT_CONST
                      ( Hours=INT_CONST (COLON Minutes=INT_CONST (COLON Seconds=INT_CONST)?)? )?
                      .*? RCURLY
                    ;
                    

keywordvo           : Token=(ACCESS | AS | ASSIGN | BEGIN | BREAK | CASE | CAST | CLASS | DLL | DO
                    | ELSE | ELSEIF | END | ENDCASE | ENDDO | ENDIF | EXIT | EXPORT | FOR | FUNCTION
                    | HIDDEN | IF | IIF | IS | LOCAL | LOOP | MEMBER | METHOD | NEXT | OTHERWISE
                    | PRIVATE | PROCEDURE | PROTECTED | PTR | PUBLIC | RECOVER | RETURN | SELF| SIZEOF | SUPER
                    | TYPEOF | WHILE | TRY | BIT_AND | BIT_NOT | BIT_OR | BIT_XOR
                    // The following new keywords cannot be in the keywordxs list because it will match an expression when used on their own
                    | REPEAT | CONSTRUCTOR | CATCH | DESTRUCTOR | FINALLY
                    )
                    ;


keywordxs           : Token=(AUTO | CHAR | CONST |  DEFAULT | GET | IMPLEMENTS | NEW | OUT | REF | SET |  VALUE | VIRTUAL | INTERNAL
                    // The following did not exist in Vulcan
                    | ADD | ARGLIST | ASCENDING | ASTYPE | ASYNC | AWAIT | BY | CHECKED | DESCENDING | DYNAMIC | EQUALS | EXTERN | FIXED | FROM
                    | GROUP | INIT | INTO | JOIN | LET | NAMEOF | OF | ON | ORDERBY | OVERRIDE |PARAMS | REMOVE
                    | SELECT | STACKALLOC | UNCHECKED | VAR | VOLATILE | WHEN | WHERE | BINARY | CHAR | CURRENCY | DECIMAL | DATETIME | NINT | NUINT
                    // Added as XS keywords to allow them to be treated as IDs
                    // the following entity keywords will be never used 'alone' and can therefore be safely defined as identifiers
                    | DELEGATE | ENUM | GLOBAL | INHERIT | STRUCTURE
                    // The following 'old' keywords are never used 'alone' and are harmless as identifiers
                    | ALIGN | CALLBACK | CLIPPER  | DIM | DOWNTO | DLLEXPORT
                    | FASTCALL | IN | INIT1 | INIT2 | INIT3 | INSTANCE | PASCAL |  SEQUENCE
                    | STEP | STRICT | TO | THISCALL | TUPLE |  UPTO | USING | WINCALL
                    // The following keywords are handled in the fixPositionalKeyword() method of the lexer and will only be keywords at the right place
                    // but when they code event->(DoSomething()) we still need them in this rule...
                    | DEFINE | TRY | SWITCH | EVENT| EXPLICIT | FIELD | FOREACH | UNTIL | PARAMETERS | YIELD | MEMVAR | NOP
                    | PARTIAL | SEALED | ABSTRACT | UNSAFE | SCOPE | NAMESPACE | LOCK | IMPLICIT | IMPLIED | INITONLY | PROPERTY | INTERFACE
                    | VOSTRUCT | UNION | DECLARE | OPERATOR
                    )
                    ;

/// XBase++ Parser definities

keywordxpp         : Token=(SHARING| SHARED| ASSIGNMENT| EXPORTED| READONLY| NOSAVE )
                   ;
                   // context sensitive keywords
                   // ENDCLASS, FREEZE, FINAL, INTRODUCE, SYNC, DEFERRED, INLINE



xppclass           :  Attributes=attributes?                                // NEW Optional Attributes
                      Modifiers=xppclassModifiers?                          // [STATIC|FREEZE|FINAL]
                       C=CLASS (Namespace=nameDot)? Id=identifier           // CLASS <ClassName>
                       TypeParameters=typeparameters?                       // Optional Type Parameters
                       (
                          From=(FROM| SHARING) BaseTypes+=datatype (COMMA BaseTypes+=datatype)*  // [FROM <SuperClass,...>] ;
                       )?                                                                   // [SHARING <SuperClass,...>]
                       (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)? // NEW Implements
                       (ConstraintsClauses+=typeparameterconstraintsclause)*             // Optional typeparameterconstraints for Generic Class
                      e=eos
                      Members+=xppclassMember*
                      ENDCLASS
                      eos
                    ;

xppclassModifiers   : ( Tokens+=(STATIC | FREEZE | FINAL | SEALED | ABSTRACT) )+
                    ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs

xppclassMember      : Member=xppmethodvis                           #xppclsvisibility
                    | Member=xppclassvars                           #xppclsvars
                    | Member=xppinlineMethod                        #xppclsinlinemethod
                    | Member=xppdeclareMethod                       #xppclsdeclaremethod
                    | Member=xppdeclareproperty                     #xppclsproperty
                    ;

xppmethodvis        : Vis=xppvisibility COLON eos
                    ;

xppvisibility       : Token=(HIDDEN | PROTECTED | EXPORTED | INTERNAL | PUBLIC | PRIVATE ) // The first 3 are XPP. The others are X# extensions/synonyms
                    ;

xppdeclareMethod    : Attributes=attributes?                                  // NEW Optional Attributes
                      Modifiers=xppmemberModifiers?                           // [DEFERRED |FINAL | INTRODUCE | OVERRIDE] [CLASS]
                      METHOD Methods+=identifier xppdeclmethodparams?         // METHOD <MethodName,...>
                      (
                          Is=xppisin                                          //  [IS <Name>] [IN <SuperClass>]
                          | (COMMA Methods+=identifier xppdeclmethodparams?)* // or an optional comma seperated list of other names
                      )
                      eos
                    ;

xppdeclmethodparams : LPAREN (identifier  (COMMA identifier)*)? RPAREN      // ( id, ..)  THis clause is recognized but ignored
                    ;


xppisin             : (IS identifier)? (IN identifier)?                     //  IS <Name> [IN <SuperClass>] = ignored
                    ;


xppmemberModifiers : ( Tokens+=( DEFERRED | FINAL | INTRODUCE | OVERRIDE | CLASS | SYNC | STATIC // Xbase++ modifiers
                        | ABSTRACT | NEW | ASYNC | UNSAFE | EXTERN |VIRTUAL) )+ // Our modifiers. ABSTRACT = DEFERRED, NEW = INTRODUCE, SEALED = FINAL
                    ; // make sure all tokens are also in the IsModifier method inside XSharpLexerCode.cs


xppclassvars        : Modifiers=xppmemberModifiers?                               // [CLASS | STATIC]
                      VAR Vars+=identifier                                        // VAR <VarName>
                      (
                        Is=xppisin                                                // [IS <Name>] [IN <SuperClass>]
                        | ((COMMA Vars+=identifier)*                              // <,...>
                        (AS DataType=datatype)?  )                                // Optional data type

                      )
                      Shared=SHARED?                                            // [SHARED]
                      ReadOnly=READONLY?                                        // [READONLY]
                      Assignment=xppvarassignment?                              // [ASSIGNMENT HIDDEN | PROTECTED | EXPORTED]
                      Nosave= NOSAVE?                                           // [NOSAVE]
                      eos
                    ;


xppvarassignment    : ASSIGNMENT xppvisibility                                    // [ASSIGNMENT HIDDEN | PROTECTED | EXPORTED]
                    ;
xppdeclareproperty  : Attributes=attributes?                                      // NEW Optional Attributes
                      Accessors=xppaccessors                                     // [ACCESS | ASSIGN]
                      Modifiers=xppmemberModifiers?                               // [CLASS | STATIC]
                      M=METHOD Id=identifier                                      // METHOD <MethodName>
                      (VAR VarName=identifier)?                                   // [VAR <VarName>]
                      (AS Type=datatype)?                                         // NEW Optional data type
                      end=eos
                    ;


xppaccessors        : ( Tokens+=(ACCESS | ASSIGN ) )+
                    ;


xppmethod           : Attributes=attributes?                                // NEW Optional Attributes
                      Accessors=xppaccessors?                               // [ACCESS | ASSIGN]. These are ignored by Xbase++
                      Modifiers=xppmemberModifiers?                         // [CLASS]
                      M=METHOD (ClassId=identifier COLON)?
                      Sig=signature
                      end=eos
                      StmtBlk=statementBlock
                      (END METHOD eos)?
                    ;

xppinlineMethod     : Attributes=attributes?                                 // NEW Optional Attributes
                      I=INLINE
                      Accessors=xppaccessors?                                // [ACCESS | ASSIGN] 
                      Modifiers=xppmemberModifiers?                          // [CLASS]
                      METHOD Sig=signature
                      end=eos
                      StmtBlk=statementBlock
                      (END METHOD eos)?
                    ;


/// FoxPro Parser definities
keywordfox          :  Token=( OLEPUBLIC | EACH | EXCLUDE| THISACCESS| HELPSTRING| NOINIT | FOX_AND| FOX_OR| FOX_NOT| FOX_XOR | THEN | FOX_M)
                      // These tokens are already marked as 'only valid in a certain context ' in the lexer
                      // ENDDEFINE | DIMENSION | LPARAMETERS
                    ;
// class declaration
// text ... endtext



foxclass            : (Attributes=attributes)?
                      D=DEFINE (Modifiers=classModifiers)?
                      CLASS (Namespace=nameDot)? Id=identifier
                      TypeParameters=typeparameters?
                      (AS BaseType=datatype)?
                      foxclasslib?
                      (ConstraintsClauses+=typeparameterconstraintsclause)*             // Optional typeparameterconstraints for Generic Class
                      (OLEPUBLIC) ?
                      e=eos
                      (Members+=foxclassmember)*
                      (ENDDEFINE | END DEFINE) eos
                    ;

foxclassmember      : Member=foxclassvars          #foxclsvars
                    | Member=foxfield              #foxclsvarinit
                    | Member=foxmethod             #foxclsmethod
                    | Member=foximplementsclause   #foximplements
                    | Member=foxaddobjectclause    #foxaddobject
                    | Member=foxpemcomattrib       #foxpemcom
                    | Member=constructor           #foxclsctor
                    | Member=destructor            #foxclsdtor
                    ;
                    // do we also add support for events, operators, nested classes etc in the foxpro classes ?
                    //| Member=event_                #foxevent
                    //| Member=operator_             #foxoperator
                    //| Member=property              #foxproperty
                    //| Member=structure_            #foxnestedStructure
                    //| Member=delegate_             #foxnestedDelegate
                    //| Member=enum_                 #foxnestedEnum
                    //| Member=event_                #foxnestedEvent
                    //| Member=interface_            #foxnestedInterface


foxmethod           : (Attributes=attributes)? (Modifiers=memberModifiers)?
                      T=funcproctype  Sig=signature
                      (HelpString=HELPSTRING HelpText=expression)?
                      (ThisAccess=THISACCESS LPAREN MemberId=identifier RPAREN)?
                      end=eos
                      StmtBlk=statementBlock
                      (END T2=funcproctype  EOS)?
                    ;

foxclassvars        : (Attributes=attributes)? (Modifiers=classvarModifiers)?
                      (Fld=FIELD)? Vars += identifier (COMMA Vars += identifier )*  (AS DataType=datatype)?
                      end=eos
                    ;


foxfield            : (Modifiers=classvarModifiers)? (Fld=FIELD)? F=foxfieldinitializer end=eos
                    ;

foxfieldinitializer : Name=name assignoperator Expr=expression
                    ;

foximplementsclause : IMPLEMENTS Type=datatype (Excl=EXCLUDE)? (IN Library=expression)?
                      end=eos
                    ;

foxaddobjectclause  : (Attributes=attributes)? ADD OBJECT (Modifiers=classvarModifiers)?
                      Id=identifier AS Type=datatype (NoInit=NOINIT)?
                      (WITH FieldsInits += foxfieldinitializer (COMMA FieldsInits += foxfieldinitializer)* )?
                      end=eos
                    ;

foxpemcomattrib     : DIMENSION Id=identifier LBRKT  expression (COMMA expression)? RBRKT end=eos
                    | DIMENSION Id=identifier LPAREN expression (COMMA expression)? RPAREN end=eos
                    | Id=identifier LBRKT expression RBRKT assignoperator expression end=eos
                    ;
