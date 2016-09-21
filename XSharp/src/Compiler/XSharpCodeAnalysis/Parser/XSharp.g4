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
grammar XSharp;

/*
 * Parser Rules
*/

// Known issues:
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

    internal void SetSequencePoint (ParserRuleContext context, IToken endtoken)
    {
        if (context != null && endtoken != null)
            context.SetSequencePoint(endtoken.StartIndex);
    }
    internal void SetSequencePoint (ParserRuleContext context)
    {
        if (context != null )
        {
                if (context.Stop != null)
                    context.SetSequencePoint(context.Stop.StopIndex - context.Start.StartIndex+1);
                else
                    context.SetSequencePoint(context.Start.StopIndex - context.Start.StartIndex+1);
        }
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
                    | voglobal                  // This will become part of the 'Globals' class
                    | vodefine                  // This will become part of the 'Globals' class
                    | vodll                     // External method of the Globals class
                    | vostruct					// Compatibility (unsafe) structure
                    | vounion					// Compatibility (unsafe) structure with members aligned at FieldOffSet 0
                    ;

function            : (Attributes=attributes)? (Modifiers=funcprocModifiers)? 
                        FUNCTION Id=identifier TypeParameters=typeparameters? (ParamList=parameterList)?
                       (AS Type=datatype)? 
                       (ConstraintsClauses+=typeparameterconstraintsclause)*
                       (CallingConvention=callingconvention)? end=EOS 
                       StmtBlk=statementBlock
                    { SetSequencePoint(_localctx, $end); }
                    ;

procedure           : (Attributes=attributes)? (Modifiers=funcprocModifiers)? 
                       PROCEDURE Id=identifier TypeParameters=typeparameters? (ParamList=parameterList)?
                       (ConstraintsClauses+=typeparameterconstraintsclause)*
                       (CallingConvention=callingconvention)? Init=(INIT1|INIT2|INIT3)? end=EOS 
                       StmtBlk=statementBlock
                    { SetSequencePoint(_localctx,$end); }
                    ;

callingconvention	: Convention=(CLIPPER | STRICT | PASCAL) 
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

vodll				: (Modifiers=funcprocModifiers)? DLL 
                      ( T=FUNCTION Id=identifier ParamList=parameterList (AS Type=datatype)?
                      | T=PROCEDURE Id=identifier ParamList=parameterList )
                      (CallingConvention=dllcallconv) COLON 
                      Dll=identifierString (DOT Extension=identifierString)?
                        ( DOT Entrypoint=identifierString (NEQ2 INT_CONST)?
                        | Ordinal=REAL_CONST)
                      ( CharSet=(AUTO | ANSI | UNICODE) )?
                      end=EOS
                    { SetSequencePoint(_localctx,$end); }
                    ;

dllcallconv         : Cc=( CLIPPER | STRICT | PASCAL | THISCALL | FASTCALL)
                    ;


parameterList		: LPAREN (Params+=parameter (COMMA Params+=parameter)*)? RPAREN
                    ;

parameter			: (Attributes=attributes)? Self=SELF? Id=identifier (ASSIGN_OP Default=expression)? (Modifiers=parameterDeclMods Type=datatype)?
                    ;

parameterDeclMods   : Tokens+=(AS | REF | OUT | IS | PARAMS) Tokens+=CONST?
                    ;

statementBlock      : (Stmts+=statement)*
                    ;


funcprocModifiers	: ( Tokens+=(STATIC | INTERNAL | PUBLIC | EXPORT | UNSAFE) )+
                    ;


using_              : (HASHUSING|USING) (Static=STATIC)? (Alias=identifierName ASSIGN_OP)? Name=name     end=EOS
                    { SetSequencePoint(_localctx,$end); }
                    ;

// nvk: roslyn treats #pragma directives as trivia attached to parse nodes. The parser does not handle them directly.
pragma              : PRAGMA OPTIONS    LPAREN Compileroption=STRING_CONST COMMA Switch=pragmaswitch RPAREN end=EOS         #pragmaOptions
                    | PRAGMA WARNINGS   LPAREN WarningNumber=INT_CONST     COMMA Switch=pragmaswitch RPAREN end=EOS         #pragmaWarnings
                    ;

pragmaswitch        : ON | OFF | DEFAULT
                    ;

voglobal			: (Attributes=attributes)? (Modifiers=funcprocModifiers)? GLOBAL (Const=CONST)? Vars=classVarList end=EOS
                    { SetSequencePoint(_localctx,$end); }
                    ;


// Separate method/access/assign with Class name -> convert to partial class with just one method
// And when Class is outside of assembly, convert to Extension Method?
// nvk: we have no knowledge of whether a class is outside of the assembly at the parser stage!
method				: (Attributes=attributes)? (Modifiers=memberModifiers)?
                      T=methodtype (ExplicitIface=nameDot)? Id=identifier TypeParameters=typeparameters? (ParamList=parameterList)? (AS Type=datatype)? 
                      (ConstraintsClauses+=typeparameterconstraintsclause)*
                      (CallingConvention=callingconvention)? (CLASS (Namespace=nameDot)? ClassId=identifier)? end=EOS 
                      StmtBlk=statementBlock		
                    { SetSequencePoint(_localctx,$end); }
                    ;

methodtype			: Token=(METHOD | ACCESS | ASSIGN)
                    ;

// Convert to constant on Globals class. Expression must be resolvable at compile time
vodefine			: (Modifiers=funcprocModifiers)? DEFINE Id=identifier ASSIGN_OP Expr=expression (AS DataType=nativeType)? end=EOS
                    { SetSequencePoint(_localctx,$end); }
                    ;

vostruct			: (Modifiers=votypeModifiers)? 
                      VOSTRUCT (Namespace=nameDot)? Id=identifier (ALIGN Alignment=INT_CONST)? end=EOS
                      (Members+=vostructmember)+
                    { SetSequencePoint(_localctx,$end); }
                    ;

vostructmember		: MEMBER Dim=DIM Id=identifier LBRKT ArraySub=arraysub RBRKT (As=(AS | IS) DataType=datatype)? end=EOS
                    | MEMBER Id=identifier (As=(AS | IS) DataType=datatype)? end=EOS
                    { SetSequencePoint(_localctx,$end); }
                    ;


vounion				: (Modifiers=votypeModifiers)? 
                      UNION (Namespace=nameDot)? Id=identifier end=EOS
                      (Members+=vounionmember)+
                    { SetSequencePoint(_localctx,$end); }
                    ;

votypeModifiers		: ( Tokens+=(INTERNAL | PUBLIC | EXPORT | UNSAFE) )+
                    ;


vounionmember		: MEMBER Dim=DIM Id=identifier LBRKT ArraySub=arraysub RBRKT (As=(AS | IS) DataType=datatype)? end=EOS
                    | MEMBER Id=identifier (As=(AS | IS) DataType=datatype)? end=EOS
                    { SetSequencePoint(_localctx,$end); }
                    ;

namespace_			: BEGIN NAMESPACE Name=name end=EOS
                      (Entities+=entity)*
                      END NAMESPACE EOS
                      { SetSequencePoint(_localctx,$end); }
                    ;

interface_			: (Attributes=attributes)? (Modifiers=interfaceModifiers)?
                      INTERFACE (Namespace=nameDot)? Id=identifier TypeParameters=typeparameters?
                      ((INHERIT|COLON) Parents+=datatype)? (COMMA Parents+=datatype)*
                      (ConstraintsClauses+=typeparameterconstraintsclause)* end=EOS         // Optional typeparameterconstraints for Generic Class
                      (Members+=classmember)*
                      END INTERFACE EOS
                      { SetSequencePoint(_localctx,$end); }
                    ;

interfaceModifiers	: ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE | PARTIAL) )+
                    ;

class_				: (Attributes=attributes)? (Modifiers=classModifiers)?
                      CLASS (Namespace=nameDot)? Id=identifier TypeParameters=typeparameters?						// TypeParameters indicate Generic Class
                      (INHERIT BaseType=datatype)?
                      (IMPLEMENTS Implements+=datatype (COMMA Implements+=datatype)*)?
                      (ConstraintsClauses+=typeparameterconstraintsclause)* end=EOS         // Optional typeparameterconstraints for Generic Class
                      (Members+=classmember)*
                      END CLASS  EOS
                      { SetSequencePoint(_localctx,$end); }
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
                      (ConstraintsClauses+=typeparameterconstraintsclause)* end=EOS
                      (Members+=classmember)+
                      END STRUCTURE EOS
                      { SetSequencePoint(_localctx,$end); }
                    ;

structureModifiers	: ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE | PARTIAL) )+
                    ;


delegate_			: (Attributes=attributes)? (Modifiers=delegateModifiers)?
                      DELEGATE (Namespace=nameDot)? Id=identifier TypeParameters=typeparameters?
                      ParamList=parameterList? (AS Type=datatype)?
                      (ConstraintsClauses+=typeparameterconstraintsclause)* end=EOS
                    { SetSequencePoint(_localctx,$end); }
                    ;

delegateModifiers	: ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | UNSAFE) )+
                    ;


enum_				: (Attributes=attributes)? (Modifiers=enumModifiers)?
                      ENUM (Namespace=nameDot)? Id=identifier (AS Type=datatype)? end=EOS
                      (Members+=enummember)+
                      END (ENUM)? EOS
                       { SetSequencePoint(_localctx,$end); }
                    ;

enumModifiers		: ( Tokens+=(NEW | PUBLIC| EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN) )+
                    ;

enummember			: (Attributes=attributes)? MEMBER? Id=identifier (ASSIGN_OP Expr=expression)? end=EOS
                    { SetSequencePoint(_localctx,$end); }
                    ;

event_				:  (Attributes=attributes)? (Modifiers=eventModifiers)?
                       EVENT (ExplicitIface=nameDot)? Id=identifier (AS Type=datatype)? 
                       ( end=EOS
                        | (LineAccessors += eventLineAccessor)+ end=EOS
                        | Multi=EOS (Accessors+=eventAccessor)+  END EVENT? EOS
                       )
                       { SetSequencePoint(_localctx,$end); }
                    ;

eventModifiers		: ( Tokens+=(NEW | PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | STATIC | VIRTUAL | SEALED | ABSTRACT | UNSAFE) )+
                    ;


eventLineAccessor   : Attributes=attributes? Modifiers=eventModifiers? 
                      ( {InputStream.La(2) != REMOVE}? Key=ADD ExprList=expressionList?
                      | {InputStream.La(2) != ADD}?    Key=REMOVE ExprList=expressionList?
                      | Key=(ADD|REMOVE) )
                    ;
eventAccessor       : Attributes=attributes? Modifiers=eventModifiers? 
                      ( Key=ADD     end=EOS StmtBlk=statementBlock END ADD?
                      | Key=REMOVE  end=EOS StmtBlk=statementBlock END REMOVE? )
                      end=EOS
                    { SetSequencePoint(_localctx,$end); }
                    ;



classvars			: (Attributes=attributes)? (Modifiers=classvarModifiers)? Vars=classVarList end=EOS
                       { SetSequencePoint(_localctx,$end); }
                    ; 

classvarModifiers	: ( Tokens+=(INSTANCE| STATIC | CONST | INITONLY | PRIVATE | HIDDEN | PROTECTED | PUBLIC | EXPORT | INTERNAL | VOLATILE | UNSAFE | FIXED) )+
                    ;

classVarList		: Var+=classvar (COMMA Var+=classvar)* (As=(AS | IS) DataType=datatype)?
                       { SetSequencePoint(_localctx); }
                    ;

classvar			: (Dim=DIM)? Id=identifier (LBRKT ArraySub=arraysub RBRKT)? (ASSIGN_OP Initializer=expression)?
                    { SetSequencePoint(_localctx); }
                    ;

arraysub			: ArrayIndex+=expression (RBRKT LBRKT ArrayIndex+=expression)+		// x][y
                    | ArrayIndex+=expression (COMMA ArrayIndex+=expression)+			// x,y
                    | ArrayIndex+=expression
                    ;

property			: (Attributes=attributes)? (Modifiers=memberModifiers)? 
                      PROPERTY (SELF ParamList=propertyParameterList | (ExplicitIface=nameDot)? Id=identifier) (ParamList=propertyParameterList)?  (AS Type=datatype)? 
                      ( Auto=AUTO (AutoAccessors+=propertyAutoAccessor)* (ASSIGN_OP Initializer=expression)? end=EOS	// Auto
                      | (LineAccessors+=propertyLineAccessor)+ end=EOS													// Single Line
                      | Multi=EOS (Accessors+=propertyAccessor)+  END PROPERTY? EOS									// Multi Line
                      )
                       { SetSequencePoint(_localctx, $Multi); }
                    ;

propertyParameterList
                    : LBRKT  (Params+=parameter (COMMA Params+=parameter)*)? RBRKT
                    | LPAREN (Params+=parameter (COMMA Params+=parameter)*)? RPAREN			// Allow Parentheses as well
                    ;

propertyAutoAccessor: Attributes=attributes? Modifiers=memberModifiers? Key=(GET|SET)
                    ;

propertyLineAccessor: Attributes=attributes? Modifiers=memberModifiers? 
                      ( {InputStream.La(2) != SET}? Key=GET Expr=expression?
                      | {InputStream.La(2) != GET}? Key=SET ExprList=expressionList?
                      | Key=(GET|SET) )
                    ;

expressionList	    : Exprs+=expression (COMMA Exprs+=expression)*
                    ;

propertyAccessor    : Attributes=attributes? Modifiers=memberModifiers? 
                      ( Key=GET end=EOS StmtBlk=statementBlock END GET?
                      | Key=SET end=EOS StmtBlk=statementBlock END SET? )
                      end=EOS
                    { SetSequencePoint(_localctx,$end); }
                    ;

classmember			: Member=method										{ SetSequencePoint(_localctx); } #clsmethod
                    | (Attributes=attributes)?
                      (Modifiers=constructorModifiers)? 
                      CONSTRUCTOR (ParamList=parameterList)? (CallingConvention=callingconvention)? end=EOS 
                      (Chain=(SELF | SUPER) 
					  ( 
						  (LPAREN RPAREN)
						| (LPAREN ArgList=argumentList RPAREN)
					  ) EOS)?
                      StmtBlk=statementBlock							 { SetSequencePoint(_localctx,$end); } #clsctor
                    | (Attributes=attributes)? 
                      (Modifiers=destructorModifiers)?
                      DESTRUCTOR (LPAREN RPAREN)?  end=EOS 
                      StmtBlk=statementBlock							{ SetSequencePoint(_localctx,$end); } #clsdtor
                    | Member=classvars									{ SetSequencePoint(_localctx); } #clsvars
                    | Member=property									{ SetSequencePoint(_localctx); } #clsproperty
                    | Member=operator_									{ SetSequencePoint(_localctx); } #clsoperator
                    | Member=structure_									{ SetSequencePoint(_localctx); } #nestedStructure
                    | Member=class_										{ SetSequencePoint(_localctx); } #nestedClass
                    | Member=delegate_									{ SetSequencePoint(_localctx); } #nestedDelegate
                    | Member=enum_										{ SetSequencePoint(_localctx); } #nestedEnum
                    | Member=event_										{ SetSequencePoint(_localctx); } #nestedEvent
                    | Member=interface_									{ SetSequencePoint(_localctx); } #nestedInterface
                    | Pragma=pragma										#nestedPragma
                    | {_ClsFunc}? Member=function						{ SetSequencePoint(_localctx); } #clsfunction		// Equivalent to method
                    | {_ClsFunc}? Member=procedure						{ SetSequencePoint(_localctx); } #clsprocedure		// Equivalent to method
                    ;


constructorModifiers: ( Tokens+=( PUBLIC | EXPORT | PROTECTED | INTERNAL | PRIVATE | HIDDEN | EXTERN | STATIC ) )+
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
overloadedOps		: Token= (PLUS | MINUS | NOT | TILDE | INC | DEC | TRUE_CONST | FALSE_CONST |
                              MULT | DIV | MOD | AMP | PIPE | LSHIFT | RSHIFT | EEQ | NEQ | NEQ2 |
                              GT | LT | GTE | LTE |
                              AND | OR )  // these two do not exist in C# and are mapped to & and |
                    ;

conversionOps		: Token=( IMPLICIT | EXPLICIT )
                    ;

operator_			: Attributes=attributes? Modifiers=operatorModifiers? 
                      OPERATOR (Operation=overloadedOps | Conversion=conversionOps) Gt=GT?
                      ParamList=parameterList (AS Type=datatype)? end=EOS StmtBlk=statementBlock
                    { SetSequencePoint(_localctx,$end); }
                    ;

operatorModifiers	: ( Tokens+=(PUBLIC | STATIC | EXTERN) )+
                    ;

memberModifiers		: ( Tokens+=(NEW | PRIVATE | HIDDEN | PROTECTED | PUBLIC | EXPORT | INTERNAL | STATIC | VIRTUAL | SEALED | ABSTRACT | ASYNC | UNSAFE | EXTERN | OVERRIDE) )+
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

globalAttributes    : LBRKT Target=globalAttributeTarget Attributes+=attribute (COMMA Attributes+=attribute)* RBRKT end=EOS
                    { SetSequencePoint(_localctx,$end); }
                    ;

globalAttributeTarget : Token=(ASSEMBLY | MODULE) COLON
                    ;

statement           : Decl=localdecl                                            #declarationStmt
                    | {_xBaseVars}? xbasedecl									#xbasedeclStmt
                    | Decl=fielddecl											#fieldStmt
                    | DO? WHILE Expr=expression end=EOS
                      StmtBlk=statementBlock (END DO? | ENDDO) EOS				{ SetSequencePoint(_localctx,$end); } #whileStmt
                    | NOP end=EOS												{ SetSequencePoint(_localctx,$end); } #nopStmt
                    | FOR 
                        ( AssignExpr=expression
                        | (LOCAL? ForDecl=IMPLIED | ForDecl=VAR) ForIter=identifier ASSIGN_OP Expr=expression
                        | ForDecl=LOCAL ForIter=identifier ASSIGN_OP Expr=expression (AS Type=datatype)?
                        )
                      Dir=(TO | UPTO | DOWNTO) FinalExpr=expression
                      (STEP Step=expression)? end=EOS
                      StmtBlk=statementBlock NEXT EOS							{ SetSequencePoint(_localctx,$end); } #forStmt
                    | IF IfStmt=ifElseBlock
                      (END IF? | ENDIF)  EOS									#ifStmt	
                    | DO CASE end=EOS
                      CaseStmt=caseBlock?
                      (END CASE? | ENDCASE) EOS									{ SetSequencePoint(_localctx,$end); } #caseStmt
                    | EXIT end=EOS												{ SetSequencePoint(_localctx,$end); } #exitStmt
                    | LOOP end=EOS												{ SetSequencePoint(_localctx,$end); } #loopStmt
                    | BREAK Expr=expression? end=EOS							{ SetSequencePoint(_localctx,$end); } #breakStmt
                    | RETURN (VOID | Expr=expression)? end=EOS					{ SetSequencePoint(_localctx,$end); } #returnStmt
                    | Q=(QMARK | QQMARK)
                       (Exprs+=expression (COMMA Exprs+=expression)*)? end=EOS	{ SetSequencePoint(_localctx,$end); } #qoutStmt
                    | BEGIN SEQUENCE end=EOS
                      StmtBlk=statementBlock
                      (RECOVER RecoverBlock=recoverBlock)?
                      (FINALLY EOS FinBlock=statementBlock)?
                      END (SEQUENCE)? EOS									{ SetSequencePoint(_localctx,$end); } #seqStmt
                    //
                    // New in Vulcan
                    //
                    | REPEAT end=EOS
                      StmtBlk=statementBlock
                      UNTIL Expr=expression EOS									{ SetSequencePoint(_localctx,$end); }#repeatStmt
                    | FOREACH
                      (IMPLIED Id=identifier | Id=identifier AS Type=datatype| VAR Id=identifier)
                      IN Container=expression end=EOS
                      StmtBlk=statementBlock NEXT EOS							{ SetSequencePoint(_localctx,$end); }#foreachStmt
                    | THROW Expr=expression? end=EOS							{ SetSequencePoint(_localctx,$end); }#throwStmt
                    | TRY end=EOS StmtBlk=statementBlock
                      (CATCH CatchBlock+=catchBlock?)*
                      (FINALLY EOS FinBlock=statementBlock)?
                      END TRY? EOS												{ SetSequencePoint(_localctx,$end); }#tryStmt
                    | BEGIN Key=LOCK Expr=expression end=EOS
                      StmtBlk=statementBlock
                      END LOCK? EOS												{ SetSequencePoint(_localctx,$end); }#blockStmt
                    | BEGIN Key=SCOPE end=EOS
                      StmtBlk=statementBlock
                      END SCOPE? EOS											{ SetSequencePoint(_localctx,$end); }#blockStmt
                    //
                    // New XSharp Statements
                    //
                    | YIELD RETURN (VOID | Expr=expression)? end=EOS			{ SetSequencePoint(_localctx,$end); }#yieldStmt
                    | YIELD Break=(BREAK|EXIT) end=EOS							{ SetSequencePoint(_localctx,$end); }#yieldStmt
                    | SWITCH Expr=expression end=EOS
                      (SwitchBlock+=switchBlock)+
                      END SWITCH?  EOS											{ SetSequencePoint(_localctx,$end); }#switchStmt
                    | BEGIN Key=USING ( Expr=expression | VarDecl=variableDeclaration ) end=EOS
                        StmtBlk=statementBlock
                      END USING? EOS											{ SetSequencePoint(_localctx,$end); }#blockStmt
                    | BEGIN Key=UNSAFE end=EOS
                      StmtBlk=statementBlock
                      END UNSAFE? EOS											{ SetSequencePoint(_localctx,$end); }#blockStmt
                    | BEGIN Key=CHECKED end=EOS
                      StmtBlk=statementBlock
                      END CHECKED? EOS											{ SetSequencePoint(_localctx,$end); }#blockStmt
                    | BEGIN Key=UNCHECKED end=EOS
                      StmtBlk=statementBlock
                      END UNCHECKED? EOS										{ SetSequencePoint(_localctx,$end); }#blockStmt
                    | BEGIN Key=FIXED ( VarDecl=variableDeclaration ) end=EOS
                      StmtBlk=statementBlock
                      END FIXED? EOS											{ SetSequencePoint(_localctx,$end); }#blockStmt
                    | {InputStream.La(2) != LPAREN || // This makes sure that CONSTRUCTOR, DESTRUCTOR etc will not enter the expression rule
                       (InputStream.La(1) != CONSTRUCTOR && InputStream.La(1) != DESTRUCTOR) }?
                      Exprs+=expression (COMMA Exprs+=expression)* end=EOS		{ SetSequencePoint(_localctx,$end); }#expressionStmt
                    
                    ;

ifElseBlock			: Cond=expression end=EOS StmtBlk=statementBlock
                      (ELSEIF ElseIfBlock=ifElseBlock | ELSE EOS ElseBlock=statementBlock)?
                    { SetSequencePoint(_localctx,$end); }
                    ;

caseBlock			: Key=CASE Cond=expression end=EOS StmtBlk=statementBlock NextCase=caseBlock?
                    | Key=OTHERWISE end=EOS StmtBlk=statementBlock
                    { SetSequencePoint(_localctx,$end); }
                    ;

// Note that literalValue is not enough. We also need to support members of enums
switchBlock         : (Key=CASE Const=expression | Key=OTHERWISE) end=EOS StmtBlk=statementBlock			 
                    { SetSequencePoint(_localctx,$end); }
                    ;

catchBlock			: (Id=identifier (AS Type=datatype)?)? end=EOS StmtBlk=statementBlock
                    { SetSequencePoint(_localctx,$end); }
                    ;

recoverBlock		: (USING Id=identifier)? end=EOS StmtBlock=statementBlock
                    { SetSequencePoint(_localctx,$end); }
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
                     LocalVars+=localvar (COMMA LocalVars+=localvar)*						end=EOS   { SetSequencePoint(_localctx); } #commonLocalDecl	// STATIC LOCAL or LOCAL
                   | (Static=STATIC LOCAL? IMPLIED | LOCAL IMPLIED | Static=STATIC? VAR)							// LOCAL IMPLIED
                     ImpliedVars+=impliedvar (COMMA ImpliedVars+=impliedvar)*               end=EOS   { SetSequencePoint(_localctx); }  #varLocalDecl		// VAR special for Robert !
                    
                   ;

localvar           : (Const=CONST)? ( Dim=DIM )? Id=identifier (LBRKT ArraySub=arraysub RBRKT)? 
                     (ASSIGN_OP Expression=expression)? (As=(AS | IS) DataType=datatype)?
                     { SetSequencePoint(_localctx); }
                   ;
                      
impliedvar         : (Const=CONST)? Id=identifier ASSIGN_OP Expression=expression 
                     { SetSequencePoint(_localctx); }
                   ;


fielddecl		   : FIELD Fields+=identifierName (COMMA Fields+=identifierName)* (IN Alias=identifierName)? end=EOS       
                   ;

// Old Style xBase declarations

xbasedecl        : T=(PRIVATE												// PRIVATE Foo, Bar
                      |PUBLIC												// PUBLIC  Foo, Bar
                      |MEMVAR												// MEMVAR  Foo, Bar
                      |PARAMETERS											// PARAMETERS Foo, Bar
                     )   Vars+=identifierName (COMMA Vars+=identifierName)* end=EOS       
                    { SetSequencePoint(_localctx,$end); }
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
                    | Expr=expression LPAREN                       RPAREN		#methodCall				// method call, no params
                    | Expr=expression LPAREN ArgList=argumentList  RPAREN		#methodCall				// method call, with params
                    | Expr=expression LBRKT ArgList=bracketedArgumentList  RBRKT #arrayAccess			// Array element access
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
                    | Left=expression Op=LSHIFT Right=expression				#binaryExpression		// expr << expr (shift)
                    | Left=expression Op=GT	Gt=GT Right=expression				#binaryExpression		// expr >> expr (shift)
                    | Left=expression
                      Op=( LT | LTE | GT | GTE | EQ | EEQ | SUBSTR | NEQ | NEQ2)
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
                      Right=expression											#assignmentExpression	// expr := expr, also expr += expr etc.
                    | Expr=primary												#primaryExpression
                    ;

                    // Primary expressions
primary				: Key=SELF													#selfExpression
                    | Key=SUPER													#superExpression
                    | Literal=literalValue										#literalExpression		// literals
                    | LiteralArray=literalArray									#literalArrayExpression	// { expr [, expr] }
                    | AnonType=anonType											#anonTypeExpression		// { .id := expr [, .id := expr] }
                    | CbExpr=codeblock											#codeblockExpression	// {| [id [, id...] | expr [, expr...] }
                    | Query=linqQuery											#queryExpression        // LINQ
                    | Type=datatype LCURLY Obj=expression COMMA
                      ADDROF Func=name LPAREN RPAREN RCURLY						#delegateCtorCall		// delegate{ obj , @func() }
                    | Type=datatype LCURLY                       RCURLY			#ctorCall				// id{  }
                    | Type=datatype LCURLY ArgList=argumentList  RCURLY			#ctorCall				// id{ expr [, expr...] }
                    | ch=CHECKED LPAREN ( Expr=expression ) RPAREN				#checkedExpression		// checked( expression )
                    | ch=UNCHECKED LPAREN ( Expr=expression ) RPAREN			#checkedExpression		// unchecked( expression )
                    | TYPEOF LPAREN Type=datatype RPAREN						#typeOfExpression		// typeof( typeORid )
                    | SIZEOF LPAREN Type=datatype RPAREN						#sizeOfExpression		// sizeof( typeORid )
                    | DEFAULT LPAREN Type=datatype RPAREN						#defaultExpression		// default( typeORid )
                    | Name=simpleName											#nameExpression			// generic name
                    | Type=nativeType LPAREN Expr=expression RPAREN				#voConversionExpression	// nativetype( expr )
                    | XType=xbaseType LPAREN Expr=expression RPAREN				#voConversionExpression	// xbaseType( expr )
                    | Type=datatype LPAREN CAST COMMA Expr=expression RPAREN	#voCastExpression		// typename(_CAST, expr )
                    | PTR LPAREN Type=datatype COMMA Expr=expression RPAREN		#voCastPtrExpression	// PTR( typeName, expr )
					| Name=voTypeName											#voTypeNameExpression	// LONG, STRING etc., used as NUMERIC in expressions
                    //| Type=nativeType											#typeExpression			// Standard DotNet Types
                    //| XType=xbaseType											#typeExpression			// ARRAY, CODEBLOCK, etc.
                    | Expr=iif													#iifExpression			// iif( expr, expr, expr )
                    | Op=(VO_AND | VO_OR | VO_XOR | VO_NOT) LPAREN Exprs+=expression 
                      (COMMA Exprs+=expression)* RPAREN							#intrinsicExpression	// _Or(expr, expr, expr)
                    | FIELD_ ALIAS (Alias=identifier ALIAS)? Field=identifier   #aliasedField		    // _FIELD->CUSTOMER->NAME 
                    | {InputStream.La(4) != LPAREN}?                            // this makes sure that CUSTOMER->NAME() is not matched
                          Alias=identifier ALIAS Field=identifier               #aliasedField		    // CUSTOMER->NAME 
                    | Id=identifier ALIAS Expr=expression                       #aliasedExpr            // id -> expr	
                    | LPAREN Alias=primary RPAREN ALIAS Expr=expression         #aliasedExpr            // (expr) -> expr	
                    | AMP LPAREN Expr=expression RPAREN							#macro					// &( expr )
                    | AMP Id=identifierName										#macro					// &id
                    | LPAREN Expr=expression RPAREN							    #parenExpression		// ( expr )
                    ;

boundExpression		: Expr=boundExpression Op=(DOT | COLON) Name=simpleName		#boundAccessMember		// member access The ? is new
                    | Expr=boundExpression LPAREN						RPAREN	#boundMethodCall		// method call, no params
                    | Expr=boundExpression LPAREN ArgList=argumentList  RPAREN	#boundMethodCall		// method call, with params
                    | Expr=boundExpression 
                      LBRKT ArgList=bracketedArgumentList RBRKT					#boundArrayAccess		// Array element access
                    | <assoc=right> Left=boundExpression
                      Op=QMARK Right=boundExpression							#boundCondAccessExpr	// expr ? expr
                    | Op=(DOT | COLON) Name=simpleName							#bindMemberAccess
                    | LBRKT ArgList=bracketedArgumentList RBRKT					#bindArrayAccess
                    ;

bracketedArgumentList	: Args+=bracketedargument (COMMA Args+=bracketedargument)*
						;


bracketedargument	   // NOTE: Separate rule for bracketedarguments because they cannot use idendifierName syntax
					:  Expr=expression?
                    ;

argumentList		  // NOTE: Optional argumentlist is handled in the rules that use this rule
					:  Args+=argument (COMMA Args+=argument)*
                    ;

argument			   // NOTE: Expression is optional so we can skip arguments for VO/Vulcan compatibility
					:  ( COLON Name=identifierName ASSIGN_OP )? ( RefOut=(REF | OUT) )? Expr=expression?
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

                    // Separate rule for Array with zero elements, to prevent entering the first arrayElement rule 
                    // with a missing Expression which would not work for the core dialect
literalArray		: (LT Type=datatype GT)? LCURLY RCURLY															// {}
					| (LT Type=datatype GT)? LCURLY Elements+=arrayElement (COMMA Elements+=arrayElement)* RCURLY   // {e,e,e} or {e,,e} or {,e,} etc
                    ;

arrayElement        : Expr=expression?      // VO Array elements are optional
                    ;

anonType			: CLASS LCURLY (Members+=anonMember (COMMA Members+=anonMember)*)? RCURLY
                    ;

anonMember			: Name=identifierName ASSIGN_OP Expr=expression
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
identifier			: Token=(ID  | KWID)
                    | VnToken=keywordvn 
                    | XsToken=keywordxs
                    ;

identifierString	: Token=(ID | KWID | STRING_CONST)
                    | VnToken=keywordvn 
                    | XsToken=keywordxs
                    ;

// xBaseTypes are NOT available in the Core dialect and therefore separated here.
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

voTypeName			: Token=
					( ARRAY
					| BYTE
					| CHAR				// New in XSharp
					| CODEBLOCK
					| DATE
					| DWORD
					| DYNAMIC			// new in XSharp
					| FLOAT
					| SHORTINT
					| INT
					| INT64				// New in Vulcan
					| LOGIC
					| LONGINT
					| OBJECT
					| PSZ
					| PTR
					| REAL4
					| REAL8
					| SHORTINT
					| STRING
					| SYMBOL
					| UINT64			// New in Vulcan
					| USUAL
					| VOID
					| WORD)
					;
literalValue		: Token=
                    ( TRUE_CONST
                    | FALSE_CONST
                    | CHAR_CONST
                    | STRING_CONST
                    | ESCAPED_STRING_CONST
                    | INTERPOLATED_STRING_CONST         
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


keyword             : (KwVo=keywordvo | KwVn=keywordvn | KwXs=keywordxs) ;

keywordvo           : Token=(ACCESS | ALIGN | AS | ASSIGN | BEGIN | BREAK | CASE | CAST | CLASS | CLIPPER | DEFINE | DIM | DLL | DO | DOWNTO
                    | ELSE | ELSEIF | END | ENDCASE | ENDDO | ENDIF | EXIT | EXPORT | FASTCALL | FIELD | FOR | FUNCTION | GLOBAL
                    | HIDDEN | IF | IIF | INHERIT | INSTANCE |  IS | LOCAL | LOOP | MEMBER | METHOD | NEXT | OTHERWISE 
                    | PASCAL | PRIVATE | PROCEDURE | PROTECTED | PTR | PUBLIC | RECOVER | RETURN | SELF| SEQUENCE | SIZEOF | STEP | STRICT | SUPER
                    | THISCALL | TO | TYPEOF | UNION | UPTO | USING | WHILE | CATCH | FINALLY | TRY |VO_AND| VO_NOT| VO_OR| VO_XOR
                    | CONSTRUCTOR | DELEGATE | DESTRUCTOR | ENUM | EVENT | INTERFACE | OPERATOR	| PROPERTY | STRUCTURE | VOSTRUCT   )
                    ;
                    // Entity Keywords are added to the keywordvo list, although not strictly VO keyword. 
                    // But this prevents STATIC <Keyword> from being seen as a STATIC LOCAL declaration

keywordvn           : Token=(ABSTRACT | ANSI | AUTO | CONST |  DEFAULT | EXPLICIT | FOREACH | GET | IMPLEMENTS | IMPLICIT | IMPLIED | IN | INITONLY | INTERNAL 
                    | LOCK | NAMESPACE | NEW | OPTIONS | OFF | ON | OUT | PARTIAL | REPEAT | SCOPE | SEALED | SET |  TRY | UNICODE | UNTIL | VALUE | VIRTUAL  | WARNINGS)
                    ;

keywordxs           : Token=( ADD | ASCENDING | ASSEMBLY | ASYNC | AWAIT | BY | CHECKED | DESCENDING | DYNAMIC | EQUALS | EXTERN | FIXED | FROM | 
                              GROUP | INTO | JOIN | LET | MODULE | NAMEOF | NOP |  ORDERBY | OVERRIDE |PARAMS | REMOVE | 
                              SELECT | SWITCH | UNCHECKED | UNSAFE | VAR | VOLATILE | WHERE | YIELD | CHAR |
                              MEMVAR | PARAMETERS // Added as XS keywords to allow them to be treated as IDs
                            )
                    ;
