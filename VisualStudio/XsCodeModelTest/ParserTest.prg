// ParserTest.prg
// Created by    : robert
// Creation Date : 5/29/2020 9:21:37 AM
// Created for   : 
// WorkStation   : ARTEMIS


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XsCodeModelTest

	/// <summary>
    /// The ParserTest class.
    /// </summary>
	CLASS ParserTest
 
    CONSTRUCTOR()
         RETURN

   PROPERTY Foo AS Bar
      GET 
         RETURN BarP{
      END GET
      SET
         NOP
      END SET
   END PROPERTY
   EVENT MyEvent AS Test
      ADD
         NOP
      END ADD
      REMOVE
         NOP
      END REMOVE
   END EVENT

	END CLASS
END NAMESPACE // XsCodeModelTest





FUNCTION Start AS VOID
   LOCAL i1,i2,i3 AS LONG, j1,j2 AS STRING
   STATIC s,a,b AS STRING
   STATIC LOCAL sl AS DATE
   LOCAL STATIC d AS DateTime
   LOCAL IMPLIED s,s1,s2 := "abc"
   VAR t,t1,t2 := "def"
   VAR myList := List< Foo >{}
   STATIC VAR sv := 10
   STATIC LOCAL IMPLIED sli := 100
   STATIC IMPLIED si := "si"
   FOR LOCAL IMPLIED I := 1 TO 10
      ? i
   NEXT
   FOR VAR j := 10 DOWNTO 1
      ? j
   NEXT
   FOR LOCAL k := 1 AS INT TO 10
      ? k
   NEXT
   FOREACH VAR iter1 IN MyList1
      ? iter1
   NEXT
   FOREACH IMPLIED iter2 IN MyList2
      ? iter2
   NEXT
   FOREACH LOCAL IMPLIED iter3 IN MyList3
      ? iter3
   NEXT
   
   BEGIN USING VAR us := SomeExpression()
      ? us
   END USING
   
   BEGIN FIXED VAR fix := SomeExpression()
      ? fix
   END FIXED
   
   TRY
   CATCH e AS Exception
   END TRY
   
   
//  | BEGIN Key=USING ( Expr=expression | VarDecl=variableDeclaration ) end=eos
//                        StmtBlk=statementBlock
//                      (e=END USING? eos)?		 
//                    | BEGIN Key=FIXED ( VarDecl=variableDeclaration ) end=eos
//                      StmtBlk=statementBlock
//                      (e=END FIXED? eos)?						                      #blockStmt

//variableDeclaration	: (LOCAL? VAR=IMPLIED | VAR=VAR) Decl+=variableDeclarator (COMMA Decl+=variableDeclarator)*
//                    | LOCAL Decl+=variableDeclarator (COMMA Decl+=variableDeclarator)* (AS Type=datatype)?
//                    ;
//
//variableDeclarator  : Id=identifier Op=assignoperator Expr=expression
//                    ;


// IS expression
//  | Expr=expression Op=IS Type=datatype (VAR Id=identifier)?  #typeCheckExpression    // expr IS typeORid [VAR identifier] 

// namedArgument       :  {AllowNamedArgs}?  Name=identifierName Op=assignoperator  ( RefOut=(REF | OUT) )? Expr=expression?
//                    |   RefOut=OUT VAR=VAR  Id=identifier
//                    |   RefOut=OUT Id=identifier AS Type=datatype
//                    |   RefOut=OUT NULL=NULL
//                    |  ( RefOut=(REF | OUT) )? Expr=expression?
  
   
// watch out:
// XPP Class vars
//xppclassvars        : (Modifiers=xppmemberModifiers)?                             // [CLASS] 
//                      VAR Vars+=identifier                                        // VAR <VarName> 
//                      (
//                        IS=xppisin                                                // [IS <Name>] [IN <SuperClass>] 
//                        | ((COMMA Vars+=identifier)*                              // <,...> 
//                        (AS DataType=datatype)?  )                                // Optional data type
//
//                      )
//                      (Shared=SHARED)?                                            // [SHARED]
//                      (ReadOnly=READONLY)?                                        // [READONLY] 
//                      (Assignment=xppvarassignment)?                              // [ASSIGNMENT HIDDEN | PROTECTED | EXPORTED] 
//                      (Nosave= NOSAVE)?                                           // [NOSAVE] 
//                      eos
//                    ;
// xppproperty         : (Attributes=attributes)?                                    // NEW Optional Attributes
//                      (   ACCESS=ACCESS Assign=ASSIGN?                            // ACCESS | ASSIGN  | ACCESS ASSIGN | ASSIGN ACCESS
//                        | ASSIGN=ASSIGN Access=ACCESS?
//                      ) 
//                      Modifiers=xppmemberModifiers?                               // [CLASS]
//                      M=METHOD Id=identifier                                        // METHOD <MethodName>
//                      (VAR VarName=identifier)?                                   // [VAR <VarName>]
//                      (AS Type=datatype)?                                         // NEW Optional data type
//                      END=eos
//                    ;


