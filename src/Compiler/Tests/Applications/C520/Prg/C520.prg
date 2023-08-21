// 520. Incorrect preprocessor results
#pragma warnings(9043, off) // zGEDBCOmboNew double defined
#include "C520.ch"

FUNCTION Start() AS VOID

@2,5 zGet o COMBO_USER STRICT .t. EMPTY_ALLOWED .f.

/*
Harbour and build 11 preprocess it to:
zGEDBComboNew( .F. , "SSUsers" , "SSUserSU" , "UserId" , NIL ,"UserId" , "SSUserSU" , .t. )

build 12 (spaces are mine):
zGEDBComboNew( .F. ,           , "SSUserSU" ,          , NIL ,"UserId" , "SSUserSU" ,     )
*/


#command zGET <getvar> TEST <id> => Test( <"getvar"> , <id> )
#command zSAY <sayxpr> zGET <getvar> [<getClauses,...>] => zGET <getvar> [<getClauses>]

zSAY NIL zGET 1 TEST NIL
/*
Harbour and build 11 preprocess it to:
Test( "1" , NIL )

In build 12: (note the space inside the quotes)
Test( "1 " , NIL )
*/

FUNCTION zGEDBComboNew( p1,p2,p3,p4,p5,p6,p7,p8)
? "zGEDBComboNew() params:", p1,p2,p3,p4,p5,p6,p7,p8
xAssert(p1 == FALSE)
xAssert(p2 == "SSUsers")
xAssert(p3 == "SSUserSU")
xAssert(p4 == "UserId")
xAssert(p5 == NIL)
xAssert(p6 == "UserId")
xAssert(p7 == "SSUserSU")
xAssert(p8 == TRUE)
RETURN NIL

FUNCTION Test(a,b) CLIPPER
	? "Test() params:", "'" + a + "'",b
	xAssert(a == "1")
	xAssert(b == NIL)
RETURN NIL

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

