#pragma warnings(9071, off) // ref modifier
FUNCTION Start AS VOID
	LOCAL a,b AS USUAL
	a := 10
	b := 20

	Test(a,,@b)
	xAssert(a == 10)
	xAssert(b == 300)
	? __Usual._NIL:ToString()
	xAssert( __Usual._NIL:ToString() == "NIL")



FUNCTION Test(a AS USUAL, b := NIL OUT USUAL, c := NIL OUT USUAL, d := NIL AS USUAL)  AS VOID
	a := 100
	b := 200
	c := 300
	d := 400

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
