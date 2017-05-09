// 483. Harbour dialect, iif() with omitted argument is allowed
// or should we report an error on this also in harbour dialect?

// /dialect:harbour in the compiler options
FUNCTION Start( ) AS VOID
	LOCAL l := TRUE AS LOGIC
	? iif(l,1,)
	? iif(l,,2)
	xAssert(iif(l,1,) == 1)
	xAssert(iif(l,,2) == NIL)
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

