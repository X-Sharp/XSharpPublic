// 589. #pragma options recognised but are ignored

// Main problem is with using same code with vulcan and x#,
// until it's possible to move everything to x#

FUNCTION Start() AS VOID
	xAssert(Test(0) == 1)
	xAssert(Test(1) == 2)
	xAssert(Test(2) == 3)
RETURN

#pragma options("az", on)
FUNCTION Test(nIndex AS INT) AS INT
	LOCAL a AS INT[]
	a := <INT>{1,2,3}
	? a[nIndex]
RETURN a[nIndex]
#pragma options("az", off)


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

