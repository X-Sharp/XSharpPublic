// 909. VFP: Bogus warning when calling a function with /fox2+
// https://github.com/X-Sharp/XSharpPublic/issues/1476

// warning XS9073: Variable 'Test' has not been declared. Assuming this is a FIELD or a MEMVAR.

// /memvar+ /undeclared+ /fox2+

#pragma options (memvar, on)
#pragma options (undeclared, on)
#pragma options (fox2, on)

// warnings as errors set in app properties
// #pragma options (wx, on)

FUNCTION Start() AS VOID
LOCAL u := 123
? Test(u)
? Test(Test(u))

xAssert(u == 123)
xAssert(Test(u) == 123)
xAssert(Test(Test(u)) == 123)

FUNCTION Test(u)
RETURN u


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
