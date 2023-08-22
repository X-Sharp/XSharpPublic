// 506. error XS0126: An object of a type convertible to 'int' is required
// Vulcan does not handle the RETURN VOID statement either, but it is allowed in VO
// same as with <identifier> := VOID which does work ok in x#/vulcan

// note when using /vo9, then an warning is reported
// warning XS9026: Missing RETURN value. A default 'empty' return value is returned
// which is of course not correct, either.
#pragma warnings(9026, off) // missing return value
FUNCTION Start( ) AS VOID
	LOCAL n AS INT
	n := TestInt()
	xAssert(n == 0)
	n := TestUsual()
	xAssert(n == 8)
RETURN

FUNCTION TestInt() AS INT
	LOCAL u AS USUAL
	u := INT // ok
	? u
	xAssert(u == 1)
	u := LOGIC // ok
	? u
	xAssert(u == 8)
	u := VOID // ok
	? u
	xAssert(u == 0)
	DO CASE
	CASE u == INT
		RETURN INT // ok
	CASE u == LOGIC
		RETURN LOGIC // ok
	CASE u == VOID
		RETURN VOID // error
	END CASE
RETURN 0

FUNCTION TestUsual() AS USUAL
	LOCAL lFalse := FALSE AS LOGIC
	IF lFalse
		RETURN DATE // ok
	END IF
	IF lFalse
		RETURN VOID // error
	END IF
RETURN LOGIC


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

