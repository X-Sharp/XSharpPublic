// 650. Runtime exception when casting PTR in USUAL to DWORD
// Vulcan error: Specified cast is not valid.
FUNCTION Start() AS VOID
	LOCAL u AS USUAL
	LOCAL d AS DWORD
	LOCAL n AS INT
	u := @u
	d := DWORD(_CAST,u) // exception
	? d
	? d == DWORD(u)
	xAssert(d == DWORD(u))

	n := INT(_CAST,u)
	? n
	? n == INT(u)
	xAssert(n == INT(u))
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

