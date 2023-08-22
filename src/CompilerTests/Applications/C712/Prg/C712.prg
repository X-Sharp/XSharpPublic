// 712. Problem with SHORTINT CAST arithmetic
// /vo4+
FUNCTION Start() AS VOID
	LOCAL w AS WORD
	w := 65535
	? SHORTINT(_CAST, w) // -1, correct
	? SHORTINT(_CAST, w) == -1 // FALSE, wrong!       
	
	? SHORTINT(_CAST, w) < 0 // FALSE, wrong!
	
	xAssertTrue(SHORTINT(_CAST, w) == -1)
	xAssertTrue(SHORTINT(_CAST, w) < 0)
RETURN 

PROC xAssertTrue(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}     
END IF

