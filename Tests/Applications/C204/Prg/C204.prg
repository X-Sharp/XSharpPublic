//204. compiler crash
// vulcan dialect
FUNCTION Start() AS VOID
Test(NULL)

FUNCTION Test(u AS USUAL) AS VOID
IF u != NIL
	THROW Exception{"value not NIL"}
END IF

