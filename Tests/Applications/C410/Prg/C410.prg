CLASS SQLStatement
	PROTECT aParams[0] AS ARRAY
	EXPORT aNew[10] AS ARRAY
	CONSTRUCTOR()
		xAssert(ALen(aParams) == 0)
		xAssert(ALen(SELF:aNew) == 10)
		
		xAssert(SELF:aNew[10] == NIL)

		SELF:aNew[10] := 100
		? SELF:aNew[10]
		
		xAssert(SELF:aNew[10] == 100)
	RETURN
END CLASS

FUNCTION Start() AS VOID
	LOCAL aParams[0] AS ARRAY
	? aParams
	? ALen(aParams)
	LOCAL aNew[1] AS ARRAY
	? ALen(aNew)
	? aNew[1] := 123
	? aNew[1]
	SQLStatement{}
	xAssert(ALen(aParams) == 0)
	xAssert(ALen(aNew) == 1)
	xAssert(aNew[1] == 123)
RETURN


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF

