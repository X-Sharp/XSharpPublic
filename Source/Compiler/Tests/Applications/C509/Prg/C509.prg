// 509. Compiler crash with Chr(<unsigned>)
FUNCTION Start( ) AS VOID
	LOCAL c AS STRING
	c := "A"
	
	? Chr(65)
	? _Chr(65)
	? Chr(65L)
	? _Chr(65L)
	
	? Chr((WORD)65)
	? Chr((BYTE)65)

	LOCAL dw := 65 AS DWORD
	? Chr(dw)
	? _Chr(dw)
	
	? Chr(65U)
	? _Chr(65U)
	
	xAssert(Chr(65) == c)
	xAssert(_Chr(65) == c)
	xAssert(Chr(65L) == c)
	xAssert(_Chr(65L) == c)
	xAssert(Chr(65U) == c)
	xAssert(_Chr(65U) == c)
	xAssert(Chr(dw) == c)
	xAssert(_Chr(dw) == c)
	xAssert(Chr(WORD(65)) == c)
	xAssert(_Chr((BYTE)65) == c)
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

