// R 735 - Binary values in Core dialect
FUNCTION Start( ) AS VOID
	var b1 := 0h1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
	var b2 := 0h34
	var b3 := 0h12
	xAssert(b1:Length == 32)
	xAssert(b1[1] == 0x12)
	xAssert(b1[32] ==0xef)
	xAssert(b2:Length == 1)
	xAssert(b2[1] == 0x34)
	xAssert(b3:Length == 1)
	xAssert(b3[1] == 0x12)

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

