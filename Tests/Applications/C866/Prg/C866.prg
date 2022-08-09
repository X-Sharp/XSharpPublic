#pragma warnings(9043, off)
FUNCTION Start() AS VOID
LOCAL u AS USUAL
u := "abc"
? FGetFileName(u)
xAssert(FGetFileName(u) == 123)
RETURN



PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
