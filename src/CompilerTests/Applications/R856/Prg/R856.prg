#pragma options("vo7", on)
#pragma warnings(9071, off) // out expected
FUNCTION Start() AS VOID
    LOCAL nOldArea     AS DWORD
    LOCAL nArea := 0 AS USUAL
    VoDbSelect(nArea,@nOldArea)
    xAssert(nOldArea == 1)
RETURN



PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
