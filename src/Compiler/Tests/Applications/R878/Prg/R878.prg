// https://github.com/X-Sharp/XSharpPublic/issues/1178

PUBLIC GLOBAL dSQL_NULL_DATETIME := {^1900-01-01:00:00:00} as datetime
FUNCTION Start( ) AS VOID
    xAssert (   dSQL_NULL_DATETIME == DateTime{1900,1,1,0,0,0})
RETURN



PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
