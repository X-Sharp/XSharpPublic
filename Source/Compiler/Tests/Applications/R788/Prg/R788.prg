// R788 LOCAL and Dimension. 
// https://github.com/X-Sharp/XSharpPublic/issues/683
FUNCTION Start( ) AS VOID
    LOCAL a
    Dimension a(10) 
    ? ALen(a)
    xAssert(ALen(a) == 10)
RETURN


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
