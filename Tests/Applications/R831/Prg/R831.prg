
FUNCTION Start( ) AS VOID
    LOCAL u1, u2
    u1 := 42
    u2 := .NULL.
    ? u1     
    ? u2         
    xAssert(u1 + u2 == DBNull.Value)
    xAssert(u1 - u2 == DBNull.Value)
    xAssert(u1 * u2 == DBNull.Value)
    xAssert(u1 / u2 == DBNull.Value)
    xAssert(u2 + u1 == DBNull.Value)
    xAssert(u2 - u1 == DBNull.Value)
    xAssert(u2 * u1 == DBNull.Value)
    xAssert(u2 / u1 == DBNull.Value)

RETURN





PROC xAssert(l AS LOGIC) AS VOID
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
