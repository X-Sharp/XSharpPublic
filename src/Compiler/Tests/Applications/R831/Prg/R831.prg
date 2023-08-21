
FUNCTION Start( ) AS VOID
    LOCAL u1, u2
    u1 := 42
    u2 := .NULL.
    ? u1     
    ? u2            
    xAssert(IsDbNull(u1 + u2 ))
    xAssert(IsDbNull(u1 - u2 ))
    xAssert(IsDbNull(u1 * u2 ))
    xAssert(IsDbNull(u1 / u2 ))
    xAssert(IsDbNull(u2 + u1 ))
    xAssert(IsDbNull(u2 - u1 ))
    xAssert(IsDbNull(u2 * u1 ))
    xAssert(IsDbNull(u2 / u1 ))

RETURN



FUNCTION IsDbNull(u as usual) AS LOGIC
    local o := u as object
    return o:Equals(DBNull.Value)



PROC xAssert(l AS LOGIC) AS VOID
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
