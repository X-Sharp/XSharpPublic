
FUNCTION Start( ) AS VOID
	LOCAL e AS OBJECT
	e := Exception{}
	? "Exception"
    xAssert(e IS Exception)
	xAssert(!e IS not Exception)
	xAssert(e IS not NULL)
	xAssert(!e IS NULL)
	e := "123"
	? "String"
	xAssert(!e IS Exception)
	xAssert(e IS not Exception)
	xAssert(e IS not NULL)
	xAssert(!e IS NULL)
	e := NULL
	? "NULL_OBJECT"
	xAssert(!e IS Exception)
	xAssert(e IS not Exception)
	xAssert(e IS NULL)
	xAssert(!e IS not NULL)

RETURN


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
