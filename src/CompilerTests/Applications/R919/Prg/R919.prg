
FUNCTION Start( ) AS VOID
	local e as object
	e := Exception{}
	? "Exception"
    xAssert(e is Exception)
	xAssert(!e is not Exception)
	xAssert(e is not null)
	xAssert(!e is null)
	e := "123"
	? "String"
	xAssert(!e is Exception)
	xAssert(e is not Exception)
	xAssert(e is not null)
	xAssert(!e is null)
	e := null
	? "NULL_OBJECT"
	xAssert(!e is Exception)
	xAssert(e is not Exception)
	xAssert(e is null)
	xAssert(!e is not null)
	Console.ReadLine()

RETURN


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
