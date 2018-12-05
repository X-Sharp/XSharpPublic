// error XS0208: Cannot take the address of, get the size of, or declare a pointer to a managed type ('Vulcan.__Usual')
FUNCTION Start() AS VOID
	LOCAL DIM p[25] AS USUAL
	? @p
	? @p[1]
	? @p[2]
	xAssert(@p == @p[1])
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

