// R842
//See https://github.com/X-Sharp/XSharpPublic/issues/988
#pragma warnings(9073, off) // undeclared, field or memvar
FUNCTION Start( ) AS VOID
Test1()
Test2()

PROCEDURE Test1()
PRIVATE xxxx

xxxx = 12
xAssert(xxxx == 12)

xxxx(24)
? xxxx(24)
xAssert(xxxx(24) == 24)

xxxx = 36
xAssert(xxxx == 36)
RETURN

PROCEDURE Test2()

yyyy = 12
xAssert(yyyy == 12)

yyyy(24)
? yyyy(24)
xAssert(yyyy(24) == 24)

yyyy = 36
xAssert(yyyy == 36)
RETURN

FUNCTION xxxx( x )
	? x
RETURN x
FUNCTION yyyy( y )
	? y
RETURN y

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
