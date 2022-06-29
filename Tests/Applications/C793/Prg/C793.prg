// 793. Declared PUBLIC variables get created as PRIVATE in the FoxPro dialect
#pragma warnings(9073, off) //   undeclared field or memver
FUNCTION Start( ) AS VOID
	PUBLIC pub1
	pub1 = 1

	test()

	? "pub1" , pub1 // 1
	xAssert(pub1 == 1)
	? "pub2" , pub2 // runtime error: Variable does not exist
	xAssert(pub2 == 2)
RETURN

PROCEDURE Test
	? "pub1", pub1 // 1 OK, but pub1 is private
	xAssert(pub1 == 1)

	PUBLIC pub2
	pub2 = 2
	xAssert(pub2 == 2)
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
