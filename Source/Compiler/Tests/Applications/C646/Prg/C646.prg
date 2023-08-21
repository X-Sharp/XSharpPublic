// 646. error XS0266: Cannot implicitly convert type 'int' to 'TestEnum'. An explicit conversion exists (are you missing a cast?)
ENUM TestEnum
	MEMBER m1
	MEMBER m2
	MEMBER m3
	MEMBER m4
	MEMBER m5
END ENUM

FUNCTION Start() AS VOID
	LOCAL e AS TestEnum
	e := TestEnum.m3
	? e:ToString()
	xAssert(e == TestEnum.m3)
	
	e += 1
	? e:ToString()
	xAssert(e == TestEnum.m4)

	e -= 1
	? e:ToString()
	xAssert(e == TestEnum.m3)
	
	e := e + 2
	? e:ToString()
	xAssert(e == TestEnum.m5)

	e := e - 3
	? e:ToString()
	xAssert(e == TestEnum.m2)
	
	LOCAL n AS INT
	n := 2
	e := (TestEnum)n
	? e:ToString()
	xAssert(e == TestEnum.m3)
	
	e := e + n
	? e:ToString()
	xAssert(e == TestEnum.m5)
	
	n := 1
	e := e - n
	? e:ToString()
	xAssert(e == TestEnum.m4)
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

