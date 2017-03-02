// 441. error XS9007: Feature 'equivalency of : and . member access operators' is not available in the selected dialect Vulcan

FUNCTION Start() AS VOID
	LOCAL s IS _MyVOSTR
	s:m1 := 456
	s.m1 := 456 // OK
	s.m2 := TRUE
	xAssert(s.m1 == 456)
	xAssert(s.m2)
	
    LOCAL DIM ddd[1] IS _MyVOSTR
    ddd[1]:m1 := 123
	xAssert(ddd[1].m1 == 123)
    
    ddd[1].m1 := 1234 // error
	xAssert(ddd[1].m1 == 1234)

	TestClass{}:Test()
RETURN

VOSTRUCT _MyVOSTR
MEMBER m1 AS INT
MEMBER m2 AS LOGIC

CLASS TestClass
	PROTECT vostr AS _MyVOSTR
CONSTRUCTOR 
	vostr := MemAlloc(_sizeof(_MyVoStr))	
METHOD Test() AS VOID
	LOCAL s IS _MyVOSTR
	s:m1 := 123
	s.m1 := 123 // OK
	? s.m1
	? s:m1
	
	SELF:vostr:m1 := 5
	xAssert(SELF:vostr.m1 == 5)
	SELF:vostr.m1 := 15 // Error
	xAssert(SELF:vostr:m1 == 15)
	
RETURN

END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

