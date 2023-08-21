// error XS0029: Cannot implicitly convert type 'TestVOStr*' to 'TestVOStr'
// Using the ACCESS syntax works as expected

VOSTRUCT TestVOStr
MEMBER m1 AS INT
MEMBER m2 AS INT

FUNCTION Start() AS VOID
	LOCAL s AS TestVOStr
	s := GetVOStr()
	? s:m1
	? s:m2
	xAssert(s:m1 == 123)
	xAssert(s:m2 == 456)

	LOCAL o AS TestClass
	o := TestClass{}

	s := o:Test1()
	xAssert(s:m2 == 456)

	s := o:Test2()
	xAssert(s:m2 == 456)

	s := o:TestAccess1
	xAssert(s:m2 == 456)

	s := o:TestAccess2
	xAssert(s:m2 == 456)

	s := o:TestProp1
	xAssert(s:m2 == 456)

	s := o:TestProp2
	xAssert(s:m2 == 456)

RETURN

FUNCTION GetVOStr() AS TestVOStr
	LOCAL s AS TestVOStr
	s := (TestVOStr PTR)MemAlloc(Sizeof(TestVOStr))
	s:m1 := 123
	s:m2 := 456
RETURN s

CLASS TestClass
	METHOD Test1() AS TestVOStr
	RETURN GetVOStr()
	METHOD Test2() AS TestVOStr
		LOCAL s AS TestVOStr
		s := GetVOStr()
	RETURN s

	ACCESS TestAccess1 AS TestVOStr
	RETURN GetVOStr()
	ACCESS TestAccess2 AS TestVOStr
		LOCAL s AS TestVOStr
		s := GetVOStr()
	RETURN s

	PROPERTY TestProp1 AS TestVOStr GET GetVOStr()
	PROPERTY TestProp2 AS TestVOStr
		GET
			LOCAL s AS TestVOStr
			s := GetVOStr()
			RETURN s
		END GET
	END PROPERTY
END CLASS

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

