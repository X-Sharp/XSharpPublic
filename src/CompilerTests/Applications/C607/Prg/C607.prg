// 607. Incorrect ARRAY element accesed when using Enum as subscript

ENUM TestEnum0
	MEMBER m1
	MEMBER m2
END ENUM

ENUM TestEnum1
	MEMBER m1 := 1
	MEMBER m2 := 2
END ENUM

FUNCTION Start() AS VOID

	LOCAL a AS ARRAY
	a := {1,2,3}

//	? a[TestEnum0.m1] // must report error
	xAssert(a[TestEnum0.m2] == 1)

	xAssert(a[TestEnum1.m1] == 1)
	xAssert(a[TestEnum1.m2] == 2)
	
	a := {{1,2},{3,4},{5,6}}
	
	xAssert(a[TestEnum0.m2 , TestEnum0.m2] == 1)

	xAssert(a[TestEnum1.m1 , TestEnum1.m1] == 1)
	xAssert(a[TestEnum1.m1 , TestEnum1.m2] == 2)
	xAssert(a[TestEnum1.m2 , TestEnum1.m1] == 3)
	xAssert(a[TestEnum1.m2 , TestEnum1.m2] == 4)

	xAssert(a[(INT)TestEnum1.m1 , (INT)TestEnum1.m1] == 1)
	xAssert(a[(INT)TestEnum1.m1 , (INT)TestEnum1.m2] == 2)
	xAssert(a[(INT)TestEnum1.m2 , (INT)TestEnum1.m1] == 3)
	xAssert(a[(INT)TestEnum1.m2 , (INT)TestEnum1.m2] == 4)

	xAssert(a[TestEnum1.m1 , 1] == 1)
	xAssert(a[TestEnum1.m1 , 2] == 2)
	xAssert(a[2 , TestEnum1.m1] == 3)
	xAssert(a[2 , TestEnum1.m2] == 4)
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

