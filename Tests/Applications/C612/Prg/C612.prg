// 612. Problem with LOCAL arr[length] syntax
FUNCTION Start() AS VOID
	// problem is here, compiler does not add a call to ArrayCreate()
	LOCAL a[20]
	a[20] := 20 // NullReferenceException
	xAssert(a[20] == 20)
	a[1] := 1
	xAssert(a[1] == 1)

	LOCAL aa[20] AS ARRAY // this syntax works fine
	aa[20] := 20
	xAssert(aa[20] == 20)
	aa[1] := 1
	xAssert(aa[1] == 1)
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

