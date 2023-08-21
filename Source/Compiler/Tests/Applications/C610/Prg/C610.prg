// 610. Accessing elements with /az-
FUNCTION Start() AS VOID
	LOCAL a AS ARRAY
	a := {{"a","b"},{"c","d","e"}}
	xAssert(a[1,1] == "a")
	xAssert(a[1,2] == "b")
	xAssert(a[2,1] == "c")
	xAssert(a[2,2] == "d")
	xAssert(a[2,3] == "e")
	xAssert(ALen(a[2])== 3)

	a[1,1] := 1
	a[1,2] := 2
	a[2,1] := 3
	a[2,2] := 4
	xAssert(a[1,1] == 1)
	xAssert(a[1,2] == 2)
	xAssert(a[2,1] == 3)
	xAssert(a[2,2] == 4)
	
	LOCAL an AS INT[,]
	an := INT[,]{2,2}
	an[1,1] := 1
	an[1,2] := 2
	an[2,1] := 3
	an[2,2] := 4
	xAssert(an[1,1] == 1)
	xAssert(an[1,2] == 2)
	xAssert(an[2,1] == 3)
	xAssert(an[2,2] == 4)
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

