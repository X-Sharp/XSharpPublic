FUNCTION Start() AS VOID
	LOCAL a AS ARRAY
	a := {{"a","b"},{"c","d","e"}}
	xAssert(a[0,0] == "a")
	xAssert(a[0,1] == "b")
	xAssert(a[1,0] == "c")
	xAssert(a[1,1] == "d")
	xAssert(a[1,2] == "e")
	xAssert(ALen(a[1])== 3)

	a[0,0] := 1
	a[0,1] := 2
	a[1,0] := 3
	a[1,1] := 4
	xAssert(a[0,0] == 1)
	xAssert(a[0,1] == 2)
	xAssert(a[1,0] == 3)
	xAssert(a[1,1] == 4)
	
	LOCAL an AS INT[,]
	an := INT[,]{2,2}
	an[0,0] := 1
	an[0,1] := 2
	an[1,0] := 3
	an[1,1] := 4
	xAssert(an[0,0] == 1)
	xAssert(an[0,1] == 2)
	xAssert(an[1,0] == 3)
	xAssert(an[1,1] == 4)
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

