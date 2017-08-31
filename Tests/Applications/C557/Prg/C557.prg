// 557. Incorrect multi dim array indexing with usual subscripts
FUNCTION Start() AS VOID
	LOCAL a AS ARRAY
	a := {{11,12},{21,22}}
	LOCAL n AS INT
	LOCAL u AS USUAL
	n := 1
	u := 1
	? a[1,1]
	? a[n,n]
	? a[u,u]
	
	n := 2
	u := 2
	? a[2,2]
	? a[n,n]
	? a[u,u] // System.ArgumentOutOfRangeException

	xAssert(a[1,1] == 11)
	xAssert(a[1,2] == 12)
	xAssert(a[2,1] == 21)
	xAssert(a[2,2] == 22)
	
	n := 1;u := 1;xAssert(a[n,u] == 11)
	n := 1;u := 2;xAssert(a[n,u] == 12)
	n := 2;u := 1;xAssert(a[n,u] == 21)
	n := 2;u := 2;xAssert(a[n,u] == 22)

	u := 1;n := 1;xAssert(a[u,n] == 11)
	u := 1;n := 2;xAssert(a[u,n] == 12)
	u := 2;n := 1;xAssert(a[u,n] == 21)
	u := 2;n := 2;xAssert(a[u,n] == 22)



	a := { { {111,112},{121,122} } , { {211,212},{221,222} } }
	? a[1,2,1]
	? a[2,1,2]

	xAssert(a[1,2,1] == 121)
	xAssert(a[2,1,2] == 212)
	xAssert(a[2,2,1] == 221)
	xAssert(a[1,1,2] == 112)
	
	n := 1;u := 1;xAssert(a[n,u,n] == 111)
	n := 1;u := 2;xAssert(a[n,u,n] == 121)
	n := 2;u := 1;xAssert(a[n,u,n] == 212)
	n := 2;u := 2;xAssert(a[n,u,n] == 222)

	u := 1;n := 1;xAssert(a[u,n,u] == 111)
	u := 1;n := 2;xAssert(a[u,n,u] == 121)
	u := 2;n := 1;xAssert(a[u,n,u] == 212)
	u := 2;n := 2;xAssert(a[u,n,u] == 222)

RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

