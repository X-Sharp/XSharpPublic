// 615. Problem with LOCAL a[<len>] AS USUAL syntax
/*
This stupid syntax is supported in VO (and in Vulcan).
In x# it does not give a compiler error, but also does not create an array, causing the program to fail at runtime
So we either need to make the "a[] AS USUAL" syntax a synonym for "a[] AS ARRAY", 
or make this syntax report a compiler error instead.
*/
FUNCTION Start( ) AS VOID
	LOCAL a[5] AS USUAL
	xAssert(ALen(a) == 5)
	xAssert(a[1] == NIL)
	xAssert(a[5] == NIL)
	
	LOCAL a2[3] AS ARRAY
	xAssert(ALen(a2) == 3)
	LOCAL a3[4]
	xAssert(ALen(a3) == 4)
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
