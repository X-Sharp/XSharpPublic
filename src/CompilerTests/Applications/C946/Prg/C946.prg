// 946. Cannot use the arr[ind1][ind2] syntax to access array members in VFP dialect
// https://github.com/X-Sharp/XSharpPublic/issues/1532

FUNCTION Start() AS VOID

	DIMENSION arr(5, 5)
	
	arr[2,3] := 123 // ok
	? arr[2,3]
	xAssert(arr[2,3] == 123)
	? arr[2][3]
	xAssert(arr[2][3] == 123)

	arr[1][4] := 456 // exception
	? arr[1][4]
	xAssert(arr[1][4] == 456)

PROC xAssert(l AS LOGIC) AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN

