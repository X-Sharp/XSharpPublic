// 942. Invalid cast exception with assignment from usual (int) to Nullable<float>
// https://github.com/X-Sharp/XSharpPublic/issues/1748
FUNCTION Start() AS VOID
	LOCAL u := 1.1 AS USUAL
	LOCAL f := 1 AS FLOAT?

	f := u // OK
	? f
	xAssert(f == 1.1)

	u := 1.0
	f := u // OK
	? f
	xAssert(f == 1.0)

	u := 1
	f := u // Invalid cast exception
	? f
	xAssert(f == 1)
	
	LOCAL l AS LOGIC?
	u := TRUE
	l := u
	? l
	xAssert(l:HasValue .and. l:Value)


PROC xAssert(l AS LOGIC)
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN

