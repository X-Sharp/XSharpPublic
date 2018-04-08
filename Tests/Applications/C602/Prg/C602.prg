// 602. iif(true,-1,1) returns 255 instead of 1 with /ovf

// Note: Need to make sure the code works correctly with any combination of /ovf and /vo10

FUNCTION Start() AS VOID
	LOCAL l AS LOGIC
	LOCAL f AS FLOAT
	LOCAL res AS FLOAT
	f := 1234.0

	l := TRUE
	? iif(l,-1,1) // 255
	? iif(l,-1,1) * f

	res := iif(l,-1,1)
	xAssert(res == -1)

	res := iif(l,-1,1) * f
	xAssert(res == -1234.0)

	
	l := FALSE
	? iif(l,-1,1)
	? iif(l,-1,1) * f

	res := iif(l,-1,1)
	xAssert(res == 1)

	res := iif(l,-1,1) * f
	xAssert(res == 1234.0)
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

