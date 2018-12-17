// C643. Problems with overflows of arithmetic between literals
FUNCTION Start() AS VOID
	LOCAL f AS FLOAT
	LOCAL u AS USUAL
	? 2147483647 + 1
	? 2147483647 + 1 == 2147483648
	xAssert(2147483647 + 1 == 2147483648)
	
	// Max INT
	u := 2147483647 + 1
	f := 2147483647 + 1
	? u,f
	? u == 2147483648
	xAssert(u == 2147483648)
	xAssert(f == 2147483648)

	u := 2147483647 + 1000
	f := 2147483647 + 1000
	? u
	? u == 2147483648
	xAssert(u == 2147484647)
	xAssert(f == 2147484647)
	
	// max DWORD
	u := 4294967295 + 1
	f := 4294967295 + 1
	? u
	xAssert(u == 4294967296)
	xAssert(f == 4294967296)
	u := 4294967295 + 100
	f := 4294967295 + 100
	? u
	xAssert(u == 4294967395)
	xAssert(f == 4294967395)
	

	u := 100000 * 100000
	f := 100000 * 100000
	? u,f
	xAssert(u == 10000000000)
	xAssert(f == 10000000000)
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

