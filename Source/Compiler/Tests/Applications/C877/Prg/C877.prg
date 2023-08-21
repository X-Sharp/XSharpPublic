// 877. Problems with interpolated strings
// https://github.com/X-Sharp/XSharpPublic/issues/1184
FUNCTION Start( ) AS VOID
	LOCAL n := 123 AS INT
	LOCAL c AS STRING
	c := i"{n}"
	? c
	xAssert(c == "123")

	c := i"{1}"
	? c
	xAssert(c == "1")

	c := i"{n} {{"
	? c
	xAssert(c == "123 {")

	c := i"{n} }}"
	? c
	xAssert(c == "123 }")

	c := i"{n} {{}}"
	? c
	xAssert(c == "123 {}")
RETURN

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN 	
