// 740. Problem with passing VOSTRUCT when /vo7 is enabled
/*
error XS0029: Cannot implicitly convert type 'strTest' to 'strTest*'

Note that the error is pointing at the FUNCTION Start() line, not in the actual line where the VOSTRUCT is used
*/
// vo7+
VOSTRUCT strTest
MEMBER m1 AS INT
MEMBER m2 AS INT

FUNCTION Start() AS VOID // error XS0029 pointing here
	LOCAL w IS strTest
	LOCAL u AS USUAL
	w.m1 := 1
	w.m2 := 2
	
	u := TestClass{}
	u:Test(	@w)
RETURN

CLASS TestClass
	METHOD Test(w AS strTest)
		? w.m1
		? w.m2
		xAssert(w.m1 == 1)
		xAssert(w.m2 == 2)
	RETURN NIL
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

