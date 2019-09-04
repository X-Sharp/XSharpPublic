
Function Start as void
	local o as object
	local u as usual
	o := Error{}
	u := (Usual) o  
	XAssert(u != null_object)
	u := USUAL(_CAST, o)
	XAssert(u != null_object)
	u := USUAL(o)
	XAssert(u != null_object)
	return
	
PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"	
