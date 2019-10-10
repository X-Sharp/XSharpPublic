Function Start as void
	local a,b as usual
	a := 10
	b := 20
	Test(a,,@b)
	xAssert(a == 10)
	xAssert(b == 300)                                                                     
	
	
	
function Test(a as usual, b := NIL out usual, c := NIL out usual, d := NIL out usual)  as void
	a := 100
	b := 200
	c := 300
	d := 400
	
PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"		
