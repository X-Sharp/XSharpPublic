// 740. Problem with passing VOSTRUCT when /vo7 is enabled
/*
error XS0029: Cannot implicitly convert type 'strTest' to 'strTest*'

Note that the error is pointing at the FUNCTION Start() line, not in the actual line where the VOSTRUCT is used
*/
// vo7+
VOSTRUCT strTest
MEMBER m1 AS INT
MEMBER m2 AS INT

VOSTRUCT strTest2
MEMBER p IS strTest               

CLASS xTest
    EXPORT p IS strTest                   
END CLASS    

FUNCTION Start() AS VOID // error XS0029 pointing here
	LOCAL w IS strTest
	LOCAL w2 AS strTest2
	local c  as XTest
	LOCAL u AS USUAL
	w.m1 := 1
	w.m2 := 2       
	
	u := TestClass{}
	u:Test(	@w)
	w2 := MemAlloc(sizeof(strTest2))
	w2.p.m1 := 1
	w2.p.m2 := 2
	u:Test(	@w2.p)
    MemFree(w2)
	c := xtest{}
	c:p:m1 := 1
	c:p.m2 := 2
	u:Test(	@c:p)
	
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

