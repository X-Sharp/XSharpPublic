Function Start as VOID
	local dw as DWORD
	dw := (DWORD) ToDay()
	dw -= 1
	xAssert(Date() == ToDay())
	xAssert(Date(dw) == ToDay()-1)  // this casts the number to a date, so the param is NOT passed to the function
	xAssert(ALen(Array()) == 0)
	xAssert(ALen(Array(1)) == 1)

RETURN 

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF


	
