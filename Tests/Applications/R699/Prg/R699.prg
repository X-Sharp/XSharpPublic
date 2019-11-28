FUNCTION Start as VOID
	local cValue as STRING

	cValue := "12345"
	xAssert(Int32.TryParse(cValue, OUT NULL))
	xAssert(Int32.TryParse(cValue, OUT VAR myvalue))
	xAssert(myvalue == 12345)
	xAssert(Int32.TryParse(cValue, OUT myvalue2 as Int32))
		xAssert(Int32.TryParse(cValue, OUT VAR _))
	xAssert(myvalue2 == 12345)
	RETURN	

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
