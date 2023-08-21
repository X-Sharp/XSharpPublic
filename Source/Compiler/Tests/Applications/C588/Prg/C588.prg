// 588. iif() with literal negative values returns positive result (converted to BYTE)

FUNCTION Start( ) AS VOID
	LOCAL u AS USUAL
	LOCAL l := FALSE AS LOGIC
	
	u := iif(TRUE , -2 , - 3)
	? u // prints 254 instead of -2
	xAssert(u == -2)
	
	? iif(FALSE , 10 , -20)
	// there's also a warning here:
	// warning XS0652: Comparison to integral constant is useless; the constant is outside the range of type 'byte'
	xAssert(iif(l , 10 , -20) == -20)
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

