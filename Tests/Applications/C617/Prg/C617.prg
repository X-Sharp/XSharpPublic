// 617. || and && translate to binary or and and operators, instad of the logical ones

GLOBAL gnCalled AS INT

FUNCTION TestOr() AS LOGIC
	gnCalled ++
	? "TestOr() called", gnCalled, "times"
RETURN TRUE

FUNCTION TestAnd() AS LOGIC
	gnCalled ++
	? "TestAnd() called", gnCalled, "times"
RETURN FALSE

FUNCTION Start() AS VOID
	LOCAL l AS LOGIC
	
	gnCalled := 0
	l := TestOr() .or. TestOr()
	xAssert(gnCalled == 1) // ok

	gnCalled := 0
	l := TestOr() || TestOr()
	xAssert(gnCalled == 1) // error


	gnCalled := 0
	l := TestAnd() .and. TestAnd()
	xAssert(gnCalled == 1) // ok

	gnCalled := 0
	l := TestAnd() && TestAnd()
	xAssert(gnCalled == 1) // error

RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

