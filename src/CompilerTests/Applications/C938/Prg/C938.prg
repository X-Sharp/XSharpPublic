// 938. Latebound assign of iVars

CLASS TestClass

	EXPORT nValue AS INT
	EXPORT cValue AS STRING
	EXPORT lValue AS LOGIC
	EXPORT uValue AS USUAL
	METHOD GetTrue() AS USUAL
	RETURN TRUE

END CLASS

FUNCTION Start() AS VOID
	LOCAL n AS INT
	LOCAL u AS USUAL
	u := TRUE
	n := u
	? n		// 1 OK
	xAssert(n == 1)
	u := FALSE
	n := u
	xAssert(n == 0)
	
	LOCAL t AS USUAL
	t := TestClass{}
	n := t:GetTrue()
	? n		// 1 OK
	xAssert(n == 1)
	xAssert(.not. t:GetTrue() == 0)
	
	t:nValue := TRUE
	? t:nValue
	xAssert(t:nValue == 1)

	t:nValue := .not. t:GetTrue()
	n := t:nValue
	xAssert(n == 0)
	xAssert(t:nValue == 0)

	t:nValue := t:GetTrue()
	n := t:nValue
	xAssert(n == 1)
	xAssert(t:nValue == 1)
	? n 		// 0
	? t:nValue	// 0
	
	t:cValue := "123"
	xAssert(t:cValue == "123")
	t:lValue := t:GetTrue()
	xAssert(t:lValue)
	t:uValue := t:GetTrue()
	xAssert(t:uValue)


PROC xAssert(l AS LOGIC)
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN

