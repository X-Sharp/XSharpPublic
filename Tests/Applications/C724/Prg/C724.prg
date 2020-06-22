FUNCTION Start() AS VOID
	LOCAL cb, uResult AS USUAL
	cb := &("{ || NIL }")
	uResult := Eval(cb)
	? uResult == NIL 	// TRUE, ok
	? UsualType(uResult)// 6, wrong, should be 0
	? uResult 			// "{(0x0000)0x00000000} CLASS", hould be "NIL"

xAssert(uResult == NIL)
xAssert(UsualType(uResult) == 0)
xAssert(AsString(uResult) == "NIL")
RETURN


PROC xAssert(l AS LOGIC)
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN	
