// 936. Exception with default parameters from type float
// https://github.com/X-Sharp/XSharpPublic/issues/1684

CLASS TestBase
	METHOD TestMethodNotWorking(x := 1 AS FLOAT) AS USUAL
		? i"TestMethodNotWorking {x}"
	RETURN x
	
	METHOD TestMethodWorking(x := 1 AS INT) AS USUAL
		? i"TestMethodWorking {x}"
	RETURN x

END CLASS

FUNCTION Start() AS VOID STRICT
	LOCAL test := TestBase{} AS TestBase
	LOCAL test2 AS USUAL
	test2 := test
	
	xAssert( test.TestMethodWorking() == 1)
	xAssert( test2.TestMethodWorking() == 1)
	xAssert( test.TestMethodWorking(2) == 2)
	xAssert( test2.TestMethodWorking(2) == 2)
	
	
	xAssert( test.TestMethodNotWorking() == 1)
	xAssert( test2.TestMethodNotWorking() == 1)
	xAssert( test.TestMethodNotWorking(2) == 2)
	xAssert( test2.TestMethodNotWorking(2) == 2)
	
	
	
	
PROC xAssert(l AS LOGIC)
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
