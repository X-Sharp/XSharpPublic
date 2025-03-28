// 936. Exception with default parameters from type float
// https://github.com/X-Sharp/XSharpPublic/issues/1684

CLASS TestBase
	METHOD TestMethodNotWorking(x := 1 AS FLOAT) AS USUAL
		? i"TestMethodNotWorking {x}"
	RETURN x

	METHOD TestMethodWorking(x := 1 AS INT) AS USUAL
		? i"TestMethodWorking {x}"
	RETURN x
	
	METHOD TestNull(s := NULL_STRING AS USUAL, p := NULL_PSZ AS USUAL, o := NULL_OBJECT AS USUAL) AS USUAL // 3 errors XS037
	RETURN s
	METHOD TestNull_PTR(p := NULL_PTR AS USUAL) AS USUAL // 3 errors XS037
	RETURN AsString(p)

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

	xAssert( test:TestNull("abc") == "abc") // OK
	xAssert( test2:TestNull("abc") == "abc") // error
	
	LOCAL p AS USUAL
	p := NULL_PSZ // OK
	p := NULL_PTR // OK
	
	test:TestNull_PTR() // OK
	test:TestNull_PTR(NULL_PTR) // OK

	test2:TestNull_PTR() // error
	test2:TestNull_PTR(NULL_PTR) // OK


PROC xAssert(l AS LOGIC)
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN


//PROCEDURE TestNull(x := NULL AS FLOAT) AS VOID // no error
