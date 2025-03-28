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

	METHOD TestNullPsz(p := NULL_PSZ AS PSZ) AS USUAL
	RETURN AsString(p)
	METHOD TestNullSymbol(s := NULL_SYMBOL AS SYMBOL) AS USUAL
	RETURN AsString(s)

END CLASS


FUNCTION Start() AS VOID STRICT
	LOCAL typed := TestBase{} AS TestBase
	LOCAL untyped AS USUAL
	untyped := typed

	xAssert( typed.TestMethodWorking() == 1)
	xAssert( untyped.TestMethodWorking() == 1)
	xAssert( typed.TestMethodWorking(2) == 2)
	xAssert( untyped.TestMethodWorking(2) == 2)


	xAssert( typed.TestMethodNotWorking() == 1)
	xAssert( untyped.TestMethodNotWorking() == 1)
	xAssert( typed.TestMethodNotWorking(2) == 2)
	xAssert( untyped.TestMethodNotWorking(2) == 2)

	xAssert( typed:TestNull("abc") == "abc") // OK
	xAssert( untyped:TestNull("abc") == "abc") // error
	
	LOCAL p AS USUAL
	p := NULL_PSZ // OK
	p := NULL_PTR // OK
	
	typed:TestNull_PTR() // OK
	typed:TestNull_PTR(NULL_PTR) // OK

	untyped:TestNull_PTR() // error
	untyped:TestNull_PTR(NULL_PTR) // OK


	xAssert( typed:TestNullPsz() == "") // OK
	xAssert( typed:TestNullPsz("abc") == "abc") // OK
	xAssert( typed:TestNullSymbol() == "") // OK
	xAssert( typed:TestNullSymbol(#abc) == "ABC") // OK

	xAssert( untyped:TestNullPsz() == "") // error
	xAssert( untyped:TestNullPsz("abc") == "abc") // OK
	xAssert( untyped:TestNullSymbol() == "") // OK
	xAssert( untyped:TestNullSymbol(#abc) == "ABC") // OK


PROC xAssert(l AS LOGIC)
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN


//PROCEDURE TestNull(x := NULL AS FLOAT) AS VOID // no error
