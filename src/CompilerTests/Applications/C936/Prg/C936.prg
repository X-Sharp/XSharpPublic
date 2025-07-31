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

	METHOD TestNullable1(n := NULL AS Nullable<INT>) AS USUAL
		IF .not. n:HasValue
			RETURN 0
		END IF
	RETURN n:Value
	METHOD TestNullable2(n := NULL AS INT?) AS USUAL
		IF .not. n:HasValue
			RETURN 0
		END IF
	RETURN n:Value
	METHOD TestNullable3(n := NULL AS INT?) AS INT
		IF .not. n:HasValue
			RETURN 0
		END IF
	RETURN n:Value
	METHOD TestNullable4(n := NULL AS INT?) AS INT?
	RETURN n

	METHOD TestNullable5(d := NULL AS DateTime?) AS USUAL
		IF .not. d:HasValue
			RETURN 0
		END IF
	RETURN d:Value
	METHOD TestNullable6(d := NULL AS DateTime?) AS DateTime
		IF .not. d:HasValue
			RETURN DateTime{1,1,1}
		END IF
	RETURN d:Value
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
	xAssert( untyped.TestMethodNotWorking() == 1) // did not work < 2.23
	xAssert( typed.TestMethodNotWorking(2) == 2)
	xAssert( untyped.TestMethodNotWorking(2) == 2)

	xAssert( typed:TestNull("abc") == "abc") // OK
	xAssert( untyped:TestNull("abc") == "abc") // did not work < 2.23
	
	LOCAL p AS USUAL
	p := NULL_PSZ // OK
	p := NULL_PTR // OK
	
	typed:TestNull_PTR() // OK
	typed:TestNull_PTR(NULL_PTR) // OK

	untyped:TestNull_PTR() // did not work < 2.23
	untyped:TestNull_PTR(NULL_PTR) // OK


	xAssert( typed:TestNullPsz() == "") // OK
	xAssert( typed:TestNullPsz("abc") == "abc") // OK
	xAssert( typed:TestNullSymbol() == "") // OK
	xAssert( typed:TestNullSymbol(#abc) == "ABC") // OK

	xAssert( untyped:TestNullPsz() == "") // did not work < 2.23
	xAssert( untyped:TestNullPsz("abc") == "abc") // OK
	xAssert( untyped:TestNullSymbol() == "") // OK
	xAssert( untyped:TestNullSymbol(#abc) == "ABC") // OK

	xAssert( typed:TestNullable1() == 0)
	xAssert( typed:TestNullable1(0) == 0)
	xAssert( typed:TestNullable1(1) == 1)
	xAssert( typed:TestNullable2() == 0)
	xAssert( typed:TestNullable2(0) == 0)
	xAssert( typed:TestNullable2(1) == 1)
	xAssert( typed:TestNullable3() == 0)
	xAssert( typed:TestNullable3(0) == 0)
	xAssert( typed:TestNullable3(1) == 1)
	xAssert( .not. typed:TestNullable4():HasValue )
	xAssert( typed:TestNullable4(0):HasValue)
	xAssert( typed:TestNullable4(1):Value == 1)
	xAssert( typed:TestNullable5() == 0)
	xAssert( typed:TestNullable5(DateTime{1976,7,5}) == DateTime{1976,7,5})
	xAssert( typed:TestNullable6() == DateTime{1,1,1})
	xAssert( typed:TestNullable6(DateTime{1976,7,5}) == DateTime{1976,7,5})

	// following 3 commented lines did not work in builds < 2.23 either
//	xAssert( untyped:TestNullable1() == 0) // System.InvalidCastException: Null object cannot be converted to a value type.
	xAssert( untyped:TestNullable1(0) == 0)
	xAssert( untyped:TestNullable1(1) == 1)
//	xAssert( untyped:TestNullable2() == 0) // System.InvalidCastException: Null object cannot be converted to a value type.
	xAssert( untyped:TestNullable2(0) == 0)
	xAssert( untyped:TestNullable2(1) == 1)
//	xAssert( untyped:TestNullable3() == 0) // System.InvalidCastException: Null object cannot be converted to a value type.
	xAssert( untyped:TestNullable3(0) == 0)
	xAssert( untyped:TestNullable3(1) == 1)
//	xAssert( .not. untyped:TestNullable4():HasValue ) // did not work < 2.23
//	xAssert( untyped:TestNullable4(0):HasValue) // did not work < 2.23
//	xAssert( untyped:TestNullable4(1):Value == 1) // did not work < 2.23
//	xAssert( untyped:TestNullable5() == 0) // did not work < 2.23
	xAssert( untyped:TestNullable5(DateTime{1976,7,5}) == DateTime{1976,7,5})
//	xAssert( untyped:TestNullable6() == DateTime{1,1,1}) // did not work < 2.23
	xAssert( untyped:TestNullable6(DateTime{1976,7,5}) == DateTime{1976,7,5})


PROC xAssert(l AS LOGIC)
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN


//PROCEDURE TestNull(x := NULL AS FLOAT) AS VOID // no error
