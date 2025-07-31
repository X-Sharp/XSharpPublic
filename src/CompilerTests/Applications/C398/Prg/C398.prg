// 398. error XS0182: An attribute argument must be a constant expression, typeof expression or array creation expression of an attribute parameter type
// problem is only when /vo14+ is enabled (FLOAT decimal literals)
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	xAssert( o:_setfont("")           )
	xAssert( o:_setfont("" , 0.0)     )
	xAssert( o:AnotherTest(1) == 1         )
	xAssert( o:AnotherTest(2,123.456) == 2 )
RETURN

CLASS TestClass
METHOD _setfont(lfamily AS STRING,lsize := 0.0 AS REAL8) AS LOGIC
	? lsize
	IF lsize != 0.0
		THROW Exception{"Incorrect result"}
	END IF
RETURN TRUE
METHOD AnotherTest(n AS INT , f := 123.456 AS FLOAT) AS INT
	? f
	IF f != 123.456
		THROW Exception{"Incorrect result"}
	END IF
RETURN n
END CLASS

PROC xAssert(l AS LOGIC)
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN

