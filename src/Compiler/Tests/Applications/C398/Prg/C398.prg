// 398. error XS0182: An attribute argument must be a constant expression, typeof expression or array creation expression of an attribute parameter type
// problem is only when /vo14+ is enabled (FLOAT decimal literals)
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	o:_setfont("")
	o:_setfont("" , 0.0)
	o:AnotherTest(1)
	o:AnotherTest(1,123.456)
RETURN

CLASS TestClass
METHOD _setfont(lfamily AS STRING,lsize := 0.0 AS REAL8) AS LOGIC
	? lsize
	IF lsize != 0.0
		THROW Exception{"Incorrect result"}
	END IF
RETURN TRUE
METHOD AnotherTest(n AS INT , f := 123.456 AS FLOAT) AS VOID
	? f
	IF f != 123.456
		THROW Exception{"Incorrect result"}
	END IF
RETURN
END CLASS
