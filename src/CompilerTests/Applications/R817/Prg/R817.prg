FUNCTION Start() AS VOID STRICT

	LOCAL o AS TestClass
	LOCAL uName AS USUAL

	uName := Today() // intend for this example

	o := TestClass{}
	xAssert(o:DoSomething("abc", uName) == Today())

	RETURN

CLASS TestClass
	METHOD DoSomething(s AS STRING, symName AS SYMBOL) AS USUAL STRICT
		RETURN s
	METHOD DoSomething(s AS STRING, names AS ARRAY OF USUAL) AS USUAL STRICT
		RETURN s
// If you uncomment the next one then that will be called and no compiler error is thrown
	METHOD DoSomething(s AS STRING, u AS USUAL) AS USUAL STRICT
	    ? u
		RETURN u
END CLASS



PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
