FUNCTION Start() AS VOID STRICT

	LOCAL o AS TestClass
	LOCAL uName AS USUAL

	uName := Today() // intend for this example

	o := TestClass{}
	o:DoSomething("abc", uName)

	RETURN

CLASS TestClass
	METHOD DoSomething(s AS STRING, symName AS SYMBOL) AS VOID STRICT
		RETURN
	METHOD DoSomething(s AS STRING, names AS ARRAY OF USUAL) AS VOID STRICT
		RETURN         
// If you uncomment the next one then that will be called and no compiler error is thrown
//	METHOD DoSomething(s AS STRING, u AS USUAL) AS VOID STRICT
//		RETURN		
END CLASS
