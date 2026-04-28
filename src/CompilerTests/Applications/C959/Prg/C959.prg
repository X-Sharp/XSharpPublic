// Calling a suitable overload for a parameter of type USUAL [#836]
// https://github.com/X-Sharp/XSharpPublic/issues/836
FUNCTION Start() AS VOID STRICT

	LOCAL o AS TestClass
	LOCAL uName AS USUAL
	
	uName := Today() // intend for this example
	
	o := TestClass{}
	o:DoSomething(uName)

	RETURN

CLASS TestClass
	METHOD DoSomething(symName AS SYMBOL) AS VOID STRICT
		RETURN
	METHOD DoSomething(names AS ARRAY OF SYMBOL) AS VOID STRICT
		RETURN
END CLASS
