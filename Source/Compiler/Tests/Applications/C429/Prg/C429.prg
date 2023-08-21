// 429. compiler crash with STATIC LOCAL in CODEBLOCK
// now is error XS9045: Cannot use static local 'cString' in a codeblock or lambda expression.
FUNCTION Start() AS VOID
	Test()
FUNCTION Test() AS VOID
	STATIC LOCAL cString := "ABC" AS STRING
	LOCAL cb AS CODEBLOCK
	cb := {|x|cString += x}
	
	? Eval(cb  , "Z")
	IF Eval(cb  , "1") != "ABCZ1"
		THROW Exception{"Incorrect result"}
	ENDIF
RETURN

